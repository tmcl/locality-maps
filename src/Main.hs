{-# LANGUAGE Rank2Types #-}

module Main (main)
where

import Control.Monad.Trans.Class
import qualified Graphics.PDF as Pdf
import EachMap

{- todo yet.
 -
 - * state/territory location maps
 - * canberra — D vs G??
 - * cc, cx, ni
 -}

import Data.Csv (decodeByName)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import UpdatedMapper
import Unicode
import           ClassyPrelude                (traceM, traceShowId, trace)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as CC
import qualified Data.Conduit.List            as CL
import           Data.Dbase.Conduit
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import qualified Data.Vector                  as V
import           Data.Vector                  (Vector)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Geometry.Shapefile.Conduit
import           System.Environment
import           FindLocalities hiding (lgaColumnName)
import Data.List
import Utils 

import Types
import Municipality
import NewMap
import BaseMap
import Settings
import SpecialCases

getSpecialCases ∷ IO SpecialCaseMap
getSpecialCases = do
   csv ← BS.readFile "places-out.csv"
   return $ convert (specialCases csv)
   where
      convert (Left e) = error e
      convert (Right r) = r

main ∷ IO ()
main = do
   special ← getSpecialCases
   getArgs >>= Main.mapThing special

mapThing' ∷ SpecialCaseMap → [(Mappable, String)] → FilePath → FilePath → IO ()
mapThing' scm [(Named named, _)] source dest = 
   runRSettingsT 
      (mapBoundingBoxAsMappable (namedName named) (namedBBox named) dest) 
      (RunSettings (withFiles source) scm)
mapThing' scm [(Arbitrary bbox, _)] source dest = 
   runRSettingsT 
      (mapBoundingBoxAsMappable "custom" bbox dest) 
      (RunSettings (withFiles source) scm)
mapThing' scm [(Suburb suburb, _)] source dest = 
   runRSettingsT 
      (mapLocalityAsMappable suburb dest) 
      (RunSettings (withFiles source) scm)
mapThing' scm [(MappableMunicipalities m, _)] source dest = 
   runRSettingsT 
      (mapMunicipalitiesInMappable m dest) 
      (RunSettings (withFiles source) scm)
mapThing' scm [(City city, _)] source dest = 
   runRSettingsT 
      (mapMunicipalitiesInMappable city dest) 
      (RunSettings (withFiles source) scm)
mapThing' scm [(State state, _)] source dest = 
   runRSettingsT 
      (mapMunicipalitiesInMappable state dest) 
      (RunSettings (withFiles source) scm)
mapThing' _ b c d = error $ "Please give me a mappable thing and an in and an out" ⧺ show b ⧺ "/" ⧺ show c ⧺ ":" ⧺ show d

mapThing ∷ SpecialCaseMap → [String] → IO ()
mapThing scm (state:source:dest:_) = mapThing' scm (traceShowId $ reads $ traceShowId state) source dest
mapThing _ _ = error "I wanted a mappable thing and an in and an out"

municipalitiesByFilePath ∷ FilePath → State → IO (Set Municipality)
municipalitiesByFilePath fp state = fmap S.fromList <$> runResourceT $ CB.sourceFile (toDbf fp)
  =$= dbfConduit
  =$= CL.mapMaybe (\f →
      Municipality state
         <$> (dbfFieldCharacter <$> readLgaColumnLongName f)
         <*> (dbfFieldCharacter <$> readLgaColumnName f))
  =$= CL.groupBy (==)
  =$= CC.filter ((0 <) . length)
  =$= CL.mapMaybe uncons
  =$= CC.map fst
  -- =$= CC.take 2 -- todo while debugging only
  $$ CC.sinkList


allFilter ∷ a → Bool
allFilter = const True

localityFilter ∷ Shape → Bool
localityFilter = matchTextDbfField "G" localityTypeColumn

districtFilter ∷ Shape → Bool
districtFilter = matchTextDbfField "D" localityTypeColumn

localityTypeColumn ∷ Text → Bool
localityTypeColumn t = "_LOCA_5" `T.isSuffixOf` t || "_LOCAL_5" `T.isSuffixOf` t

-- municipalityStateName ∷ Municipality → Text
-- municipalityStateName = stateName . mState
-- 
-- stateName ∷ State → Text
-- stateName NSW = "New South Wales"
-- stateName Vic = "Victoria"
-- stateName Qld = "Queensland"
-- stateName WA = "Western Australia"
-- stateName SA = "South Australia"
-- stateName Tas = "Tasmania"
-- stateName NT = "Northern Territory"
-- stateName OT = "Other Territories"
-- stateName ACT = "ACT + Jervis Bay"

-- localityFilePathsByState ∷ State → FilePaths → Yielder
-- localityFilePathsByState NSW fps = [(nswLocalities fps, allFilter)]
-- localityFilePathsByState Vic fps = [(vicLocalities fps, allFilter)]
-- localityFilePathsByState Qld fps = [(qldLocalities fps, allFilter)]
-- localityFilePathsByState WA  fps = [(waLocalities  fps, allFilter)]
-- localityFilePathsByState SA  fps = [(saLocalities  fps, localityFilter)]
-- localityFilePathsByState Tas fps = [(tasLocalities fps, allFilter)]
-- localityFilePathsByState NT  fps = [(ntLocalities  fps, allFilter)]
-- localityFilePathsByState OT  fps = [(otLocalities  fps, allFilter)]
-- localityFilePathsByState ACT fps = (actLocalities fps, districtFilter)
--    : localityFilePathsByState OT fps

-- muniFileName ∷ Municipality -> Text
-- muniFileName m = T.replace "/" "-" (mName m)

settingsFromShapefileStream ∷ (Monad m) ⇒ Shape → RunSettingsT m Settings
settingsFromShapefileStream (header, _, _) = do
   rs ← ask
   return $ settingsFromRecBBox rs 0.1 . toRecBB . shpBB $ header

settingsFromRecBBox ∷ RunSettings
                    → Complex Double 
                    → RecBBox 
                    → Settings
settingsFromRecBBox rs amount bbox =
   withDefaultSettings rs . resizeBoundingBox amount $ bbox

settingsFromState ∷ State → RunSettingsT IO (Maybe Settings)
settingsFromState state = do
   fps ← asks rsFilePaths
   shape ← lift $ shapeSource (municipalityFilePathByState state fps) Nothing CL.head 
   settingsFromShapefileStream <$$> shape

(<$$>) ∷ (Monad m) 
       ⇒ (a → RunSettingsT m b) 
       → Maybe a
       → RunSettingsT m (Maybe b)
f <$$> v = maybe (return Nothing) f' v
   where
      f' v = f v ⇉ return . Just

metros ∷ ByteString → [MetroArea]
metros = decoded . decodeByName
   where
      decoded (Left a) = error a
      decoded (Right (_, v)) = V.toList v

metrosByCity ∷ City → ByteString → [MunicipalityShortName]
metrosByCity city bs = map metroAreaMuni $ filter (\(MetroArea c _) → c ≡ city) (metros bs)

getMetrosByCity ∷ City → IO [MunicipalityShortName]
getMetrosByCity city = metrosByCity city <$> BS.readFile "metro.csv"

anyMatch ∷ [a → Bool] → a → Bool
anyMatch fs s = or (applyMap fs s)

applyMap ∷ [a → b] → a → [b]
applyMap fs v = map ($ v) fs

bboxFromMunicipality ∷ Municipality → RunSettingsT IO (Maybe RecBBox)
bboxFromMunicipality muni = do
   fps ← asks rsFilePaths
   let matchMunis = mName muni `matchTextDbfField` lgaColumnName
       state = mState muni
   lift $ municipalitySourceByState fps state (CC.filter matchMunis =$= shapesToRecBBox)

bboxFromMunicipalities ∷ [Municipality] → RunSettingsT IO (Maybe RecBBox)
bboxFromMunicipalities munis = do
   bboxen ← mapM bboxFromMunicipality munis ∷ RunSettingsT IO [Maybe RecBBox]
   liftT $ lift $ foldl' bigBoundingBox Nothing bboxen

settingsFromMunicipalities ∷ [Municipality] → State → RunSettingsT IO (Maybe Settings)
settingsFromMunicipalities munis state = do
   bbox' ← bboxFromMunicipalities munis
   rs ← ask
   (return . settingsFromRecBBox rs 0.05) <$$> bbox'

settingsFromCity ∷ City → RunSettingsT IO (Maybe Settings)
settingsFromCity city = do
   munis ← getMunicipalities city
   settingsFromMunicipalities munis (cityToState city)

settingsFromMunicipality ∷ RunSettings 
                         → Municipality 
                         → IO (Maybe Settings)
settingsFromMunicipality rs municipality = do
   let fps = rsFilePaths rs
   bbox ← municipalitySource fps municipality Nothing conduit
   return $ settingsFromRecBBox rs 0.1 <$> bbox
   where
    conduit ∷ Sink Shape IO (Maybe RecBBox)
    conduit = CC.filter (matchMunicipality municipality)
      =$= CC.map shapeToBBox
      =$= CL.fold bigBoundingBox Nothing
      

shapesToRecBBox ∷ Sink Shape IO (Maybe RecBBox)
shapesToRecBBox = CC.map shapeToBBox =$= CL.fold bigBoundingBox Nothing

shapeToBBox ∷ Shape → Maybe RecBBox
shapeToBBox (_, shp, _) = shpRecBBox shp

mapMunicipality ∷ PenGetter → Municipality → SettingsT IO (Pdf.PDF XForm)
mapMunicipality mapper muni = do
   m ← mapMuni mapper muni
   mCircle ← mapMuni2 mapper muni
   let drawing = writeTitle (muniLongName muni) >> m >> mCircle
   xformify' drawing

mapLocality2 ∷ Text 
             → Locality 
             → SettingsT IO (Pdf.PDF XForm)
mapLocality2 context locality = do
   let mln = show context
   lift $ putStrLn $ show locality ⧺ " in " ⧺ mln
   l ← mapLoc'y [locality]
   let title = T.concat [fixMc locality, " in ", context]
       drawing = writeTitle title >> l
   xformify' drawing
   
mapLocalitiesByMunicipality ∷ Municipality 
                            → SettingsT IO [Pdf.PDF XForm]
mapLocalitiesByMunicipality m = do
   fps ← asks filePaths
   manyLocalities ← lift $ 
      localitiesByMunicipality 
         (mName m) 
         (municipalityFilePathByMunicipality fps m) 
         (localityFilePaths fps)
   cases ← asks settingsSpecialCases
   let scs = traceShowId (M.lookup m $ traceShowId cases)
   let localities = maybe 
                      (S.toList manyLocalities) 
                      (localitiesWithoutErrors $ S.toList manyLocalities) 
                      scs
   lift $ mapM_ print localities
   mapM (mapLocality2 $ muniLongName m) localities

localitiesWithoutErrors ∷ [Locality] → Map Text SpecialCase → [Locality]
localitiesWithoutErrors locs maps = locs2 `without` errors2
   where 
      locs2 = trace (show locs) locs
      errors2 = trace (show errors) errors
      errors = M.keysSet $ M.filter (ApparentError ≡) maps

without ∷ Ord a ⇒ [a] → Set a → [a]
a `without` b = filter (`S.notMember` b) a

writeTitle ∷ Text → Pdf.Draw ()
writeTitle text = Pdf.drawText $ do
   let font = Pdf.PDFFont Pdf.Times_Roman 15 
   Pdf.setFont font
   Pdf.textStart 10 10
   Pdf.leading $ Pdf.getHeight font
   Pdf.renderMode Pdf.FillText
   Pdf.displayText (Pdf.toPDFString $ T.unpack text)

-- todo specialcase mackay


muzzle ∷ Rect → FilePath → Pdf.PDF () → IO ()
muzzle rect out = Pdf.runPdf 
  out
  Pdf.standardDocInfo {Pdf.author="tr", Pdf.compressed = False} 
  (rectToPdfRect rect)

mozzle ∷ [XForm] → Rect → Pdf.PDF ()
mozzle drawings rect = do 
   traceM "."
   page ← Pdf.addPage (Just $ rectToPdfRect rect)
   Pdf.drawWithPage page $! mapM_ Pdf.drawXObject drawings

mapMunicipalities ∷ SettingsT IO (Pdf.PDF XForm)
mapMunicipalities =
   xformify' ⇇ mapMunis (outlineCoordinates municipalLines)

mapMunicipalitiesInState1 ∷ [State] → [Municipality] → SettingsT IO (Pdf.PDF ())
mapMunicipalitiesInState1 states munis = do
  lift $ mapM_ (print . muniLongName) munis
  coastPoints ← mapCoast Nothing 
  riverPoints ← xformify' ⇇ mapRivers
  urbanPoints ← mapUrbanAreas 
  statePoints ← xformify' ⇇ mapStates states
  munisPoints ← mapMunicipalities 
  traceM $ show munis
  let points = [coastPoints, 
                statePoints, 
                urbanPoints, 
                riverPoints, 
                munisPoints]
  muniPoints ← mapM (mapMunicipality highlitArea) munis
  liftT $ combineXFormsIntoPages (sequence points) muniPoints
  
mapMunicipalityLocally ∷ Settings
                       → FilePath
                       → String
                       → Municipality
                       → RunSettingsT IO ()
mapMunicipalityLocally s out context muni = do
  pdf2 ← lift $ runReaderT (mapMunicipalityLocally2 muni) s
  let name = T.unpack $ muniLongName muni
      fn2 = out ⧺ "/" ⧺ name ⧺ " and " ⧺ context
  lift $ muzzle (settingsRect s) fn2 $! pdf2

mapMunicipalityBackground ∷ Municipality 
                         → SettingsT IO [Pdf.PDF XForm]
mapMunicipalityBackground muni =
  sequence [mapCoast (Just $ mState muni),
            xformify' ⇇ mapMuniCropped subjectArea muni, 
            mapUrbanAreas, 
            mapReserves,
            xformify' ⇇ mapRivers, 
            mapLakes, 
            xformify' ⇇ mapMuni subjectAreaHighlight muni,
            mapMunicipalities, 
            xformify' ⇇ mapLocalities (outlineCoordinates localityLines)]

mapMunicipalityLocally2 ∷ Municipality → SettingsT IO (Pdf.PDF ())
mapMunicipalityLocally2 muni = do
  bg ← mapMunicipalityBackground muni
  fg ← mapLocalitiesByMunicipality muni
  liftT $ combineXFormsIntoPages (sequence bg) fg

class Show a ⇒ IMappable a where
   getStates ∷ a → [State]
   getMunicipalities ∷ a → RunSettingsT IO [Municipality]
   getSettings ∷ a → RunSettingsT IO (Maybe Settings)
   getSettingsByMunicipality ∷ a 
                             → Settings  -- contextual settings
                             → RunSettings
                             → Municipality -- that we're mapping
                             → IO (Maybe Settings)

instance IMappable Municipalities where
   getStates (Municipalities m mm) = map mState (m:mm)
   getMunicipalities (Municipalities m mm) = return (m:mm)
   getSettings (Municipalities m mm) = settingsFromMunicipalities (m:mm) (mState m)
   getSettingsByMunicipality _ settings _ _ = return $ return settings

instance IMappable City where
   getStates s = [cityToState s]
   getMunicipalities = municipalitiesByCity
   getSettings = settingsFromCity
   getSettingsByMunicipality _ settings _ _ = return $ return settings

instance IMappable State where
   getStates s = [s]
   getMunicipalities = municipalitiesByState
   getSettings = settingsFromState
   getSettingsByMunicipality _ _ = settingsFromMunicipality 

municipalitiesByCity ∷ City → RunSettingsT IO [Municipality]
municipalitiesByCity city = do
  let state = cityToState city
  munis ← lift $ getMetrosByCity city
  munisByState ← municipalitiesByState state
  let munis2 = filterMunis munis state munisByState 
      muniNames = map mName munisByState
      allMatch = all ((∈ muniNames) `orF` T.isPrefixOf "??")
      munis3 = if allMatch munis 
               then munis2 
               else error $ show muniNames ⧺ " vs " ⧺ show munis
  return munis3

mapMunicipalitiesInMappable ∷ IMappable a 
                            ⇒ a
                            → FilePath 
                            → RunSettingsT IO ()
mapMunicipalitiesInMappable it out = do
   let fn = out ⧺ "/" ⧺ show it
   rs ← ask
   munis' ← getMunicipalities it
   --- let munis = filter (\l → mName l ≡ "BRIGHTON" ) munis'
   let munis = munis'
   Just settings ← getSettings it
   let mapLocally muni = do
          Just s ← lift $ getSettingsByMunicipality it settings rs muni
          mapMunicipalityLocally s out (show it) muni
   lift $ muzzle (settingsRect settings) fn
      ⇇ runReaderT (mapMunicipalitiesInState1 (getStates it) munis) settings
   mapM_ mapLocally munis

bboxFromPolygons ∷ [Vector Point] → Maybe RecBBox
bboxFromPolygons = foldl' addToBoundingBox Nothing

addToBoundingBox ∷ Maybe RecBBox → Vector Point → Maybe RecBBox
addToBoundingBox box p = bigBoundingBox box (Just $ bboxFromPoints p)

bboxSink ∷ Consumer Shape IO (Maybe RecBBox)
bboxSink = CL.fold (\b (_, a, _) → bigBoundingBox b (shpRecBBox a)) Nothing

mapBoundingBoxAsMappable ∷ Text → RecBBox → FilePath → RunSettingsT IO ()
mapBoundingBoxAsMappable label bbox out = do
   rs ← ask
   fps ← asks rsFilePaths
   let municipality = Municipality Vic "MOUNT ISA CITY" "MOUNT ISA"
       fn = out ⧺ "/" ⧺ show label ⧺ " - " ⧺ show bbox
       settings = settingsFromRecBBox rs 0.1 bbox
       mapLocally = do
         background ← mapMunicipalityBackground municipality
         localities ← lift $ localitiesByBoundingBox bbox (localityFilePaths fps)
         fg ← mapM (mapLocality2 label) (S.toList localities)
         liftT $ combineXFormsIntoPages (sequence background) fg
   pdf2 ← lift $ runReaderT mapLocally settings
   lift $ muzzle (settingsRect settings) fn pdf2


mapLocalityAsMappable ∷ [Text] → FilePath → RunSettingsT IO ()
mapLocalityAsMappable them out = do
   let fn = out ⧺ "/" ⧺ show them
   rs ← ask
   bbox ← getLocalitiesByNames them bboxSink
   lift $ print bbox
   let municipality = Municipality Vic "MOUNT ISA CITY" "MOUNT ISA"
       Just s = settingsFromRecBBox rs 0.1 <$> bbox
       squizzle = do
         background ← mapMunicipalityBackground municipality
         fg ← mapM (mapLocality2 "custom") them
         liftT $ combineXFormsIntoPages (sequence background) fg
   pdf2 ← lift $ runReaderT squizzle s
   lift $ muzzle (settingsRect s) fn pdf2

orF ∷ (a → Bool) → (a → Bool) → a → Bool
orF p q a = p a ∨ q a

municipalityFilePathByCity = municipalityFilePathByState . cityToState

filterMunis ∷ [MunicipalityShortName] → State → [Municipality] → [Municipality]
filterMunis toKeep state = filter (\s → mState s ≡ state ∧ mName s ∈ toKeep)

municipalitiesByState ∷ State → RunSettingsT IO [Municipality]
municipalitiesByState state = do
   fps ← asks rsFilePaths
   lift $ S.toList <$> municipalitiesByFilePath (municipalityFilePathByState state fps) state
   
combineXFormsIntoPages ∷ Pdf.PDF [XForm] → [Pdf.PDF XForm] → SettingsT Pdf.PDF ()
combineXFormsIntoPages maps pages = do
   maps' ← lift maps
   pages' ← lift $ sequence pages
   rect ← asks settingsRect
   lift $ mozzle maps' rect
   lift $ mapM_ (\pageMap → mozzle (maps' ⧺ [pageMap]) rect) pages'

--todo hm. convert to conduit?
{- todo this algorithm leaves everything too griddy
reducePrecision ∷ (RealFloat a, RealFloat b) ⇒ Int → a → b
reducePrecision places' number = encodeFloat (s `div` (floatRadix number)^amount) places
   where
      places = -places'
      (s, e) = decodeFloat number
      amount = places - e

isLike ∷ (Num a, Ord a) ⇒ a → a → a → Bool
isLike epsilon x y = x - epsilon < y && x + epsilon > y

reduceShapePrecision ∷ RecBBox → [Point] → [Point]
reduceShapePrecision bbox boxes = 
   if range < 0.5 
      then boxes 
      else reduceShapePrecision' . reverse $ boxes
   where
     range = max (recXMax bbox - recXMin bbox) (recYMax bbox - recYMin bbox)
     places 
        | range < 1 = 11
        | 1 <= range && range < 2 = 10 
        | 2 <= range && range < 5 = 9 
        | 5 <= range && range < 10 = 8
        | otherwise = 7
     e = encodeFloat 2 (-(places + 2))
     isNear = isLike e
     reduceShapePrecision' = foldr reducer []  
     reducer (x1', y1') points@((x2, y2):_) = 
         if x1 `isNear` x2 && y1 `isNear` y2 then 
            points 
         else 
            (x1, y1):points
         where
           x1 = reducePrecision places x1'
           y1 = reducePrecision places y1'
     reducer new old = new:old -}


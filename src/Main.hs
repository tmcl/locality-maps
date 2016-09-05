{-# LANGUAGE Rank2Types, BangPatterns #-}

module Main (main)
where

import Control.Monad.Trans.Class
import qualified Graphics.PDF as Pdf
import EachMap

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
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Geometry.Shapefile.Conduit
import           System.Environment
import           FindLocalities hiding (lgaColumnName)
import Data.List
import Utils 
--import System.Directory
--import Control.Monad

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
   getArgs >>= Main.mapState special

mapState ∷ SpecialCaseMap → [String] → IO ()
-- mapState ("cities":source:dest:_) = mapCities (withFiles source) dest
mapState scm (state:source:dest:_) =
   runRSettingsT 
      (mapMunicipalitiesInState (read state) dest)
      (RunSettings (withFiles source) scm)
mapState _ _ = error "I expect three arguments: a state/'cities', and source and dest folders"

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
settingsFromShapefileStream (header, _, _) = 
   settingsFromRecBBox . toRecBB . shpBB $ header

settingsFromRecBBox ∷ (Monad m) ⇒ RecBBox → RunSettingsT m Settings
settingsFromRecBBox bbox = do
   rs ← ask
   return $ withDefaultSettings rs . embiggenBoundingBox $ bbox


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

settingsFromMunicipality ∷ Municipality → RunSettingsT IO (Maybe Settings)
settingsFromMunicipality municipality = do
   fps ← asks rsFilePaths
   let bbox = municipalitySource fps municipality Nothing conduit
   bbox' ← lift bbox
   settingsFromRecBBox <$$> bbox'
  where
    conduit ∷ Sink Shape IO (Maybe RecBBox)
    conduit = CC.filter (matchMunicipality municipality)
      =$= CC.map shapeToBBox
      =$= CL.fold bigBoundingBox Nothing

shapeToBBox ∷ Shape → Maybe RecBBox
shapeToBBox (_, shp, _) = shpRecBBox shp

mapMunicipality ∷ PenGetter → Municipality → SettingsT IO (Pdf.PDF XForm)
mapMunicipality mapper muni = do
   m ← mapMuni mapper muni
   mCircle ← mapMuni2 mapper muni
   let drawing = writeTitle (muniLongName muni) >> m >> mCircle
   xformify' drawing

mapLocality2 ∷ Municipality 
             → Locality 
             → SettingsT IO (Pdf.PDF XForm)
mapLocality2 muni locality = do
   let mln = show (muniLongName muni)
   lift$putStrLn$(show locality)⧺" in "⧺mln
   l ← mapLoc'y (fillCoordinates narrowArea) locality
   ll ← mapLoki2 narrowLines locality
   let title = T.concat [locality, " in ", muniLongName muni]
       drawing = writeTitle title >> l >> ll
   xformify' drawing
   
mapLocality ∷ Municipality → SettingsT IO [Pdf.PDF XForm]
mapLocality m = do
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
   mapM (mapLocality2 m) localities

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
   let font = Pdf.PDFFont Pdf.Times_Roman 70 
   Pdf.setFont font
   Pdf.textStart 100 100
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
mozzle (!drawings) rect = do 
   page ← Pdf.addPage (Just $ rectToPdfRect rect)
   Pdf.drawWithPage page $! mapM_ Pdf.drawXObject drawings

mapMunicipalities ∷ PenGetter → SettingsT IO (Pdf.PDF XForm)
mapMunicipalities pen = 
   xformify' ⇇ mapMunis (outlineCoordinates pen)

mapMunicipalitiesInState1 ∷ State → [Municipality] → SettingsT IO (Pdf.PDF ())
mapMunicipalitiesInState1 state munis = do
  lift $ mapM_ (print . muniLongName) munis
  coastPoints ← mapCoast
  riverPoints ← xformify' ⇇ mapRivers
  urbanPoints ← mapUrbanAreas 
  statePoints ← xformify' ⇇ EachMap.mapState state
  munisPoints ← mapMunicipalities narrowLines
  traceM $ show munis
  let points = [coastPoints, 
                statePoints, 
                urbanPoints, 
                riverPoints, 
                munisPoints]
  muniPoints ← mapM (mapMunicipality narrowArea) munis
  liftT $ runMagic (sequence points) muniPoints
  
mapMunicipalitiesLocally1 ∷ FilePath
                          → [Municipality] 
                          → RunSettingsT IO ()
mapMunicipalitiesLocally1 out munis = do 
  traceM $ show munis
  mapM_ (mapMunicipalityLocally1 out) munis

mapMunicipalityLocally1 ∷ FilePath 
                        → Municipality 
                        → RunSettingsT IO ()
mapMunicipalityLocally1 out muni = do
  Just settings ← settingsFromMunicipality muni
  pdf2 ← lift $ runReaderT (mapMunicipalityLocally2 muni) settings
  let state = show $ mState muni
      name = T.unpack $ muniLongName muni
      fn2 = out ⧺ "/" ⧺ name ⧺ " and " ⧺ state ⧺ ".pdf"
  lift $ muzzle (settingsRect settings) fn2 $! pdf2

mapMunicipalityLocally2 ∷ Municipality → SettingsT IO (Pdf.PDF ())
mapMunicipalityLocally2 muni = do
  coastPoints ← mapCoast
  riverPoints ← xformify' ⇇ mapRivers
  (lakePoints1, lakePoints2) ← mapLakes
  urbanPoints ← mapUrbanAreas 
  broadBorders ← mapMunicipalities broadLines
  broadPoints ← xformify' ⇇ mapMuni broadArea muni
  narrowBorders ← xformify' ⇇ mapLocalities (outlineCoordinates narrowLines)
  let points = [coastPoints, 
                broadPoints, 
                riverPoints, 
                lakePoints1, 
                lakePoints2, 
                broadBorders, 
                narrowBorders,
                urbanPoints]
  muniPoints ← mapLocality muni
  liftT $ runMagic (sequence points) muniPoints
  

mapMunicipalitiesInState ∷ State → FilePath → RunSettingsT IO ()
mapMunicipalitiesInState state out = do
  let fn1 = out ⧺ "/" ⧺ show state ⧺ "1.pdf"
  Just settings ← settingsFromState state
  fps ← asks rsFilePaths
  munis ← lift $ S.toList 
    <$> municipalitiesByFilePath (municipalityFilePathByState state fps) state
  lift $ muzzle (settingsRect settings) fn1 
    ⇇ strictly (runReaderT (mapMunicipalitiesInState1 state munis) settings)
  strictly $ mapMunicipalitiesLocally1 out munis

strictly ∷ a → a
strictly a = seq a a

runMagic ∷ Pdf.PDF [XForm] → [Pdf.PDF XForm] → SettingsT Pdf.PDF ()
runMagic maps pages = do
   maps' ← lift maps
   pages' ← lift $ sequence pages
   rect ← asks settingsRect
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


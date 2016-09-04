{-# LANGUAGE Rank2Types, BangPatterns #-}

module Main (main)
where

import Control.Monad.Trans.Class
import qualified Graphics.PDF as Pdf
import EachMap

import UpdatedMapper
import Unicode
import           ClassyPrelude                (traceM)
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

main ∷ IO ()
main = getArgs >>= Main.mapState

mapState ∷ [String] → IO ()
-- mapState ("cities":source:dest:_) = mapCities (withFiles source) dest
mapState (state:source:dest:_) = mapMunicipalitiesInState (withFiles source) (read state) dest
mapState _ = error "I expect three arguments: a state/'cities', and source and dest folders"

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

-- muniFileName :: Municipality -> Text
-- muniFileName m = T.replace "/" "-" (mName m)

settingsFromShapefileStream ∷ FilePaths -> Shape → Settings
settingsFromShapefileStream fps (header, _, _) = 
   settingsFromRecBBox fps . toRecBB . shpBB $ header

settingsFromRecBBox ∷ FilePaths → RecBBox → Settings
settingsFromRecBBox fps = withDefaultSettings fps . embiggenBoundingBox


settingsFromState ∷ FilePaths → State → IO (Maybe Settings)
settingsFromState fps state = settingsFromShapefileStream fps 
   <$$> shapeSource (municipalityFilePathByState state fps) Nothing CL.head

(<$$>) ∷ (Monad m, Monad n) => (a → b) → m (n a) → m (n b)
(<$$>) a b = fmap a <$> b

settingsFromMunicipality ∷ FilePaths → Municipality → IO (Maybe Settings)
settingsFromMunicipality fps municipality = settingsFromRecBBox fps <$$> bbox
  where
    conduit ∷ Sink Shape IO (Maybe RecBBox)
    conduit = CC.filter (matchMunicipality municipality)
      =$= CC.map shapeToBBox
      =$= CL.fold bigBoundingBox Nothing
    bbox = municipalitySource fps municipality Nothing conduit


shapeToBBox ∷ Shape → Maybe RecBBox
shapeToBBox (_, shp, _) = shpRecBBox shp



-- localitySources ∷ FilePaths → Maybe RecBBox → Sink Shape IO a → IO [a]
-- localitySources fps = multiSources (localityFilePaths fps)

-- TODO mapMunicipality and this are ~identical + filter
-- stuffed naming convention

mapMunicipality ∷ PenGetter → Municipality → SettingsT IO (Pdf.PDF XForm)
mapMunicipality mapper muni = do
   m ← mapMuni mapper muni
   let drawing = writeTitle (muniLongName muni) >> m
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
   localities ← lift $ localitiesByMunicipality (mName m) (municipalityFilePathByMunicipality fps m) (localityFilePaths fps)
   lift $ mapM_ print localities
   mapM (mapLocality2 m) $ S.toList localities

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
                          → FilePaths 
                          → [Municipality] 
                          → IO ()
mapMunicipalitiesLocally1 out fps munis = do 
  traceM $ show munis
  mapM_ (mapMunicipalityLocally1 out fps) munis

mapMunicipalityLocally1 ∷ FilePath 
                        → FilePaths 
                        → Municipality → IO ()
mapMunicipalityLocally1 out fps muni = do
  Just settings ← settingsFromMunicipality fps muni
  pdf2 ← runReaderT (mapMunicipalityLocally2 muni) settings
  let state = show $ mState muni
      name = T.unpack $ muniLongName muni
      fn2 = out ⧺ "/" ⧺ name ⧺ " and " ⧺ state ⧺ ".pdf"
  muzzle (settingsRect settings) fn2 $! pdf2

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
  

mapMunicipalitiesInState ∷ FilePaths → State → FilePath → IO ()
mapMunicipalitiesInState fps state out = do
  let fn1 = out ⧺ "/" ⧺ show state ⧺ "1.pdf"
  Just settings ← settingsFromState fps state
  munis ← S.toList <$> municipalitiesByFilePath (municipalityFilePathByState state fps) state
  muzzle (settingsRect settings) fn1 ⇇ strictly ( runReaderT (mapMunicipalitiesInState1 state munis) settings)
  strictly $ mapMunicipalitiesLocally1 out fps munis

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


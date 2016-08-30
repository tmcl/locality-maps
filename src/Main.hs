{-# LANGUAGE Rank2Types #-}
module Main (main)
where

import Data.Complex
import Prelude.Unicode
import           Algebra.Clipper
import           ClassyPrelude                (traceM, traceShowId)
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B8
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as CC
import qualified Data.Conduit.List            as CL
import           Data.Dbase.Conduit
import           Data.Maybe
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Geometry.Shapefile.Conduit
import           Map                          hiding (mapCoast)
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Process
import           FindLocalities hiding (lgaColumnName)
import Data.List
import Utils
import System.Directory
import Control.Monad

import qualified Data.Vector as V
import Types
import Municipality
import NewMap
import Graphics.PDF (runPdf, addPage, drawWithPage, standardDocInfo, author, compressed, fillColor, addPolygonToPath, Draw, PDF)
import qualified Graphics.PDF as Pdf
import PolygonReduce

main ∷ IO ()
main = getArgs >>= mapState

mapState ∷ [String] → IO ()
mapState ("cities":source:dest:_) = mapCities (withFiles source) dest
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
  $$ CC.sinkList


municipalityFilePaths ∷ FilePaths → Yielder
municipalityFilePaths fps = [ (nswMunicipalities fps, allFilter),
                              (vicMunicipalities fps, allFilter),
                              (qldMunicipalities fps, allFilter),
                              (waMunicipalities fps, allFilter),
                              (saMunicipalities fps, allFilter),
                              (tasMunicipalities fps, allFilter),
                              (ntMunicipalities fps, allFilter),
                              (actMunicipalities fps, allFilter),
                              (otMunicipalities fps, allFilter)
                            ]

localityFilePaths ∷ FilePaths → Yielder
localityFilePaths fps = [ (nswLocalities fps, allFilter),
                          (vicLocalities  fps, allFilter),
                          (qldLocalities  fps, allFilter),
                          (waLocalities fps, allFilter),
                          (saLocalities fps, localityFilter),
                          (tasLocalities  fps, allFilter),
                          (ntLocalities fps, allFilter),
                          (otLocalities fps, allFilter),
                          (actLocalities fps, districtFilter)
                        ]

allFilter ∷ a → Bool
allFilter = const True

localityFilter ∷ Shape → Bool
localityFilter = matchTextDbfField "G" localityTypeColumn

districtFilter ∷ Shape → Bool
districtFilter = matchTextDbfField "D" localityTypeColumn

localityTypeColumn ∷ Text → Bool
localityTypeColumn t = "_LOCA_5" `T.isSuffixOf` t || "_LOCAL_5" `T.isSuffixOf` t

toDbf ∷ FilePath → FilePath
toDbf = (`replaceExtension` "dbf")

municipalityFilePathByState ∷ State → FilePaths → FilePath
municipalityFilePathByState NSW = nswMunicipalities
municipalityFilePathByState Vic = vicMunicipalities
municipalityFilePathByState Qld = qldMunicipalities
municipalityFilePathByState WA  = waMunicipalities
municipalityFilePathByState SA  = saMunicipalities
municipalityFilePathByState Tas = tasMunicipalities
municipalityFilePathByState NT  = ntMunicipalities
municipalityFilePathByState OT  = otMunicipalities
municipalityFilePathByState ACT  = actMunicipalities

municipalityStateName ∷ Municipality → Text
municipalityStateName = stateName . muniState
stateName ∷ State → Text
stateName NSW = "New South Wales"
stateName Vic = "Victoria"
stateName Qld = "Queensland"
stateName WA = "Western Australia"
stateName SA = "South Australia"
stateName Tas = "Tasmania"
stateName NT = "Northern Territory"
stateName OT = "Other Territories"
stateName ACT = "ACT + Jervis Bay"

localityFilePathsByState ∷ State → FilePaths → Yielder
localityFilePathsByState NSW fps = [(nswLocalities fps, allFilter)]
localityFilePathsByState Vic fps = [(vicLocalities fps, allFilter)]
localityFilePathsByState Qld fps = [(qldLocalities fps, allFilter)]
localityFilePathsByState WA  fps = [(waLocalities  fps, allFilter)]
localityFilePathsByState SA  fps = [(saLocalities  fps, localityFilter)]
localityFilePathsByState Tas fps = [(tasLocalities fps, allFilter)]
localityFilePathsByState NT  fps = [(ntLocalities  fps, allFilter)]
localityFilePathsByState OT  fps = [(otLocalities  fps, allFilter)]
localityFilePathsByState ACT fps = (actLocalities fps, districtFilter)
   : (localityFilePathsByState OT fps)

muniFileName :: Municipality -> Text
muniFileName m = T.replace "/" "-" (muniName m)

settingsFromShapefileStream ∷ Shape → Settings
settingsFromShapefileStream (header, _, _) = 
   settingsFromRecBBox . toRecBB . shpBB $ header

settingsFromRecBBox ∷ RecBBox → Settings
settingsFromRecBBox = withDefaultSettings . embiggenBoundingBox . traceShowId

municipalityFilePathByMunicipality ∷ FilePaths → Municipality → FilePath
municipalityFilePathByMunicipality fps muni = municipalityFilePathByState (muniState muni) fps

settingsFromState ∷ FilePaths → State → IO (Maybe Settings)
settingsFromState fps state = settingsFromShapefileStream 
   <$$> shapeSource (municipalityFilePathByState state fps) Nothing CL.head

(<$$>) ∷ (Monad m, Monad n) => (a → b) → m (n a) → m (n b)
(<$$>) a b = fmap a <$> b

settingsFromMunicipality ∷ FilePaths → Municipality → IO (Maybe Settings)
settingsFromMunicipality fps municipality = settingsFromRecBBox <$$> bbox
  where
    conduit ∷ Sink Shape IO (Maybe RecBBox)
    conduit = (CC.filter (matchMunicipality municipality)
      =$= CC.map shapeToBBox
      =$= CL.fold bigBoundingBox Nothing)
    bbox = municipalitySource fps municipality Nothing conduit


settingsFromShape ∷ Shape → Maybe Settings
settingsFromShape (_, shape, _) = settingsFromRecBBox <$> shpRecBBox shape

shapeToBBox ∷ Shape → Maybe RecBBox
shapeToBBox (_, shp, _) = shpRecBBox shp

withShpFile ∷ FilePath → (Handle → Handle → IO a) → IO a
withShpFile filePath cb =
  withFile filePath ReadMode $ \shp →
    withFile (toDbf filePath) ReadMode $ \dbf →
      cb shp dbf

shapeSource ∷ FilePath → Maybe RecBBox → Sink Shape IO a → IO a
shapeSource fp bbox sink = withShpFile fp $ \shp dbf →
   shapesFromDbfShpSource bbox shp dbf $$ sink

municipalitySource ∷ FilePaths → Municipality → Maybe RecBBox → Sink Shape IO a → IO a
municipalitySource fps muni = shapeSource (municipalityFilePathByMunicipality fps muni)

municipalitySources ∷ FilePaths → Maybe RecBBox → Sink Shape IO a → IO [a]
municipalitySources fps = multiSources (municipalityFilePaths fps)

localitySources ∷ FilePaths → Maybe RecBBox → Sink Shape IO a → IO [a]
localitySources fps = multiSources (localityFilePaths fps)

multiSources ∷ Yielder → Maybe RecBBox → Sink Shape IO a → IO [a]
multiSources yielder bbox sink = mapM go yielder
  where
    go (filePath, filter') = shapeSource filePath bbox (CC.filter filter' =$= sink)

matchUrbanAreaType ∷ Text → Shape → Bool
matchUrbanAreaType = (`matchTextDbfField` (== "SOS_NAME11"))

matchFeatCode ∷ Text → Shape → Bool
matchFeatCode = (`matchTextDbfField` (== "FEAT_CODE"))

mapUrbanAreas3 ∷ Rect → Settings 
                 → [[Point]] → [[Point]] → [[Point]] 
                 → Draw ()
mapUrbanAreas3 rect settings bLoc othUrban majUrban = do
   let bbox = boundingBox settings
       matrix = matrixForBBox rect bbox
       width = invertScale matrix 1
   Pdf.applyMatrix matrix
   mapM_ (fillPoints bbox (width/4) (ptc $ boundedLocality settings)) bLoc
   mapM_ (fillPoints bbox (width/4) (ptc $ otherUrban settings)) othUrban
   mapM_ (fillPoints bbox (width/4) (ptc $ majorUrban settings)) majUrban


ptc ∷ Pen → Pdf.Color
ptc = colorToColor . penColor

colorToColor ∷ Color → Pdf.Color
colorToColor (Color r g b) = Pdf.Rgb (toRatio r) (toRatio g) (toRatio b)
   where toRatio n = (fromIntegral n) / 255

type Point = Complex Double
   
fillPoints ∷ RecBBox → Double → Pdf.Color → [Point] → Draw ()
fillPoints bbox epsilon color points = do
   addPolygonToPath . V.toList . reduce epsilon . clipPath bbox . V.fromList 
      $ points
   fillColor color
   Pdf.fillPathEO

mapUrbanAreas2 ∷ FilePaths → Settings → IO ([[Point]], [[Point]], [[Point]])
mapUrbanAreas2 fps settings = do
  (bLoc', othUrban', majUrban') <- shapeSource (urbanAreas fps) (Just $ boundingBox settings)
    (CC.foldl (\(a, b, c) shape →
      if matchUrbanAreaType "Bounded Locality" shape then (shape:a, b, c) else
      if matchUrbanAreaType "Other Urban" shape then (a, shape:b, c) else
      if matchUrbanAreaType "Major Urban" shape then (a, b, shape:c) else
      (a, b, c)) ([], [], []))
  bLoc <- CC.yieldMany bLoc'
    =$= eachPolygon 
    $$ CC.sinkList
  othUrban <- CC.yieldMany othUrban'
    =$= eachPolygon
    $$ CC.sinkList
  majUrban <- CC.yieldMany majUrban'
    =$= eachPolygon
    $$ CC.sinkList
  return (concat bLoc, concat othUrban, concat majUrban)

mapUrbanAreas ∷ FilePaths → Settings → IO [ByteString]
mapUrbanAreas fps settings = do
   let bbox = boundingBox settings
   (bLoc', othUrban', majUrban') <- shapeSource (urbanAreas fps) (Just bbox)
     (CC.foldl (\(a, b, c) shape →
       if matchUrbanAreaType "Bounded Locality" shape then (shape:a, b, c) else
       if matchUrbanAreaType "Other Urban" shape then (a, shape:b, c) else
       if matchUrbanAreaType "Major Urban" shape then (a, b, shape:c) else
       (a, b, c)) ([], [], []))
   bLoc <- CC.yieldMany bLoc'
     =$= eachPolygon
     =$= mapPoints3 settings (boundedLocality settings)
     $$ CC.sinkList
   othUrban <- CC.yieldMany othUrban'
     =$= eachPolygon
     =$= mapPoints3 settings (otherUrban settings)
     $$ CC.sinkList
   majUrban <- CC.yieldMany majUrban'
     =$= eachPolygon
     =$= mapPoints3 settings (majorUrban settings)
     $$ CC.sinkList
   let rect = pageFromBBox bbox
   (b1, o1, m1) ← mapUrbanAreas2 fps settings
   runPdf 
      "marjorie.pdf" 
      standardDocInfo {author="tmap", compressed = False}
      (rectToPdfRect rect)
      (mozzle (mapUrbanAreas3 rect settings b1 o1 m1) rect)
         
   return $ concat [bLoc, othUrban, majUrban, undefined]

mozzle ∷ Draw () → Rect → PDF ()
mozzle drawing rect = do 
   page ← addPage (Just $ rectToPdfRect rect)
   drawWithPage page $ do
      drawing
      Pdf.strokeColor Pdf.red
      Pdf.stroke $ Pdf.Rectangle (10 :+ 0) (200 :+ 300)
 
mapCoast ∷ FilePaths → Settings → IO ByteString
mapCoast fps settings = do
   let bbox = boundingBox settings
   lands <- shapeSource (states fps) (Just bbox)
      (CC.filter (not . matchFeatCode "sea")
        =$= eachPlace 
        =$= CC.concat
        =$= CC.sinkList)
   let landPolygons = Polygons (map toClipperPolygon lands)
   Polygons seaPolygons <- (Polygons [toClipperPolygon . bboxToPolygon $ bbox] <-> landPolygons)
   let sea = map fromClipperPolygon seaPolygons
   mappedSea <- CC.yieldMany [sea] =$= mapPoints3 settings (water settings) $$ CC.sinkList
   mappedLand <- CC.yieldMany [lands] =$= mapPoints3 settings (land settings) $$ CC.sinkList
   return $ B8.concat (mappedSea ++ mappedLand)

bboxToPolygon ∷ RecBBox → [Point]
bboxToPolygon box = [
   lowerLeft box,
   lowerRight box,
   upperRight box,
   upperLeft box,
   lowerLeft box]

lowerRight ∷ RecBBox → Point
lowerRight box = recXMax box :+ recYMin box
upperLeft ∷ RecBBox → Point
upperLeft box = recXMin box :+ recYMax box

-- TODO this and that are ~identical -localitySources +municipalitySources
mapMunicipalities ∷ FilePaths → Settings → Pen → IO [ByteString]
mapMunicipalities fps settings pen = concat <$>
   (municipalitySources fps (Just $ boundingBox settings) (mapLines settings pen))

-- TODO that and this are ~identical +localitySources -municipalitySources
-- TODO this and that are ~identical -filter, areas/lines
mapLocalities ∷ FilePaths → Settings → Pen → IO [ByteString]
mapLocalities fps settings pen = concat <$>
   (localitySources fps (Just $ boundingBox settings) (mapLines settings pen))

-- TODO that and this are ~identical + filter, areas/lines
mapLocality' ∷ FilePaths → Locality → Settings → Pen → IO [ByteString]
mapLocality' fps locality settings pen = concat <$>
   (localitySources fps (Just $ boundingBox settings)
      (CC.filter (matchLocality locality) =$= mapAreas settings pen))

-- TODO that and this are ~identical + filter
mapMunicipality ∷ FilePaths → Municipality → Settings → Pen → IO [ByteString]
mapMunicipality fps muni settings pen =
   (municipalitySource fps muni (Just $ boundingBox settings)
      (CC.filter (matchMunicipality muni) =$= mapAreas settings pen))

-- TODO that and this are ~identical + filter
-- stuffed naming convention
mapStateLocally ∷ FilePaths → Settings → State → Pen → IO [ByteString]
mapStateLocally fps settings state pen =
  shapeSource (states fps) (Just $ boundingBox settings)
    (CC.filter (matchState state) =$= mapAreas settings pen)

mapAreas ∷ Settings → Pen → Sink Shape IO [ByteString]
mapAreas settings pen = eachPlace =$= mapPoints3 settings pen =$= CC.sinkList
mapLines ∷ Settings → Pen → Sink Shape IO [ByteString]
mapLines settings pen = eachPolygon =$= mapPoints3 settings pen =$= CC.sinkList

mapRivers ∷ FilePaths → Settings → IO [ByteString]
mapRivers fps settings = shapeSource (rivers fps) (Just $ boundingBox settings)
     (mapLines settings (riverPen settings))

mapLakes ∷ FilePaths → Settings → IO [ByteString]
mapLakes fps settings = do
  let justWater = CC.filter (matchType "riverbank" ⋁ matchType "water")
  filled <- shapeSource (lakes fps) (Just $ boundingBox settings)
    (justWater =$= mapAreas settings (water settings))
  outlined <- shapeSource (lakes fps) (Just $ boundingBox settings)
    (justWater =$= mapLines settings (riverPen settings))
  return $ filled ++ outlined

(⋁) ∷ (a → Bool) → (a → Bool) → (a → Bool)
(p ⋁ q) a = p a ∨ q a

mapMunicipalityLocally ∷ FilePaths → Municipality → FilePath → IO ()
mapMunicipalityLocally fps muni out = do
  Just settings <- settingsFromMunicipality fps muni
  title <- mapTitle settings (muniLongName muni)
  baseMap <- makeBaseMap fps settings
  rivers' <- mapRivers fps settings
  lakes' <- mapLakes fps settings
  munisPoints <- mapMunicipalities fps settings (broadLines settings)
  muniPoints <- mapMunicipality fps muni settings (broadArea settings)
  localitiesPoints <- mapLocalities fps settings (narrowLines settings)
  finalisation <- closeMap settings
  let base = baseMap ++ rivers'  ++ lakes'  ++ munisPoints ++ muniPoints ++ localitiesPoints
  writeMap
    (base ++ [title, finalisation])
    (out ++ "/" ++ T.unpack (muniFileName muni) ++ " (" ++ show (muniState muni) ++ ").pdf")
  localities <- localitiesByMunicipality (muniName muni) (municipalityFilePathByMunicipality fps muni) (localityFilePathsByState (muniState muni) fps)
  mapM_ (\l → mapLocalityInMunicipality fps (out ++ "/" ++ T.unpack l ++ " in " ++ T.unpack (muniFileName muni) ++ " and "  ++ show (muniState muni) ++ ".pdf") settings base finalisation muni l) localities


--
-- todo specialcase mackay

mapCities ∷ FilePaths → FilePath → IO ()
mapCities fps out = getCities fps >>= mapM_ (mapCity fps out)

getCities ∷ FilePaths → IO [(Int, Shape)]
getCities fps = shapeSource (urbanAreas fps) Nothing
   (CC.filter (matchUrbanAreaType "Major Urban") =$=
      CC.concatMap separatePolies =$=
      CC.foldl (\acc next → (length acc, next):acc) [])

separatePolies ∷ Shape → [Shape]
separatePolies s@(a, shape, c) = maybe [s] bar (shpRecContents shape)
   where
     bar p@(RecPolygon {recPolPoints = fs}) = 
         map (\f → (a, shape { shpRecContents = Just (p { recPolPoints = [f] }) }, c)) fs
     bar _ = [s]

mapCity ∷ Show a => FilePaths → FilePath → (a, Shape) → IO ()
mapCity fps out (ident, city) = do
   print ident
   let Just settings' = (settingsFromShape city)
       settings = settings' {majorUrban = colorTheMunicipality_}
   title <- mapTitle settings "A city, but I don't know which"
   baseMap <- makeBaseMap fps settings
   rivers' <- mapRivers fps settings
   lakes' <- mapLakes fps settings
   munisPoints <- mapMunicipalities fps settings (broadLines settings)
   localitiesPoints <- mapLocalities fps settings (narrowLines settings)
   finalisation <- closeMap settings
   let base = baseMap ++ rivers' ++ lakes' ++ munisPoints ++ localitiesPoints
   writeMap
     (base ++ [title, finalisation])
     (out ++ "/" ++ "city " ++ show ident ++ ".pdf")

mapMunicipalitiesInState ∷ FilePaths → State → FilePath → IO ()
mapMunicipalitiesInState fps state out = do
  Just settings <- settingsFromState fps state
  traceM $ show (boundingBox settings)
  baseMap <- makeBaseMap fps settings
  finalisation <- closeMap settings
  statePoints <- mapStateLocally fps settings state (broadArea settings)
  munisPoints <- mapMunicipalities fps settings (narrowLines settings)
  munis' <- municipalitiesByFilePath (municipalityFilePathByState state fps) state
  let munis = munis'
  --let munis = S.filter (\l → any (`T.isInfixOf` muniName l) ["KING"] ) munis'
  mapM_ (mapMunicipalityInState fps out settings (baseMap ++ statePoints ++ munisPoints) finalisation) munis

mapMunicipalityInState ∷ FilePaths → FilePath → Settings → [ByteString] → ByteString → Municipality → IO ()
mapMunicipalityInState fps out settings baseMap finalisation muni = do
  print muni
  let fn = (out ++ "/" ++ show (muniState muni) ++ " showing " ++ T.unpack (muniFileName muni) ++ ".pdf")
  exists <- doesFileExist fn
  when (not exists) $ do
     title <- mapTitle settings (muniLongName muni)
     municipalityPoints <- mapMunicipality fps muni settings (narrowArea settings)
     writeMap
       (baseMap ++ municipalityPoints ++ [title, finalisation])
       fn
     mapMunicipalityLocally fps muni out

mapLocalityInMunicipality ∷ FilePaths → FilePath → Settings → [ByteString] → ByteString → Municipality → Locality → IO ()
mapLocalityInMunicipality fps out settings baseMap finalisation muni locality = do
  print locality
  title <- mapTitle settings (T.concat [locality, " in ", muniLongName muni, " and ", municipalityStateName muni])
  municipalityPoints <- mapLocality' fps locality settings (narrowArea settings)
  writeMap
    (baseMap ++ municipalityPoints ++ [title, finalisation])
    out

makeBaseMap ∷ FilePaths → Settings → IO [ByteString]
makeBaseMap fps settings = do
   initialisation <- initialiseMap settings
   coastMap <- mapCoast fps settings
   urbanPoints <- mapUrbanAreas fps settings
   return $ [initialisation, coastMap] ++ urbanPoints

writeMap ∷ [ByteString] → FilePath → IO ()
--writeMap bs fp = B8.writeFile fp (B8.concat bs)
writeMap bytestrings filename = do
  _ <- readProcess "epstopdf" ["-f", "-o=" ++ filename] (B8.unpack . B8.concat $ bytestrings)
  return ()

pointsFromRecord ∷ ShpRec → [[Point]]
pointsFromRecord r = concatMap pointsFromRecContents (catMaybes [shpRecContents r])

pointsFromRecContents ∷ RecContents → [[Point]]
pointsFromRecContents r@RecPolygon {}  = recPolPoints r
pointsFromRecContents r@RecPolyLine {} = recPolLPoints r
pointsFromRecContents _                = []

eachPolygon ∷ Conduit (a, ShpRec, b) IO [[Point]]
eachPolygon = CC.map (\(_, r, _) → pointsFromRecord r)

eachPlace ∷ Conduit (a, ShpRec, b) IO [[Point]]
eachPlace = CC.map (\(_, r, _) →
   return . concat . andReverseTheRest . (map wrapEnds) . pointsFromRecord $ r)

andReverseTheRest ∷ [[a]] → [[a]]
andReverseTheRest (first:rest) = first:(intersperse [last first] rest)
andReverseTheRest a = a

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

wrapEnds ∷ [Point] → [Point]
wrapEnds [] = []
wrapEnds [a] = [a]
wrapEnds line = (last line):line

matchType ∷ T.Text → Shape → Bool
matchType t = matchTextDbfField t (== "type")

matchMunicipality ∷ Municipality → Shape → Bool
matchMunicipality m = matchTextDbfField (muniName m) lgaColumnName

matchState ∷ State → Shape → Bool
matchState s = matchNumericDbfField (stateCode s) stateCodeColumnName

matchLocality ∷ Locality → Shape → Bool
matchLocality m = matchTextDbfField m localityColumnName

lgaColumnName ∷ Text → Bool
lgaColumnName t = "_LGA__3" `T.isSuffixOf` t || "_LGA_s_3" `T.isSuffixOf` t

stateCodeColumnName ∷ Text → Bool
stateCodeColumnName = (== "STATE_CODE")

stateCode ∷ State → Int
stateCode ACT = 1
stateCode OT = 2
stateCode NSW = 3
stateCode NT = 4
stateCode Qld = 5
stateCode SA = 6
stateCode Tas = 7
stateCode Vic = 8
stateCode WA = 9

lgaColumnLongName ∷ Text → Bool
lgaColumnLongName t = "_LGA__2" `T.isSuffixOf` t || "_LGA_s_2" `T.isSuffixOf` t

readLgaColumnLongName ∷ DbfRow → Maybe DbfField
readLgaColumnLongName = shapeFieldByColumnNameRule lgaColumnLongName
readLgaColumnName ∷ DbfRow → Maybe DbfField
readLgaColumnName = shapeFieldByColumnNameRule lgaColumnName


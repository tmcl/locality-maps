{-# LANGUAGE Rank2Types #-}
module Main (main)
where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as CC
import qualified Data.Conduit.List            as CL
import           Data.Dbase.Conduit
import           Data.Dbase.Parser
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Geometry.Shapefile.Conduit
import           Geometry.Shapefile.Types
import           System.Environment
import           System.FilePath
import           System.IO
import qualified Data.Set as S
import Data.Set (Set)
import System.Process
import qualified Data.ByteString.Char8 as B8

import Map hiding (mapCoast)
import qualified Map
import FindLocalities hiding (lgaColumnName)
import Utils
import Data.List
import Algebra.Clipper

snoc a b = a ++ b

main ∷ IO ()
main = getArgs >>= mapState

mapState ∷ [String] → IO ()
mapState (state:source:dest:_) = mapSomeState (read state) (withFiles source) dest
mapState _ = error "I expect three arguments: a state, the source folder and the dest folder"

mapSomeState ∷ State → FilePaths → FilePath → IO ()
mapSomeState state fps dest = mapMunicipalitiesInState fps state dest

municipalitiesByFilePath ∷ FilePath → State → IO (Set Municipality)
municipalitiesByFilePath fp state = fmap S.fromList <$> runResourceT $ CB.sourceFile (toDbf fp)
  =$= dbfConduit
  =$= CL.mapMaybe (\f -> 
      Municipality 
         state 
         <$> (dbfFieldCharacter <$> readLgaColumnLongName f) 
         <*> (dbfFieldCharacter <$> readLgaColumnName f))
  =$= CL.groupBy (==)
  =$= CC.filter ((0 <) . length)
  =$= CL.mapMaybe uncons
  =$= CC.map fst
  $$ CC.sinkList

data FilePaths = FilePaths {
   states            :: FilePath,
   nswLocalities     :: FilePath,
   nswMunicipalities :: FilePath,
   vicLocalities     :: FilePath,
   vicMunicipalities :: FilePath,
   qldLocalities     :: FilePath,
   qldMunicipalities :: FilePath,
   waLocalities      :: FilePath,
   waMunicipalities  :: FilePath,
   saLocalities      :: FilePath,
   saMunicipalities  :: FilePath,
   tasLocalities     :: FilePath,
   tasMunicipalities :: FilePath,
   actMunicipalities     :: FilePath,
   actLocalities     :: FilePath,
   ntLocalities      :: FilePath,
   ntMunicipalities  :: FilePath,
   otLocalities      :: FilePath,
   otMunicipalities  :: FilePath,
   urbanAreas        :: FilePath
}

withFiles ∷ FilePath → FilePaths
withFiles sourceFolder = FilePaths {
    states = sourceFolder ++ "/cth/GEODATA COAST 100k/australia/cstauscd_r.shp",
    nswMunicipalities = sourceFolder ++ "/cth/NSWLGAPOLYGON/NSW_LGA_POLYGON_shp.shp",
    vicMunicipalities = sourceFolder ++ "/cth/VICLGAPOLYGON/VIC_LGA_POLYGON_shp.shp",
    qldMunicipalities = sourceFolder ++ "/cth/QLDLGAPOLYGON/QLD_LGA_POLYGON_shp.shp",
    waMunicipalities  = sourceFolder ++ "/cth/WALGAPOLYGON/WA_LGA_POLYGON_shp.shp",
    saMunicipalities  = sourceFolder ++ "/cth/SALGAPOLYGON/SA_LGA_POLYGON_shp.shp",
    tasMunicipalities = sourceFolder ++ "/cth/TASLGAPOLYGON/TAS_LGA_POLYGON_shp.shp",
    ntMunicipalities  = sourceFolder ++ "/cth/NTLGAPOLYGON/NT_LGA_POLYGON_shp.shp",
    otMunicipalities  = sourceFolder ++ "/cth/OTLGAPOLYGON/OT_LGA_POLYGON_shp.shp",
    actMunicipalities  = sourceFolder ++ "/cth/GEODATA COAST 100k/tristan/act munis.shp",
    nswLocalities     = sourceFolder ++ "/cth/NSWLOCALITYPOLYGON/NSW_LOCALITY_POLYGON_shp.shp",
    vicLocalities     = sourceFolder ++ "/cth/VICLOCALITYPOLYGON/VIC_LOCALITY_POLYGON_shp.shp",
    qldLocalities     = sourceFolder ++ "/cth/QLDLOCALITYPOLYGON/QLD_LOCALITY_POLYGON_shp.shp",
    waLocalities      = sourceFolder ++ "/cth/WALOCALITYPOLYGON/WA_LOCALITY_POLYGON_shp.shp",
    saLocalities      = sourceFolder ++ "/cth/SALOCALITYPOLYGON/SA_LOCALITY_POLYGON_shp.shp",
    tasLocalities     = sourceFolder ++ "/cth/TASLOCALITYPOLYGON/TAS_LOCALITY_POLYGON_shp.shp",
    actLocalities     = sourceFolder ++ "/cth/ACTLOCALITYPOLYGONshp/ACT_LOCALITY_POLYGON_shp.shp",
    ntLocalities      = sourceFolder ++ "/cth/NTLOCALITYPOLYGON/NT_LOCALITY_POLYGON_shp.shp",
    otLocalities      = sourceFolder ++ "/cth/OTLOCALITYPOLYGON/OT_LOCALITY_POLYGON_shp.shp",
    urbanAreas       = sourceFolder ++ "/abs/1270055004_sos_2011_aust_shape/SOS_2011_AUST.shp"
   }

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

allFilter :: a -> Bool
allFilter = const True

localityFilter :: Shape -> Bool
localityFilter = matchTextDbfField "G" localityTypeColumn 

districtFilter :: Shape -> Bool
districtFilter = matchTextDbfField "D" localityTypeColumn

localityTypeColumn t = "_LOCA_5" `T.isSuffixOf` t || "_LOCAL_5" `T.isSuffixOf` t

toDbf ∷ FilePath → FilePath
toDbf = (`replaceExtension` "dbf")

data State = NSW | Vic | Qld | WA | SA | Tas | ACT | NT | OT
  deriving (Eq, Show, Read, Ord)

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

municipalityStateName :: Municipality -> Text
municipalityStateName = stateName . muniState
stateName :: State -> Text
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

data Municipality = Municipality {
   muniState    :: State,
   muniLongName :: Text,
   muniName     :: Text
}
   deriving (Show, Eq, Read, Ord)

colorMajorUrban ∷ Pen
colorMajorUrban = Solid (Color 100 100 100)
colorOtherUrban ∷ Pen
colorOtherUrban = Solid (Color 125 125 125)
colorBoundedLocality ∷ Pen
colorBoundedLocality = Solid (Color 150 150 150)

colorTheLocality ∷ Pen
colorTheLocality = Solid (Color 100 0 0)

colorAllLocalities ∷ Pen
colorAllLocalities = Outline (Points 0.4) (Color 100 0 0)

colorAllMunicipalities ∷ Pen
colorAllMunicipalities = Outline (Points 2.0) (Color 150 150 150)

colorTheMunicipality ∷ Pen
colorTheMunicipality = Solid (Color 254 254 233)

settingsFromShapefileStream ∷ Shape → Settings
settingsFromShapefileStream (header, _, _) = settingsFromRecBBox . toRecBB . shpBB $ header

settingsFromPolygon ∷ ShpRec → Maybe Settings
settingsFromPolygon b = settingsFromRecBBox <$> shpRecBBox b

settingsFromRecBBox ∷ RecBBox → Settings
settingsFromRecBBox = withDefaultSettings . embiggenBoundingBox

embiggenBoundingBox ∷ RecBBox → RecBBox
embiggenBoundingBox (RecBBox a b c d) = RecBBox (a - width) (b + width) (c - height) (d + height)
   where
      width = (b - a) / 10
      height = (d - c) / 10

municipalityFilePathByMunicipality ∷ FilePaths → Municipality → FilePath
municipalityFilePathByMunicipality fps muni = municipalityFilePathByState (muniState muni) fps

settingsFromState ∷ FilePaths → State → IO (Maybe Settings)
settingsFromState fps state = settingsFromShapefileStream <$$> shapeSource (municipalityFilePathByState state fps) Nothing CL.head

(<$$>) :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
(<$$>) a b = fmap a <$> b

settingsFromMunicipality ∷ FilePaths → Municipality → IO (Maybe Settings)
settingsFromMunicipality fps municipality = settingsFromRecBBox <$$> bbox
  where
    conduit :: Sink Shape IO (Maybe RecBBox)
    conduit = (CC.filter (matchMunicipality municipality)
      =$= CC.map (shapeToBBox)
      =$= CL.fold bigBoundingBox Nothing)
    bbox = municipalitySource fps municipality Nothing conduit

shapeToBBox :: Shape -> Maybe RecBBox
shapeToBBox (_, shp, _) = shpRecBBox shp

withShpFile ∷ FilePath → (Handle → Handle → IO a) → IO a
withShpFile filePath cb =
  withFile filePath ReadMode $ \shp ->
    withFile (toDbf filePath) ReadMode $ \dbf ->
      cb shp dbf

shapeSource :: FilePath -> Maybe RecBBox -> Sink Shape IO a -> IO a
shapeSource fp bbox sink = withShpFile fp $ \shp dbf -> 
   shapesFromDbfShpSource bbox shp dbf $$ sink

municipalitySource :: FilePaths -> Municipality -> Maybe RecBBox -> Sink Shape IO a -> IO a
municipalitySource fps muni = shapeSource (municipalityFilePathByMunicipality fps muni)

municipalitySources :: FilePaths -> Maybe RecBBox -> Sink Shape IO a -> IO [a]
municipalitySources fps = multiSources (municipalityFilePaths fps)

localitySources :: FilePaths -> Maybe RecBBox -> Sink Shape IO a -> IO [a]
localitySources fps = multiSources (localityFilePaths fps)

multiSources :: Yielder -> Maybe RecBBox -> Sink Shape IO a -> IO [a]
multiSources yielder bbox sink = mapM (go sink) yielder
  where 
    go sink (filePath, filter) = shapeSource filePath bbox (CC.filter filter =$= sink)

matchUrbanAreaType ∷ Text → Shape → Bool
matchUrbanAreaType = (`matchTextDbfField` (== "SOS_NAME11"))

matchFeatCode ∷ Text → Shape → Bool
matchFeatCode = (`matchTextDbfField` (== "FEAT_CODE"))

mapUrbanAreas ∷ FilePaths → Settings → IO [ByteString]
mapUrbanAreas fps settings = do 
  (bLoc', othUrban', majUrban') <- shapeSource (urbanAreas fps) (Just $ boundingBox settings)
    (CC.foldl (\(a, b, c) shape -> 
      if matchUrbanAreaType "Bounded Locality" shape then (shape:a, b, c) else
      if matchUrbanAreaType "Other Urban" shape then (a, shape:b, c) else
      if matchUrbanAreaType "Major Urban" shape then (a, b, shape:c) else
      (a, b, c)) ([], [], []))
  bLoc <- CC.yieldMany bLoc'
    =$= eachPolygon
    =$= mapPoints3 settings colorBoundedLocality 60
    $$ CC.sinkList
  othUrban <- CC.yieldMany othUrban'
    =$= eachPolygon
    =$= mapPoints3 settings colorOtherUrban 60
    $$ CC.sinkList
  majUrban <- CC.yieldMany majUrban'
    =$= eachPolygon
    =$= mapPoints3 settings colorMajorUrban 60
    $$ CC.sinkList
  return $ concat [bLoc, othUrban, majUrban]

mapCoast :: FilePaths -> Settings -> IO ByteString
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
   mappedSea <- CC.yieldMany [sea] =$= mapPoints3 settings (water settings) 0 $$ CC.sinkList
   mappedLand <- CC.yieldMany [lands] =$= mapPoints3 settings (land settings) 0 $$ CC.sinkList
   return $ B8.concat (mappedSea ++ mappedLand)
   
bboxToPolygon :: RecBBox -> [(Double, Double)]
bboxToPolygon box = [
   (recXMin box, recYMin box), 
   (recXMax box, recYMin box), 
   (recXMax box, recYMax box), 
   (recXMin box, recYMax box),
   (recXMin box, recYMin box)]

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
-- TODO embed transparency in pen bc the current flexibility just maxe confusion in my kopf
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

mapAreas :: Settings -> Pen -> Sink Shape IO [ByteString]
mapAreas settings pen = eachPlace =$= mapPoints3 settings pen 60 =$= CC.sinkList
mapLines :: Settings -> Pen -> Sink Shape IO [ByteString]
mapLines settings pen = eachPolygon =$= mapPoints3 settings pen 0 =$= CC.sinkList

mapMunicipalityLocally ∷ FilePaths → Municipality → FilePath → IO ()
mapMunicipalityLocally fps muni out = do
  Just settings <- settingsFromMunicipality fps muni
  title <- mapTitle settings (muniLongName muni)
  baseMap <- makeBaseMap fps settings
  munisPoints <- mapMunicipalities fps settings colorAllMunicipalities
  muniPoints <- mapMunicipality fps muni settings colorTheMunicipality
  localitiesPoints <- mapLocalities fps settings colorAllLocalities
  finalisation <- closeMap settings
  let base = baseMap ++ munisPoints ++ muniPoints ++ localitiesPoints
  writeMap
    (base ++ [title, finalisation])
    (out ++ "/" ++ T.unpack (muniName muni) ++ " (" ++ show (muniState muni) ++ ").pdf")
  localities <- localitiesByMunicipality (muniName muni) (municipalityFilePathByMunicipality fps muni) (localityFilePathsByState (muniState muni) fps)
  mapM_ (mapLocalityInMunicipality fps (out ++ "/" ++ show (muniState muni) ++ " " ++ T.unpack (muniName muni) ++ " showing ") settings base finalisation muni) localities

mapMunicipalitiesInState ∷ FilePaths → State → FilePath → IO ()
mapMunicipalitiesInState fps state out = do
  Just settings <- settingsFromState fps state
  baseMap <- makeBaseMap fps settings
  finalisation <- closeMap settings
  statePoints <- mapStateLocally fps settings state colorTheMunicipality
  munisPoints <- mapMunicipalities fps settings colorAllLocalities
  munis <- municipalitiesByFilePath (municipalityFilePathByState state fps) state
  mapM_ (mapMunicipalityInState fps out settings (snoc baseMap munisPoints) finalisation) munis

mapMunicipalityInState ∷ FilePaths → FilePath → Settings → [ByteString] → ByteString → Municipality → IO ()
mapMunicipalityInState fps out settings baseMap finalisation muni = do
  print muni
  title <- mapTitle settings (muniLongName muni)
  municipalityPoints <- mapMunicipality fps muni settings colorTheLocality
  writeMap
    (baseMap ++ municipalityPoints ++ [title, finalisation])
    (out ++ "/" ++ show (muniState muni) ++ " showing " ++ T.unpack (muniName muni) ++ ".pdf")
  mapMunicipalityLocally fps muni out

mapLocalityInMunicipality ∷ FilePaths → FilePath → Settings → [ByteString] → ByteString -> Municipality → Locality → IO ()
mapLocalityInMunicipality fps out settings baseMap finalisation muni locality = do
  print locality
  title <- mapTitle settings (T.concat [locality, " in ", muniLongName muni, " and ", municipalityStateName muni])
  municipalityPoints <- mapLocality' fps locality settings colorTheLocality
  writeMap
    (baseMap ++ municipalityPoints ++ [title, finalisation])
    (out ++ T.unpack (locality) ++ ".pdf")

makeBaseMap ∷ FilePaths → Settings → IO [ByteString]
makeBaseMap fps settings = do
   initialisation <- initialiseMap settings
   coastMap <- mapCoast fps settings
   urbanPoints <- mapUrbanAreas fps settings
   return $ [initialisation, coastMap] ++ urbanPoints

writeMap ∷ [ByteString] → FilePath → IO ()
writeMap bytestrings filename = do
  readProcess "epstopdf" ["-f", "-o=" ++ filename] (B8.unpack . B8.concat $ bytestrings) 
  return ()

pointsFromRecord ∷ ShpRec → [[(Double, Double)]]
pointsFromRecord r = concatMap pointsFromRecContents (catMaybes [shpRecContents r])
pointsFromRecContents ∷ RecContents → [[(Double, Double)]]
pointsFromRecContents r@RecPolygon {} = recPolPoints r
pointsFromRecContents _               = []

eachPolygon ∷ Conduit (a, ShpRec, b) IO [[(Double, Double)]]
eachPolygon = CC.map (\(_, r, _) -> pointsFromRecord r)

eachPlace ∷ Conduit (a, ShpRec, b) IO [[(Double, Double)]]
eachPlace = CC.map (\(_, r, _) -> 
   return . concat . andReverseTheRest . (map wrapEnds) . pointsFromRecord $ r)

andReverseTheRest :: [[a]] -> [[a]]
andReverseTheRest (main:rest) = main:(intersperse [home] rest)
  where home = last main
andReverseTheRest a = a


wrapEnds :: [(Double, Double)] -> [(Double, Double)]
wrapEnds [] = []
wrapEnds [a] = [a]
wrapEnds line = (last line):line

matchMunicipality ∷ Municipality → Shape → Bool
matchMunicipality m = matchTextDbfField (muniName m) lgaColumnName

matchState :: State -> Shape -> Bool
matchState s = matchNumericDbfField (stateCode s) stateCodeColumnName

matchLocality ∷ Locality → Shape → Bool
matchLocality m = matchTextDbfField m localityColumnName

lgaColumnName ∷ Text → Bool
lgaColumnName t = "_LGA__3" `T.isSuffixOf` t || "_LGA_s_3" `T.isSuffixOf` t

stateCodeColumnName ∷ Text → Bool
stateCodeColumnName = (== "STATE_CODE")

stateCode :: State -> Int
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

readLgaColumnLongName = shapeFieldByColumnNameRule lgaColumnLongName
readLgaColumnName = shapeFieldByColumnNameRule lgaColumnName

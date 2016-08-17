{-# LANGUAGE Rank2Types #-}
module Main (main)
where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Dbase.Parser
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Geometry.Shapefile.Conduit
import Geometry.Shapefile.Types
import System.Environment
import System.IO
import System.FilePath

import Map

main :: IO ()
main = getArgs >>= mapFile

mapFile :: [FilePath] -> IO ()
mapFile (source:dest:_) = do
  --mapMunicipalityInState (withFiles source) muni dest
  mapMunicipalityLocality (withFiles source) muni dest
    where
      muni = Municipality {municipalityState = Qld, municipalityName = "MORNINGTON", municipalityLongName = "MORNINGTON SHIRE"}
mapFile _ = error "You need to specify the source data folder and the destination filename"


data FilePaths = FilePaths {
   nswLocalities :: FilePath,
   nswMunicipalities :: FilePath,
   vicLocalities :: FilePath,
   vicMunicipalities :: FilePath,
   qldLocalities :: FilePath,
   qldMunicipalities :: FilePath,
   waLocalities :: FilePath,
   waMunicipalities :: FilePath,
   saLocalities :: FilePath,
   saMunicipalities :: FilePath,
   tasLocalities :: FilePath,
   tasMunicipalities :: FilePath,
   actLocalities :: FilePath,
   ntLocalities :: FilePath,
   ntMunicipalities :: FilePath,
   urbanAreas :: FilePath
}

withFiles :: FilePath -> FilePaths
withFiles sourceFolder = FilePaths {
    nswMunicipalities = sourceFolder ++ "/cth/NSWLGAPOLYGON/NSW_LGA_POLYGON_shp.shp",
    vicMunicipalities = sourceFolder ++ "/cth/VICLGAPOLYGON/VIC_LGA_POLYGON_shp.shp",
    qldMunicipalities = sourceFolder ++ "/cth/QLDLGAPOLYGON/QLD_LGA_POLYGON_shp.shp",
    waMunicipalities  = sourceFolder ++ "/cth/WALGAPOLYGON/WA_LGA_POLYGON_shp.shp",
    saMunicipalities  = sourceFolder ++ "/cth/SALGAPOLYGON/SA_LGA_POLYGON_shp.shp",
    tasMunicipalities = sourceFolder ++ "/cth/TASLGAPOLYGON/TAS_LGA_POLYGON_shp.shp",
    ntMunicipalities  = sourceFolder ++ "/cth/NTLGAPOLYGON/NT_LGA_POLYGON_shp.shp",
    nswLocalities     = sourceFolder ++ "/cth/NSWLOCALITYPOLYGON/NSW_LOCALITY_POLYGON_shp.shp",
    vicLocalities     = sourceFolder ++ "/cth/VICLOCALITYPOLYGON/VIC_LOCALITY_POLYGON_shp.shp",
    qldLocalities     = sourceFolder ++ "/cth/QLDLOCALITYPOLYGON/QLD_LOCALITY_POLYGON_shp.shp",
    waLocalities      = sourceFolder ++ "/cth/WALOCALITYPOLYGON/WA_LOCALITY_POLYGON_shp.shp",
    saLocalities      = sourceFolder ++ "/cth/SALOCALITYPOLYGON/SA_LOCALITY_POLYGON_shp.shp",
    tasLocalities     = sourceFolder ++ "/cth/TASLOCALITYPOLYGON/TAS_LOCALITY_POLYGON_shp.shp",
    actLocalities     = sourceFolder ++ "/cth/ACTLOCALITYPOLYGON/ACT_LOCALITY_POLYGON_shp.shp",
    ntLocalities      = sourceFolder ++ "/cth/NTLOCALITYPOLYGON/NT_LOCALITY_POLYGON_shp.shp",
    urbanAreas       = sourceFolder ++ "/abs/1270055004_sos_2011_aust_shape/SOS_2011_AUST.shp"
   }

municipalityFilePaths :: FilePaths -> [FilePath]
municipalityFilePaths fps = [ nswMunicipalities fps,
                              vicMunicipalities fps,
                              qldMunicipalities fps,
                              waMunicipalities fps,
                              saMunicipalities fps,
                              tasMunicipalities fps,
                              ntMunicipalities fps
                            ]


toDbf :: FilePath -> FilePath
toDbf = (`replaceExtension` "dbf")

data State = NSW | Vic | Qld | WA | SA | Tas {-| ACT-} | NT
  deriving (Eq, Show, Read)

municipalityFilePathByState :: State -> FilePaths -> FilePath
municipalityFilePathByState NSW = nswMunicipalities
municipalityFilePathByState Vic = vicMunicipalities
municipalityFilePathByState Qld = qldMunicipalities
municipalityFilePathByState WA = waMunicipalities
municipalityFilePathByState SA = saMunicipalities
municipalityFilePathByState Tas = tasMunicipalities
municipalityFilePathByState NT = ntMunicipalities

data Municipality = Municipality {
   municipalityState :: State,
   municipalityLongName :: Text,
   municipalityName :: Text
}

colorMajorUrban :: Pen
colorMajorUrban = Solid (Color 100 100 100)
colorOtherUrban :: Pen
colorOtherUrban = Solid (Color 125 125 125)
colorBoundedLocality :: Pen
colorBoundedLocality = Solid (Color 150 150 150)

colorTheLocality :: Pen
colorTheLocality = Solid (Color 100 0 0)

colorAllLocalities :: Pen
colorAllLocalities = Outline (Points 0.4) (Color 100 0 0)

-- colorAllMunicipalities :: Pen
-- colorAllMunicipalities = Outline (Points 1.0) (Color 150 150 150)

-- colorTheMunicipality :: Pen
-- colorTheMunicipality = Solid (Color 254 254 233)


sourceFromStart :: (MonadIO m) => Handle -> ConduitM i ByteString m ()
sourceFromStart handle = CB.sourceHandleRange handle (Just 0) Nothing

shapesFromDbfShpSource :: Handle -> Handle -> Source IO Shape
shapesFromDbfShpSource shp dbf = shpDbfConduit (sourceFromStart shp) (sourceFromStart dbf)

settingsFromShapefileStream :: Shape -> Settings
settingsFromShapefileStream (header, _, _) = settingsFromRecBBox . toRecBB . shpBB $ header

settingsFromPolygon :: ShpRec -> Maybe Settings
settingsFromPolygon b = settingsFromRecBBox <$> shpRecBBox b

settingsFromRecBBox :: RecBBox -> Settings
settingsFromRecBBox = withDefaultSettings . embiggenBoundingBox

embiggenBoundingBox :: RecBBox -> RecBBox
embiggenBoundingBox (RecBBox a b c d) = RecBBox (a-width) (b+width) (c-height) (d+height)
   where
      width = (b-a) / 10
      height = (d-c) / 10

bigBoundingBox :: Maybe RecBBox -> Maybe RecBBox -> Maybe RecBBox
bigBoundingBox Nothing a = a
bigBoundingBox a Nothing = a
bigBoundingBox (Just (RecBBox a1 b1 c1 d1)) (Just (RecBBox a2 b2 c2 d2))
    = Just $ RecBBox (min a1 a2) (max b1 b2) (min c1 c2) (max d1 d2)

municipalityFilePathByMunicipality :: FilePaths -> Municipality -> FilePath
municipalityFilePathByMunicipality fps muni = municipalityFilePathByState (municipalityState muni) fps

settingsFromMunicipalityInState :: FilePaths -> Municipality -> IO (Maybe Settings)
settingsFromMunicipalityInState paths municipality = withMunicipalityFile paths municipality $ \shp dbf ->
  (fmap settingsFromShapefileStream) <$> (shapesFromDbfShpSource shp dbf $$ CL.head)

settingsFromMunicipality :: FilePaths -> Municipality -> IO (Maybe Settings)
settingsFromMunicipality paths municipality = withMunicipalityFile paths municipality $ \shp dbf ->
  (settingsFromRecBBox <$>) <$> (shapesFromDbfShpSource shp dbf =$= CC.filter (matchMunicipality municipality) =$= CC.map (\(_, a, _) -> shpRecBBox a) $$ CL.fold bigBoundingBox Nothing)

withShpFile :: FilePath -> (Handle -> Handle -> IO a) -> IO a
withShpFile filePath cb =
  withFile filePath ReadMode $ \shp ->
    withFile (toDbf filePath) ReadMode $ \dbf ->
      cb shp dbf

withMunicipalityFile :: FilePaths -> Municipality -> (Handle -> Handle -> IO a) -> IO a
withMunicipalityFile paths muni = withShpFile (municipalityFilePathByMunicipality paths muni)

withMunicipalityFiles :: FilePaths -> (Handle -> Handle -> IO a) -> IO [a]
withMunicipalityFiles paths cb = mapM (`withShpFile` cb) (municipalityFilePaths paths)

matchUrbanAreaType :: Text -> Shape -> Bool
matchUrbanAreaType = matchTextDbfField (\t -> t == "SOS_NAME11")

mapUrbanAreas :: FilePaths -> Settings -> IO [ByteString]
mapUrbanAreas paths settings = withShpFile (urbanAreas paths) $ \shp dbf -> do
  bLoc <- shapesFromDbfShpSource shp dbf
    =$= CC.filter (matchUrbanAreaType "Bounded Locality")
    =$= eachPolygon
    =$= mapPoints3 settings colorBoundedLocality 60
    $$ CC.sinkList
  othUrban <- shapesFromDbfShpSource shp dbf
    =$= CC.filter (matchUrbanAreaType "Other Urban")
    =$= eachPolygon
    =$= mapPoints3 settings colorOtherUrban 60
    $$ CC.sinkList
  majUrban <- shapesFromDbfShpSource shp dbf
    =$= CC.filter (matchUrbanAreaType "Major Urban")
    =$= eachPolygon
    =$= mapPoints3 settings colorMajorUrban 60
    $$ CC.sinkList
  return $ concat [bLoc, othUrban, majUrban]

mapMunicipalities :: FilePaths -> Settings -> IO [ByteString]
mapMunicipalities fps settings = concat <$> (withMunicipalityFiles fps $ \shp dbf ->
  shapesFromDbfShpSource shp dbf
   =$= eachPolygon
    =$= mapPoints3 settings colorAllLocalities 0
   $$ CC.sinkList)

mapMunicipality :: FilePaths -> Municipality -> Settings -> IO [ByteString]
mapMunicipality fps muni settings = withMunicipalityFile fps muni $ \shp dbf ->
  shapesFromDbfShpSource shp dbf
   =$= CC.filter (matchMunicipality muni)
   =$= eachPlace
     =$= mapPoints3 settings colorTheLocality 60
   $$ CC.sinkList

mapMunicipalityLocality :: FilePaths -> Municipality -> FilePath -> IO ()
mapMunicipalityLocality filePaths municipality out = do
  Just settings <- settingsFromMunicipality filePaths municipality
  initialisation <- initialiseMap settings
  title <- mapTitle settings (municipalityLongName municipality)
  coastMap <- mapCoast settings
  urbanPoints <- mapUrbanAreas filePaths settings
  municipalitiesPoints <- mapMunicipalities filePaths settings
  municipalityPoints <- mapMunicipality filePaths municipality settings
  finalisation <- closeMap settings
  withFile (out ++ "/" ++ T.unpack (municipalityName municipality) ++ " (" ++ show (municipalityState municipality) ++ ").eps") WriteMode $ \dst ->  -- TODO perhaps not a conduit?
    CC.yieldMany ([initialisation, coastMap, title] ++ urbanPoints ++ municipalitiesPoints ++ municipalityPoints ++ [finalisation])
      $$ CC.sinkHandle dst


mapMunicipalityInState :: FilePaths -> Municipality -> FilePath -> IO ()
mapMunicipalityInState filePaths municipality out = do
  Just settings <- settingsFromMunicipalityInState filePaths municipality
  initialisation <- initialiseMap settings
  title <- mapTitle settings (municipalityLongName municipality)
  coastMap <- mapCoast settings
  urbanPoints <- mapUrbanAreas filePaths settings
  municipalitiesPoints <- mapMunicipalities filePaths settings
  municipalityPoints <- mapMunicipality filePaths municipality settings
  finalisation <- closeMap settings
  withFile (out ++ "/" ++ T.unpack (municipalityName municipality) ++ " in " ++ show (municipalityState municipality) ++ ".eps") WriteMode $ \dst ->  -- TODO perhaps not a conduit?
    CC.yieldMany ([initialisation, coastMap, title] ++ urbanPoints ++ municipalitiesPoints ++ municipalityPoints ++ [finalisation])
      $$ CC.sinkHandle dst


 -- mapFilePath :: FilePath -> FilePath -> ResourceT IO ()
 -- mapFilePath = mapFileHandle
 -- --mapFilePath src dst = mapFileHandle
 --    --liftIO withFile src ReadMode $ \srcH ->
 --    --liftIO withFile dst WriteMode $ \dstH ->
 --    --mapFileHandle srcH dstH
 --
 -- cdt :: Settings -> ConduitM () ByteString IO () -> IO [ByteString]
 -- cdt settings source = source
 --       =$= shapefileConduit
 --       =$= points
 --       =$= mapPoints3 settings (Outline (Points 1) (Color 0 100 200)) 5
 --       $$ CC.sinkList
 --
 -- cdt' :: Settings -> ResourceT (ConduitM () ByteString IO) () -> ResourceT IO [ByteString]
 -- cdt' settings source = source >>= (lift . cdt settings)
 --
 -- mapFileHandle :: FilePath -> FilePath -> ResourceT IO ()
 -- mapFileHandle src dst = do
 --    Just (shpHead, _) <- CC.sourceFile src =$= shapefileConduit $$ CL.head
 --    let settings = withDefaultSettings (toRecBB $ shpBB shpHead)
 --    initialisation <- liftIO $ initialiseMap settings
 --    mapPoints' <- (Lift.distribute $
 --       CB.sourceFile "/home/cassowary/Projects/source-data/cth/NSWLGAPOLYGON/NSW_LGA_POLYGON_shp.shp")
 --       >>= cdt settings
 --    --  mapPoints <-
 --    --     CB.sourceFile "/home/cassowary/Projects/source-data/cth/NSWLGAPOLYGON/NSW_LGA_POLYGON_shp.shp"
 --    --     =$= shapefileConduit
 --    --     =$= points
 --    --     =$= mapPoints3 settings (Outline (Points 1) (Color 0 100 200)) 5
 --    --     $$ CC.sinkList
 --    finalisation <- liftIO $ closeMap settings
 --    liftIO $ CC.yieldMany ([initialisation] ++ mapPoints' ++ [finalisation]) $$ CC.sinkHandle dst

pointsFromRecord :: ShpRec -> [[(Double, Double)]]
pointsFromRecord r = concatMap pointsFromRecContents (catMaybes [shpRecContents r])
pointsFromRecContents :: RecContents -> [[(Double, Double)]]
pointsFromRecContents r@RecPolygon {} = recPolPoints r
pointsFromRecContents _ = []

eachPolygon :: Conduit (a, ShpRec, b) IO [[(Double, Double)]]
eachPolygon = CC.map (\(_, r, _) -> pointsFromRecord r)

eachPlace :: Conduit (a, ShpRec, b) IO [[(Double, Double)]]
eachPlace = CC.map (\(_, r, _) -> return . concat . pointsFromRecord $ r)

toRecBB :: ShpBBox -> RecBBox
toRecBB bb = RecBBox {
      recXMin = shpXMin bb,
      recXMax = shpXMax bb,
      recYMin = shpYMin bb,
      recYMax = shpYMax bb
   }

matchTextDbfField :: (Text -> Bool) -> Text -> Shape -> Bool
matchTextDbfField checkColumn t (_, _, s) = columnHasCharacter t (shapeFieldByColumnNameRule checkColumn s)

matchMunicipality :: Municipality -> Shape -> Bool
matchMunicipality m = matchTextDbfField lgaColumnName (municipalityName m)

columnHasCharacter :: Text -> DbfField -> Bool
columnHasCharacter c (DbfFieldCharacter d) = c == d
columnHasCharacter _ _ = False

lgaColumnName :: Text -> Bool
lgaColumnName t = "_LGA__3" `T.isSuffixOf` t

shapeFieldByColumnNameRule :: (Text -> Bool) -> DbfRow -> DbfField
shapeFieldByColumnNameRule rule (DbfRow c) = snd . head $ filter (\(l,_) -> rule $ dbfcName l) c

-- makeBaseMap settings = BS.concat [initialiseMap settings

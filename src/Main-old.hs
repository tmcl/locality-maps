{-# LANGUAGE OverloadedStrings #-}
module Main
where

-- import Turtle
import System.Environment
import System.Process
import Data.Csv
import Data.Maybe
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Set as S

import qualified Data.Conduit.Shapefile as Sh

import Geometry.Shapefile.MergeShpDbf
import Geometry.Shapefile.Types

import Map
import Municipality hiding (fullname)
import Victoria
import SouthAustralia
import Queensland
import Shapefile2

import Control.Monad


tshow :: Show z => z -> T.Text
tshow = T.pack . show

j :: T.Text -> T.Text
j = id

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

main :: IO ()
main = do
   args <- getArgs
   print args
   let state = (read . head) args :: State
   print state
   case state of
     Vic -> mapM_ (it . MuniVic) allVictorian
     Qld -> mapM_ (it . MuniQld) allQueensland


getLocalitiesByMunicipality :: MunicipalityInState -> IO (S.Set T.Text)
getLocalitiesByMunicipality muni = do
   vic' <- getVictorianLocalities
   let vic = getRightOrElse mzero vic'
   sa <- getSouthAustralianLocalities "/home/cassowary/Projects/map/source-data/sa/Gazetteer_shp/GazetteerSites.dbf"
   qld <- getQueenslandLocalities "/home/cassowary/Projects/map/source-data/qld/QSC_Extracted_Data_20160801_222211270000-15668/Place_names_gazetteer.shp"

   --let muni = readByState state municipality
   return $ filterLocalities' muni (Localities vic sa qld)

--it :: State -> String -> IO ()
it muni = do
   localities <- getLocalitiesByMunicipality muni
   case S.size localities of
      0 -> error . show $ muni
      _ -> print localities
   maybeMapMatters' muni localities

getRightOrElse :: a -> Either b a -> a
getRightOrElse a (Left _) = a
getRightOrElse _ (Right a) = a

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

colorAllMunicipalities :: Pen
colorAllMunicipalities = Outline (Points 1.0) (Color 150 150 150)

colorTheMunicipality :: Pen
colorTheMunicipality = Solid (Color 254 254 233)

maybeMapMatters' :: Foldable f => MunicipalityInState -> f T.Text -> IO ()
maybeMapMatters' (MuniVic m) = maybeMapMatters m
maybeMapMatters' (MuniQld m) = maybeMapMatters m
maybeMapMatters' (MuniSA  m) = maybeMapMatters m

isLocalityByName :: T.Text -> (T.Text, String) -> Bool
isLocalityByName loc (field, locality)
    = "_LOCA_2" `T.isSuffixOf` field && T.strip . T.toUpper $ T.takeWhile (/= '(') loc == (T.strip . T.pack $ locality)

allRecsInShapes :: [ShpData] -> [ShpRec]
allRecsInShapes = concatMap shpRecs

maybeMapMatters :: (Municipality m, Foldable f) => m -> f T.Text -> IO ()
maybeMapMatters municipality localities = do
    stateMunicipalityShapes <- readShpWithDbf (municipalityShapePath municipality)
    let theMunicipalityRecs = shpRecByField (isMunicipalityByName municipality) stateMunicipalityShapes
    let bboxes = catMaybes [catBoundingBoxes $ map Shapefile2.boundingBox theMunicipalityRecs]

    mapM_ (\bbox -> mapMatters bbox municipality theMunicipalityRecs localities) bboxes

outputEPSStreamToPDF :: Prelude.FilePath -> T.Text -> IO ()
outputEPSStreamToPDF outfile stream = do
   _ <- inproc "epstopdf" ["-f", T.pack $ "-o=" ++ outfile] stream
   return ()

-- mapLocality :: Municipality m => m -> T.Text -> Settings -> [ShpData] -> T.Text -> IO ()
-- mapLocality m base settings allLocalityShapes locality = do
--     let theLocalityRecs = concatMap (shpRecByField (isLocalityByName locality)) allLocalityShapes
--     localityMap <- mapPoints settings (colorTheLocality, 60, concatMap getPoints theLocalityRecs)
--     mapClosing <- closeMap settings
--     outputEPSStreamToPDF ("out/" ++ T.unpack locality ++ " - " ++ show m ++ ".pdf") (T.concat [base, localityMap, mapClosing])


mapMatters :: (Municipality m, Foldable f) => RecBBox -> m -> [ShpRec] -> f T.Text -> IO ()
mapMatters bbox municipality theMunicipalityRecs localities = do
    allLocalityShapes <- allLocalities
    let settings = withDefaultSettings bbox
    base <- makeBaseMap settings allLocalityShapes theMunicipalityRecs 
    mapClosing <- closeMap settings
    outputEPSStreamToPDF ("out/" ++ show municipality ++ ".pdf") (T.concat [base, mapClosing])
    -- mapM_ (mapLocality municipality base settings allLocalityShapes) localities

makeBaseMap :: Settings -> [ShpData] -> [ShpRec] -> IO T.Text
makeBaseMap settings allLocalityShapes theMunicipalityRecs = do
    allMunicipalityShapes <- allMunicipalities
    let allMunicipalityRecs = allRecsInShapes allMunicipalityShapes
    (majorUrban, otherUrban, boundedLocality) <- fmap pickUrbans allUrbans
    let localityPoints = concatMap getPoints (concatMap shpRecs allLocalityShapes)
    T.concat <$> sequence [ initialiseMap settings ,
               mapCoast settings ,
               mapPoints settings colorTheMunicipality, 0, concatMap getPoints theMunicipalityRecs,
               mapPoints settings colorAllMunicipalities, 0, concatMap getPoints allMunicipalityRecs,
               mapPoints settings colorAllLocalities, 0, localityPoints,
               mapPoints settings colorMajorUrban, 60, concatMap getPoints majorUrban,
               mapPoints settings colorOtherUrban, 60, concatMap getPoints otherUrban,
               mapPoints settings colorBoundedLocality, 60, concatMap getPoints boundedLocality]
    -- return $ T.concat thing

municipalityShapePath :: Municipality m => m -> Prelude.FilePath
municipalityShapePath = stateShapePath . municipalityState

stateShapePath :: State -> Prelude.FilePath
stateShapePath Qld = "/home/cassowary/Projects/map/source-data/cth/QLDLGAPOLYGON/QLD_LGA_POLYGON_shp.shp"
stateShapePath Vic = "/home/cassowary/Projects/map/source-data/cth/VICLGAPOLYGON/VIC_LGA_POLYGON_shp.shp"
stateShapePath SA  = "/home/cassowary/Projects/map/source-data/cth/SALGAPOLYGON/SA_LGA_POLYGON_shp.shp"

allUrbans :: IO ShpData
allUrbans = readShpWithDbf "/home/cassowary/Projects/map/source-data/abs/1270055004_sos_2011_aust_shape/SOS_2011_AUST.shp"
pickUrbans :: ShpData -> ([ShpRec], [ShpRec], [ShpRec])
pickUrbans = pickRecords (("SOS_NAME11", "Major Urban"), ("SOS_NAME11", "Other Urban"), ("SOS_NAME11", "Bounded Locality"))

allMunicipalities = do
    shpDbfConduit "/home/cassowary/Projects/map/source-data/cth/NSWLGAPOLYGON/NSW_LGA_POLYGON_shp.shp"
    shpDbfConduit "/home/cassowary/Projects/map/source-data/cth/VICLGAPOLYGON/VIC_LGA_POLYGON_shp.shp"
    shpDbfConduit "/home/cassowary/Projects/map/source-data/cth/QLDLGAPOLYGON/QLD_LGA_POLYGON_shp.shp"

allLocalities = do
    shpDbfConduit "/home/cassowary/Projects/map/source-data/cth/NSWLOCALITYPOLYGON/NSW_LOCALITY_POLYGON_shp.shp"
    shpDbfConduit "/home/cassowary/Projects/map/source-data/cth/VICLOCALITYPOLYGON/VIC_LOCALITY_POLYGON_shp.shp"
    shpDbfConduit "/home/cassowary/Projects/map/source-data/cth/QLDLOCALITYPOLYGON/QLD_LOCALITY_POLYGON_shp.shp"

readByState :: State -> String -> MunicipalityInState
readByState Vic m = MuniVic $ read ("Vic" ++ m)
readByState SA  m = MuniSA $ read ("SA" ++ m)
readByState Qld m = MuniQld $ read ("Qld" ++ m)

data MunicipalityInState =
    MuniVic VictorianMunicipality
    | MuniSA SouthAustralianMunicipality
    | MuniQld QueenslandMunicipality
   deriving (Show)

getVictorianLocalities :: IO (Either String VictorianLocalities)
getVictorianLocalities = fmap victorianLocalities readVictorianDb

readVictorianDb :: IO BS.ByteString
readVictorianDb = BS.readFile "/home/cassowary/Projects/map/source-data/vic/filesvicnamesplaces2014.csv"

filterLocalities' :: MunicipalityInState -> Localities -> S.Set T.Text
filterLocalities' (MuniVic m) = filterLocalities m
filterLocalities' (MuniQld m) = filterLocalities m
filterLocalities' (MuniSA  m) = filterLocalities m

filterLocalities :: Municipality m => m -> Localities -> S.Set T.Text
filterLocalities = flip localitiesByMunicipality

filterVictorianMunicipalities :: Localities -> VictorianMunicipality -> S.Set T.Text
filterVictorianMunicipalities = localitiesByMunicipality

data GazetteTypeCode = LOCB | Other
   deriving (Eq, Show)
instance FromField GazetteTypeCode where
   parseField "LOCB" = pure LOCB
   parseField _ = pure Other

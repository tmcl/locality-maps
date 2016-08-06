{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Turtle
import System.Environment -- to be used later
import Data.Csv
import Data.Bifunctor
import Data.Maybe
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Set as S

import Geometry.Shapefile.MergeShpDbf
import Geometry.Shapefile.ReadShp
import Geometry.Shapefile.Types

import Map
import Municipality
import Victoria
import SouthAustralia
import Queensland
import Shapefile2

import qualified Filesystem.Path.CurrentOS as FP

tshow :: Show z => z -> T.Text
tshow = T.pack . show

j :: Text -> Text
j = id

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

main :: IO ()
main = do
   mapMunicipality QldBrisbane
   args <- getArgs
   print args
   let state = (read . head) args :: State
   let municipality = (head . tail) args
   it state municipality

it :: State -> String -> IO ()
it state municipality = do
   vic' <- getVictorianLocalities
   let vic = getRightOrElse mzero vic'
   sa <- getSouthAustralianLocalities "/home/cassowary/Projects/map/source-data/sa/Gazetteer_shp/GazetteerSites.dbf"
   qld <- getQueenslandLocalities "/home/cassowary/Projects/map/source-data/qld/QSC_Extracted_Data_20160801_222211270000-15668/Place_names_gazetteer.shp"
   let muni = readByState state municipality
   mapMunicipality' muni
   let localities = filterLocalities' muni (Localities vic sa qld)
   print localities
   mapM_ (mapLocality' muni) localities

getRightOrElse a (Left _) = a
getRightOrElse _ (Right a) = a

colorTheLocality :: Pen
colorTheLocality = Solid (Color 100 0 0)

colorAllLocalities :: Pen
colorAllLocalities = Outline (Points 0.4) (Color 100 0 0)

colorAllMunicipalities :: Pen
colorAllMunicipalities = Outline (Points 1.0) (Color 150 150 150)

colorTheMunicipality :: Pen
colorTheMunicipality = Solid (Color 254 254 233)

mapMunicipality' (MuniVic m) = mapMunicipality m
mapMunicipality' (MuniQld m) = mapMunicipality m
mapMunicipality' (MuniSA  m) = mapMunicipality m

mapLocality' (MuniVic m) = mapLocality m
mapLocality' (MuniQld m) = mapLocality m
mapLocality' (MuniSA  m) = mapLocality m

mapMunicipality :: Municipality m => m -> IO ()
mapMunicipality = mapMunicipalityAndLocalities (\_ -> []) (FP.fromText $ T.pack $ "out/" ++ show lga ++ ".eps")

mapLocality :: Municipality m => m -> T.Text -> IO ()
mapLocality m locality = mapMunicipalityAndLocalities pickDrawLocality m
   where
      pickDrawLocality recs = [(colorTheLocality, (concatMap getPoints) $ concatMap (shpRecByField (isLocalityByName locality)) recs)]

isLocalityByName loc (field, locality) = "_LOCA_2" `T.isSuffixOf` field && loc == (T.strip . T.pack $ locality)

mapMunicipalityAndLocalities :: Municipality m => ([ShpData] -> [(Pen, [[Point]])]) -> FP.FilePath -> m -> IO ()
mapMunicipalityAndLocalities extras name lga = do
    shpdata <- readShpWithDbf (municipalityShapePath lga)
    let recs = shpRecByField (isMunicipalityByName lga) shpdata
    let bbox = catBoundingBoxes $ map Shapefile2.boundingBox recs
    let alls = shpRecs shpdata
    localities <- allLocalities

    let polygons = [ (colorTheMunicipality, (concat $ map getPoints recs)), (colorAllMunicipalities, (concat $ map getPoints alls)), (colorAllLocalities, (concat $ map getPoints (concatMap shpRecs localities))) ] ++ extras localities
    mapM_ (\box -> output name (drawMap (withDefaultSettings box) polygons)) (catMaybes [bbox])

municipalityShapePath :: Municipality m => m -> Prelude.FilePath
municipalityShapePath = stateShapePath . municipalityState

stateShapePath :: State -> Prelude.FilePath
stateShapePath Qld = "/home/cassowary/Projects/map/source-data/cth/QLDLGAPOLYGON/QLD_LGA_POLYGON_shp.shp"
stateShapePath Vic = "/home/cassowary/Projects/map/source-data/cth/VICLGAPOLYGON/VIC_LGA_POLYGON_shp.shp"

allLocalities = do
    qld <- readShpWithDbf "/home/cassowary/Projects/map/source-data/cth/QLDLOCALITYPOLYGON/QLD_LOCALITY_POLYGON_shp.shp"
    return $ [qld]

readByState :: State -> String -> MunicipalityInState
readByState Vic m = MuniVic $ read ("Vic" ++ m)
readByState SA  m = MuniSA $ read ("SA" ++ m)
readByState Qld m = MuniQld $ read ("Qld" ++ m)

data MunicipalityInState =
    MuniVic VictorianMunicipality
    | MuniSA SouthAustralianMunicipality
    | MuniQld QueenslandMunicipality

getVictorianLocalities :: IO (Either String VictorianLocalities)
getVictorianLocalities = fmap victorianLocalities readVictorianDb

readVictorianDb :: IO BS.ByteString
readVictorianDb = BS.readFile "/home/cassowary/Projects/map/source-data/vic/filesvicnamesplaces2014.csv"

-- pooy :: Municipality m => (m -> a) -> MunicipalityInState -> a
-- pooy f (MuniVic m) = f m
-- pooy f (MuniQld m) = f m
-- pooy f (MuniSA  m) = f m

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

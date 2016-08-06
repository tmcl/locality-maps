{-# LANGUAGE OverloadedStrings #-}

module Victoria (victorianLocalities)
where

-- This module reads csv files

import Data.Csv
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Municipality
import Logic

type VictorianGazettals = V.Vector VictorianGazettal

victorianLocalities :: BS.ByteString -> Either String VictorianLocalities
victorianLocalities csv = second victorianLocalitiesFromCsv (readVictorianLocalities csv)

readVictorianLocalities :: BS.ByteString -> Either String VictorianGazettals
readVictorianLocalities = decode HasHeader

isCurrent :: VictorianGazettal -> Bool
isCurrent l = vlStatus l == VlsRegistered

isLocality :: VictorianGazettal -> Bool
isLocality l = vlType l == LOCB

victorianLocalitiesFromCsv :: VictorianGazettals -> VictorianLocalities
victorianLocalitiesFromCsv csv = do
   locality <- V.filter (isCurrent ⋀ isLocality) csv
   return $ victorianLocalityFromCsv locality

victorianLocalityFromCsv :: VictorianGazettal -> Locality VictorianMunicipality
victorianLocalityFromCsv csv = Locality {
   localityMunicipality = (victorianMunicipalityFromCsv . vlMunicipality) csv,
   localityName = vlName csv,
   localityType = "LOCB"
}

data VictorianGazettalStatus 
   = VlsRegistered 
   | VlsRecorded 
   | VlsIgnorable
   | VlsHistorical
   | VlsArchived
   | VlsProposedNew
   | VlsProposedChange
   | VlsBase
      deriving (Eq, Show)

instance FromField VictorianGazettalStatus where
   parseField "REGISTERED" = pure VlsRegistered
   parseField "RECORDED" = pure VlsRecorded
   parseField "HISTORICAL" = pure VlsHistorical
   parseField "ARCHIVED" = pure VlsArchived
   parseField "PROPOSED NEW" = pure VlsProposedNew
   parseField "PROPOSED CHANGE" = pure VlsProposedChange
   parseField "BASE" = pure VlsBase
   parseField p = error $ show p

instance FromRecord VictorianGazettal where
   parseRecord m = VictorianGazettal <$> m .! 1 <*> m .! 3 <*> m .! 4 <*> m .! 5

data VictorianMunicipalityFromCsv = VictorianMunicipalityFromCsv {
   victorianMunicipalityFromCsv :: VictorianMunicipality
}
   deriving (Show)

instance FromField VictorianMunicipalityFromCsv where
   parseField s 
      | s == "ALPINE SHIRE" = use VicAlpine
      | s == "FALLS CREEK ALPINE RESORT (UNINCORPORATED)" = use VicAlpine
      | s == "MOUNT HOTHAM ALPINE RESORT (UNINCORPORATED)" = use VicAlpine
      | s == "ARARAT RURAL CITY" = use VicArarat
      | s == "BALLARAT CITY" = use VicBallarat
      | s == "BANYULE CITY" = use VicBanyule
      | s == "BASS COAST SHIRE" = use VicBassCoast
      | s == "BAW BAW SHIRE" = use VicBawBaw
      | s == "BAYSIDE CITY" = use VicBayside
      | s == "BENALLA RURAL CITY" = use VicBenalla
      | s == "GREATER BENDIGO CITY" = use VicBendigo
      | s == "BOROONDARA CITY" = use VicBoroondara
      | s == "BRIMBANK CITY" = use VicBrimbank
      | s == "BULOKE SHIRE" = use VicBuloke
      | s == "CAMPASPE SHIRE" = use VicCampaspe
      | s == "CARDINIA SHIRE" = use VicCardinia
      | s == "CASEY CITY" = use VicCasey
      | s == "CENTRAL GOLDFIELDS SHIRE" = use VicCentralGoldfields
      | s == "COLAC OTWAY SHIRE" = use VicColacOtway
      | s == "CORANGAMITE SHIRE" = use VicCorangamite
      | s == "GREATER DANDENONG CITY" = use VicDandenong
      | s == "DAREBIN CITY" = use VicDarebin
      | s == "EAST GIPPSLAND SHIRE" = use VicEastGippsland
      | s == "FRANKSTON CITY" = use VicFrankston
      | s == "FRENCH-ELIZABETH-SANDSTONE ISLANDS (UNINCORPORATED)" = use VicWesternportIslands
      | s == "FRENCH-ELIZABETH-SANDSTONE ISLANDS (UNINC)" = use VicWesternportIslands
      | s == "GANNAWARRA SHIRE" = use VicGannawarra
      | s == "GREATER GEELONG CITY" = use VicGeelong
      | s == "GLEN EIRA CITY" = use VicGlenEira
      | s == "GLENELG SHIRE" = use VicGlenelg
      | s == "GOLDEN PLAINS SHIRE" = use VicGoldenPlains
      | s == "HEPBURN SHIRE" = use VicHepburn
      | s == "HINDMARSH SHIRE" = use VicHindmarsh
      | s == "HOBSONS BAY CITY" = use VicHobsonsBay
      | s == "HORSHAM RURAL CITY" = use VicHorsham
      | s == "HUME CITY" = use VicHume
      | s == "INDIGO SHIRE" = use VicIndigo
      | s == "KINGSTON CITY" = use VicKingston
      | s == "KNOX CITY" = use VicKnox
      | s == "LATROBE CITY" = use VicLatrobe
      | s == "LODDON SHIRE" = use VicLoddon
      | s == "MACEDON RANGES SHIRE" = use VicMacedonRanges
      | s == "MANNINGHAM CITY" = use VicManningham
      | s == "MANSFIELD SHIRE" = use VicMansfield
      | s == "MARIBYRNONG CITY" = use VicMaribyrnong
      | s == "MAROONDAH CITY" = use VicMaroondah
      | s == "MELBOURNE CITY" = use VicMelbourne
      | s == "DOCKLANDS" = use VicMelbourne
      | s == "MELTON CITY" = use VicMelton
      | s == "MILDURA RURAL CITY" = use VicMildura
      | s == "MITCHELL SHIRE" = use VicMitchell
      | s == "MOIRA SHIRE" = use VicMoira
      | s == "MONASH CITY" = use VicMonash
      | s == "MOONEE VALLEY CITY" = use VicMooneeValley
      | s == "MOORABOOL SHIRE" = use VicMoorabool
      | s == "MORELAND CITY" = use VicMoreland
      | s == "MORNINGTON PENINSULA SHIRE" = use VicMorningtonPeninsula
      | s == "MOUNT ALEXANDER SHIRE" = use VicMtAlexander
      | s == "MOYNE SHIRE" = use VicMoyne
      | s == "MURRINDINDI SHIRE" = use VicMurrindindi
      | s == "NILLUMBIK SHIRE" = use VicNillumbik
      | s == "NORTHERN GRAMPIANS SHIRE" = use VicNorthernGrampians
      | s == "PORT PHILLIP CITY" = use VicPortPhillip
      | s == "PYRENEES SHIRE" = use VicPyrenees
      | s == "QUEENSCLIFFE BOROUGH" = use VicQueenscliffe
      | s == "GREATER SHEPPARTON CITY" = use VicShepparton
      | s == "SOUTHERN GRAMPIANS SHIRE" = use VicSouthernGrampians
      | s == "SOUTH GIPPSLAND SHIRE" = use VicSouthGippsland
      | s == "STONNINGTON CITY" = use VicStonnington
      | s == "STRATHBOGIE SHIRE" = use VicStrathbogie
      | s == "SURF COAST SHIRE" = use VicSurfCoast
      | s == "SWAN HILL RURAL CITY" = use VicSwanHill
      | s == "TOWONG SHIRE" = use VicTowong
      | s == "UNINCORPORATED" = use VicWesternportIslands
      | s == "WANGARATTA RURAL CITY" = use VicWangaratta
      | s == "WARRNAMBOOL CITY" = use VicWarrnambool
      | s == "WELLINGTON SHIRE" = use VicWellington
      | s == "WEST WIMMERA SHIRE" = use VicWestWimmera
      | s == "WHITEHORSE CITY" = use VicWhitehorse
      | s == "WHITTLESEA CITY" = use VicWhittlesea
      | s == "WODONGA CITY" = use VicWodonga
      | s == "WYNDHAM CITY" = use VicWyndham
      | s == "YARRA CITY" = use VicYarra
      | s == "YARRA RANGES SHIRE" = use VicYarraRanges
      | s == "YARRIAMBIACK SHIRE" = use VicYarriambiack
      | s == "UNKNOWN" = use VicNowhere
      | s == "MOUNT STIRLING ALPINE RESORT (UNINCORPORATED)" = use VicMansfield
      | s == "MOUNT BULLER ALPINE RESORT (UNINCORPORATED)" = use VicMansfield
      | otherwise = error $ show s
         where 
            use = pure . VictorianMunicipalityFromCsv

data VictorianGazettal = VictorianGazettal {
   vlMunicipality :: VictorianMunicipalityFromCsv,
   vlName :: T.Text,
   vlStatus :: VictorianGazettalStatus,
   vlType :: GazetteTypeCode
}
   deriving (Show)

data GazetteTypeCode = LOCB | Other
   deriving (Eq, Show)
instance FromField GazetteTypeCode where
   parseField "LOCB" = pure LOCB
   parseField _ = pure Other


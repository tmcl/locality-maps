{-# LANGUAGE OverloadedStrings #-}
module SouthAustralia (getSouthAustralianLocalities)
where

import qualified Data.Text as T
import Data.Maybe
import Database.Shapefile
import Municipality
import Shapefile

getSouthAustralianLocalities :: FilePath -> IO SouthAustralianLocalities
getSouthAustralianLocalities fp = do
    dbfHandle <- openDbf fp True
    numRecords <- dbfNumRecords dbfHandle
    closeDbf dbfHandle

    shpHandle <- openShp fp True
    fields <- shpDbfFields shpHandle
    let myFields = pickFields "LGA" "F_CODE" "NAME" fields
    records <- mapM (getShpRecord shpHandle) [0..(fromInteger numRecords-1)]
    results2 <- maybe (return []) (\ff -> mapM (recordToGazettal ff) (mapMaybe snd records)) myFields
    closeShp shpHandle
    return (map genericToSouthAustralian (filterLOCBs results2))

genericToSouthAustralian :: GenericGazettal -> SouthAustralianLocality
genericToSouthAustralian gg = Locality {
   localityMunicipality = parse (ggMunicipality gg),
   localityName = ggName gg,
   localityType = ggType gg
}

parse :: T.Text -> SouthAustralianMunicipality
parse "The District Council of Peterborough" = SAPeterborough
parse "Naracoorte Lucindale Council" = SANaracoorteLucindale
parse "Alexandrina Council" = SAAlexandrina
parse "The District Council of Lower Eyre Peninsula" = SALowerEyrePeninsula
parse "District Council of Lower Eyre Peninsula" = SALowerEyrePeninsula
parse "City of Mount Gambier" = SAMtGambier
parse "Out of Councils" = SAOutOfCouncils
parse "The Barossa Council" = SABarossa
parse "Wakefield Regional Council" = SAWakefield
parse "Light Regional Council" = SALight
parse "Mid Murray Council" = SAMidMurray
parse "Kingston District Council" = SAKingston
parse "Kangaroo Island Council" = SAKangarooIsland
parse "The Berri Barmera Council" = SABerriBarmera
parse "The District Council of Mount Barker" = SAMtBarker
parse "The Rural City of Murray Bridge" = SAMurrayBridge
parse "Rural City of Murray Bridge" = SAMurrayBridge
parse "The District Council of Mallala" = SAMallala
parse "The District Council of Grant" = SAGrant
parse "The District Council of Kimba" = SAKimba
parse "District Council of The Copper Coast" = SACopperCoast
parse "Adelaide Hills Council" = SAAdelaideHills
parse "The District Council of Ceduna" = SACeduna
parse "The Flinders Ranges Council" = SAFlindersRanges
parse "The Regional Council of Goyder" = SAGoyder
parse "Regional Council of Goyder" = SAGoyder
parse "Coorong District Council" = SACoorong
parse "The District Council of Loxton Waikerie" = SACoorong
parse "The District Council of Cleve" = SACleve
parse "The Corporation of the City of Whyalla" = SAWhyalla
parse "The District Council of Mount Remarkable" = SAMtRemarkable
parse "The District Council of Yankalilla" = SAYankalilla
parse "The District Council of Streaky Bay" = SAStreakyBay
parse "City of Victor Harbor" = SAVictorHarbor
parse "Clare and Gilbert Valleys Council" = SAClareAndGilbertValleys
parse "Wattle Range Council" = SAWattleRange
parse "Tatiara District Council" = SATatiara
parse "Yorke Peninsula Council" = SAYorkePeninsula
parse "Northern Areas Council" = SANorthernAreas
parse "The District Council of Orroroo/Carrieton" = SAOrrorooCarrieton
parse "Renmark Paringa Council" = SARenmarkParinga
parse "City of Port Lincoln" = SAPortLincoln
parse "The District Council of Barunga West" = SABarungaWest
parse "Port Pirie Regional Council" = SAPortPirie
parse "The District Council of Tumby Bay" = SATumbyBay
parse "City of Onkaparinga" = SAOnkaparinga
parse "The District Council of Karoonda East Murray" = SAKaroondaEastMurray
parse "The District Council of Franklin Harbour" = SAFranklinHarbor
parse "City of Playford" = SAPlayford
parse "District Council of Robe" = SARobe
parse "Disctict Council of Elliston" = SAElliston
parse "Southern Mallee District Council" = SASouthernMallee
parse "Port Augusta City Council" = SAPortAugusta
parse "The Corporation of the City of Port Augusta" = SAPortAugusta
parse "Wudinna District Council" = SAWudinna
parse "The District Council Coober Pedy" = SACooberPedy
parse "Municipal Council of Roxby Downs" = SARoxbyDowns
parse "City of Port Adelaide Enfield" = SAPortAdelaideEnfield
parse "The City of Prospect" = SAProspect
parse "City of Burnside" = SABurnside
parse "City of Unley" = SAUnley
parse "City of Salisbury" = SASalisbury
parse "City of Tea Tree Gully" = SATeaTreeGully
parse "City of Marion" = SAMarion
parse "City of Charles Sturt" = SACharlesSturt
parse "City of Mitcham" = SAMitcham
parse "City of West Torrens" = SAWestTorrens
parse "Town of Gawler" = SAGawler
parse "Campbelltown City Council" = SACampbelltown
parse "City of Holdfast Bay" = SAHoldfastBay
parse "The City of Norwood Payneham and St Peters" = SANorwoodPaynehamAndStPeters
parse "The Corporation of the Town of Walkerville" = SAWalkerville
parse "Adelaide City Council" = SAAdelaide
parse other = (error . T.unpack) other

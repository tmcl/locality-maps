{-# LANGUAGE OverloadedStrings #-}
module Queensland (getQueenslandLocalities)
where

import qualified Data.Text as T

import Municipality
import Shapefile

getQueenslandLocalities :: FilePath -> IO QueenslandLocalities
getQueenslandLocalities fp = do
    gazettals <- getGazettals "LGA_NAME" "TYPE" "PLACE_NAME" fp
    return $ map genericToQueensland (filterLOCBs gazettals)

genericToQueensland :: GenericGazettal -> QueenslandLocality
genericToQueensland gg = Locality {
   localityMunicipality = parse (ggMunicipality gg),
   localityName = ggName gg,
   localityType = ggType gg
}

parse :: T.Text -> QueenslandMunicipality
parse "" = QldUnincorporated
parse "Bundaberg Regional" = QldBundaberg
parse "Brisbane City" = QldBrisbane
parse "Aurukun Shire" = QldAurukun
parse "Balonne Shire" = QldBalonne
parse "Banana Shire" = QldBanana
parse "Barcaldine Regional" = QldBarcaldine
parse "Barcoo Shire" = QldBarcoo
parse "Blackall Tambo Regional" = QldBlackallTambo
parse "Boulia Shire" = QldBoulia
parse "Bulloo Shire" = QldBulloo
parse "Burdekin Shire" = QldBurdekin
parse "Burke Shire" = QldBurke
parse "Cairns Regional" = QldCairns
parse "Carpentaria Shire" = QldCarpentaria
parse "Cassowary Coast Regional" = QldCassowaryCoast
parse "Central Highlands Regional" = QldCentralHighlands
parse "Charters Towers Regional" = QldChartersTowers
parse "Cherbourg Aboriginal Shire" = QldCherbourg
parse "Cloncurry Shire" = QldCloncurry
parse "Cook Shire" = QldCook
parse "Croydon Shire" = QldCroydon
parse "Diamantina Shire" = QldDiamantina
parse "Douglas Shire" = QldDouglas
parse "Etheridge Shire" = QldEtheridge
parse "Flinders Shire" = QldFlinders
parse "Fraser Coast Regional" = QldFraserCoast
parse "Gladstone Regional" = QldGladstone
parse "Gold Coast City" = QldGoldCoast
parse "Goondiwindi Regional" = QldGoondiwindi
parse "Gympie Regional" = QldGympie
parse "Hinchinbrook Shire" = QldHinchinbrook
parse "Hope Vale Aboriginal Shire" = QldHopeVale
parse "Ipswich City" = QldIpswich
parse "Isaac Regional" = QldIsaac
parse "Kowanyama Aboriginal Shire" = QldKowanyama
parse "Livingstone Shire" = QldLivingstone
parse "Lockhart River Aboriginal Shire" = QldLockhartRiver
parse "Lockyer Valley Regional" = QldLockyerValley
parse "Logan City" = QldLogan
parse "Longreach Regional" = QldLongreach
parse "Mackay Regional" = QldMackay
parse "Mapoon Aboriginal Shire" = QldMapoon
parse "Maranoa Regional" = QldMaranoa
parse "Mareeba Shire" = QldMareeba
parse "Mckinlay Shire" = QldMcKinlay
parse "Moreton Bay Regional" = QldMoretonBay
parse "Mornington Shire" = QldMornington
parse "Mount Isa City" = QldMountIsa
parse "Murweh Shire" = QldMurweh
parse "Napranum Aboriginal Shire" = QldNapranum
parse "Noosa Shire" = QldNoosa
parse "North Burnett Regional" = QldNorthBurnett
parse "Northern Peninsula Area Regional" = QldNorthernPeninsula
parse "Palm Island Aboriginal Shire" = QldPalmIsland
parse "Paroo Shire" = QldParoo
parse "Pormpuraaw Aboriginal Shire" = QldPormpuraaw
parse "Quilpie Shire" = QldQuilpie
parse "Redland City" = QldRedland
parse "Richmond Shire" = QldRichmond
parse "Rockhampton Regional" = QldRockhampton
parse "Scenic Rim Regional" = QldScenicRim
parse "Somerset Regional" = QldSomerset
parse "South Burnett Regional" = QldSouthBurnett
parse "Southern Downs Regional" = QldSouthernDowns
parse "Sunshine Coast Regional" = QldSunshineCoast
parse "Tablelands Regional" = QldTablelands
parse "Toowoomba Regional" = QldToowoomba
parse "Torres Shire" = QldTorres
parse "Torres Strait Island Regional" = QldTorresStraitIsland
parse "Townsville City" = QldTownsville
parse "Weipa Town" = QldWeipaTown
parse "Western Downs Regional" = QldWesternDowns
parse "Whitsunday Regional" = QldWhitsunday
parse "Winton Shire" = QldWinton
parse "Woorabinda Aboriginal Shire" = QldWoorabinda
parse "Yarrabah Aboriginal Shire" = QldYarrabah
parse "Wujal Wujal Aboriginal Shire" = QldWujalWujal
parse "Doomadgee Aboriginal Shire" = QldDoomadgee
parse other = error $ "Unknown qld muni: '" ++ T.unpack other ++ "'"

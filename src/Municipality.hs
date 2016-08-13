module Municipality
where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S

-- the states and inhabited territories
data State = -- NSW |
   Vic
   | Qld
   -- | WA
   | SA
   -- | Tas
   -- | ACT
   -- | NT
   -- | Nfk
   -- | CC
   -- | CX
   -- | JBT
   deriving (Eq, Show, Read)

vectorToSet :: Ord a => V.Vector a -> S.Set a
vectorToSet = V.foldr S.insert S.empty

data Municipality m => Locality m = Locality {
   localityMunicipality :: m,
   localityName :: T.Text,
   localityType :: T.Text
}
   deriving (Show, Eq)

instance (Municipality l) => Ord (Locality l) where
   compare a b = compare (localityName a) (localityName b)

data Localities = Localities {
   locVictorian :: VictorianLocalities,
   locSouthAustralian :: SouthAustralianLocalities,
   locQueensland :: QueenslandLocalities
}
   deriving (Show)

type VictorianLocality = Locality VictorianMunicipality
type VictorianLocalities = V.Vector VictorianLocality

type SouthAustralianLocality = Locality SouthAustralianMunicipality
type SouthAustralianLocalities = [SouthAustralianLocality]

type QueenslandLocality = Locality QueenslandMunicipality
type QueenslandLocalities = [QueenslandLocality]

data MunicipalityKind = City | Shire | Municipality | Region | District | Borough | RuralCity | AboriginalShire | Area | Unincorporated | Town | MetropolitanCity | GreaterCity | GreaterMetropolitanCity
   deriving (Show, Eq)

class (Show m, Eq m) => Municipality m where
   localitiesByMunicipality :: Localities -> m -> S.Set T.Text
   lgaShpName :: m -> T.Text
   isMunicipalityByName :: m -> (T.Text, String) -> Bool
   municipalityState :: m -> State
   municipalityKind :: m -> MunicipalityKind
   fullname :: m -> String

allVictorian :: [VictorianMunicipality]
allVictorian = [ VicMelbourne,
                 VicPortPhillip,
                 VicStonnington,
                 VicYarra,
                 VicBanyule,
                 VicBayside,
                 VicBoroondara,
                 VicBrimbank,
                 VicDarebin,
                 VicGlenEira,
                 VicHobsonsBay,
                 VicKingston,
                 VicManningham,
                 VicMaribyrnong,
                 VicMonash,
                 VicMooneeValley,
                 VicMoreland,
                 VicWhitehorse,
                 VicCardinia,
                 VicCasey,
                 VicFrankston,
                 VicDandenong,
                 VicHume,
                 VicKnox,
                 VicMaroondah,
                 VicMelton,
                 VicMorningtonPeninsula,
                 VicNillumbik,
                 VicWhittlesea,
                 VicWyndham,
                 VicYarraRanges,
                 VicColacOtway,
                 VicGoldenPlains,
                 VicGeelong,
                 VicQueenscliffe,
                 VicSurfCoast,
                 VicBallarat,
                 VicCentralGoldfields,
                 VicBendigo,
                 VicHepburn,
                 VicLoddon,
                 VicMacedonRanges,
                 VicMitchell,
                 VicMoorabool,
                 VicMtAlexander,
                 VicCampaspe,
                 VicShepparton,
                 VicMoira,
                 VicStrathbogie,
                 VicAlpine,
                 VicBenalla,
                 VicIndigo,
                 VicMansfield,
                 VicMurrindindi,
                 VicTowong,
                 VicWangaratta,
                 VicWodonga,
                 VicBassCoast,
                 VicBawBaw,
                 VicEastGippsland,
                 VicLatrobe,
                 VicSouthGippsland,
                 VicWellington,
                 VicArarat,
                 VicCorangamite,
                 VicGlenelg,
                 VicMoyne,
                 VicPyrenees,
                 VicSouthernGrampians,
                 VicWarrnambool,
                 VicHindmarsh,
                 VicHorsham,
                 VicNorthernGrampians,
                 VicWestWimmera,
                 VicYarriambiack,
                 VicBuloke,
                 VicGannawarra,
                 VicMildura,
                 VicSwanHill,
                 VicWesternportIslands ]

data VictorianMunicipality
   = VicNowhere
   | VicMelbourne
   | VicPortPhillip
   | VicStonnington
   | VicYarra
   | VicBanyule
   | VicBayside
   | VicBoroondara
   | VicBrimbank
   | VicDarebin
   | VicGlenEira
   | VicHobsonsBay
   | VicKingston
   | VicManningham
   | VicMaribyrnong
   | VicMonash
   | VicMooneeValley
   | VicMoreland
   | VicWhitehorse
   | VicCardinia
   | VicCasey
   | VicFrankston
   | VicDandenong
   | VicHume
   | VicKnox
   | VicMaroondah
   | VicMelton
   | VicMorningtonPeninsula
   | VicNillumbik
   | VicWhittlesea
   | VicWyndham
   | VicYarraRanges
   | VicColacOtway
   | VicGoldenPlains
   | VicGeelong
   | VicQueenscliffe
   | VicSurfCoast
   | VicBallarat
   | VicCentralGoldfields
   | VicBendigo
   | VicHepburn
   | VicLoddon
   | VicMacedonRanges
   | VicMitchell
   | VicMoorabool
   | VicMtAlexander
   | VicCampaspe
   | VicShepparton
   | VicMoira
   | VicStrathbogie
   | VicAlpine
   | VicBenalla
   | VicIndigo
   | VicMansfield
   | VicMurrindindi
   | VicTowong
   | VicWangaratta
   | VicWodonga
   | VicBassCoast
   | VicBawBaw
   | VicEastGippsland
   | VicLatrobe
   | VicSouthGippsland
   | VicWellington
   | VicArarat
   | VicCorangamite
   | VicGlenelg
   | VicMoyne
   | VicPyrenees
   | VicSouthernGrampians
   | VicWarrnambool
   | VicHindmarsh
   | VicHorsham
   | VicNorthernGrampians
   | VicWestWimmera
   | VicYarriambiack
   | VicBuloke
   | VicGannawarra
   | VicMildura
   | VicSwanHill
   | VicWesternportIslands
   deriving (Eq, Show, Read)

instance Municipality VictorianMunicipality where
   localitiesByMunicipality localities m = 
      vectorToSet $ victorianLocalitiesByMunicipality (locVictorian localities) m
   lgaShpName VicSwanHill = "SWAN HILL"
   lgaShpName m = error . show $ m
   municipalityState _ = Vic
   isMunicipalityByName c ("VIC_LGA__3", b) = lgaShpName c == (T.strip . T.pack $ b)
   isMunicipalityByName _ _ = False
   municipalityKind VicWesternportIslands = Unincorporated
   municipalityKind VicArarat = RuralCity
   municipalityKind VicBallarat = City
   municipalityKind VicBanyule = MetropolitanCity
   municipalityKind VicBayside = MetropolitanCity
   municipalityKind VicBenalla = RuralCity
   municipalityKind VicBendigo = GreaterCity
   municipalityKind VicBoroondara = MetropolitanCity
   municipalityKind VicBrimbank = MetropolitanCity
   municipalityKind VicCasey = MetropolitanCity
   municipalityKind VicDandenong = GreaterMetropolitanCity
   municipalityKind VicDarebin = MetropolitanCity
   municipalityKind VicFrankston = MetropolitanCity
   municipalityKind VicGeelong = GreaterCity
   municipalityKind VicHobsonsBay = MetropolitanCity
   municipalityKind VicHorsham = RuralCity
   municipalityKind VicHume = MetropolitanCity
   municipalityKind VicKingston = MetropolitanCity
   municipalityKind VicKnox = MetropolitanCity
   municipalityKind VicLatrobe = City
   municipalityKind VicManningham = MetropolitanCity
   municipalityKind VicMaribyrnong = MetropolitanCity
   municipalityKind VicMaroondah = MetropolitanCity
   municipalityKind VicMelbourne = MetropolitanCity
   municipalityKind VicMelton = MetropolitanCity
   municipalityKind VicMildura = RuralCity
   municipalityKind VicMonash = MetropolitanCity
   municipalityKind VicMooneeValley = MetropolitanCity
   municipalityKind VicMoreland = MetropolitanCity
   municipalityKind VicPortPhillip = MetropolitanCity
   municipalityKind VicQueenscliffe = Borough
   municipalityKind VicShepparton = GreaterCity
   municipalityKind VicStonnington = MetropolitanCity
   municipalityKind VicSwanHill = RuralCity
   municipalityKind VicWangaratta = RuralCity
   municipalityKind VicWarrnambool = City
   municipalityKind VicWhitehorse = MetropolitanCity
   municipalityKind VicWhittlesea = MetropolitanCity
   municipalityKind VicWodonga = City
   municipalityKind VicYarra = MetropolitanCity
   municipalityKind _ = Shire
   fullname m = (show m) ++ " " ++ (show $ municipalityKind m)

victorianLocalitiesByMunicipality 
   :: VictorianLocalities
   -> VictorianMunicipality 
   -> V.Vector T.Text
victorianLocalitiesByMunicipality localities municipality = 
   fmap localityName (V.filter (isInMunicipality municipality) localities)

isInMunicipality :: Municipality a => a -> Locality a -> Bool
isInMunicipality m l = localityMunicipality l == m

data QueenslandMunicipality
     = QldAurukun
     | QldBalonne
     | QldBanana
     | QldBarcaldine
     | QldBarcoo
     | QldBlackallTambo
     | QldBoulia
     | QldBulloo
     | QldBurdekin
     | QldBurke
     | QldCairns
     | QldCarpentaria
     | QldCassowaryCoast
     | QldCentralHighlands
     | QldChartersTowers
     | QldCherbourg
     | QldCloncurry
     | QldUnincorporated
     | QldBundaberg
     | QldBrisbane
     | QldCook
     | QldCroydon
     | QldDiamantina
     | QldDouglas
     | QldEtheridge
     | QldFlinders
     | QldFraserCoast
     | QldGladstone
     | QldGoldCoast
     | QldGoondiwindi
     | QldGympie
     | QldHinchinbrook
     | QldHopeVale
     | QldIpswich
     | QldIsaac
     | QldKowanyama
     | QldLivingstone
     | QldLockhartRiver
     | QldLockyerValley
     | QldLogan
     | QldLongreach
     | QldMackay
     | QldMapoon
     | QldMaranoa
     | QldMareeba
     | QldMcKinlay
     | QldMoretonBay
     | QldMornington
     | QldMountIsa
     | QldMurweh
     | QldNapranum
     | QldNoosa
     | QldNorthBurnett
     | QldNorthernPeninsula
     | QldPalmIsland
     | QldParoo
     | QldPormpuraaw
     | QldQuilpie
     | QldRedland
     | QldRichmond
     | QldRockhampton
     | QldScenicRim
     | QldSomerset
     | QldSouthBurnett
     | QldSouthernDowns
     | QldSunshineCoast
     | QldTablelands
     | QldToowoomba
     | QldTorres
     | QldTorresStraitIsland
     | QldTownsville
     | QldWeipaTown
     | QldWesternDowns
     | QldWhitsunday
     | QldWinton
     | QldWoorabinda
     | QldYarrabah
     | QldWujalWujal
     | QldDoomadgee


    deriving (Eq, Show, Read)
instance Municipality QueenslandMunicipality where
   localitiesByMunicipality l m = S.fromList $ listLocalityByMunicipality (locQueensland l) m
   municipalityState _ = Qld

   isMunicipalityByName c ("QLD_LGA__3", b) = lgaShpName c == (T.strip . T.pack $ b)
   isMunicipalityByName _ _ = False

   lgaShpName QldBrisbane = "BRISBANE"
   lgaShpName QldAurukun = "AURUKUN"

   lgaShpName QldAurukun = "AURUKUN"
   lgaShpName QldBalonne = "BALONNE"
   lgaShpName QldBanana = "BANANA"
   lgaShpName QldBarcaldine = "BARCALDINE"
   lgaShpName QldBarcoo = "BARCOO"
   lgaShpName QldBlackallTambo = "BLACKALL TAMBO"
   lgaShpName QldBoulia = "BOULIA"
   lgaShpName QldBulloo = "BULLOO"
   lgaShpName QldBurdekin = "BURDEKIN"
   lgaShpName QldBurke = "BURKE"
   lgaShpName QldCairns = "CAIRNS"
   lgaShpName QldCarpentaria = "CARPENTARIA"
   lgaShpName QldCassowaryCoast = "CASSOWARY COAST"
   lgaShpName QldCentralHighlands = "CENTRAL HIGHLANDS"
   lgaShpName QldChartersTowers = "CHARTERS TOWERS"
   lgaShpName QldCherbourg = "CHERBOURG"
   lgaShpName QldCloncurry = "CLONCURRY"
   lgaShpName QldUnincorporated = "UNINCORPORATED"
   lgaShpName QldBundaberg = "BUNDABERG"
   lgaShpName QldBrisbane = "BRISBANE"
   lgaShpName QldCook = "COOK"
   lgaShpName QldCroydon = "CROYDON"
   lgaShpName QldDiamantina = "DIAMANTINA"
   lgaShpName QldDouglas = "DOUGLAS"
   lgaShpName QldEtheridge = "ETHERIDGE"
   lgaShpName QldFlinders = "FLINDERS"
   lgaShpName QldFraserCoast = "FRASER COAST"
   lgaShpName QldGladstone = "GLADSTONE"
   lgaShpName QldGoldCoast = "GOLD COAST"
   lgaShpName QldGoondiwindi = "GOONDIWINDI"
   lgaShpName QldGympie = "GYMPIE"
   lgaShpName QldHinchinbrook = "HINCHINBROOK"
   lgaShpName QldHopeVale = "HOPE VALE"
   lgaShpName QldIpswich = "IPSWICH"
   lgaShpName QldIsaac = "ISAAC"
   lgaShpName QldKowanyama = "KOWANYAMA"
   lgaShpName QldLivingstone = "LIVINGSTONE"
   lgaShpName QldLockhartRiver = "LOCKHART RIVER"
   lgaShpName QldLockyerValley = "LOCKYER VALLEY"
   lgaShpName QldLogan = "LOGAN"
   lgaShpName QldLongreach = "LONGREACH"
   lgaShpName QldMackay = "MACKAY"
   lgaShpName QldMapoon = "MAPOON"
   lgaShpName QldMaranoa = "MARANOA"
   lgaShpName QldMareeba = "MAREEBA"
   lgaShpName QldMcKinlay = "MCKINLAY"
   lgaShpName QldMoretonBay = "MORETON BAY"
   lgaShpName QldMornington = "MORNINGTON"
   lgaShpName QldMountIsa = "MOUNT ISA"
   lgaShpName QldMurweh = "MURWEH"
   lgaShpName QldNapranum = "NAPRANUM"
   lgaShpName QldNoosa = "NOOSA"
   lgaShpName QldNorthBurnett = "NORTH BURNETT"
   lgaShpName QldNorthernPeninsula = "NORTHERN PENINSULA AREA"
   lgaShpName QldPalmIsland = "PALM ISLAND"
   lgaShpName QldParoo = "PAROO"
   lgaShpName QldPormpuraaw = "PORMPURAAW"
   lgaShpName QldQuilpie = "QUILPIE"
   lgaShpName QldRedland = "REDLAND"
   lgaShpName QldRichmond = "RICHMOND"
   lgaShpName QldRockhampton = "ROCKHAMPTON"
   lgaShpName QldScenicRim = "SCENIC RIM"
   lgaShpName QldSomerset = "SOMERSET"
   lgaShpName QldSouthBurnett = "SOUTH BURNETT"
   lgaShpName QldSouthernDowns = "SOUTHERN DOWNS"
   lgaShpName QldSunshineCoast = "SUNSHINE COAST"
   lgaShpName QldTablelands = "TABLELANDS"
   lgaShpName QldToowoomba = "TOOWOOMBA"
   lgaShpName QldTorres = "TORRES"
   lgaShpName QldTorresStraitIsland = "TORRES STRAIT ISLAND"
   lgaShpName QldTownsville = "TOWNSVILLE"
   lgaShpName QldWeipaTown = "WEIPA"
   lgaShpName QldWesternDowns = "WESTERN DOWNS"
   lgaShpName QldWhitsunday = "WHITSUNDAY"
   lgaShpName QldWinton = "WINTON"
   lgaShpName QldWoorabinda = "WOORABINDA"
   lgaShpName QldYarrabah = "YARRABAH"
   lgaShpName QldWujalWujal = "WUJAL WUJAL"
   lgaShpName QldDoomadgee = "DOOMADGEE"
   lgaShpName m = error . show $ m

   municipalityKind QldBundaberg = Region
   municipalityKind QldBrisbane = City
   municipalityKind QldAurukun = Shire
   municipalityKind QldBalonne = Shire
   municipalityKind QldBanana = Shire
   municipalityKind QldBarcaldine = Region
   municipalityKind QldBarcoo = Shire
   municipalityKind QldBlackallTambo = Region
   municipalityKind QldBoulia = Shire
   municipalityKind QldBulloo = Shire
   municipalityKind QldBurdekin = Shire
   municipalityKind QldBurke = Shire
   municipalityKind QldCairns = Region
   municipalityKind QldCarpentaria = Shire
   municipalityKind QldCassowaryCoast = Region
   municipalityKind QldCentralHighlands = Region
   municipalityKind QldChartersTowers = Region
   municipalityKind QldCherbourg = AboriginalShire
   municipalityKind QldCloncurry = Shire
   municipalityKind QldCook = Shire
   municipalityKind QldCroydon = Shire
   municipalityKind QldDiamantina = Shire
   municipalityKind QldDouglas = Shire
   municipalityKind QldEtheridge = Shire
   municipalityKind QldFlinders = Shire
   municipalityKind QldFraserCoast = Region
   municipalityKind QldGladstone = Region
   municipalityKind QldGoldCoast = City
   municipalityKind QldGoondiwindi = Region
   municipalityKind QldGympie = Region
   municipalityKind QldHinchinbrook = Shire
   municipalityKind QldHopeVale = AboriginalShire
   municipalityKind QldIpswich = City
   municipalityKind QldIsaac = Region
   municipalityKind QldKowanyama = AboriginalShire
   municipalityKind QldLivingstone = Shire
   municipalityKind QldLockhartRiver = AboriginalShire
   municipalityKind QldLockyerValley = Region
   municipalityKind QldLogan = City
   municipalityKind QldLongreach = Region
   municipalityKind QldMackay = Region
   municipalityKind QldMapoon = AboriginalShire
   municipalityKind QldMaranoa = Region
   municipalityKind QldMareeba = Shire
   municipalityKind QldMcKinlay = Shire
   municipalityKind QldMoretonBay = Region
   municipalityKind QldMornington = Shire
   municipalityKind QldMountIsa = City
   municipalityKind QldMurweh = Shire
   municipalityKind QldNapranum = AboriginalShire
   municipalityKind QldNoosa = Shire
   municipalityKind QldNorthBurnett = Region
   municipalityKind QldNorthernPeninsula = Region
   municipalityKind QldPalmIsland = AboriginalShire
   municipalityKind QldParoo = Shire
   municipalityKind QldPormpuraaw = AboriginalShire
   municipalityKind QldQuilpie = Shire
   municipalityKind QldRedland = City
   municipalityKind QldRichmond = Shire
   municipalityKind QldRockhampton = Region
   municipalityKind QldScenicRim = Region
   municipalityKind QldSomerset = Region
   municipalityKind QldSouthBurnett = Region
   municipalityKind QldSouthernDowns = Region
   municipalityKind QldSunshineCoast = Region
   municipalityKind QldTablelands = Region
   municipalityKind QldToowoomba = Region
   municipalityKind QldTorres = Shire
   municipalityKind QldTorresStraitIsland = Region
   municipalityKind QldTownsville = City
   municipalityKind QldWeipaTown = Town
   municipalityKind QldWesternDowns = Region
   municipalityKind QldWhitsunday = Region
   municipalityKind QldWinton = Shire
   municipalityKind QldWoorabinda = AboriginalShire
   municipalityKind QldYarrabah = AboriginalShire
   municipalityKind QldWujalWujal = AboriginalShire
   municipalityKind QldDoomadgee = AboriginalShire
   fullname m = (T.unpack $ lgaShpName m) ++ " " ++ (show $ municipalityKind m)

allQueensland = [
     QldAurukun
     , QldBalonne
     , QldBanana
     , QldBarcaldine
     , QldBarcoo
     , QldBlackallTambo
     , QldBoulia
     , QldBulloo
     , QldBurdekin
     , QldBurke
     , QldCairns
     , QldCarpentaria
     , QldCassowaryCoast
     , QldCentralHighlands
     , QldChartersTowers
     , QldCherbourg
     , QldCloncurry
     , QldUnincorporated
     , QldBundaberg
     , QldBrisbane
     , QldCook
     , QldCroydon
     , QldDiamantina
     , QldDouglas
     , QldEtheridge
     , QldFlinders
     , QldFraserCoast
     , QldGladstone
     , QldGoldCoast
     , QldGoondiwindi
     , QldGympie
     , QldHinchinbrook
     , QldHopeVale
     , QldIpswich
     , QldIsaac
     , QldKowanyama
     , QldLivingstone
     , QldLockhartRiver
     , QldLockyerValley
     , QldLogan
     , QldLongreach
     , QldMackay
     , QldMapoon
     , QldMaranoa
     , QldMareeba
     , QldMcKinlay
     , QldMoretonBay
     , QldMornington
     , QldMountIsa
     , QldMurweh
     , QldNapranum
     , QldNoosa
     , QldNorthBurnett
     , QldNorthernPeninsula
     , QldPalmIsland
     , QldParoo
     , QldPormpuraaw
     , QldQuilpie
     , QldRedland
     , QldRichmond
     , QldRockhampton
     , QldScenicRim
     , QldSomerset
     , QldSouthBurnett
     , QldSouthernDowns
     , QldSunshineCoast
     , QldTablelands
     , QldToowoomba
     , QldTorres
     , QldTorresStraitIsland
     , QldTownsville
     , QldWeipaTown
     , QldWesternDowns
     , QldWhitsunday
     , QldWinton
     , QldWoorabinda
     , QldYarrabah
     , QldWujalWujal
     , QldDoomadgee
     ]


listLocalityByMunicipality
   :: Municipality m => [Locality m]
   -> m
   -> [T.Text]
listLocalityByMunicipality localities municipality =
   fmap localityName (filter (isInMunicipality municipality) localities)

data SouthAustralianMunicipality
   = SAAdelaide
   | SAPeterborough
   | SANaracoorteLucindale
   | SAAlexandrina
   | SALowerEyrePeninsula
   | SAMtGambier
   | SAOutOfCouncils
   | SABarossa
   | SAWakefield
   | SALight
   | SAMidMurray
   | SAKingston
   | SAKangarooIsland
   | SABerriBarmera
   | SAMtBarker
   | SAMurrayBridge
   | SAMallala
   | SAGrant
   | SAKimba
   | SACopperCoast
   | SAAdelaideHills
   | SACeduna
   | SAFlindersRanges
   | SAGoyder
   | SACoorong
   | SALoxtonWaikerie
   | SACleve
   | SAWhyalla
   | SAMtRemarkable
   | SAYankalilla
   | SAStreakyBay
   | SAVictorHarbor
   | SAClareAndGilbertValleys
   | SAWattleRange
   | SATatiara
   | SAYorkePeninsula
   | SANorthernAreas
   | SAOrrorooCarrieton
   | SARenmarkParinga
   | SAPortLincoln
   | SABarungaWest
   | SAPortPirie
   | SATumbyBay
   | SAOnkaparinga
   | SAKaroondaEastMurray
   | SAFranklinHarbor
   | SAPlayford
   | SARobe
   | SAElliston
   | SASouthernMallee
   | SAPortAugusta
   | SAWudinna
   | SACooberPedy
   | SARoxbyDowns
   | SAPortAdelaideEnfield
   | SAProspect
   | SABurnside
   | SAUnley
   | SASalisbury
   | SATeaTreeGully
   | SAMarion
   | SACharlesSturt
   | SAMitcham
   | SAWestTorrens
   | SAGawler
   | SACampbelltown
   | SAHoldfastBay
   | SANorwoodPaynehamAndStPeters
   | SAWalkerville
   deriving (Eq, Show, Read)

instance Municipality SouthAustralianMunicipality where
   municipalityState _ = SA
   localitiesByMunicipality l m = S.fromList $ listLocalityByMunicipality (locSouthAustralian l) m
   lgaShpName m = error . show $ m

   isMunicipalityByName c ("SA__LGA__3", b) = lgaShpName c == (T.strip . T.pack $ b)
   isMunicipalityByName _ _ = False

   municipalityKind SAPeterborough              = District
   municipalityKind SANaracoorteLucindale       = Area
   municipalityKind SAAlexandrina               = Area
   municipalityKind SALowerEyrePeninsula        = District
   municipalityKind SAMtGambier                 = City
   municipalityKind SAOutOfCouncils             = Unincorporated
   municipalityKind SABarossa                   = Area
   municipalityKind SAWakefield                  = Region
   municipalityKind SALight                     = Region
   municipalityKind SAMidMurray                 = Area
   municipalityKind SAKingston                  = District
   municipalityKind SAKangarooIsland            = Area
   municipalityKind SABerriBarmera              = Area
   municipalityKind SAMtBarker                  = District
   municipalityKind SAMurrayBridge              = RuralCity
   municipalityKind SAMallala                   = District
   municipalityKind SAGrant                     = District
   municipalityKind SAKimba                     = District
   municipalityKind SACopperCoast               = District
   municipalityKind SAAdelaideHills             = Area
   municipalityKind SACeduna                    = District
   municipalityKind SAFlindersRanges            = Area
   municipalityKind SAGoyder                    = Region
   municipalityKind SACoorong                   = District
   municipalityKind SALoxtonWaikerie            = District
   municipalityKind SACleve                     = District
   municipalityKind SAWhyalla                   = City
   municipalityKind SAMtRemarkable              = District
   municipalityKind SAYankalilla                = District
   municipalityKind SAStreakyBay                = District
   municipalityKind SAVictorHarbor              = City
   municipalityKind SAClareAndGilbertValleys    = Area
   municipalityKind SAWattleRange               = Area
   municipalityKind SATatiara                   = District
   municipalityKind SAYorkePeninsula            = Area
   municipalityKind SANorthernAreas             = Area
   municipalityKind SAOrrorooCarrieton          = District
   municipalityKind SARenmarkParinga            = Area
   municipalityKind SAPortLincoln               = City
   municipalityKind SABarungaWest              = District
   municipalityKind SAPortPirie                 = Region
   municipalityKind SATumbyBay                  = District
   municipalityKind SAOnkaparinga               = City
   municipalityKind SAKaroondaEastMurray        = District
   municipalityKind SAFranklinHarbor            = District
   municipalityKind SAPlayford                  = City
   municipalityKind SARobe                      = District
   municipalityKind SAElliston                  = District
   municipalityKind SASouthernMallee            = District
   municipalityKind SAPortAugusta               = City
   municipalityKind SAWudinna                   = District
   municipalityKind SACooberPedy                = District
   municipalityKind SARoxbyDowns                = Municipality
   municipalityKind SAPortAdelaideEnfield       = City
   municipalityKind SAProspect                  = City
   municipalityKind SABurnside                  = City
   municipalityKind SAUnley                     = City
   municipalityKind SASalisbury                 = City
   municipalityKind SATeaTreeGully              = City
   municipalityKind SAMarion                    = City
   municipalityKind SACharlesSturt              = City
   municipalityKind SAMitcham                   = City
   municipalityKind SAWestTorrens               = City
   municipalityKind SAGawler                    = Town
   municipalityKind SACampbelltown              = City
   municipalityKind SAHoldfastBay               = City
   municipalityKind SANorwoodPaynehamAndStPeters = City
   municipalityKind SAWalkerville               = Town
   municipalityKind SAAdelaide                  = City
   fullname m = (show m) ++ " " ++ (show $ municipalityKind m)

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

class (Show m, Eq m) => Municipality m where
   localitiesByMunicipality :: Localities -> m -> S.Set T.Text
   lgaShpName :: m -> T.Text
   isMunicipalityByName :: m -> (T.Text, String) -> Bool
   municipalityState :: m -> State

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
   lgaShpName m = error . show $ m
   municipalityState _ = Vic

victorianLocalitiesByMunicipality 
   :: VictorianLocalities
   -> VictorianMunicipality 
   -> V.Vector T.Text
victorianLocalitiesByMunicipality localities municipality = 
   fmap localityName (V.filter (isInMunicipality municipality) localities)

isInMunicipality :: Municipality a => a -> Locality a -> Bool
isInMunicipality m l = localityMunicipality l == m

data QueenslandMunicipality
    =
     QldAurukun
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
     |  QldCook
 |  QldCroydon
 |  QldDiamantina
 |  QldDouglas
 |  QldEtheridge
 |  QldFlinders
 |  QldFraserCoast
 |  QldGladstone
 |  QldGoldCoast
 |  QldGoondiwindi
 |  QldGympie
 |  QldHinchinbrook
 |  QldHopeVale
 |  QldIpswich
 |  QldIsaac
 |  QldKowanyama
 |  QldLivingstone
 |  QldLockhartRiver
 |  QldLockyerValley
 |  QldLogan
 |  QldLongreach
 |  QldMackay
 |  QldMapoon
 |  QldMaranoa
 |  QldMareeba
 |  QldMcKinlay
 |  QldMoretonBay
 |  QldMornington
 |  QldMountIsa
 |  QldMurweh
 |  QldNapranum
 |  QldNoosa
 |  QldNorthBurnett
 |  QldNorthernPeninsula
 |  QldPalmIsland
 |  QldParoo
 |  QldPormpuraaw
 |  QldQuilpie
 |  QldRedland
 |  QldRichmond
 |  QldRockhampton
 |  QldScenicRim
 |  QldSomerset
 |  QldSouthBurnett
 |  QldSouthernDowns
 |  QldSunshineCoast
 |  QldTablelands
 |  QldToowoomba
 |  QldTorres
 |  QldTorresStraitIsland
 |  QldTownsville
 |  QldWeipaTown
 |  QldWesternDowns
 |  QldWhitsunday
 |  QldWinton
 |  QldWoorabinda
 |  QldYarrabah
 |  QldWujalWujal
 | QldDoomadgee


    deriving (Eq, Show, Read)
instance Municipality QueenslandMunicipality where
   localitiesByMunicipality l m = S.fromList $ listLocalityByMunicipality (locQueensland l) m
   municipalityState _ = Qld

   isMunicipalityByName c ("QLD_LGA__3", b) = lgaShpName c == (T.strip . T.pack $ b)
   isMunicipalityByName _ y = False

   lgaShpName QldBrisbane = "BRISBANE"
   lgaShpName m = error . show $ m

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


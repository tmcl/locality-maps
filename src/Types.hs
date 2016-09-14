module Types(MetroArea(..), Municipalities(..), City(..), cityToState, Municipality(..), Mappable(..), State(..), Locality, MunicipalityShortName, Named(..), namedBBox, namedName)
where

import Data.Complex
import Geometry.Shapefile.Conduit
import Prelude.Unicode
import Data.Text (Text)
import Data.Csv
import Control.Monad

data Municipality = Municipality {
   mState :: State,
   mCouncilName :: Text,
   mName :: MunicipalityShortName
}
   deriving (Show, Read)

instance Eq Municipality where
   a == b = mState a ≡ mState b ∧ mName a ≡ mName b

instance Ord Municipality where
   compare a b = let states = compare (mState a) (mState b) in
      case states of
         EQ → compare (mName a) (mName b)
         _ → states

type MunicipalityShortName = Text
type Locality = Text

data Mappable = City City | State State | Suburb [Text] | Arbitrary RecBBox | Named Named | MappableMunicipalities Municipalities
  deriving (Eq, Show, Read)

data Municipalities = Municipalities Municipality [Municipality]
  deriving (Eq, Show, Read)

data Named = MountIsa 
  deriving (Eq, Show, Read)

namedName ∷ Named → Text
namedName MountIsa = "Mount Isa"
namedBBox ∷ Named → RecBBox
namedBBox MountIsa = RecBBox { lowerLeft = 139.44 :+ (-20.8), 
                                 upperRight = 139.57 :+ (-20.64)}
instance FromField Named where
   parseField "Mount Isa" = pure MountIsa
   parseField _ = mzero

data City = Sydney | Melbourne | Perth | Adelaide | Hobart | Darwin
  deriving (Eq, Show, Read, Ord)

cityToState ∷ City → State
cityToState Sydney = NSW
cityToState Melbourne = Vic
cityToState Perth = WA
cityToState Adelaide = SA
cityToState Hobart = Tas
cityToState Darwin = NT

instance FromField City where
   parseField "Sydney" = pure Sydney
   parseField "Melbourne" = pure Melbourne
   parseField "Perth" = pure Perth
   parseField "Adelaide" = pure Adelaide
   parseField "Hobart" = pure Hobart
   parseField "Darwin" = pure Darwin
   parseField _ = mzero

data State = NSW | Vic | Qld | WA | SA | Tas | ACTg | ACTd | NT | OT | PNG | ID
  deriving (Eq, Show, Read, Ord)

instance FromField State where
   parseField "PNG" = pure PNG
   parseField "NSW" = pure NSW
   parseField "Vic" = pure Vic
   parseField "Qld" = pure Qld
   parseField "WA"  = pure WA
   parseField "SA"  = pure SA
   parseField "Tas" = pure Tas
   parseField "ACT" = pure ACTd
   parseField "ACTg" = pure ACTg
   parseField "NT"  = pure NT
   parseField "OT"  = pure OT
   parseField _ = mzero

data MetroArea = MetroArea {
   metroAreaCity ∷ City, 
   metroAreaMuni ∷ MunicipalityShortName
}
   deriving (Eq, Show, Read, Ord)
instance FromNamedRecord MetroArea where
   parseNamedRecord m = do
      -- state ← m .: "state"
      metro ← m .: "metropolis"
      municipality ← m .: "municipality"
      return $ MetroArea metro municipality

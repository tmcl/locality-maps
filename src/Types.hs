module Types
where

import Data.Text (Text)
import Data.Csv
import Control.Monad

data Municipality = Municipality {
   misState :: State,
   muniCouncilName :: Text,
   misName :: MunicipalityShortName
}
   deriving (Show, Read)

instance Eq Municipality where
   a == b = (misState a) == (misState b)

instance Ord Municipality where
   compare a b = let states = compare (misState a) (misState b) in
      case states of
         EQ -> compare (misName a) (misName b)
         _ -> states

muniState ∷ Municipality → State
muniState = misState

muniName ∷ Municipality → MunicipalityShortName
muniName = misName

type MunicipalityShortName = Text
type Locality = Text

data State = NSW | Vic | Qld | WA | SA | Tas | ACT | NT | OT
  deriving (Eq, Show, Read, Ord)

instance FromField State where
   parseField "NSW" = pure NSW
   parseField "Vic" = pure Vic
   parseField "Qld" = pure Qld
   parseField "WA"  = pure WA
   parseField "SA"  = pure SA
   parseField "Tas" = pure Tas
   parseField "ACT" = pure ACT
   parseField "NT"  = pure NT
   parseField "OT"  = pure OT
   parseField _ = mzero

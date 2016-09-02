module Types
where

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
   a == b = (mState a) ≡ (mState b) ∧ (mName a) ≡ (mName b)

instance Ord Municipality where
   compare a b = let states = compare (mState a) (mState b) in
      case states of
         EQ -> compare (mName a) (mName b)
         _ -> states

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

module Municipality
where

import Data.Text
import Types

-- todo this should be based on a database
muniLongName :: Municipality -> Text
muniLongName m = fixMc . toTitle . transform . mCouncilName $ m
   where
      transform = case mState m of
         Vic -> remove " (UNINCORPORATED)" . remove " (UNINC)"
         Tas -> remove " COUNCIL"
         Qld -> replaceRegional
         _ -> error (show m)

remove ∷ Text → Text → Text
remove it = replace it ""

replaceRegional :: Text -> Text
replaceRegional = replace " REGIONAL" " REGION"

fixMc ∷ Text → Text
fixMc = replace "Mckinlay" "McKinlay"

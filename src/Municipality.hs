module Municipality
where

import Data.Text
import Types

-- todo this should be based on a database
muniLongName :: Municipality -> Text
muniLongName m = toTitle . transform . muniCouncilName $ m
   where
      transform = case muniState m of
         Vic -> id
         Tas -> stripCouncil
         Qld -> replaceRegional
         _ -> error (show m)

stripCouncil :: Text -> Text
stripCouncil = replace " COUNCIL" ""

replaceRegional :: Text -> Text
replaceRegional = replace " REGIONAL" "REGION"

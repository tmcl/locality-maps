{-# LANGUAGE PartialTypeSignatures #-}
module SpecialCases (specialCases, SpecialCase(..), SpecialCaseMap)
where

import Types
import Data.Map.Strict
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Csv
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad
import Data.Bifunctor

(.!!) :: FromField a => Record -> Int -> Parser a
(.!!) = unsafeIndex -- we check the length first.

data SpecialCase = ApparentError | CrossState | MinorHalf | Normal
   deriving (Eq, Show, Ord, Read)
instance FromField SpecialCase where
   parseField "apparent errors" = pure ApparentError
   parseField "barely" = pure MinorHalf
   parseField "cross state" = pure CrossState
   parseField "maybe errors" = pure ApparentError
   parseField "part" = pure MinorHalf
   parseField "mere-dupes" = pure MinorHalf
   parseField _ = mzero


data SpecialCaseRecord = SpecialCaseRecord {
   scrType :: !SpecialCase, 
   scrLocality :: !Locality, 
   scrMunicipality :: !MunicipalityShortName, 
   scrState :: !State
}
instance FromRecord SpecialCaseRecord where
   parseRecord v 
      | length v == 4 = SpecialCaseRecord 
         <$> v .!! 0 
         <*> v .!! 1 
         <*> v .!! 2 
         <*> v .!! 3
      | otherwise = mzero

type SpecialCaseMap = Map Municipality (Map Text SpecialCase)

specialCases :: ByteString -> Either String SpecialCaseMap
specialCases csv = second convert (readSpecialCases csv)
   where
      convert :: Vector SpecialCaseRecord -> SpecialCaseMap
      convert = V.foldl' addCaseToDictionary empty

addCaseToDictionary :: SpecialCaseMap -> SpecialCaseRecord -> SpecialCaseMap
addCaseToDictionary dict scRecord = 
   insertWith 
      union 
      (Municipality (scrState scRecord) (scrMunicipality scRecord) "")
      (singleton (scrLocality scRecord) (scrType scRecord))
      dict 

readSpecialCases :: ByteString -> Either String (Vector SpecialCaseRecord)
readSpecialCases = decode NoHeader

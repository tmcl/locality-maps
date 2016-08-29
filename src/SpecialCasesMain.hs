module Main
where

import SpecialCases
import qualified Data.ByteString.Lazy as BS
import Text.Show.Pretty

main :: IO ()
main = pPrint =<< specialCases <$> BS.readFile "../places-out.csv"

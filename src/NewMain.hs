module Main (main)
where

import Graphics.PDF hiding (boxHeight, boxWidth)
--import Control.Monad.Unicode
--import Prelude.Unicode
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import PolygonReduce
import NewMap

main ∷ IO ()
main = do
   print itWorks
   let rect = PDFRect 0 0 600 400
       docInfo = standardDocInfo {author="alpheccar", compressed = False}
   csv ← BS.readFile "poly.pst"
   let Right points = decode NoHeader csv
   runPdf "demo.pdf" docInfo rect (myDocument $ mkShape points)

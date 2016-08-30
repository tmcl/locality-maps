module UpdatedMapper where

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Geometry.Shapefile.Conduit
import Settings
import Data.List
import Data.Maybe
import System.IO
import System.FilePath

shapeSource ∷ FilePath → Maybe RecBBox → Sink Shape IO a → IO a
shapeSource fp bbox sink = withShpFile fp $ \shp dbf →
   shapesFromDbfShpSource bbox shp dbf $$ sink

withShpFile ∷ FilePath → (Handle → Handle → IO a) → IO a
withShpFile filePath cb =
  withFile filePath ReadMode $ \shp →
    withFile (toDbf filePath) ReadMode $ \dbf →
      cb shp dbf

toDbf ∷ FilePath → FilePath
toDbf = (`replaceExtension` "dbf")



eachPolygon ∷ Conduit (a, ShpRec, b) IO [[Point]]
eachPolygon = CC.map (\(_, r, _) → pointsFromRecord r)

eachPlace ∷ Conduit (a, ShpRec, b) IO [[Point]]
eachPlace = CC.map (\(_, r, _) →
   return . concat . andReverseTheRest . map wrapEnds . pointsFromRecord $ r)

andReverseTheRest ∷ [[a]] → [[a]]
andReverseTheRest (first:rest) = first:intersperse [last first] rest
andReverseTheRest a = a

pointsFromRecord ∷ ShpRec → [[Point]]
pointsFromRecord r = concatMap pointsFromRecContents (catMaybes [shpRecContents r])

pointsFromRecContents ∷ RecContents → [[Point]]
pointsFromRecContents r@RecPolygon {}  = recPolPoints r
pointsFromRecContents r@RecPolyLine {} = recPolLPoints r
pointsFromRecContents _                = []




wrapEnds ∷ [Point] → [Point]
wrapEnds [] = []
wrapEnds [a] = [a]
wrapEnds line = last line:line


module Main where

import Algebra.Clipper
import Geometry.Shapefile.Conduit
import Geometry.Shapefile.Internal (Point)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import System.IO
import System.FilePath
import qualified Data.Text as T
import Data.Text (Text)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run

lgaColumnName :: Text -> Bool
lgaColumnName t = "_LGA__3" `T.isSuffixOf` t

makeShpDbfConduit shp conduit =
  withFile shp ReadMode $ \shpH ->
    withFile (shp `replaceExtension` "dbf") ReadMode $ \dbfH ->
      conduit (sourceFromStart shpH) (sourceFromStart dbfH)

toClipperPolygons :: Shape -> [(Polygon, Maybe RecBBox)]
toClipperPolygons (_, shpRec, _) = map (\l -> (toClipperPolygon l, shpRecBBox shpRec)) (maybe [] recContentsPolygons (shpRecContents shpRec))


recContentsPolygons p@(RecPolygon {}) = recPolPoints p
toClipperPolygon :: [Point] -> Polygon
toClipperPolygon p = Polygon (map toClipperPoint p)
toClipperPoint :: Point -> IntPoint
toClipperPoint (x, y) = IntPoint (floor $ x*10000) (floor $ y*10000)

run :: [FilePath] -> IO ()
run (municipality:municipalities:localities:_) = do
  shapes <- shapesByName municipalities lgaColumnName (T.pack municipality)
  let (polygon, bbox) = head $ concatMap toClipperPolygons shapes
  makeShpDbfConduit localities (cdt bbox polygon municipality)

cdt bbox polygon name shpH dbfH =
  shpDbfConduit bbox shpH dbfH
    =$= CC.mapM (\r@(a, b, c) -> print (shapeFieldByColumnNameRule (== "VIC_LOCA_2") c)  >> return r)
    =$= CC.map (\l@(_, _, c) -> (toClipperPolygons l, (shapeFieldByColumnNameRule (== "VIC_LOCA_2") c)))
    =$= CC.filterM (\(a, _) -> (0 /=) <$> sizes <$> ((Polygons [polygon]) <@> (Polygons (fst . unzip $ a))))
    =$= CC.mapM (\r@(a, c) -> putStrLn (show c ++ " is in")  >> return r)
    $$ CC.sinkNull

sizes (Polygons ps) = fromIntegral $ length ps


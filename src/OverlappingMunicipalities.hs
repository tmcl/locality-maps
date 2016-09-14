module Main(main) where

import Algebra.Clipper
import Geometry.Shapefile.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import qualified Data.Text as T
import System.Environment (getArgs)
import Data.ByteString (ByteString)

import FindLocalities
import Utils

main :: IO ()
main = getArgs >>= run

run :: [FilePath] -> IO ()
run (municipality:municipalities:localities:_) = do
  shapes <- shapesByName municipalities lgaColumnName (T.pack municipality)
  let (polygons', bbox') = unzip $ concatMap toClipperPolygons shapes
      polygons = Polygons polygons'
      bbox = foldl bigBoundingBox Nothing bbox'
  makeShpDbfConduit localities (testCdt2 bbox polygons)
run _ = error "expected three args: municipality/muni source/locality source"

testCdt2 :: Maybe RecBBox -> Polygons -> Source IO ByteString -> Source IO ByteString -> IO ()
testCdt2 bbox polygons shpH dbfH = shpDbfToPolygonsInPolygonsConduit (const True) bbox polygons shpH dbfH $$ CC.sinkNull

-- testCdt :: Maybe RecBBox -> Polygon -> Source IO ByteString -> Source IO ByteString -> IO ()
-- testCdt bbox polygon shpH dbfH =  
--   shpDbfConduit bbox shpH dbfH
--     =$= CC.mapM (\r@(_, _, c) -> print (shapeFieldByColumnNameRule localityColumnName c)  >> return r)
--     =$= CC.map (\l@(_, _, c) -> (toClipperPolygons l, (shapeFieldByColumnNameRule localityColumnName c)))
--     =$= CC.mapM (addIntersection polygon)
--     =$= CC.filterM (\(_, _, c) -> return (0 /= (sizes c)))
--     =$= CC.mapM (\r@(_, b, c) -> do
--          putStrLn (show b ++ " is in" ) 
--          let (Polygons polygons) = c
--          mapM_   (\l -> polygonArea l >>= print) polygons
--          return r)
--     $$ CC.sinkNull

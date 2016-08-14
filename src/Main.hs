module Main
where

import Geometry.Shapefile.Conduit
import Data.Dbase.Conduit
import Map
import qualified Data.ByteString as BS
import System.Environment
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Geometry.Shapefile.Types
import Data.Conduit
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Maybe
import System.IO
import ClassyPrelude (trace,traceM)
import System.FilePath

main = getArgs >>= mapM_ (mapFile)

mapFile fp = mapFilePath fp "/tmp/blue.eps"

mapFilePath src dst = do
   withFile src ReadMode $ \srcH -> 
      withFile dst WriteMode $ \dstH ->
      mapFileHandle srcH dstH

--mapFile :: FilePath -> ResourceT IO ()
mapFileHandle src dst = do
   Just (shpHead, _) <- CC.sourceHandle src =$= shapefileConduit $$ CL.head
   let settings = withDefaultSettings (toRecBB $ shpBB shpHead)
   initialisation <- liftIO $ initialiseMap settings
   coast <- liftIO $ mapCoast settings
   mapPoints <- (CB.sourceHandleRange src (Just 0) Nothing =$= shapefileConduit =$= points =$= CC.map (\f -> f)) =$= mapPoints3 settings (Outline (Points 1) (Color 0 100 200)) 5  $$ CC.sinkList
   finalisation <- liftIO $ closeMap settings
   CC.yieldMany ([initialisation] ++ mapPoints ++ [finalisation]) $$ CC.sinkHandle dst

pointsFromRecord rec = concatMap (pointsFromRecContents) (catMaybes [shpRecContents rec])
pointsFromRecContents rec@(RecPolygon _ _ _ _ _) = recPolPoints rec
pointsFromRecContents _ = []

points = CC.map (pointsFromRecord . snd)

toRecBB shpBB = RecBBox {
      recXMin = shpXMin shpBB, 
      recXMax = shpXMax shpBB, 
      recYMin = shpYMin shpBB, 
      recYMax = shpYMax shpBB 
   }

-- makeBaseMap settings = BS.concat [initialiseMap settings

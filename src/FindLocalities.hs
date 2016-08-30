module FindLocalities where

import Data.Complex
import Algebra.Clipper
import Geometry.Shapefile.Conduit
import Geometry.Shapefile.Internal (Point)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Dbase.Parser
import System.IO
import System.FilePath
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set as S
import Data.Set (Set)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Utils
import Types

type ShapeSource = (FilePath, Shape -> Bool)
type Yielder = [ShapeSource]

lgaColumnName :: Text -> Bool
lgaColumnName t = "_LGA__3" `T.isSuffixOf` t || "_LGA_s_3" `T.isSuffixOf` t

localityColumnName :: Text -> Bool
localityColumnName t = "_LOCA_2" `T.isSuffixOf` t || "_LOCAL_2" `T.isSuffixOf` t

typeColumnName :: Text -> Bool
typeColumnName t = "_LOCA_5" `T.isSuffixOf` t || "_LOCAL_5" `T.isSuffixOf` t

makeShpDbfConduit :: (MonadIO m, MonadIO m1) =>
                     FilePath
                     -> (ConduitM i ByteString m ()
                         -> ConduitM i1 ByteString m1 () -> IO r)
                     -> IO r

makeShpDbfConduit shp conduit =
  withFile shp ReadMode $ \shpH ->
    withFile (shp `replaceExtension` "dbf") ReadMode $ \dbfH ->
      conduit (sourceFromStart shpH) (sourceFromStart dbfH)

toClipperPolygons :: Shape -> [(Polygon, Maybe RecBBox)]
toClipperPolygons (_, shpRec, _) = map (\l -> (toClipperPolygon l, shpRecBBox shpRec)) (maybe [] recContentsPolygons (shpRecContents shpRec))


recContentsPolygons :: RecContents -> [[Point]]
recContentsPolygons p@(RecPolygon {}) = recPolPoints p
recContentsPolygons _ = []

toClipperPolygon :: [Point] -> Polygon
toClipperPolygon p = Polygon (map toClipperPoint p)
toClipperPoint :: Point -> IntPoint
toClipperPoint (x :+ y) = IntPoint (floor $ x*bigNum) (floor $ y*bigNum)

bigNum :: Double
bigNum = 2^(63-8::Integer)

fromClipperPolygon :: Polygon -> [Point]
fromClipperPolygon (Polygon p) = map fromClipperPoint p
fromClipperPoint :: IntPoint -> Point
fromClipperPoint (IntPoint x y) = 
   fromIntegral x/bigNum :+ fromIntegral y/bigNum

localitiesByMunicipality :: T.Text -> FilePath -> Yielder -> IO (Set Locality)
localitiesByMunicipality municipality municipalities localities = do
  shapes <- shapesByName municipalities lgaColumnName municipality
  let (polygons', bbox') = unzip $ concatMap toClipperPolygons shapes
      polygons = Polygons polygons'
      bbox = foldl bigBoundingBox Nothing bbox'
  locs <- mapM (searcher polygons bbox) localities
  return $ S.fromList (concat locs)

searcher ∷ Polygons → Maybe RecBBox → ShapeSource → IO [Text]
searcher polygons bbox yielder = makeShpDbfConduit (fst yielder) (\h1 h2 -> 
      cdt (snd yielder) bbox polygons h1 h2 
      =$= CC.map snd
      =$= CL.mapMaybe readChar
      $$ CC.sinkList)

readChar :: DbfField -> Maybe Text
readChar (DbfFieldCharacter c) = Just c
readChar _ = Nothing
 
cdt :: (Shape -> Bool) -> Maybe RecBBox -> Polygons -> Source IO ByteString -> Source IO ByteString -> ConduitM () ([(Polygon, Maybe RecBBox)], DbfField) IO ()
cdt shapeFilter bbox polygons shpH dbfH =
  shpDbfConduit bbox shpH dbfH
    =$= CC.filter shapeFilter
    =$= CC.map (\l@(_, _, c) -> (toClipperPolygons l, shapeFieldByColumnNameRule localityColumnName c))
    =$= CL.mapMaybe (\(a, b) -> case b of
         Just v -> Just (a, v)
         Nothing -> Nothing)
    =$= CC.map (addIntersection polygons)
    =$= CC.filterM (\(_, _, c) -> return (0 /= sizes c))
    =$= CC.filterM (\(_, _, c) -> do
         let (Polygons pp) = c
         biggest <- maximum <$> mapM polygonArea pp
         return $ biggest > 1e25 && length pp < 6 || biggest > 1e27)
    =$= CC.mapM (\(a, b, _) -> return (a, b))

sizes :: Polygons -> Integer
sizes (Polygons ps) = fromIntegral $ length ps

addIntersection :: Polygons ->  ([(Polygon, Maybe RecBBox)], DbfField) -> ([(Polygon, Maybe RecBBox)], DbfField, Polygons)
addIntersection polygons (a, b) = (a, b, intersec)
   where intersec = polygons ∩ Polygons (fst . unzip $ a)


-- todo what's wrong with st leonards in sydney?

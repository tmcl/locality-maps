module BaseMap (mapCoast, mapUrbanAreas)
where

import Algebra.Clipper
import Utils
import qualified Graphics.PDF as Pdf
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import UpdatedMapper
import Data.Text (Text)
import FindLocalities (toClipperPolygon, fromClipperPolygon)
import Control.Monad.Trans.Class
import Data.Vector (Vector)
import qualified Data.Vector as V

import Geometry.Shapefile.Conduit 

import Settings

matchUrbanAreaType ∷ Text → Shape → Bool
matchUrbanAreaType = (`matchTextDbfField` (== "SOS_NAME11"))

bboxToPolygon ∷ RecBBox → Vector Point
bboxToPolygon box = V.fromList [
   lowerLeft box,
   lowerRight box,
   upperRight box,
   upperLeft box,
   lowerLeft box]


lowerRight ∷ RecBBox → Point
lowerRight box = recXMax box :+ recYMin box
upperLeft ∷ RecBBox → Point
upperLeft box = recXMin box :+ recYMax box


matchFeatCode ∷ Text → Shape → Bool
matchFeatCode = (`matchTextDbfField` (== "FEAT_CODE"))

mapCoast ∷ SettingsT IO (Pdf.PDF XForm)
mapCoast = drawCoast >>= (liftT . makeXForm1)

mapUrbanAreas ∷ SettingsT IO (Pdf.PDF XForm)
mapUrbanAreas = (liftT . makeXForm1) =<< drawUrbanAreas

drawUrbanAreas ∷ SettingsT IO (Pdf.Draw ())
drawUrbanAreas = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   (b1, o1, m1) ← lift $ mapUrbanAreas2 fps bbox
   liftT $ mapUrbanAreas3 b1 o1 m1

mapUrbanAreas2 ∷ FilePaths → RecBBox → IO ([Vector Point], [Vector Point], [Vector Point])
mapUrbanAreas2 fps bbox = do
  (bLoc', othUrban', majUrban') <- shapeSource (urbanAreas fps) (Just bbox)
    (CC.foldl (\(a, b, c) shape →
      if matchUrbanAreaType "Bounded Locality" shape then (shape:a, b, c) else
      if matchUrbanAreaType "Other Urban" shape then (a, shape:b, c) else
      if matchUrbanAreaType "Major Urban" shape then (a, b, shape:c) else
      (a, b, c)) ([], [], []))
  bLoc <- CC.yieldMany bLoc'
    =$= eachPolygon 
    $$ CC.sinkList
  othUrban <- CC.yieldMany othUrban'
    =$= eachPolygon
    $$ CC.sinkList
  majUrban <- CC.yieldMany majUrban'
    =$= eachPolygon
    $$ CC.sinkList
  return (concat bLoc, concat othUrban, concat majUrban)

mapUrbanAreas3 ∷ [Vector Point] → [Vector Point] → [Vector Point] 
                 → SettingsT Pdf.Draw ()
mapUrbanAreas3 bLoc othUrban majUrban = do
   applySettings
   mapM_ (fillPoints2 boundedLocality) bLoc
   mapM_ (fillPoints2 otherUrban) othUrban
   mapM_ (fillPoints2 majorUrban) majUrban

mapCoast3 ∷ [Vector Point] → [Vector Point] → SettingsT Pdf.Draw ()
mapCoast3 sea lands = do
   applySettings 
   mapM_ (fillPoints2 water) sea
   mapM_ (fillPoints2 land) lands
   pen ← asks riverPen
   mapM_ (drawPoints pen) lands

drawCoast ∷ SettingsT IO (Pdf.Draw ())
drawCoast = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   let getLandShapes = CC.filter (not . matchFeatCode "sea")
                       =$= eachPlace 
                       =$= CC.concat
                       =$= CC.sinkList
    
   lands <- lift $ shapeSource (states fps) (Just bbox) getLandShapes
   let landPolygons = Polygons (map toClipperPolygon lands)
       bboxPolygons = Polygons [toClipperPolygon . bboxToPolygon $ bbox]
       Polygons seaPolygons = bboxPolygons ∖ landPolygons
       sea = map fromClipperPolygon seaPolygons

   liftT $ mapCoast3 sea lands

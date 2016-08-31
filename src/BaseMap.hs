module BaseMap (mapCoast, mapUrbanAreas)
where

import ClassyPrelude (traceShowId)
import Algebra.Clipper
import Utils
import Data.Complex
import qualified Graphics.PDF as Pdf
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import UpdatedMapper
import Data.Text (Text)
import FindLocalities (toClipperPolygon, fromClipperPolygon)
import Control.Monad.Trans.Class

import Geometry.Shapefile.Conduit 

import Settings

matchUrbanAreaType ∷ Text → Shape → Bool
matchUrbanAreaType = (`matchTextDbfField` (== "SOS_NAME11"))

bboxToPolygon ∷ RecBBox → [Point]
bboxToPolygon box = [
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

mapCoast ∷ FilePaths → SettingsT IO (Pdf.PDF XForm)
mapCoast fps = drawCoast fps >>= (liftT . makeXForm1)

mapUrbanAreas ∷ FilePaths → SettingsT IO (Pdf.PDF XForm)
mapUrbanAreas fps = (liftT . makeXForm1) =<< drawUrbanAreas fps 

drawUrbanAreas ∷ FilePaths → SettingsT IO (Pdf.Draw ())
drawUrbanAreas fps = do
   bbox ← asks boundingBox
   (b1, o1, m1) ← lift $ mapUrbanAreas2 fps bbox
   liftT $ mapUrbanAreas3 b1 o1 m1

mapUrbanAreas2 ∷ FilePaths → RecBBox → IO ([[Point]], [[Point]], [[Point]])
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
  return (traceShowId (concat bLoc), concat othUrban, concat majUrban)

mapUrbanAreas3 ∷ [[Point]] → [[Point]] → [[Point]] 
                 → SettingsT Pdf.Draw ()
mapUrbanAreas3 bLoc othUrban majUrban = do
   applySettings
   mapM_ (fillPoints2 boundedLocality) bLoc
   mapM_ (fillPoints2 otherUrban) othUrban
   mapM_ (fillPoints2 majorUrban) majUrban

mapCoast3 ∷ [[Point]] → [[Point]] → SettingsT Pdf.Draw ()
mapCoast3 sea lands = do
   applySettings 
   mapM_ (fillPoints2 water) sea
   mapM_ (fillPoints2 land) lands

drawCoast ∷ FilePaths → SettingsT IO (Pdf.Draw ())
drawCoast fps = do
   bbox ← asks boundingBox
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

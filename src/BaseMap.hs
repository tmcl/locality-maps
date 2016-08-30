module BaseMap (makeBaseMap, XForm, makeXForm, fillPoints, applySettings, drawPoints, mapCoast, mapUrbanAreas)
where

import ClassyPrelude (traceShowId)
import Algebra.Clipper
import Utils
import Data.Complex
import qualified Graphics.PDF as Pdf
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import UpdatedMapper
import qualified Data.Vector as V
import Data.Text (Text)
import FindLocalities (toClipperPolygon, fromClipperPolygon)

import Geometry.Shapefile.Conduit 

import NewMap
import Settings
import PolygonReduce

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
 

applySettings ∷ Settings → Pdf.Draw ()
applySettings = Pdf.applyMatrix . settingsMatrix 
matchUrbanAreaType ∷ Text → Shape → Bool
matchUrbanAreaType = (`matchTextDbfField` (== "SOS_NAME11"))

makeBaseMap ∷ FilePaths → Settings → IO [Pdf.PDF XForm]
makeBaseMap fps settings = do
   c <- drawCoast fps settings
   u <- drawUrbanAreas fps settings -- todo convert the rest to IO (Draw ())
   return $ makeXForm settings <$> [c, u]

mapCoast ∷ FilePaths → Settings → IO (Pdf.PDF XForm)
mapCoast fps settings = (return . makeXForm settings) =<< drawCoast fps settings
mapUrbanAreas ∷ FilePaths → Settings → IO (Pdf.PDF XForm)
mapUrbanAreas fps settings = (return . makeXForm settings) =<< drawUrbanAreas fps settings

fillPoints ∷ Settings → Pdf.Color → [Point] → Pdf.Draw ()
fillPoints settings color points = do
   preparePoints settings points
   Pdf.fillColor color
   Pdf.fillPathEO

drawPoints ∷ Settings → Pdf.Color → [Point] → Pdf.Draw ()
drawPoints settings color points = do
   preparePoints settings points
   whenˀ $ Pdf.setWidth . (4 *) <$> settingsEpsilon settings
   Pdf.strokeColor color
   Pdf.strokePath

whenˀ ∷ Monad m ⇒  Maybe (m ()) → m ()
whenˀ (Just foo) = foo
whenˀ Nothing = return ()

preparePoints ∷ Settings → [Point] → Pdf.Draw ()
preparePoints settings points = do
   let bbox = boundingBox settings
       reducer = maybe id reduce (settingsEpsilon settings)
   Pdf.addPolygonToPath . V.toList . reducer . clipPath bbox . V.fromList 
      $ points


type XForm = Pdf.PDFReference Pdf.PDFXForm

makeXForm ∷ Settings → Pdf.Draw () → Pdf.PDF XForm
makeXForm settings = Pdf.createPDFXForm x0 y0 x1 y1
   where 
      Rect x0 y0 x1 y1 = settingsRect settings

drawUrbanAreas ∷ FilePaths → Settings → IO (Pdf.Draw ())
drawUrbanAreas fps settings = do
   (b1, o1, m1) ← mapUrbanAreas2 fps settings
   return $ mapUrbanAreas3 settings b1 o1 m1

mapUrbanAreas2 ∷ FilePaths → Settings → IO ([[Point]], [[Point]], [[Point]])
mapUrbanAreas2 fps settings = do
  (bLoc', othUrban', majUrban') <- shapeSource (urbanAreas fps) (Just $ boundingBox settings)
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

mapUrbanAreas3 ∷ Settings 
                 → [[Point]] → [[Point]] → [[Point]] 
                 → Pdf.Draw ()
mapUrbanAreas3 settings bLoc othUrban majUrban = do
   applySettings settings
   mapM_ (fillPoints settings (ptc $ boundedLocality settings)) bLoc
   mapM_ (fillPoints settings (ptc $ otherUrban settings)) othUrban
   mapM_ (fillPoints settings (ptc $ majorUrban settings)) majUrban

{-
-}

mapCoast3 ∷ Settings → [[Point]] → [[Point]] → Pdf.Draw ()
mapCoast3 settings sea lands = do
   applySettings settings
   mapM_ (fillPoints settings (ptc $ water settings)) sea
   mapM_ (fillPoints settings (ptc $ land settings)) lands

drawCoast ∷ FilePaths → Settings → IO (Pdf.Draw ())
drawCoast fps settings = do
   let bbox = boundingBox settings
       getLandShapes = CC.filter (not . matchFeatCode "sea")
                       =$= eachPlace 
                       =$= CC.concat
                       =$= CC.sinkList
    
   lands <- shapeSource (states fps) (Just bbox) getLandShapes
   let landPolygons = Polygons (map toClipperPolygon lands)
       bboxPolygons = Polygons [toClipperPolygon . bboxToPolygon $ bbox]
       Polygons seaPolygons = bboxPolygons ∖ landPolygons
       sea = map fromClipperPolygon seaPolygons

   return $ mapCoast3 settings sea lands

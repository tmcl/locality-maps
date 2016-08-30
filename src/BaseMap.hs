module BaseMap (makeBaseMap)
where

import ClassyPrelude (traceShowId)
import Algebra.Clipper
import Data.ByteString (ByteString)
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

import Map (initialiseMap)
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


 
makeBaseMap ∷ FilePaths → Settings → IO [ByteString]
makeBaseMap fps settings = do
   initialisation <- initialiseMap settings
   c <- mapCoast fps settings
   u <- mapUrbanAreas fps settings -- todo convert the rest to IO (Draw ())
   muzzle settings (makeXForm settings <$> [c, u])
   return [initialisation]

fillPoints ∷ Settings → Pdf.Color → [Point] → Pdf.Draw ()
fillPoints settings color points = do
   let bbox = boundingBox settings
       epsilon = settingsEpsilon settings
   Pdf.addPolygonToPath . V.toList . reduce epsilon . clipPath bbox . V.fromList 
      $ points
   Pdf.fillColor color
   Pdf.fillPathEO

makeXForm ∷ Settings → Pdf.Draw () → Pdf.PDF (Pdf.PDFReference Pdf.PDFXForm)
makeXForm settings = Pdf.createPDFXForm x0 y0 x1 y1
   where 
      Rect x0 y0 x1 y1 = settingsRect settings

mapUrbanAreas ∷ FilePaths → Settings → IO (Pdf.Draw ())
mapUrbanAreas fps settings = do
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

muzzle ∷ Settings → [Pdf.PDF (Pdf.PDFReference Pdf.PDFXForm)] → IO ()
muzzle settings drawings = Pdf.runPdf 
      "random.pdf"
      Pdf.standardDocInfo {Pdf.author="tr", Pdf.compressed = False} 
      (rectToPdfRect rect)
      (mozzle drawings rect)
   where rect = settingsRect settings

mozzle ∷ [Pdf.PDF (Pdf.PDFReference Pdf.PDFXForm)] → Rect → Pdf.PDF ()
mozzle drawings rect = do 
   page ← Pdf.addPage (Just $ rectToPdfRect rect)
   drawings' ← sequence drawings
   Pdf.drawWithPage page $ do
      mapM_ Pdf.drawXObject drawings'
      Pdf.strokeColor Pdf.red
      Pdf.stroke $ Pdf.Rectangle (10 :+ 0) (200 :+ 300)
      return ()

mapCoast3 ∷ Settings → [[Point]] → [[Point]] → Pdf.Draw ()
mapCoast3 settings sea lands = do
   applySettings settings
   mapM_ (fillPoints settings (ptc $ water settings)) sea
   mapM_ (fillPoints settings (ptc $ land settings)) lands

mapCoast ∷ FilePaths → Settings → IO (Pdf.Draw ())
mapCoast fps settings = do
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

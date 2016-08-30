module BaseMap (makeBaseMap)
where

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
   muzzle  settings (c >> u)
   return $ [initialisation]


fillPoints ∷ Settings → Pdf.Color → [Point] → Pdf.Draw ()
fillPoints settings color points = do
   let bbox = boundingBox settings
       epsilon = settingsEpsilon settings
   Pdf.addPolygonToPath . V.toList . reduce epsilon . clipPath bbox . V.fromList 
      $ points
   Pdf.fillColor color
   Pdf.fillPathEO


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
    =$= eachPlace 
    $$ CC.sinkList
  othUrban <- CC.yieldMany othUrban'
    =$= eachPlace
    $$ CC.sinkList
  majUrban <- CC.yieldMany majUrban'
    =$= eachPlace
    $$ CC.sinkList
  return (concat bLoc, concat othUrban, concat majUrban)



mapUrbanAreas3 ∷ Settings 
                 → [[Point]] → [[Point]] → [[Point]] 
                 → Pdf.Draw ()
mapUrbanAreas3 settings bLoc othUrban majUrban = do
   applySettings settings
   mapM_ (fillPoints settings (ptc $ boundedLocality settings)) bLoc
   mapM_ (fillPoints settings (ptc $ otherUrban settings)) othUrban
   mapM_ (fillPoints settings (ptc $ majorUrban settings)) majUrban




muzzle ∷ Settings → Pdf.Draw () → IO ()
muzzle settings pdf = Pdf.runPdf 
      "random.pdf"
      Pdf.standardDocInfo {Pdf.author="tr", Pdf.compressed = False} 
      (rectToPdfRect rect)
      (mozzle pdf rect)
   where rect = settingsRect settings

mozzle ∷ Pdf.Draw () → Rect → Pdf.PDF ()
mozzle drawing rect = do 
   page ← Pdf.addPage (Just $ rectToPdfRect rect)
   Pdf.drawWithPage page $ do
      drawing
      Pdf.strokeColor Pdf.red
      Pdf.stroke $ Pdf.Rectangle (10 :+ 0) (200 :+ 300)



mapCoast3 ∷ Settings → [[Point]] → Pdf.Draw ()
mapCoast3 settings sea = do
   applySettings settings
   mapM_ (fillPoints settings (ptc $ water settings)) sea

mapCoast ∷ FilePaths → Settings → IO (Pdf.Draw ())
mapCoast fps settings = do
   let bbox = boundingBox settings
    
   lands <- shapeSource (states fps) (Just bbox)
      (CC.filter (not . matchFeatCode "sea")
        =$= eachPlace 
        =$= CC.concat
        =$= CC.sinkList)
   let landPolygons = Polygons (map toClipperPolygon lands)
   Polygons seaPolygons <- (Polygons [toClipperPolygon . bboxToPolygon $ bbox] <-> landPolygons)
   let sea = map fromClipperPolygon seaPolygons

   return $ mapCoast3 settings sea



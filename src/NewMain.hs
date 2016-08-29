{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
where

import Graphics.PDF (PDFFloat, standardDocInfo, runPdf, Complex(..), red, PDF, PDFPage, PDFReference, drawWithPage, strokeColor, Rectangle(..), drawXObject, stroke, setWidth, strokePath, addPolygonToPath, Matrix(..), Point, addPage, compressed, author, applyMatrix, green, createPDFXForm, toPDFString, PDFString, PDFRect(..), PDFXForm, fill, blue)
--import Control.Monad.Unicode
import Prelude.Unicode
import Data.String (IsString(..))
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import Data.Vector (Vector)
-- import qualified Data.Vector as V
import Data.Foldable
import Data.Maybe
import ClassyPrelude (traceM, trace)
import PolygonReduce

instance IsString PDFString where
   fromString = toPDFString

main ∷ IO ()
main = do
   print itWorks
   let rect = PDFRect 0 0 600 400
       docInfo = standardDocInfo {author="alpheccar", compressed = False}
   csv ← BS.readFile "poly.pst"
   let Right points = decode NoHeader csv
   runPdf "demo.pdf" docInfo rect (myDocument $ mkShape points)

data Shp = Shp {
   shpBBox :: RecBBox,
   shpPoints ∷ Vector (Double, Double)
}

embiggenBoundingBox ∷ RecBBox → RecBBox
embiggenBoundingBox (RecBBox a c b d) = RecBBox (a - width) (c - height) (b + width) (d + height)
   where
      width = (b - a) / 10
      height = (d - c) / 10

mkShape ∷ Vector EarthCoord → Shp
mkShape points = Shp (embiggenBoundingBox $ bboxFromPoints points) points

flipY ∷ Functor t ⇒ t EarthCoord → t EarthCoord
flipY = fmap (\(x, y) → (x, y*(-1))) 

bboxFromPoints ∷ Vector EarthCoord → RecBBox
bboxFromPoints = fromMaybe (RecBBox 0 10 0 10) . bboxFromPointsˀ

-- we use nothing because we don't want our bbox 
-- to include (0, 0) unless the data truly does
bboxFromPointsˀ ∷ Vector EarthCoord → Maybe RecBBox
bboxFromPointsˀ = foldl' fitPointInBoxˀ Nothing

fitPointInBoxˀ ∷ Maybe RecBBox → EarthCoord → Maybe RecBBox
fitPointInBoxˀ Nothing (x, y) = Just $ RecBBox x x y y
fitPointInBoxˀ (Just b) p = Just $ fitPointInBox b p

fitPointInBox ∷ RecBBox → EarthCoord → RecBBox
fitPointInBox box point@(x, y) = if (box `fits` point) then box else RecBBox {
      recXMin = (min x (recXMin box)),
      recXMax = (max x (recXMax box)),
      recYMin = (min y (recYMin box)),
      recYMax = (max y (recYMax box))
   }

fits ∷ RecBBox → EarthCoord → Bool
box `fits` (x, y) = recXMin box ≤ x ∧ x ≤ recXMax box
                  ∧ recYMin box ≤ y ∧ y ≤ recYMax box


myDocument ∷ Shp → PDF ()
myDocument points = do
   let pageSize = pageFromPoints points
       pageSize' = Just $ rectToPdfRect pageSize
   page1 ← addPage pageSize' 
   page2 ← addPage pageSize' 
   basemap ← mapOnPage pageSize points
   createPage1Content basemap page1
   createPage2Content basemap page2

rectToPdfRect ∷ Rect → PDFRect
rectToPdfRect (Rect x0 y0 x1 y1) = 
   PDFRect (floor x0) (floor y0) (ceiling x1) (ceiling y1)

pageFromPoints ∷ Shp → Rect
pageFromPoints shape = Rect 0 0 maxX maxY
   where
      box = shpBBox shape
      width = recXMax box - recXMin box
      height = recYMax box - recYMin box
      multiplier = if width > height then 600 / width else 600 / height
      maxX = width * multiplier
      maxY = height * multiplier

data RecBBox = RecBBox { 
   recXMin ∷ Double,
   recYMin ∷ Double,
   recXMax ∷ Double,
   recYMax ∷ Double
}
   deriving (Show)

type EarthCoord = (Double, Double)

matrixForBBox ∷ Rect → RecBBox → Matrix
matrixForBBox (Rect x0 y0 x1 y1) bbox =    
   Matrix xscale 0 0 yscale tx ty
   where
      boxHeight = traceShow "bH" $ recYMax bbox - recYMin bbox
      boxWidth = traceShow "bW" $ recXMax bbox - recXMin bbox
      pageHeight = traceShow "pH" $ y1 - y0
      pageWidth = traceShow "pW" $ x1 - x0
      xscale = pageWidth / boxWidth
      yscale = pageHeight / boxHeight
      tx = xscale * (-(recXMin bbox))
      ty = yscale * (-(recYMin bbox))

traceShow ∷ Show a ⇒ String → a → a
traceShow s a = trace (s ⧺ " " ⧺ (show a)) a

transformPath ∷ Vector EarthCoord → [Point]
transformPath = foldl' addEarthCoordToPdfPoints []

addEarthCoordToPdfPoints ∷ [Point] → EarthCoord → [Point]
addEarthCoordToPdfPoints path (x, y) = (x :+ y) : path

data Rect = Rect PDFFloat PDFFloat PDFFloat PDFFloat 

mapOnPage ∷ Rect → Shp → PDF (PDFReference PDFXForm)
mapOnPage rect@(Rect x0 y0 x1 y1) shape = 
   createPDFXForm x0 y0 x1 y1 $ do
      let matrix = (matrixForBBox rect (shpBBox shape))
      applyMatrix matrix
      setWidth 0.02
      traceM (show matrix)
      traceM (show $ shpBBox shape)
      strokeColor green
      addPolygonToPath (reduce 0.005 $ transformPath $ shpPoints shape)
      strokePath

createPage1Content ∷ PDFReference PDFXForm → PDFReference PDFPage → PDF ()
createPage1Content basemap page = do
   drawWithPage page $ do
      strokeColor red
      setWidth 0.5
      stroke $ Rectangle (10 :+ 0) (200 :+ 300)
      drawXObject basemap
      return ()

createPage2Content ∷ PDFReference PDFXForm → PDFReference PDFPage → PDF ()
createPage2Content basemap page = do
   drawWithPage page $ do
      strokeColor blue
      setWidth 0.5
      fill $ Rectangle (00 :+ 10) (300 :+ 200)
      drawXObject basemap
      return ()
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NewMap
where

import Graphics.PDF hiding (boxHeight, boxWidth)
--import Control.Monad.Unicode
import Prelude.Unicode
import Data.String (IsString(..))
import Data.Vector (Vector)
-- import qualified Data.Vector as V
import Data.Foldable
import Data.Maybe
import PolygonReduce
import Geometry.Shapefile.Types (RecBBox(..), recXMin, recXMax, recYMin, recYMax, getX, getY )

instance IsString PDFString where
   fromString = toPDFString

data Shp = Shp {
   shpBBox :: RecBBox,
   shpPoints ∷ Vector Point
}

embiggenBoundingBox ∷ RecBBox → RecBBox
embiggenBoundingBox (RecBBox ll ur) = RecBBox (ll-buffer) (ur+buffer)
   where
      buffer = (ur - ll) * 0.1

mkShape ∷ Vector (Double, Double) → Shp
mkShape points = let points' = toPoint <$> points in Shp {
   shpBBox = embiggenBoundingBox $ bboxFromPoints points',
   shpPoints = points'
}

toPoint ∷ (Double, Double) → Point
toPoint (x, y) = x :+ y

bboxFromPoints ∷ Vector Point → RecBBox
bboxFromPoints = fromMaybe (RecBBox (0 :+ 0) (10 :+ 10)) . bboxFromPointsˀ

-- we use nothing because we don't want our bbox 
-- to include (0, 0) unless the data truly does
bboxFromPointsˀ ∷ Vector Point → Maybe RecBBox
bboxFromPointsˀ = foldl' fitPointInBoxˀ Nothing

fitPointInBoxˀ ∷ Maybe RecBBox → Point → Maybe RecBBox
fitPointInBoxˀ Nothing p = Just $ RecBBox p p
fitPointInBoxˀ (Just b) p = Just $ fitPointInBox b p

fitPointInBox ∷ RecBBox → Point → RecBBox
fitPointInBox box point@(x:+y) = if box `fits` point then box else box {
      lowerLeft = min x (recXMin box) :+ min y (recYMin box),
      upperRight = max x (recXMax box) :+ max y (recYMax box)
}

fits ∷ RecBBox → Point → Bool
box `fits` (x:+y) = recXMin box ≤ x ∧ x ≤ recXMax box
                  ∧ recYMin box ≤ y ∧ y ≤ recYMax box


myDocument ∷ Shp → PDF ()
myDocument points = do
   let pageSize = pageFromBBox (shpBBox points)
       pageSize' = Just $ rectToPdfRect pageSize
   page1 ← addPage pageSize' 
   page2 ← addPage pageSize' 
   basemap ← mapOnPage pageSize points
   createPage1Content basemap page1
   createPage2Content basemap page2

rectToPdfRect ∷ Rect → PDFRect
rectToPdfRect (Rect x0 y0 x1 y1) = 
   PDFRect (floor x0) (floor y0) (ceiling x1) (ceiling y1)

pageFromBBox ∷ RecBBox → Rect
pageFromBBox box = Rect 0 0 maxX maxY
   where
      width = recXMax box - recXMin box
      height = recYMax box - recYMin box
      multiplier = if width > height then 600 / width else 600 / height
      maxX = width * multiplier
      maxY = height * multiplier

matrixForBBox ∷ Rect → RecBBox → Matrix
matrixForBBox (Rect x0 y0 x1 y1) bbox =    
   Matrix xscale 0 0 yscale tx ty
   where
      boxHeight = recYMax bbox - recYMin bbox
      boxWidth = recXMax bbox - recXMin bbox
      pageHeight = y1 - y0
      pageWidth = x1 - x0
      xscale = pageWidth / boxWidth
      yscale = pageHeight / boxHeight
      tx = xscale * (-(recXMin bbox))
      ty = yscale * (-(recYMin bbox))

clipPath ∷ RecBBox → Vector Point → Vector Point
clipPath = fmap . clipPoint

clipOrd ∷ Ord a ⇒ a → a → a → a
clipOrd low i = min (max low i)

clipPoint ∷ RecBBox → Point → Point
clipPoint bbox point = clipPart getX :+ clipPart getY
   where 
      outsideLL = lowerLeft bbox - 0.01
      outsideUR = upperRight bbox - 0.01
      clipPart get = clipOrd 
         (get outsideLL)
         (get point) 
         (get outsideUR)

data Rect = Rect PDFFloat PDFFloat PDFFloat PDFFloat 

invertScale ∷ Matrix → PDFFloat → PDFFloat
invertScale (Matrix sx _ _ _ _ _) width = width / sx

mapOnPage ∷ Rect → Shp → PDF (PDFReference PDFXForm)
mapOnPage rect@(Rect x0 y0 x1 y1) shape = 
   createPDFXForm x0 y0 x1 y1 $ do
      let matrix = matrixForBBox rect (shpBBox shape)
      applyMatrix matrix
      let width = invertScale matrix 1
      setWidth width
      strokeColor green
      addPolygonToPath $ toList $
         reduce 0.005 $ clipPath (shpBBox shape) $ shpPoints shape
      strokePath

createPage1Content ∷ PDFReference PDFXForm → PDFReference PDFPage → PDF ()
createPage1Content basemap page = drawWithPage page $ do
      strokeColor red
      setWidth 0.5
      stroke $ Rectangle (10 :+ 0) (200 :+ 300)
      drawXObject basemap
      return ()

createPage2Content ∷ PDFReference PDFXForm → PDFReference PDFPage → PDF ()
createPage2Content basemap page = drawWithPage page $ do
      strokeColor blue
      setWidth 0.5
      fill $ Rectangle (00 :+ 10) (300 :+ 200)
      drawXObject basemap
      return ()


{-# OPTIONS_GHC -fno-warn-orphans #-}

module NewMap(Rect(..), bboxFromPoints, resizeBoundingBox, embiggenBoundingBox, rectToPdfRect, pageFromBBox, invertScale, matrixForBBox, addPolygonToPath, clipPath)
where

import Point
import qualified Graphics.PDF as Pdf
import Graphics.PDF hiding (addPolygonToPath, Point, boxHeight, boxWidth)
--import Control.Monad.Unicode
import Prelude.Unicode
import Data.String (IsString(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Foldable
import Data.Maybe
import Geometry.Shapefile.Types (RecBBox(..), recXMin, recXMax, recYMin, recYMax, getX, getY )

instance IsString PDFString where
   fromString = toPDFString

resizeBoundingBox ∷ Point → RecBBox → RecBBox
resizeBoundingBox size (RecBBox ll ur) = RecBBox (ll-buffer) (ur+buffer)
   where
      buffer = (ur - ll) * size

embiggenBoundingBox ∷ RecBBox → RecBBox
embiggenBoundingBox = resizeBoundingBox 0.1

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


rectToPdfRect ∷ Rect → PDFRect
rectToPdfRect (Rect x0 y0 x1 y1) = 
   PDFRect (floor x0) (floor y0) (ceiling x1) (ceiling y1)

pageFromBBox ∷ RecBBox → Rect
pageFromBBox box = Rect 0 0 maxX maxY
   where
      width = recXMax box - recXMin box
      height = recYMax box - recYMin box
      multiplier = if width > height then 1000 / width else 1000 / height
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

addPolygonToPath ∷ Vector Point → Draw ()
addPolygonToPath = Pdf.addPolygonToPath . V.toList

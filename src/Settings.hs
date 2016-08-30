module Settings
where

import Data.Complex
import Geometry.Shapefile.Types (RecBBox(..))
import qualified Graphics.PDF as Pdf
import NewMap (Rect(..), invertScale, matrixForBBox, pageFromBBox)

import Data.Text (Text)

ptc ∷ Pen → Pdf.Color
ptc = colorToColor . penColor

penColor ∷ Pen → Color
penColor = writingStyleColor . penWritingStyle

writingStyleColor ∷ WritingStyle → Color
writingStyleColor (Solid c) = c
writingStyleColor (Water c) = c
writingStyleColor (Outline _ c) = c

colorToColor ∷ Color → Pdf.Color
colorToColor (Color r g b) = Pdf.Rgb (toRatio r) (toRatio g) (toRatio b)
   where toRatio n = (fromIntegral n) / 255

type Point = Complex Double
   

data Settings = Settings {
    orientation :: Orientation,
    projection :: Text,
    land :: Pen,
    water :: Pen,
    riverPen :: Pen,
    boundingBox :: RecBBox,
    majorUrban :: Pen,
    otherUrban :: Pen,
    boundedLocality :: Pen,
    broadArea :: Pen,
    broadLines :: Pen,
    narrowArea :: Pen,
    narrowLines :: Pen
}

data Orientation = Portrait | Landscape

data Color = Color { colorRed :: Int, colorBlue :: Int, colorGreen :: Int }

data Pen = Pen { penWritingStyle ∷ WritingStyle, penAlpha ∷ Int }
newtype Width = Points Double

data WritingStyle
    = Solid Color
    | Water Color
    | Outline Width Color


settingsMatrix ∷ Settings → Pdf.Matrix
settingsMatrix settings = 
   matrixForBBox (settingsRect settings) (boundingBox settings)

settingsEpsilon ∷ Settings → Double
settingsEpsilon settings = invertScale (settingsMatrix settings) 0.25

settingsRect ∷ Settings → Rect
settingsRect = pageFromBBox . boundingBox

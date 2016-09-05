module Settings (settingsSpecialCases, filePaths, penWidth, penColor, writingStyleColor, colorToColor, SettingsT, withDefaultSettings, ptc, Orientation(..), Width(..), Color(..), Settings(..), Pen(..), WritingStyle(..), liftF, module Control.Monad.Trans.Reader, module Point)
where

import Utils
import Unicode
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Geometry.Shapefile.Types (RecBBox(..))
import qualified Graphics.PDF as Pdf
import NewMap (Rect(..), invertScale, matrixForBBox, pageFromBBox)
import Point

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
   where toRatio n = fromIntegral n / 255

type SettingsT = ReaderT Settings

liftF ∷ (Monad m) ⇒ RunSettingsT m a → SettingsT m a
liftF m = asks settingsRunSettings ⇉ lift . runReaderT m

filePaths ∷ Settings → FilePaths
filePaths = rsFilePaths . settingsRunSettings

settingsSpecialCases = rsSpecialCaseMap . settingsRunSettings

data Settings = Settings {
    settingsRunSettings ∷ RunSettings,
    settingsEpsilon ∷ Maybe Double,
    settingsMatrix ∷ Pdf.Matrix,
    settingsRect ∷ Rect,
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

withDefaultSettings ∷ RunSettings → RecBBox → Settings
withDefaultSettings rs bbox = Settings {
    settingsRunSettings = rs,
    orientation = Portrait,
    projection = if width > height then "-JM60c" else "-JM100c+",
    land = Pen (Solid (Color 245 245 245)) 0,
    water = Pen (Solid (Color 198 236 255)) 0,
    riverPen = Pen (Outline (Points 1) (Color 9 120 171)) 0,
    boundingBox = bbox,
    majorUrban = Pen (Solid (Color 100 100 100)) 40,
    otherUrban = Pen (Solid (Color 125 125 125)) 40,
    boundedLocality = Pen (Solid (Color 150 150 150)) 40,
    narrowArea = Pen (Solid (Color 100 0 0)) 60,
    narrowLines = Pen (Outline (Points 0.4) (Color 100 0 0)) 0,
    broadLines = Pen (Outline (Points 2.0) (Color 150 150 150)) 0,
    broadArea = colorTheMunicipality_,
    settingsMatrix = matrix,
    settingsEpsilon = Just $ invertScale matrix 0.25, 
    settingsRect = rect
}
   where 
      (width :+ height) = upperRight bbox - lowerLeft bbox
      matrix = matrixForBBox rect bbox
      rect = pageFromBBox bbox


colorTheMunicipality_ :: Pen
colorTheMunicipality_ = Pen (Solid (Color 254 254 233)) 20


data Orientation = Portrait | Landscape

data Color = Color { colorRed :: Int, colorBlue :: Int, colorGreen :: Int }

data Pen = Pen { penWritingStyle ∷ WritingStyle, penAlpha ∷ Int }
newtype Width = Points Double

data WritingStyle
    = Solid Color
    | Water Color
    | Outline Width Color

penWidth (Pen (Outline (Points w) _) _) = w
penWidth _ = 10 


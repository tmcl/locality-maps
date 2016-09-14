module Settings (PenGetter, SettingsT, Color(..), Settings(..), withDefaultSettings, settingsSpecialCases, filePaths, penWidth, penColor, WritingStyle(..), Width(..), Pen(..), ptc, module Control.Monad.Trans.Reader)
where

import SpecialCases
import Project
import Utils
-- import Unicode
import Control.Monad.Trans.Reader
import Geometry.Shapefile.Types (RecBBox(..))
import qualified Graphics.PDF as Pdf
import NewMap (Rect(..), invertScale, matrixForBBox, pageFromBBox)
import Point

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

filePaths ∷ Settings → FilePaths
filePaths = rsFilePaths . settingsRunSettings

settingsSpecialCases ∷ Settings → SpecialCaseMap
settingsSpecialCases = rsSpecialCaseMap . settingsRunSettings

data Settings = Settings {
    settingsRunSettings ∷ RunSettings,
    boundingBox ∷ RecBBox,
    settingsMatrix ∷ Pdf.Matrix,
    settingsEpsilon ∷ Maybe Double,
    settingsPointSize ∷ Double,
    settingsRect ∷ Rect,
    projection ∷ Point → Point,
    land ∷ Pen,
    water ∷ Pen,
    riverPen ∷ Pen,
    reservesPen ∷ Pen,
    majorUrban ∷ Pen,
    otherUrban ∷ Pen,
    boundedLocality ∷ Pen,
    stateLines ∷ Pen,
    municipalLines ∷ Pen,
    localityLines ∷ Pen,
    highlitArea ∷ Pen,
    likeHighlitArea ∷ Pen,
    subjectArea ∷ Pen,
    subjectAreaHighlight ∷ Pen,
    subjectSiblingsAreas ∷ Pen
}

withDefaultSettings ∷ RunSettings → RecBBox → Settings
withDefaultSettings rs bbox = Settings {
    settingsRunSettings = rs,
    boundingBox = bbox,
    settingsMatrix = matrix,
    settingsEpsilon = Just $ invertScale matrix 0.33, 
    settingsPointSize = invertScale matrix 1,
    settingsRect = rect,
    projection = projector,

    -- background
    land = Pen (Solid (Color 224 224 224)) 0,
    water = Pen (Solid (Color 198 236 255)) 0,
    riverPen = Pen (Outline (Points 0.5) (Color 9 120 171)) 0,

    -- superimpose on the background, but only illustrative
    reservesPen = Pen (Solid (Color 0 115 0)) 80,
    majorUrban = Pen (Solid (Color 246 225 185)) 0,
    otherUrban = Pen (Solid (Color 246 225 185)) 0,
    boundedLocality = Pen (Solid (Color 246 225 185)) 0,

    -- lines of the things (constant no matter the zoom)
    stateLines = Pen (Outline (Points 3) (Color 150 150 150)) 0,
    municipalLines = Pen (Outline (Points 1) (Color 150 150 150)) 0,
    localityLines = Pen (Outline (Points 0.4) (Color 150 0 0)) 0,

    -- areas (change according to zoom)
    highlitArea = Pen (Solid (Color 193 40 56)) 60,
    likeHighlitArea = Pen (Solid (Color 193 40 56)) 80,
    subjectArea = Pen (Solid (Color 254 254 223)) 0,
    subjectAreaHighlight = Pen (Solid (Color 254 254 223)) 75,
    subjectSiblingsAreas = Pen (Solid (Color 245 245 245)) 0
}
   where 
      projector = mercator $ lowerLeft bbox
      projectedBBox = projectBBox projector bbox
      matrix = matrixForBBox rect projectedBBox
      rect = pageFromBBox projectedBBox

projectBBox ∷ (Point → Point) → RecBBox → RecBBox
projectBBox projector RecBBox {lowerLeft = ll, upperRight = ur} 
   = RecBBox {lowerLeft = projector ll, upperRight = projector ur}

type PenGetter = Settings → Pen

data Color = Color { colorRed ∷ Int, colorBlue ∷ Int, colorGreen ∷ Int }

data Pen = Pen { penWritingStyle ∷ WritingStyle, penAlpha ∷ Int }
newtype Width = Points Double

data WritingStyle
    = Solid Color
    | Water Color
    | Outline Width Color

penWidth ∷ Pen → Double
penWidth (Pen (Outline (Points w) _) _) = w
penWidth _ = 10 


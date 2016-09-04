module UpdatedMapper where

import Data.Text (Text)
import FindLocalities (localityColumnName)
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Geometry.Shapefile.Conduit
import Settings
import Utils
import Data.Maybe
import System.IO
import System.FilePath
import qualified Graphics.PDF as Pdf
import NewMap
import Control.Monad.Trans.Class

import Data.Vector (Vector)
import qualified Data.Vector as V
import PolygonReduce

liftT ∷ (Monad m, Monad n) ⇒ ReaderT r n a → ReaderT r m (n a)
liftT = mapReaderT return

applySettings ∷ SettingsT Pdf.Draw ()
applySettings = do
   matrix ← asks settingsMatrix 
   lift $ Pdf.applyMatrix matrix

penToAlpha ∷ Pen → Double
penToAlpha (Pen _ a) = fromIntegral (100-a)/100

fillPoints2 ∷ (Settings → Pen) → [Vector Point] → SettingsT Pdf.Draw ()
fillPoints2 asker points = do
   pen ← asks asker
   fillPoints1 pen points

drawCircles ∷ Pen → [Point] → SettingsT Pdf.Draw ()
drawCircles color = mapM_ (drawCircle color)

drawCircle ∷ Pen → Point → SettingsT Pdf.Draw ()
drawCircle color (x :+ y) = do
   let pen = color {penWritingStyle = Outline (Points 5) (penColor color)}
   eps ← asks settingsEpsilon
   usePen pen
   let w = maybe 1 (100*) eps
   let c = Pdf.Circle x y w
   lift $ Pdf.addShape c
   lift Pdf.strokePath


fillPoints1 ∷ Pen → [Vector Point] → SettingsT Pdf.Draw ()
fillPoints1 color points = do
   preparePoints1 points
   lift $ Pdf.fillColor (ptc color)
   lift $ Pdf.setFillAlpha (penToAlpha color) 
   lift Pdf.fillPathEO

shapeSource ∷ FilePath → Maybe RecBBox → Sink Shape IO a → IO a
shapeSource fp bbox sink = withShpFile fp $ \shp dbf →
   shapesFromDbfShpSource bbox shp dbf $$ sink

withShpFile ∷ FilePath → (Handle → Handle → IO a) → IO a
withShpFile filePath cb =
  withFile filePath ReadMode $ \shp →
    withFile (toDbf filePath) ReadMode $ \dbf →
      cb shp dbf

toDbf ∷ FilePath → FilePath
toDbf = (`replaceExtension` "dbf")

eachPolygon ∷ Conduit Shape IO [Vector Point]
eachPolygon = CC.map (\(_, r, _) → pointsFromRecord r)

eachPlace ∷ Conduit Shape IO [Vector Point]
eachPlace = CC.map (\(_, r, _) →
   return . tsi . V.concat . pointsFromRecord $ r)
   where
      tsi a = seq a a

-- andReverseTheRest ∷ [Vector a] → [Vector a]
-- andReverseTheRest (first:rest) = first:intersperse [last first] rest
-- andReverseTheRest a = a

pointsFromRecord ∷ ShpRec → [Vector Point]
pointsFromRecord r = 
   maybe [] pointsFromRecContents (shpRecContents r)

--wrapEnds ∷ Vector Point → Vector Point
--wrapEnds v
--   | length v < 2 = v
--   | otherwise = V.snoc v (V.head v)

type XForm = Pdf.PDFReference Pdf.PDFXForm

makeXForm1 ∷ Pdf.Draw () → SettingsT Pdf.PDF XForm
makeXForm1 draw = do
   Rect x0 y0 x1 y1 ← asks settingsRect
   lift $ Pdf.createPDFXForm x0 y0 x1 y1 draw

drawPoints ∷ Pen → Vector Point → SettingsT Pdf.Draw ()
drawPoints color points = do
   preparePoints1 [points]
   usePen color
   lift Pdf.strokePath

usePen ∷ Pen → SettingsT Pdf.Draw ()
usePen color = do
   eps ← asks settingsEpsilon
   let w = penWidth color
   lift (whenˀ $ Pdf.setWidth . (w * 4 *) <$> eps)
   lift $ Pdf.strokeColor (ptc color)

bboxToPolygon ∷ RecBBox → Vector Point
bboxToPolygon box = V.fromList [
   lowerLeft box,
   lowerRight box,
   upperRight box,
   upperLeft box,
   lowerLeft box]

lowerRight ∷ RecBBox → Point
lowerRight box = recXMax box :+ recYMin box
upperLeft ∷ RecBBox → Point
upperLeft box = recXMin box :+ recYMax box

whenˀ ∷ Monad m ⇒  Maybe (m ()) → m ()
whenˀ (Just foo) = foo
whenˀ Nothing = return ()

preparePoints1 ∷ [Vector Point] → SettingsT Pdf.Draw () 
preparePoints1 points = do
   eps ← asks settingsEpsilon
   bbox ← asks boundingBox
   let reducer = maybe id (reduce . (*4) ) eps
       clipPath _ = id
   lift $ mapM_ (addPolygonToPath . reducer . clipPath bbox) points

-- squared ∷ ∀a. (a → a) → a
-- squared f = f . f

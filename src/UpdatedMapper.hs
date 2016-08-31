module UpdatedMapper (makeXForm1, drawPoints, fillPoints1, fillPoints2, applySettings, shapeSource, withShpFile, toDbf, eachPolygon, eachPlace, XForm, liftT) where

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Geometry.Shapefile.Conduit
import Settings
import Data.List
import Data.Maybe
import System.IO
import System.FilePath
import qualified Graphics.PDF as Pdf
import NewMap
import Control.Monad.Trans.Class

import qualified Data.Vector as V
import PolygonReduce

liftT ∷ (Monad m, Monad n) ⇒ ReaderT r n a → ReaderT r m (n a)
liftT = mapReaderT return

applySettings ∷ SettingsT Pdf.Draw ()
applySettings = asks settingsMatrix >>= lift . Pdf.applyMatrix

penToAlpha ∷ Pen → Double
penToAlpha (Pen _ a) = fromIntegral (100-a)/100

fillPoints2 ∷ (Settings → Pen) → [Point] → SettingsT Pdf.Draw ()
fillPoints2 asker points = do
   pen ← asks asker
   fillPoints1 pen points

fillPoints1 ∷ Pen → [Point] → SettingsT Pdf.Draw ()
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



eachPolygon ∷ Conduit (a, ShpRec, b) IO [[Point]]
eachPolygon = CC.map (\(_, r, _) → pointsFromRecord r)

eachPlace ∷ Conduit (a, ShpRec, b) IO [[Point]]
eachPlace = CC.map (\(_, r, _) →
   return . concat . andReverseTheRest . map wrapEnds . pointsFromRecord $ r)

andReverseTheRest ∷ [[a]] → [[a]]
andReverseTheRest (first:rest) = first:intersperse [last first] rest
andReverseTheRest a = a

pointsFromRecord ∷ ShpRec → [[Point]]
pointsFromRecord r = concatMap pointsFromRecContents (catMaybes [shpRecContents r])

pointsFromRecContents ∷ RecContents → [[Point]]
pointsFromRecContents r@RecPolygon {}  = recPolPoints r
pointsFromRecContents r@RecPolyLine {} = recPolLPoints r
pointsFromRecContents _                = []

wrapEnds ∷ [Point] → [Point]
wrapEnds [] = []
wrapEnds [a] = [a]
wrapEnds line = last line:line

type XForm = Pdf.PDFReference Pdf.PDFXForm

makeXForm1 ∷ Pdf.Draw () → SettingsT Pdf.PDF XForm
makeXForm1 draw = do
   Rect x0 y0 x1 y1 ← asks settingsRect
   lift $ Pdf.createPDFXForm x0 y0 x1 y1 draw

drawPoints ∷ Pen → [Point] → SettingsT Pdf.Draw ()
drawPoints color points = do
   preparePoints1 points
   eps ← asks settingsEpsilon
   lift (whenˀ $ Pdf.setWidth . (4 *) <$> eps)
   lift $ Pdf.strokeColor (ptc color)
   lift Pdf.strokePath

whenˀ ∷ Monad m ⇒  Maybe (m ()) → m ()
whenˀ (Just foo) = foo
whenˀ Nothing = return ()

preparePoints1 ∷ [Point] → SettingsT Pdf.Draw () 
preparePoints1 points = do
   eps ← asks settingsEpsilon
   bbox ← asks boundingBox
   let reducer = maybe id reduce eps
   lift $ Pdf.addPolygonToPath . V.toList . reducer . clipPath bbox . V.fromList 
      $ points

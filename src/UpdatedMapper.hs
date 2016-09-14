module UpdatedMapper(drawPoints1, matchState, statesSource, applySettings1, XForm, fillPoints1, liftT, makeXForm1, applySettings, fillPoints2, multiSources, shapeSource, drawPoints, toDbf, eachPolygon, eachPlace, bboxToPolygon, drawCircles, module Point) where

import Unicode
import Project
import Point
import Data.Text (Text)
import FindLocalities
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
import Types

type Sauce a = Sink Shape IO a → SettingsT IO a

stateCodeColumnName ∷ Text → Bool
stateCodeColumnName t = t ≡ "STATE_CODE" ∨ t ≡ "HRPcode" ∨ t ≡ "A1C_2013"

stateCode ∷ State → Int
stateCode ACTg = 1
stateCode ACTd = 1
stateCode OT = 2
stateCode NSW = 3
stateCode NT = 4
stateCode Qld = 5
stateCode SA = 6
stateCode Tas = 7
stateCode Vic = 8
stateCode WA = 9

matchState ∷ State → Shape → Bool
matchState s = matchNumericDbfField (stateCode s) stateCodeColumnName

liftT ∷ (Monad m, Monad n) ⇒ ReaderT r n a → ReaderT r m (n a)
liftT = mapReaderT return

applySettings1 ∷ Settings → Pdf.Draw ()
applySettings1 settings =
   Pdf.applyMatrix (settingsMatrix settings)

applySettings ∷ SettingsT Pdf.Draw ()
applySettings = ask ⇉ lift . applySettings1

penToAlpha ∷ Pen → Double
penToAlpha (Pen _ a) = fromIntegral (100-a)/100

fillPoints2 ∷ (Settings → Pen) → [Vector Point] → SettingsT Pdf.Draw ()
fillPoints2 asker points = do
   pen ← asks asker
   settings ← ask
   lift $ fillPoints1 settings pen points

drawCircles ∷ Settings → Pen → (Point, RecBBox) → Pdf.Draw ()
drawCircles settings color points = 
   drawCircle settings color projected diameter
   where 
      project = projection settings
      RecBBox ll ur  = snd points
      projectedLL = project ll  
      projectedUR = project ur
      (width :+ height) = ur - ll
      diameter = max width height
      projected = project $ fst points

drawCircle ∷ Settings → Pen → Point → Double → Pdf.Draw ()
drawCircle s color (x :+ y) w = do
   let pen = color { penWritingStyle = 
                        Outline 
                          (Points 8) 
                          (penColor color) }
   usePen pen s
   let bbox = boundingBox s
       penWidth = invertScale (settingsMatrix s) 8
       width :+ height = upperRight bbox - lowerLeft bbox
       -- w = width * 0.05
   Pdf.addShape (Pdf.Circle x y (w+2*penWidth))
   Pdf.strokePath


fillPoints1 ∷ Settings → Pen → [Vector Point] → Pdf.Draw ()
fillPoints1 settings color points = do
   preparePoints settings points
   Pdf.fillColor (ptc color)
   Pdf.setFillAlpha (penToAlpha color) 
   Pdf.fillPathEO

shapeSource ∷ FilePath → Maybe RecBBox → Sink Shape IO a → IO a
shapeSource fp bbox sink = withShpFile fp $ \shp dbf →
   shapesFromDbfShpSource bbox shp dbf $$ sink

multiSources ∷ Yielder 
             → Maybe RecBBox 
             → Sink Shape IO a
             → IO a
multiSources yielder bbox sink = do
  r ← concat <$> mapM go yielder
  CC.yieldMany r $$ sink
  where
    go (filePath, filter') = 
      shapeSource 
         filePath 
         bbox 
         (CC.filter filter' =$= CC.sinkList)


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

drawPoints1 ∷ Settings 
            → Pen 
            → [Vector Point] 
            → Pdf.Draw ()
drawPoints1 settings color points = do
   preparePoints settings points
   usePen color settings
   Pdf.strokePath

drawPoints ∷ Pen → Vector Point → SettingsT Pdf.Draw ()
drawPoints color points = do
   s ← ask
   lift $ drawPoints1 s color [points]

usePen ∷ Pen → Settings → Pdf.Draw ()
usePen color s = do
   let points = settingsPointSize s
       w = penWidth color
   Pdf.setWidth (w * points)
   Pdf.strokeColor (ptc color)

whenˀ ∷ Monad m ⇒  Maybe (m ()) → m ()
whenˀ (Just foo) = foo
whenˀ Nothing = return ()

preparePoints ∷ Settings 
               → [Vector Point] 
               → Pdf.Draw () 
preparePoints settings points = do
   let eps = settingsEpsilon settings
       bbox = boundingBox settings
       theProjection = projection settings
       reducer = maybe id reduce eps
       clipPath _ = id -- todo
       project = V.map theProjection
       addSimplifiedPolygonToPath = addPolygonToPath 
                                  . project 
                                  . reducer 
                                  . clipPath bbox
   mapM_ addSimplifiedPolygonToPath points

preparePoints1 ∷ [Vector Point] → SettingsT Pdf.Draw () 
preparePoints1 points = do
   settings ← ask
   lift $ preparePoints settings points

orM ∷ [a → Bool] → a → Bool
orM [] _ = False
orM (p:pp) a = p a ∨ orM pp a

statesSource ∷ [State] → Sauce [Vector Point]
statesSource states_ sink = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   lift $ multiSources (states fps) (Just bbox) 
      (CC.filter (orM $ map matchState states_) =$= sink)
   

module EachMap (fillCoordinates, mapStateLocally, mapFineLines) where

import UpdatedMapper
import qualified Data.Conduit.Combinators as CC
import Data.Conduit as CC
import Settings
import qualified Graphics.PDF as Pdf
import Types
import Utils
import Geometry.Shapefile.Conduit
import Data.Text (Text)
import Control.Monad.Trans.Class

matchState ∷ State → Shape → Bool
matchState s = matchNumericDbfField (stateCode s) stateCodeColumnName

mapStateLocally ∷ FilePaths → State → SettingsT IO (Pdf.PDF XForm)
mapStateLocally fps state = do
  bbox ← asks boundingBox
  pen ← asks broadArea
  points ← lift $ concat <$> shapeSource (states fps) (Just bbox)
    (CC.filter (matchState state) =$= eachPlace =$= CC.sinkList)
  liftT (mapStateLocally2 pen points)

fillCoordinates ∷ Pen → [[Point]] → SettingsT Pdf.Draw ()
fillCoordinates pen points = do
  applySettings
  mapM_ (fillPoints1 pen) points

outlineCoordinates ∷ Pen → [[Point]] → SettingsT Pdf.Draw ()
outlineCoordinates pen points = do
  applySettings
  mapM_ (drawPoints pen) points

mapStateLocally2 ∷ Pen → [[Point]] → SettingsT Pdf.PDF XForm
mapStateLocally2 pen points = do
  drawing ← liftT $ fillCoordinates pen points
  makeXForm1 drawing

mapFineLines ∷ IO [[Point]] → SettingsT IO (Pdf.PDF XForm)
mapFineLines source = do
   pen ← asks narrowLines
   points ← lift source
   drawing ← liftT $ outlineCoordinates pen points
   liftT $ makeXForm1 drawing

stateCodeColumnName ∷ Text → Bool
stateCodeColumnName = (== "STATE_CODE")

stateCode ∷ State → Int
stateCode ACT = 1
stateCode OT = 2
stateCode NSW = 3
stateCode NT = 4
stateCode Qld = 5
stateCode SA = 6
stateCode Tas = 7
stateCode Vic = 8
stateCode WA = 9

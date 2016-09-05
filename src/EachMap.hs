module EachMap () where

import Algebra.Clipper
import FindLocalities (localityColumnName, toClipperPolygon)
import qualified Data.Text as T
import Unicode
import UpdatedMapper
import qualified Data.Conduit.Combinators as CC
import Data.Conduit as C
import Data.Vector (Vector)
import Settings
import qualified Graphics.PDF as Pdf
import Types
import Utils
import Geometry.Shapefile.Conduit
import Data.Text (Text)
import Control.Monad.Trans.Class
import ClassyPrelude (traceShowId, trace)

type Draw = Pdf.Draw

matchState ∷ State → Shape → Bool
matchState s = matchNumericDbfField (stateCode s) stateCodeColumnName

mapLocally ∷ [Vector Point] → SettingsT Draw ()
mapLocally = fillCoordinates broadArea

mapHighlight ∷ [Vector Point] → SettingsT Draw ()
mapHighlight = fillCoordinates narrowArea

type PenGetter = Settings → Pen

mapOutline ∷ Drawer → Sauce [Vector Point] → SettingsT IO (Draw ())
mapOutline drawer source = liftT . drawer ⇇ source outlineSink

mapCircles ∷ PenGetter → Sauce [Vector Point] → SettingsT IO (Draw ())
mapCircles pen source = do
   pen' ← asks pen
   bb ← asks boundingBox
   liftT . drawCirclesX bb pen' ⇇ source outlineSink

drawCirclesX ∷ RecBBox
             → Pen 
             → [Vector Point] 
             → SettingsT Pdf.Draw ()
drawCirclesX bb p pp = drawCircles p (vlevify bb pp)

vlevify ∷ RecBBox → [Vector Point] → [Point]
vlevify box a = smallPlaces
   where
      area = polygonArea . toClipperPolygon
      eachArea = map 
         (\l → ((/mapArea). negate . area $ l, l)) 
         a
      isSmallArea (a, _) = a ≥ 0 ∧ a < 0.75e-3
      b'' = if all isSmallArea eachArea
            then eachArea
            else []
      smallPlaces = map (\(_, b) → centreOf b) b''
      mapArea = area . bboxToPolygon $ box

centreOf ∷ Vector Point → Point
centreOf pp = sum pp/fromIntegral (length pp)

outlineSink ∷ ConduitM Shape c IO [Vector Point]
outlineSink = eachPolygon 
              =$= CC.map (\p → seq p p) 
              =$= CC.concat 
              =$= CC.sinkList

mapState ∷ State → SettingsT IO (Draw ())
mapState = mapOutline (fillCoordinates broadArea) . stateSource

mapLocalities ∷ Drawer → SettingsT IO (Draw ())
mapLocalities drawer = mapOutline drawer localitiesSource

mapRivers ∷ SettingsT IO (Draw ())
mapRivers = mapOutline (outlineCoordinates riverPen) riverSource

mapLakes ∷ SettingsT IO (Pdf.PDF XForm, Pdf.PDF XForm)
mapLakes = do
   area ← xformify' ⇇ 
      mapOutline (fillCoordinates water) lakeSource
   outline ← xformify' ⇇ 
      mapOutline (outlineCoordinates riverPen) lakeSource
   return (area, outline)

xformify' = liftT . xformify . lift

mapLoc'y ∷ Drawer → Locality → SettingsT IO (Draw ())
mapLoc'y drawer loc'y = 
   mapOutline drawer (localitySource loc'y)

mapLoki2 ∷ PenGetter → Locality → SettingsT IO (Draw ())
mapLoki2 pen loc'y = 
   mapCircles pen (localitySource loc'y)

mapMunis ∷ Drawer → SettingsT IO (Draw ())
mapMunis drawer = mapOutline drawer municipalitiesSource

mapMuni ∷ PenGetter 
        → Municipality 
        → SettingsT IO (Draw ())
mapMuni mapper = mapOutline (fillCoordinates mapper) . municipalitySource'

mapMuni2 ∷ PenGetter 
         → Municipality 
         → SettingsT IO (Draw ())
mapMuni2 mapper = mapCircles mapper . municipalitySource'

type Sauce a = Sink Shape IO a → SettingsT IO a

municipalitySource' ∷ Municipality → Sauce [Vector Point]
municipalitySource' muni sink = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   lift $ municipalitySource fps muni (Just bbox) (CC.filter (matchMunicipality muni) =$= sink)

municipalitySource ∷ FilePaths → Municipality → Maybe RecBBox 
   → Sink Shape IO a → IO a
municipalitySource fps muni = 
   shapeSource (municipalityFilePathByMunicipality fps muni)

source ∷ (FilePaths → FilePath) → Sauce [Vector Point]
source yielder sink = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   lift $ shapeSource (yielder fps) (Just bbox) sink

riverSource ∷ Sauce [Vector Point]
riverSource = source rivers

lakeSource ∷ Sauce [Vector Point]
lakeSource = source lakes

municipalitiesSource ∷ Sauce [Vector Point]
municipalitiesSource = multiSources' municipalityFilePaths

localitiesSource ∷ Sauce [Vector Point]
localitiesSource = multiSources' localityFilePaths

localitySource ∷ Locality → Sauce [Vector Point]
localitySource locality sink = 
   localitiesSource (CC.filter (matchLocality locality) =$= sink)

multiSources' ∷ (FilePaths → Yielder) → Sauce [Vector Point]
multiSources' yielder sink = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   lift $ multiSources (yielder fps) (Just bbox) sink
   

multiSources ∷ Yielder 
             → Maybe RecBBox 
             → Sink Shape IO [Vector Point] 
             → IO [Vector Point]
multiSources yielder bbox sink = do
  yeld ← mapM go yielder
  return $ concat yeld
  where
    go (filePath, filter') = 
      shapeSource 
         filePath 
         bbox 
         (CC.filter filter' =$= sink)

municipalityFilePaths ∷ FilePaths → Yielder
municipalityFilePaths fps = 
   [(nswMunicipalities fps, allFilter),
    (vicMunicipalities fps, allFilter),
    (qldMunicipalities fps, allFilter),
    (waMunicipalities fps, allFilter),
    (saMunicipalities fps, allFilter),
    (tasMunicipalities fps, allFilter),
    (ntMunicipalities fps, allFilter),
    (actMunicipalities fps, allFilter),
    (otMunicipalities fps, allFilter)]


localityFilePaths ∷ FilePaths → Yielder
localityFilePaths fps = 
   [(nswLocalities fps, allFilter),
    (vicLocalities  fps, allFilter),
    (qldLocalities  fps, allFilter),
    (waLocalities fps, allFilter),
    (saLocalities fps, localityFilter),
    (tasLocalities  fps, allFilter),
    (ntLocalities fps, allFilter),
    (otLocalities fps, allFilter),
    (actLocalities fps, districtFilter)]

localityTypeColumn ∷ Text → Bool
localityTypeColumn t 
   = any (`T.isSuffixOf` t) ["_LOCA_5", "_LOCAL_5"]

allFilter ∷ a → Bool
allFilter = const True

localityFilter ∷ Shape → Bool
localityFilter = matchTextDbfField "G" localityTypeColumn

districtFilter ∷ Shape → Bool
districtFilter = matchTextDbfField "D" localityTypeColumn

stateSource ∷ State → Sauce [Vector Point]
stateSource state sink = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   lift $ shapeSource (states fps) (Just bbox) 
      (CC.filter (matchState state) =$= sink)
   
type Drawer = [Vector Point] → SettingsT Draw ()

fillCoordinates ∷ PenGetter → Drawer
fillCoordinates pen points = do
  applySettings
  fillPoints2 pen points

outlineCoordinates ∷ PenGetter → Drawer
outlineCoordinates penGetter points = do
  applySettings
  pen ← asks penGetter
  mapM_ (drawPoints pen) points

--xformify1 ∷ _ → SettingsT Pdf.PDF XForm
--xformify1 = makeXForm1 . lift

xformify ∷ SettingsT Draw () → SettingsT Pdf.PDF XForm
xformify drawing = makeXForm1 ⇇ liftT drawing

mapFineLines ∷ IO [Vector Point] → SettingsT IO (Pdf.PDF XForm)
mapFineLines source = do
   points ← lift source
   drawing ← liftT $ outlineCoordinates narrowLines points
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

matchLocality ∷ Locality → Shape → Bool
matchLocality m = matchTextDbfField m localityColumnName


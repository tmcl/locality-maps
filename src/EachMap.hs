module EachMap (xformify', mapLoc'y, mapLakes, mapRivers, mapMunis, multiSources, outlineCoordinates, mapLocalities, localityFilePaths, mapMuni, PenGetter, Drawer, xformify, municipalitySource, fillCoordinates, mapState, mapFineLines) where

import FindLocalities (localityColumnName)
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
import ClassyPrelude (traceM, trace, traceShowId)

type Draw = Pdf.Draw

matchState ∷ State → Shape → Bool
matchState s = matchNumericDbfField (stateCode s) stateCodeColumnName

mapLocally ∷ [Vector Point] → SettingsT Draw ()
mapLocally = fillCoordinates broadArea

mapHighlight ∷ [Vector Point] → SettingsT Draw ()
mapHighlight = fillCoordinates narrowArea

type PenGetter = Settings → Pen

mapSource ∷ Drawer → Sauce [Vector Point] → SettingsT IO (Draw ())
mapSource drawer source = liftT . drawer ⇇ source areaSink

areaSink ∷ ConduitM Shape c IO [Vector Point]
areaSink = eachPlace =$= CC.map (\p → seq p p) =$= CC.concat =$= CC.sinkList

mapState ∷ State → SettingsT IO (Draw ())
mapState = mapSource (fillCoordinates broadArea) . stateSource

mapLocalities ∷ Drawer → SettingsT IO (Draw ())
mapLocalities drawer = mapSource drawer localitiesSource

mapRivers ∷ SettingsT IO (Draw ())
mapRivers = mapSource (outlineCoordinates riverPen) riverSource

mapLakes ∷ SettingsT IO (Pdf.PDF XForm, Pdf.PDF XForm)
mapLakes = do
   let go pen = xformify' ⇇ mapSource pen lakeSource
   area ← go $ fillCoordinates water
   outline ← go $ outlineCoordinates riverPen
   return (area, outline)

xformify' = liftT . xformify . lift

mapLoc'y ∷ Drawer → Locality → SettingsT IO (Draw ())
mapLoc'y drawer loc'y = mapSource drawer (localitySource loc'y)

mapMunis ∷ Drawer → SettingsT IO (Draw ())
mapMunis drawer = mapSource drawer municipalitiesSource

mapMuni ∷ PenGetter → Municipality → SettingsT IO (Draw ())
mapMuni mapper = mapSource (fillCoordinates mapper) . municipalitySource'

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
   

multiSources ∷ Yielder → Maybe RecBBox → Sink Shape IO [a] → IO [a]
multiSources yielder bbox sink = concat <$> mapM go yielder
  where
    go (filePath, filter') = shapeSource filePath bbox (CC.filter filter' =$= sink)

municipalityFilePaths ∷ FilePaths → Yielder
municipalityFilePaths fps = [ (nswMunicipalities fps, allFilter),
                              (vicMunicipalities fps, allFilter),
                              (qldMunicipalities fps, allFilter),
                              (waMunicipalities fps, allFilter),
                              (saMunicipalities fps, allFilter),
                              (tasMunicipalities fps, allFilter),
                              (ntMunicipalities fps, allFilter),
                              (actMunicipalities fps, allFilter),
                              (otMunicipalities fps, allFilter)
                            ]


localityFilePaths ∷ FilePaths → Yielder
localityFilePaths fps = [ (nswLocalities fps, allFilter),
                          (vicLocalities  fps, allFilter),
                          (qldLocalities  fps, allFilter),
                          (waLocalities fps, allFilter),
                          (saLocalities fps, localityFilter),
                          (tasLocalities  fps, allFilter),
                          (ntLocalities fps, allFilter),
                          (otLocalities fps, allFilter),
                          (actLocalities fps, districtFilter)
                        ]

localityTypeColumn ∷ Text → Bool
localityTypeColumn t = "_LOCA_5" `T.isSuffixOf` t || "_LOCAL_5" `T.isSuffixOf` t

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
  mapM_ (fillPoints2 pen) points

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


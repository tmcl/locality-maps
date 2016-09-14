module EachMap (getLocalitiesByNames, localitySourceMany, outlineSink, mapMuniCropped, municipalitySourceByState, mapStates, municipalitiesSource, mapRivers, mapLakes, fillCoordinates, mapMuni, mapMuni2, municipalitySource, mapMunis, mapLocalities, xformify', outlineCoordinates, localityFilePaths, mapLoc'y) where

import Data.List (foldl')
import BaseMap (getLands, operate)
import NewMap
import Data.Maybe
import Algebra.Clipper
import FindLocalities (localityColumnName, toClipperPolygon)
import qualified Data.Text as T
import qualified Data.Vector as V
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
import ClassyPrelude (traceShowId, trace, traceM)

type Draw = Pdf.Draw


mapOutline ∷ Drawer → Sauce [Vector Point] → SettingsT IO (Draw ())
mapOutline drawer source = liftT . drawer ⇇ source outlineSink

mapCircles ∷ PenGetter → Sauce [Vector Point] → SettingsT IO (Draw ())
mapCircles pen source = do
   pen' ← asks pen
   bb ← asks boundingBox
   s ← ask
   points ← source outlineSink
   liftT $ lift $ drawCirclesX s bb pen' points

drawCirclesX ∷ Settings
             → RecBBox
             → Pen 
             → [Vector Point] 
             → Pdf.Draw ()
drawCirclesX s bb p pp = drawCircles s p (vlevify bb pp)

vlevify ∷ RecBBox → [Vector Point] → (Point, RecBBox)
vlevify box a = (smallPlaces, smallPlacesBBox)
   where
      area = polygonArea . toClipperPolygon
      eachArea = map 
         (\l → ((/mapArea). negate . area $ l, l)) 
         a
      isSmallArea (a, _) = a ≥ 0 ∧ a < 0.5e-3
      circleables = if all isSmallArea eachArea
            then eachArea
            else []
      smallPlacesUnited ∷ Vector Point
      smallPlacesUnited = foldl' (\bs (_, b) → bs `mappend` b) mempty circleables
      smallPlaces = centreOf smallPlacesUnited
      smallPlacesBBox = bboxFromPoints smallPlacesUnited
      mapArea = area . bboxToPolygon $ box

centreOf ∷ Vector Point → Point
centreOf pp = sum pp/fromIntegral (length pp)

outlineSink ∷ ConduitM Shape c IO [Vector Point]
outlineSink = eachPolygon 
              =$= CC.map (\p → seq p p) 
              =$= CC.concat 
              =$= CC.sinkList

mapStates ∷ [State] → SettingsT IO (Draw ())
mapStates = mapOutline (fillCoordinates subjectArea) . statesSource

mapLocalities ∷ Drawer → SettingsT IO (Draw ())
mapLocalities drawer = mapOutline drawer localitiesSource

mapRivers ∷ SettingsT IO (Draw ())
mapRivers = mapOutline (outlineCoordinates riverPen) riverSource

mapLakes ∷ SettingsT IO (Pdf.PDF XForm)
mapLakes = do
   lakes ← lakeSource outlineSink
   waterPen ← asks water
   river ← asks riverPen
   settings ← ask
   xformify' $ do
      applySettings1 settings
      fillPoints1 settings waterPen lakes
      drawPoints1 settings river lakes
      
xformify' = liftT . xformify . lift

stripSuffix s t = fromMaybe t (T.stripSuffix s t)

mapLoc'y ∷ [Locality] → SettingsT IO (Draw ())
mapLoc'y loc'ies = do
   bbox ← asks boundingBox
   mainPen ← asks highlitArea
   extraPen ← asks likeHighlitArea
   localities ← localitySourceMany loc'ies outlineSink
   let findSimilar loc'y = do
         let basename = stripSuffix " CENTRAL" 
                           (stripSuffix " CITY" loc'y)
         localitySourceLike basename outlineSink

   likeThem ← concat <$> mapM findSimilar loc'ies
   settings ← ask
   lift . return $ do
      applySettings1 settings
      fillPoints1 settings extraPen likeThem
      fillPoints1 settings mainPen localities
      drawCirclesX settings bbox mainPen localities

mapMunis ∷ Drawer → SettingsT IO (Draw ())
mapMunis drawer = mapOutline drawer municipalitiesSource'

mapMuni ∷ PenGetter 
        → Municipality 
        → SettingsT IO (Draw ())
mapMuni mapper m = do
   traceM (show $ mName m) 
   mapOutline (fillCoordinates mapper) . municipalitySource' $ m

mapMuniCropped ∷ PenGetter 
        → Municipality 
        → SettingsT IO (Draw ())
mapMuniCropped mapper m = do
   traceM (show $ mName m) 
   p ← municipalitySource' m outlineSink
   lands ← getLands 
   let points = operate (∩) lands p
   liftT $ fillCoordinates mapper points

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

municipalitySourceByState ∷ FilePaths 
                          → State 
                          → Sink Shape IO a 
                          → IO a
municipalitySourceByState fps state = 
   shapeSource (municipalityFilePathByState state fps) Nothing

source ∷ (FilePaths → FilePath) → Sauce [Vector Point]
source yielder sink = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   lift $ shapeSource (yielder fps) (Just bbox) sink

riverSource ∷ Sauce [Vector Point]
riverSource = source rivers

lakeSource ∷ Sauce [Vector Point]
lakeSource = source lakes

municipalitiesSource ∷ FilePaths → Sink Shape IO a → IO a
municipalitiesSource fps = multiSources (municipalityFilePaths fps) Nothing

municipalitiesSource' ∷ Sauce [Vector Point]
municipalitiesSource' = multiSources' municipalityFilePaths

localitiesSource ∷ Sauce [Vector Point]
localitiesSource = multiSources' localityFilePaths

orList ∷ [a → Bool] → a → Bool
orList predicates a = foldl' (\b pred → (b ∨ pred a)) False predicates

getLocalitiesByNames ∷ [Text] → Sink Shape IO a → RunSettingsT IO a
getLocalitiesByNames localities sink = do
   fps ← asks rsFilePaths
   lift $ multiSources 
      (localityFilePaths fps) 
      Nothing 
      (CC.filter (orList (map matchLocality localities)) =$= sink)

localitySourceMany ∷ [Locality] → Sauce [Vector Point]
localitySourceMany localities sink = 
   localitiesSource (CC.filter (orList (map matchLocality localities)) =$= sink)

localitySource ∷ Locality → Sauce [Vector Point]
localitySource locality sink = 
   localitiesSource (CC.filter (matchLocality locality) =$= sink)

localitySourceLike ∷ Locality → Sauce [Vector Point]
localitySourceLike locality sink = 
   localitiesSource (CC.filter (likeLocality locality) =$= sink)

multiSources' ∷ (FilePaths → Yielder) → Sauce [Vector Point]
multiSources' yielder sink = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   lift $ multiSources (yielder fps) (Just bbox) sink

type Drawer = [Vector Point] → SettingsT Draw ()

fillCoordinates ∷ PenGetter → Drawer
fillCoordinates pen points = do
  applySettings
  fillPoints2 pen points

outlineCoordinates ∷ PenGetter → Drawer
outlineCoordinates penGetter points = do
  applySettings
  outlineCoordinates2 penGetter points

outlineCoordinates2 ∷ PenGetter → Drawer
outlineCoordinates2 penGetter points = do
  pen ← asks penGetter
  mapM_ (drawPoints pen) points

--xformify1 ∷ _ → SettingsT Pdf.PDF XForm
--xformify1 = makeXForm1 . lift

xformify ∷ SettingsT Draw () → SettingsT Pdf.PDF XForm
xformify drawing = makeXForm1 ⇇ liftT drawing

matchLocality ∷ Locality → Shape → Bool
matchLocality m = matchTextDbfField m localityColumnName

likeLocality ∷ Locality → Shape → Bool
likeLocality m = likeTextDbfField m localityColumnName


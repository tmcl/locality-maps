module BaseMap (mapUrbanAreas, mapReserves, mapCoast, getLands, operate)
where

import Unicode
import Algebra.Clipper
import Utils
import qualified Graphics.PDF as Pdf
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import UpdatedMapper
import Data.Text (Text)
import FindLocalities (toClipperPolygon, fromClipperPolygon)
import Control.Monad.Trans.Class
import Data.Vector (Vector)

import Geometry.Shapefile.Conduit 

import Settings
import Types


-- todo: harmonise these defns with the simpler EachMap ones.

matchUrbanAreaType ∷ Text → Shape → Bool
matchUrbanAreaType = (`matchTextDbfField` (== "SOS_NAME11"))

matchFeatCode ∷ Text → Shape → Bool
matchFeatCode = (`matchTextDbfField` 
   (\l → l ≡ "FEAT_CODE" ∨ l ≡ "HRPcode" ∨ l ≡ "A1C_2013" ))

mapCoast ∷ Maybe State → SettingsT IO (Pdf.PDF XForm)
mapCoast state = drawCoast state >>= (liftT . makeXForm1)

mapUrbanAreas ∷ SettingsT IO (Pdf.PDF XForm)
mapUrbanAreas = (liftT . makeXForm1) =<< drawUrbanAreas

mapReserves ∷ SettingsT IO (Pdf.PDF XForm)
mapReserves = (liftT . makeXForm1) =<< drawReserves

drawUrbanAreas ∷ SettingsT IO (Pdf.Draw ())
drawUrbanAreas = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   (b1, o1, m1) ← lift $ getUrbanAreas fps bbox
   liftT $ mapUrbanAreas3 b1 o1 m1 

drawReserves ∷ SettingsT IO (Pdf.Draw ())
drawReserves = liftT . mapReserves2 ⇇ getReserves

getUrbanAreas ∷ FilePaths → RecBBox → IO ([Vector Point], [Vector Point], [Vector Point])
getUrbanAreas fps bbox = do
  (bLoc', othUrban', majUrban') <- shapeSource (urbanAreas fps) (Just bbox)
    (CC.foldl (\(a, b, c) shape →
      if matchUrbanAreaType "Bounded Locality" shape then (shape:a, b, c) else
      if matchUrbanAreaType "Other Urban" shape then (a, shape:b, c) else
      if matchUrbanAreaType "Major Urban" shape then (a, b, shape:c) else
      (a, b, c)) ([], [], []))
  bLoc <- CC.yieldMany bLoc'
    =$= eachPolygon 
    $$ CC.sinkList
  othUrban <- CC.yieldMany othUrban'
    =$= eachPolygon
    $$ CC.sinkList
  majUrban <- CC.yieldMany majUrban'
    =$= eachPolygon
    $$ CC.sinkList
  return (concat bLoc, concat othUrban, concat majUrban)

mapReserves2 ∷ [Vector Point] → SettingsT Pdf.Draw ()
mapReserves2 reservePoints = do
   applySettings
   fillPoints2 reservesPen reservePoints

mapUrbanAreas3 ∷ [Vector Point] → [Vector Point] → [Vector Point]
                 → SettingsT Pdf.Draw ()
mapUrbanAreas3 bLoc othUrban majUrban = do
   applySettings
   fillPoints2 boundedLocality bLoc
   fillPoints2 otherUrban othUrban
   fillPoints2 majorUrban majUrban

mapCoast3 ∷ [Vector Point]  -- sea
          → [Vector Point]  -- local lands
          → [Vector Point]  -- foreign lands
          → SettingsT Pdf.Draw ()
mapCoast3 sea localLands foreignLands = do
   applySettings 
   fillPoints2 water sea
   fillPoints2 subjectSiblingsAreas localLands
   fillPoints2 land foreignLands
   pen ← asks riverPen
   mapM_ (drawPoints pen) sea

getReserves ∷ SettingsT IO [Vector Point]
getReserves = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   let getShapes = eachPolygon 
                   =$= CC.concat
                   =$= CC.sinkList
   lands ← getLands
   reservePoints ← lift $ shapeSource (reserves fps) (Just bbox) getShapes
   return $ operate (∩) lands reservePoints
   
getLocalLands ∷ Maybe State → SettingsT IO [Vector Point]
getLocalLands state = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   let filter = maybe (const True) matchState state
   lift $ multiSources 
            [(auStates fps, filter)] 
            (Just bbox) 
            getLandShapes

getLandShapes = CC.filter (not . matchFeatCode "sea")
                =$= eachPolygon 
                =$= CC.concat
                =$= CC.sinkList
   
getLands ∷ ReaderT Settings IO [Vector Point]
getLands = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   lift $ multiSources (states fps) (Just bbox) getLandShapes
   
getForeignLands ∷ Maybe State 
                → SettingsT IO [Vector Point]
getForeignLands state = do
   bbox ← asks boundingBox
   fps ← asks filePaths
   let otherStates = provinces fps ⧺ [(auStates fps, selector)]
       selector = maybe (const False) (\a b → not (matchState a b)) state
   lift $ multiSources otherStates (Just bbox) getLandShapes

drawCoast ∷ Maybe State  → SettingsT IO (Pdf.Draw ())
drawCoast state = do
   bbox ← asks boundingBox
   localLands ← getLocalLands state
   foreignLands ← getForeignLands state
   lands ← getLands
   let bboxPolygons = [bboxToPolygon bbox]
       sea = operate (∖)  bboxPolygons lands
   liftT $ mapCoast3 sea localLands foreignLands

operate ∷ (Polygons → Polygons → Polygons) → [Vector Point] → [Vector Point] → [Vector Point]
operate operation subject object = map fromClipperPolygon resultPolygons
   where
       Polygons resultPolygons = subjectPolygons `operation` objectPolygons
       objectPolygons = Polygons $ map toClipperPolygon object
       subjectPolygons = Polygons $ map toClipperPolygon subject

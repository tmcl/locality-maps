module Shapefile2
where

import Geometry.Shapefile.MergeShpDbf
import Geometry.Shapefile.Types
import qualified Data.Text as T

type DbfRecordPredicate = (T.Text, String) -> Bool

type Point = (Double, Double)

catBoundingBoxes :: [Maybe RecBBox] -> Maybe RecBBox
catBoundingBoxes boxes = foldr bigBoundingBox Nothing boxes

bigBoundingBox :: Maybe RecBBox -> Maybe RecBBox -> Maybe RecBBox
bigBoundingBox Nothing a = a
bigBoundingBox a Nothing = a
bigBoundingBox (Just (RecBBox a1 b1 c1 d1)) (Just (RecBBox a2 b2 c2 d2))
    = Just $ RecBBox (min a1 a2) (max b1 b2) (min c1 c2) (max d1 d2)

boundingBox :: ShpRec -> Maybe RecBBox
boundingBox (ShpRec _ _ Nothing _ _) = Nothing
boundingBox (ShpRec _ _ (Just point) _ _) = boundingBox2 point

getPoints :: ShpRec -> [[Point]]
getPoints (ShpRec _ _ (Just f) _ _) = getPoints2 f
getPoints _ = []

getPoints2 :: RecContents -> [[Point]]
getPoints2 (RecPolygon _ _ _ _ p) = p
getPoints2 _ = []

boundingBox2 :: RecContents -> Maybe RecBBox
boundingBox2 RecNull = Nothing
boundingBox2 (RecPoint _) = Nothing
boundingBox2 (RecPointM  _ _) = Nothing
boundingBox2 (RecPointZ _ _ _) = Nothing
boundingBox2 (RecMultiPoint a _ _) = Just a
boundingBox2 (RecMultiPointM a _ _ _ _) = Just a
boundingBox2 (RecMultiPointZ a _ _ _ _ _ _) = Just a
boundingBox2 (RecPolyLine a _ _ _ _) = Just a
boundingBox2 (RecPolyLineM a _ _ _ _ _ _) = Just a
boundingBox2 (RecPolyLineZ a _ _ _ _ _ _ _ _) = Just a
boundingBox2 (RecPolygon a _ _ _ _) = Just a
boundingBox2 (RecPolygonM a _ _ _ _ _ _) = Just a
boundingBox2 (RecPolygonZ a _ _ _ _ _ _ _ _) = Just a

shpRecByField :: DbfRecordPredicate -> ShpData -> [ShpRec]
shpRecByField predicate shpData = shpRecByField2 predicate (dbfFieldDescs shpData) (shpRecs shpData)

shpRecByField2 :: DbfRecordPredicate -> Maybe [DbfFieldDesc] -> [ShpRec] -> [ShpRec]
shpRecByField2 _ Nothing _ = []
shpRecByField2 pred (Just descs) recs = shpRecByField3 pred descs recs

shpRecByField3 :: DbfRecordPredicate -> [DbfFieldDesc] -> [ShpRec] -> [ShpRec]
shpRecByField3 pred descs recs = filter (shpRecMatches pred descs) recs

shpRecMatches :: DbfRecordPredicate -> [DbfFieldDesc] -> ShpRec -> Bool
shpRecMatches pred descs rec = dbfFieldsMatch pred descs (shpRecLabel rec)

dbfFieldsMatch :: DbfRecordPredicate -> [DbfFieldDesc] -> Maybe [DbfRecord] -> Bool
dbfFieldsMatch _ _ Nothing = False
dbfFieldsMatch pred descs (Just recs) = dbfFieldsMatch2 pred (map (T.strip . T.pack . fieldName) descs) recs

dbfFieldsMatch2 :: DbfRecordPredicate -> [T.Text] -> [DbfRecord] -> Bool
dbfFieldsMatch2 predicate descs recs = dbfFieldsMatch3 predicate (zip descs recs)

dbfFieldsMatch3 :: DbfRecordPredicate -> [(T.Text, DbfRecord)] -> Bool
dbfFieldsMatch3 predicate dbfData = any (dbfFieldMatches predicate) dbfData

dbfFieldMatches :: DbfRecordPredicate -> (T.Text, DbfRecord) -> Bool
dbfFieldMatches predicate (field, rec) = predicate (field, (readDbfRecord rec))

readDbfRecord :: DbfRecord -> String
readDbfRecord (DbfString s) = s
readDbfRecord (DbfNum s) = s
readDbfRecord (DbfBS s) = "DbfBS " ++ (show s)

module Shapefile2
where

import Geometry.Shapefile.Types
import qualified Data.Text as T

type DbfRecordPredicate = (T.Text, String) -> Bool

type Point = (Double, Double)

catBoundingBoxes :: [Maybe RecBBox] -> Maybe RecBBox
catBoundingBoxes boxes = fmap embiggenBoundingBox $ foldr bigBoundingBox Nothing boxes

embiggenBoundingBox (RecBBox a b c d) = RecBBox (a-width) (b+width) (c-height) (d+height)
   where 
      width = (b-a) / 10
      height = (d-c) / 10

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

type RecordPickerInput = ((T.Text, T.Text), (T.Text, T.Text), (T.Text, T.Text))
type RecordPickerOutput = ([ShpRec], [ShpRec], [ShpRec])

pickRecords :: RecordPickerInput -> ShpData -> RecordPickerOutput
pickRecords conditions shpData = pickRecords2 conditions (dbfFieldDescs shpData) (shpRecs shpData)

pickRecords2 :: RecordPickerInput -> Maybe [DbfFieldDesc] -> [ShpRec] -> RecordPickerOutput
pickRecords2 _ Nothing _ = ([], [], [])
pickRecords2 conditions (Just dbf) recs = foldr (matchToCond conditions (dbfFieldNames dbf)) ([], [], []) recs

matchToCond :: RecordPickerInput -> [T.Text] -> ShpRec -> RecordPickerOutput -> RecordPickerOutput
matchToCond cond fields rec acc = matchToDbfCond cond fields (shpRecLabel rec) acc rec

matchToDbfCond :: RecordPickerInput -> [T.Text] -> Maybe [DbfRecord] -> RecordPickerOutput -> ShpRec -> RecordPickerOutput
matchToDbfCond _ _ Nothing acc _ = acc
matchToDbfCond conds fields (Just recs) accs candidate = matchToDbfCondZipped conds (zip fields recs) accs candidate

matchToDbfCondZipped :: RecordPickerInput -> [(T.Text, DbfRecord)] -> RecordPickerOutput -> ShpRec -> RecordPickerOutput
matchToDbfCondZipped (cond1, cond2, cond3) fields (acc1, acc2, acc3) candidate = (extra1 ++ acc1, extra2 ++ acc2, extra3 ++ acc3)
    where
        matches (conditionField, conditionValue) (candidateName, candidateValue) = conditionField == candidateName && conditionValue == (T.strip . T.pack . readDbfRecord) candidateValue
        match condition = any (matches condition)
        extra1
            | match cond1 fields = [candidate]
            | otherwise = []
        extra2
            | match cond2 fields = [candidate]
            | otherwise = []
        extra3
            | match cond3 fields = [candidate]
            | otherwise = []


dbfFieldNames :: [DbfFieldDesc] -> [T.Text]
dbfFieldNames = map (T.strip . T.pack . fieldName)

shpRecByField :: DbfRecordPredicate -> ShpData -> [ShpRec]
shpRecByField predicate shpData = shpRecByField2 predicate (dbfFieldDescs shpData) (shpRecs shpData)

shpRecByField2 :: DbfRecordPredicate -> Maybe [DbfFieldDesc] -> [ShpRec] -> [ShpRec]
shpRecByField2 _ Nothing _ = []
shpRecByField2 predicate (Just descs) recs = shpRecByField3 predicate descs recs

shpRecByField3 :: DbfRecordPredicate -> [DbfFieldDesc] -> [ShpRec] -> [ShpRec]
shpRecByField3 predicate descs recs = filter (shpRecMatches predicate descs) recs

shpRecMatches :: DbfRecordPredicate -> [DbfFieldDesc] -> ShpRec -> Bool
shpRecMatches predicate descs rec = dbfFieldsMatch predicate descs (shpRecLabel rec)

dbfFieldsMatch :: DbfRecordPredicate -> [DbfFieldDesc] -> Maybe [DbfRecord] -> Bool
dbfFieldsMatch _ _ Nothing = False
dbfFieldsMatch predicate descs (Just recs) = dbfFieldsMatch2 predicate (dbfFieldNames descs) recs

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

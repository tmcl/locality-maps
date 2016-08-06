{-# LANGUAGE OverloadedStrings #-}
module Shapefile
where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Database.Shapefile
import System.Posix
import System.FilePath.Posix
import Logic

getFileSize :: FilePath -> IO FileOffset
getFileSize fp = fileSize <$> getFileStatus fp

getShxFileSize :: FilePath -> IO FileOffset
getShxFileSize shp = getFileSize (shp -<.> "shx")

shxNumRecords :: FileOffset -> Integer
shxNumRecords size = ((toInteger size) - 100) `div` 8

getShxNumRecords :: FilePath -> IO Integer
getShxNumRecords fp = shxNumRecords <$> getShxFileSize fp

getGazettals :: String -> String -> String -> FilePath -> IO [GenericGazettal]
getGazettals lgaField typeField placeField fp = do
    numRecords <- getShxNumRecords fp
    print numRecords
    shpHandle <- openShp fp True
    fields <- shpDbfFields shpHandle
    let myFields = pickFields lgaField typeField placeField fields
    records <- mapM (getShpRecord shpHandle) [0..(fromInteger $ numRecords - 1)]
    results2 <- maybe (return []) (\ff -> mapM (recordToGazettal ff) (mapMaybe snd records)) myFields
    closeShp shpHandle
    return results2

filterLOCBs :: [GenericGazettal] -> [GenericGazettal]
filterLOCBs = filter (\gg -> ggType gg == "LOCB" || ggType gg == "SUB" )

fieldsByName :: String -> [DbfFieldHandle] -> [DbfFieldHandle]
fieldsByName name = filter (\f -> fieldName f == name)

fieldByName :: String -> [DbfFieldHandle] -> Maybe DbfFieldHandle
fieldByName name fields = safeHead $ fieldsByName name fields

recordToGazettal :: DbfFieldGroup -> DbfRecHandle -> IO GenericGazettal
recordToGazettal fg rec = do
    municipalityValue <- readDbfField rec (dbfMunicipality fg)
    typeValue <- readDbfField rec (dbfType fg)
    nameValue <- readDbfField rec (dbfName fg)
    return $ GenericGazettal {
        ggMunicipality = convert municipalityValue,
        ggType = convert typeValue,
        ggName = convert nameValue
    }
    where convert = T.strip . TL.toStrict . decodeUtf8

data GenericGazettal = GenericGazettal {
    ggMunicipality :: T.Text,
    ggType :: T.Text,
    ggName :: T.Text
}
    deriving (Show, Eq)

data DbfFieldGroup = DbfFieldGroup {
    dbfMunicipality :: DbfFieldHandle,
    dbfType :: DbfFieldHandle,
    dbfName :: DbfFieldHandle
}
pickFields :: String -> String -> String -> [DbfFieldHandle] -> Maybe DbfFieldGroup
pickFields fieldNameLGA fieldNameType fieldNameName fields = do
    municipalityField <- fieldByName fieldNameLGA fields
    typeField <- fieldByName fieldNameType fields
    nameField <- fieldByName fieldNameName fields
    return $ DbfFieldGroup {
        dbfMunicipality = municipalityField,
        dbfType = typeField,
        dbfName = nameField
    }

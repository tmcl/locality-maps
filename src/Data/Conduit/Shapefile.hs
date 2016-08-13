module Data.Conduit.Shapefile
where

import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Conduit.Combinators as C

import Data.Attoparsec.ByteString


-- parseDbfFile fp = C.sourceFile fp $= parseDbf

-- parseDbf = 

data DbfHeader = DbfHeader {
   numRecords :: !Word32,
   headerLength :: !Word16,
   recordLength :: !Word16
}

parseDbfHeader :: Parser DbfHeader
parseDbfHeader = do
   take 4
   numRecs <- anyWord32le
   headerLen <- anyWord16le
   recordLen <- anyWord16le
   take 20 -- transaction completeness, encryption, multiuser dos, mdx
   return DbfHeader {
      numRecords = numRecs, 
      headerLength = headerLen, 
      recordLength = recordLen
   }

data DbfFieldType = DbfFieldTypeC |DbfFieldTypeD |DbfFieldTypeF |DbfFieldTypeL |DbfFieldTypeM | DbfFieldTypeN

data DbfFieldHeader = DbfFieldHeader {
                          fieldName:: !T.Text,
                          fieldType:: DbfFieldType,
                          fieldLength:: !Word8
                      }

data DbfField = DbfString T.Text | DbfOther BS.ByteString

parseDbfRecord dbfFieldHeader = do
   field <- take (fieldLength dbfFieldHeader)
   case fieldType dbfFieldHeader of
      DbfFieldTypeC -> return $ DbfString field
      otherwise -> return $ DbfOther field

parseFieldHeaders remaining acc 
   | remaining < 32 = return acc
   | otherwise = do
      header <- parseFieldHeader
      parseFieldHeaders (n-32) (header:acc)

parseFieldHeader = do
    name <- take 11
    fieldType <- take 1
    take 2
    len <- anyWord8
    take 15
    return DbfFieldHeader { 
      fieldName = name, 
      fieldType = case fieldType of
          "C" -> DbfFieldTypeC 
          "D" -> DbfFieldTypeD
          "F" -> DbfFieldTypeF
          "L" -> DbfFieldTypeL
          "M" -> DbfFieldTypeM
          "N" -> DbfFieldTypeN
          otherwise -> DbfFieldTypeN,
      fieldLength = len }
    

getDbfHeader :: Get DbfHeader
getDbfHeader = do
   _ <- skip 4 --what is the header? assert it.
   numRecs <- getWord32le
   firstRecPos <- getWord16le 
   _ <- skip 22 --record length, table flags, code page mark (consider!)
   return $! DbfHeader numRecs firstRecPos
      

data DbfFieldValue = DbfText T.Text | DbfRaw BS.ByteString

data DbfRecord = DbfRecord [(T.Text, DbfFieldValue)]

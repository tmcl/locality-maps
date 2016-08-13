{-# LANGUAGE OverloadedStrings #-}
module Main
where

import ClassyPrelude (traceM)
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Environment
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Text as T
import Data.Attoparsec.Binary
import Data.Time.Calendar
import Text.Show.Pretty
import Data.Text.Encoding
import System.IO.Unsafe

import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import GHC.Word

main = getArgs >>= mapM_ (\fp -> runResourceT $ CB.sourceFile fp =$= dbfConduit $$ CC.mapM_ (liftIO . putStrLn . ppShow))

dbfConduit :: ConduitM ByteString (CA.PositionRange, DbfRow) (ResourceT IO) ()
dbfConduit = do
   hdr <- CA.sinkParser parseDbfHeader
   traceM $ (ppShow hdr)
   columns <- CA.sinkParser $ parseDbfColumns hdr
   CA.conduitParser $ parseDbfRow columns

parseDbf = do
   hdr <- parseDbfHeader
   columns <- parseDbfColumns hdr
   rows <- parseDbfRows hdr columns
   let r = Dbf hdr columns rows
   return r 

data Dbf = Dbf {
   dbfHeader :: DbfHeader,
   dbfColumns :: [DbfColumn],
   dbfData :: [DbfRow]
}
   deriving (Show, Read, Eq)

data DbfHeader = DbfHeader { 
   dbfRecordsLength :: Int,  
   dbfFirstRecordPosition :: Int
}
   deriving (Show, Read, Eq)

parseDbfHeader :: Parser DbfHeader
parseDbfHeader = do
   f <- P.take 4
   traceM (ppShow f)
   length <- anyWord32le
   traceM (ppShow length)
   firstPosition <- anyWord16le
   traceM (ppShow firstPosition)
   z <- P.take 22
   traceM (ppShow z)
   return $ DbfHeader (fromIntegral length) (fromIntegral firstPosition)

data DbfColumn = DbfColumn {
   dbfcName :: T.Text,
   dbfcType :: DbfColumnType,
   dbfcLength :: Int
}
   deriving (Show, Read, Eq)

readColumnType :: ByteString -> DbfColumnType
readColumnType "C" = DbfColumnTypeCharacter 
readColumnType "F" = DbfColumnTypeFloat
readColumnType "D" = DbfColumnTypeDate 
readColumnType "N" = DbfColumnTypeNumeric 
readColumnType "M" = DbfColumnTypeMemo 
readColumnType "L" = DbfColumnTypeLogical 

parseDbfColumn :: Parser DbfColumn
parseDbfColumn = do
   name <- parseUtf8 11
   columnType <- readColumnType <$> P.take 1
   _ <- P.take 4
   length <- anyWord8
   _ <- P.take 15
   return $ DbfColumn name columnType (fromIntegral length)

parseDbfColumns :: DbfHeader -> Parser [DbfColumn]
parseDbfColumns header = do
   r <- count ((fromIntegral . dbfFirstRecordPosition) header `div` 32 - 1) parseDbfColumn
   _ <- P.take 1
   return r

parseDbfRows header columns = count (fromIntegral . dbfRecordsLength $ header) (parseDbfRow columns)

parseDbfRow columns = do
   _ <- anyWord8
   fields <- mapM parseDbfField columns
   return $ DbfRow fields

data DbfColumnType
   = DbfColumnTypeCharacter
   | DbfColumnTypeDate
   | DbfColumnTypeFloat
   | DbfColumnTypeLogical
   | DbfColumnTypeMemo
   | DbfColumnTypeNumeric
   deriving (Eq, Show, Read)

data DbfField 
   = DbfFieldCharacter T.Text 
   | DbfFieldDate (Maybe Day)  
   | DbfFieldFloat ByteString 
   | DbfFieldLogical Bool
   | DbfFieldMemo ByteString
   | DbfFieldNumeric ByteString
   deriving (Eq, Show, Read)

data DbfRow = DbfRow [(DbfColumn, DbfField)]
   deriving (Eq, Show, Read)

parseDbfField column = fmap (\l -> (column, l)) (parseField (fromIntegral . dbfcLength $ column))
   where 
      parseField len = case dbfcType column of
         DbfColumnTypeCharacter -> DbfFieldCharacter <$> parseUtf8 len
         DbfColumnTypeDate -> DbfFieldDate <$> parseDay len
         DbfColumnTypeFloat -> DbfFieldFloat <$> P.take len
         DbfColumnTypeLogical -> DbfFieldLogical <$> parseBool len
         DbfColumnTypeMemo -> DbfFieldMemo <$> P.take len
         DbfColumnTypeNumeric -> DbfFieldNumeric <$> P.take len

readBs = read . T.unpack . decodeUtf8

parseUtf8 len = T.strip . decodeUtf8 . (BS.filter (/= 0)) <$> P.take len

parseDay 8 = do
   year <- P.take 4
   month <- P.take 2
   day <- P.take 2
   if (month == "00" || day == "00") then
      return $ Nothing
   else
      return $ Just $ fromGregorian (readBs year) (fromInteger $ readBs month) (fromInteger $ readBs day)

parseBool length = do
   logical <- anyWord8
   _ <- P.take (length - 1)
   return $ inClass "YyTt" logical

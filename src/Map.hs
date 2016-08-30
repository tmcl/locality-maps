{-# LANGUAGE RankNTypes #-}

module Map where

import Data.Complex
import Data.List
import Geometry.Shapefile.Types
import System.Process
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import Control.Monad.IO.Class
import qualified Data.Vector as V
import Settings

class GMTOption a where
    tshow :: a -> String

instance GMTOption T.Text where
    tshow = T.unpack

instance GMTOption RecBBox where
    tshow b = concat ["-R",
                      show $ recXMin b,
                      "/",
                      show $ recXMax b,
                      "/",
                      show $ recYMin b,
                      "/",
                      show $ recYMax b]

instance GMTOption Orientation where
    tshow Portrait = "-P"
    tshow Landscape = ""


instance GMTOption Pen where
    tshow (Pen (Solid color) _) = "-G" ++ colorToText color
    tshow (Pen (Water color)  _)= "-S" ++ colorToText color
    tshow (Pen (Outline (Points pt) color)  _)= concat ["-W", show pt, "p,", colorToText color]


transparency :: Pen -> Int
transparency (Pen _ t) = t

colorToText :: Color -> String
colorToText (Color red green blue) = intercalate "/" (map show [red, green, blue])

pointToPstPoint :: Point -> ByteString
pointToPstPoint (easting :+ westing) = B8.pack $ show easting ++ " " ++ show westing

polygonToPstPoints :: [Point] -> ByteString
polygonToPstPoints pts = BS.intercalate "\n" (map pointToPstPoint pts)

polygonsToPstPoints :: [[Point]] -> ByteString
polygonsToPstPoints pts = f
   where f = BS.intercalate "\n>\n" ("":(polygonToPstPoints <$> pts))

initialiseMap :: Settings -> IO ByteString
initialiseMap settings
    = inproc "gmt" ["psxy", "-A", "-T", setting projection, setting boundingBox, "-K", setting orientation] BS.empty
    where
        setting getter = tshow . getter $ settings

mapTitle :: Settings -> T.Text -> IO ByteString
mapTitle settings title
    = inproc "gmt" ["pstext", setting projection, setting boundingBox, "-F+cTL+f30p,Palatino-Roman", "-O", "-K", setting orientation] (encodeUtf8 title)
    where
        setting getter = tshow . getter $ settings

closeMap :: Settings -> IO ByteString
closeMap settings
    = inproc "gmt" ["psxy", "-A", "-T", setting projection, setting boundingBox, "-O", setting orientation] BS.empty
    where
        setting getter = tshow . getter $ settings

mapPointsParams :: Settings -> Pen -> [String]
mapPointsParams settings pen = params
    where
        setting getter = tshow . getter $ settings
        params = [
            "psxy",
            setting projection,
            setting boundingBox,
            tshow pen,
            "-K",
            "-O",
            setting orientation,
            "-t" ++ show (transparency pen)
            ]

mapPoints3 :: Settings -> Pen -> Conduit [[Point]] IO ByteString
mapPoints3 settings pen = CC.conduitVector 200 =$= awaitForever go
   where
      go points = liftIO (mapPoints1 settings (pen, concat $ V.toList points)) >>= yield

mapPoints1 :: Settings -> (Pen, [[Point]]) -> IO ByteString
mapPoints1 settings (pen, points) = inproc "gmt" (mapPointsParams settings pen) (polygonsToPstPoints points)

inproc :: String -> [String] -> ByteString -> IO ByteString
inproc a b c = B8.pack <$> readProcess a b (B8.unpack c)

mapCoast :: Settings -> IO ByteString
mapCoast settings = inproc "gmt" [
    "pscoast",
    setting projection,
    setting boundingBox,
    setting land,
    "-A200",
    setting water,
    "-N2/2.4p,120/120/120",
    "-Df",
    "-Y1.8c",
    "-X2.2c",
    setting orientation,
    "-K"
    ] BS.empty
    where
        setting getter = tshow . getter $ settings




{-# LANGUAGE RankNTypes #-}

module Map
  (closeMap
  ,mapCoast
  ,initialiseMap
  ,mapTitle
  ,mapPoints3
  ,Settings(..)
  ,Color(..)
  ,Pen(..), colorTheMunicipality_
    ,Width(..), withDefaultSettings)
  where

import Data.List
import Geometry.Shapefile.Types
import qualified Shapefile2 as S
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

data Orientation = Portrait | Landscape
instance GMTOption Orientation where
    tshow Portrait = "-P"
    tshow Landscape = ""

data Settings = Settings {
    orientation :: Orientation,
    projection :: T.Text,
    land :: Pen,
    water :: Pen,
    riverPen :: Pen,
    boundingBox :: RecBBox,
    majorUrban :: Pen,
    otherUrban :: Pen,
    boundedLocality :: Pen,
    broadArea :: Pen,
    broadLines :: Pen,
    narrowArea :: Pen,
    narrowLines :: Pen
}

colorTheMunicipality_ :: Pen
colorTheMunicipality_ = Pen (Solid (Color 254 254 233)) 60

withDefaultSettings :: RecBBox  -> Settings
withDefaultSettings bbox = Settings {
    orientation = Portrait,
    projection = if isWide then "-JM60c" else "-JM100c+",
    land = Pen (Solid (Color 245 245 245)) 0,
    water = Pen (Solid (Color 198 236 255)) 0,
    riverPen = Pen (Outline (Points 1) (Color 9 120 171)) 0,
    boundingBox = bbox,
    majorUrban = Pen (Solid (Color 100 100 100)) 60,
    otherUrban = Pen (Solid (Color 125 125 125)) 60,
    boundedLocality = Pen (Solid (Color 150 150 150)) 60,
    narrowArea = Pen (Solid (Color 100 0 0)) 60,
    narrowLines = Pen (Outline (Points 0.4) (Color 100 0 0)) 0,
    broadLines = Pen (Outline (Points 2.0) (Color 150 150 150)) 0,
    broadArea = colorTheMunicipality_
}
   where isWide = recXMax bbox - recXMin bbox > recYMax bbox - recYMin bbox

newtype Width = Points Double

data Color = Color { colorRed :: Int, colorBlue :: Int, colorGreen :: Int }

data Pen = Pen WritingStyle Int

data WritingStyle
    = Solid Color
    | Water Color
    | Outline Width Color
instance GMTOption Pen where
    tshow (Pen (Solid color) _) = "-G" ++ colorToText color
    tshow (Pen (Water color)  _)= "-S" ++ colorToText color
    tshow (Pen (Outline (Points pt) color)  _)= concat ["-W", show pt, "p,", colorToText color]

transparency :: Pen -> Int
transparency (Pen _ t) = t

colorToText :: Color -> String
colorToText (Color red green blue) = intercalate "/" (map show [red, green, blue])

pointToPstPoint :: S.Point -> ByteString
pointToPstPoint (easting, westing) = B8.pack $ show easting ++ " " ++ show westing

polygonToPstPoints :: [S.Point] -> ByteString
polygonToPstPoints pts = BS.intercalate "\n" (map pointToPstPoint pts)

polygonsToPstPoints :: [[S.Point]] -> ByteString
polygonsToPstPoints pts = f
   where f = BS.intercalate "\n>\n" ("":(polygonToPstPoints <$> pts))

polygonsToPstPointsConduit :: Conduit [S.Point] IO ByteString
polygonsToPstPointsConduit = CC.map polygonToPstPoints =$= CC.intersperse ">\n"

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

mapPoints3 :: Settings -> Pen -> Conduit [[S.Point]] IO ByteString
mapPoints3 settings pen = CC.conduitVector 200 =$= awaitForever go
   where
      go points = liftIO (mapPoints1 settings (pen, concat $ V.toList points)) >>= yield

mapPoints1 :: Settings -> (Pen, [[S.Point]]) -> IO ByteString
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

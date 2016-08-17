{-# LANGUAGE RankNTypes #-}
module Map
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
    boundingBox :: RecBBox
}

withDefaultSettings :: RecBBox  -> Settings
withDefaultSettings bbox = Settings {
    orientation = Portrait,
    projection = "-JM144/25/15.8c",
    -- land = Solid (Color 224 224 224),
    land = Solid (Color 245 245 245),
    water = Water (Color 198 236 255),
    boundingBox = bbox
}

newtype Width = Points Double

data Color = Color { colorRed :: Int, colorBlue :: Int, colorGreen :: Int }

data Pen
    = Solid Color
    | Water Color
    | Outline Width Color
instance GMTOption Pen where
    tshow (Solid color) = "-G" ++ colorToText color
    tshow (Water color) = "-S" ++ colorToText color
    tshow (Outline (Points pt) color) = concat ["-W", show pt, "p,", colorToText color]

colorToText :: Color -> String
colorToText (Color red green blue) = intercalate "/" (map show [red, green, blue])

-- settingsToArgs :: GMTSettings -> [T.Text]
-- settingsToArgs settings = [orientation settings, projection settings, penToText . pen $ settings]

-- withPen :: GMTPen -> GMTSettings
-- withPen newColor = GMTSettings { orientation = "-P", projection = "-JM144/37/15c", pen = newColor }

pointToPstPoint :: S.Point -> ByteString
pointToPstPoint (easting, westing) = B8.pack $ show easting ++ " " ++ show westing

polygonToPstPoints :: [S.Point] -> ByteString
polygonToPstPoints pts = BS.intercalate "\n" (map pointToPstPoint pts)

polygonsToPstPoints :: [[S.Point]] -> ByteString
polygonsToPstPoints pts = f
   where f = BS.intercalate "\n>\n" ("":(polygonToPstPoints <$> pts))

polygonsToPstPointsConduit :: Conduit [S.Point] IO ByteString
polygonsToPstPointsConduit = CC.map polygonToPstPoints =$= CC.intersperse ">\n"

--drawMap :: Settings -> [(Pen, Int, [[S.Point]])] -> IO T.Text
--drawMap settings points = do
    --initialiseMap settings <|> mapCoast settings
        -- <|> (foldr (<|>) empty (map (mapPoints settings) points))
        -- <|> closeMap settings

initialiseMap :: Settings -> IO ByteString
initialiseMap settings
    = inproc "gmt" ["psxy", "-A", "-T", setting projection, setting boundingBox, "-K", setting orientation] BS.empty
    where
        setting getter = tshow . getter $ settings

mapTitle :: Settings -> T.Text -> IO ByteString
mapTitle settings title
    = inproc "gmt" ["pstext", setting projection, setting boundingBox, "-F+cTL", "-O", "-K", setting orientation] (encodeUtf8 title)
    where
        setting getter = tshow . getter $ settings

closeMap :: Settings -> IO ByteString
closeMap settings
    = inproc "gmt" ["psxy", "-A", "-T", setting projection, setting boundingBox, "-O", setting orientation] BS.empty
    where
        setting getter = tshow . getter $ settings

mapPointsParams :: (Show s, GMTOption o) => Settings -> (o, s) -> [String]
mapPointsParams settings (pen, transparency) = params
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
            "-t" ++ show transparency
            ]

mapPoints3 :: Settings -> Pen -> Int -> Conduit [[S.Point]] IO ByteString
mapPoints3 settings pen transparency = CC.conduitVector 200 =$= awaitForever go
   where
      go points = liftIO (mapPoints1 settings (pen, transparency, concat $ V.toList points)) >>= yield
--mapPoints4 settings pen transparency = do
   --blah <- await
   --traceM (show blah)
   --case blah of
      --Nothing -> return ()
      --Just points -> (liftIO $ mapPoints1 settings (pen, transparency, [points])) >>= yield

-- mapPoints2 ::Settings -> Pen -> Int -> Producer IO [S.Point] -> IO ByteString
-- mapPoints2 settings pen transparency points
--    = do
--     (a, builder, c ) <- CP.sourceProcessWithStreams 
--        (proc "gmt" (mapPointsParams settings (pen, transparency)))
--        (points =$= polygonsToPstPointsConduit)
--        CC.sinkBuilder
--        CC.sinkBuilder
--     traceM $ show a
--     traceM $ show $ toByteString builder
--     traceM $ show $ toByteString c
--     return $ toByteString builder

mapPoints1 :: Settings -> (Pen, Int, [[S.Point]]) -> IO ByteString
mapPoints1 settings (pen, transparency, points) = inproc "gmt" (mapPointsParams settings (pen, transparency)) (polygonsToPstPoints points)

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

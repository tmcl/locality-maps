module Map
where

import Geometry.Shapefile.Types
import qualified Shapefile2 as S
import System.Process
import qualified Data.Text as T

class GMTOption a where
    tshow :: a -> T.Text

instance GMTOption T.Text where
    tshow = id

instance GMTOption RecBBox where
    tshow b = T.concat $ map T.pack ["-R", show $ recXMin b, "/", show $ recXMax b, "/", show $ recYMin b, "/", show $ recYMax b]

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
    tshow (Solid color) = T.concat ["-G", colorToText color]
    tshow (Water color) = T.concat ["-S", colorToText color]
    tshow (Outline (Points pt) color) = T.concat ["-W", T.pack . show $ pt, "p,", colorToText color]

colorToText :: Color -> T.Text
colorToText (Color red green blue) = T.intercalate "/" (map (T.pack . show) [red, green, blue])

-- settingsToArgs :: GMTSettings -> [T.Text]
-- settingsToArgs settings = [orientation settings, projection settings, penToText . pen $ settings]

-- withPen :: GMTPen -> GMTSettings
-- withPen newColor = GMTSettings { orientation = "-P", projection = "-JM144/37/15c", pen = newColor }

pointToPstPoint :: S.Point -> T.Text
pointToPstPoint (easting, westing) = T.pack $ show easting ++ " " ++ show westing

polygonToPstPoints :: [S.Point] -> T.Text
polygonToPstPoints pts = T.unlines $ map pointToPstPoint pts

polygonsToPstPoints :: [[S.Point]] -> T.Text
polygonsToPstPoints pts = T.intercalate ">\n" (map polygonToPstPoints pts)

--drawMap :: Settings -> [(Pen, Int, [[S.Point]])] -> IO T.Text
--drawMap settings points = do
    --initialiseMap settings <|> mapCoast settings
        -- <|> (foldr (<|>) empty (map (mapPoints settings) points))
        -- <|> closeMap settings

initialiseMap :: Settings -> IO T.Text
initialiseMap settings
    = inproc "gmt" ["psxy", "-A", "-T", setting projection, setting boundingBox, "-K", setting orientation] T.empty
    where
        setting getter = tshow . getter $ settings

closeMap :: Settings -> IO T.Text
closeMap settings
    = inproc "gmt" ["psxy", "-A", "-T", setting projection, setting boundingBox, "-O", setting orientation] T.empty
    where
        setting getter = tshow . getter $ settings

mapPoints :: Settings -> (Pen, Int, [[S.Point]]) -> IO T.Text
mapPoints settings (pen, transparency, points) = inproc "gmt" params (polygonsToPstPoints points)
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
            T.pack $ "-t" ++ show transparency
            ]

inproc :: String -> [T.Text] -> T.Text -> IO T.Text
inproc a b c = fmap T.pack (readProcess a (fmap T.unpack b) (T.unpack c))

mapCoast :: Settings -> IO T.Text
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
    ] T.empty
    where
        setting getter = tshow . getter $ settings

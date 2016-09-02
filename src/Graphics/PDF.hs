{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.PDF(module Graphics.PDF, module Point) where

import Point
import Unicode
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.State.Strict
import ClassyPrelude (traceM)

class PDFLike a 

data PdfState = PdfState {
   drawings ∷ [DrawState],
   len ∷ ℤ
} deriving (Show)

data DrawState = DrawState {
   drawStatePolygons ∷ Vector (Vector Point)
} deriving (Show)


newtype PDF a = PDF (State PdfState a)
   deriving (Functor, Applicative, Monad, MonadState PdfState)

newtype Draw a = Draw (State DrawState a)
   deriving (Functor, Applicative, Monad, MonadState DrawState)

drawState ∷ Draw a → DrawState
drawState (Draw d) = execState d (DrawState V.empty)


data Text a = Text a
instance Functor Text where
   f `fmap` (Text a) = Text $ f a
instance Applicative Text where
   pure = Text
   (Text f) <*> (Text a) = Text $ f a
instance Monad Text where
   return = Text
   (Text a) >>= f = f a

data Rectangle = Rectangle Point Point
data PDFRect = PDFRect ℤ ℤ ℤ ℤ
data PDFReference a = PDFReference a

data PDFPage = PDFPage
instance PDFLike PDFPage

data PDFXForm = PDFXForm
instance PDFLike PDFXForm

data Matrix = Matrix Double Double Double Double Double Double
type PDFFloat = Double
newtype PDFString = PDFString T.Text

data DocInfo = DocInfo { compressed ∷ Bool, author ∷ PDFString }

runPdf ∷ FilePath → DocInfo → PDFRect → PDF () → IO ()
runPdf _ _ _ (PDF p) = do
   traceM $ show $ runState p (PdfState [] 0)
   return ()

standardDocInfo ∷ DocInfo
standardDocInfo = DocInfo { compressed = True, author = PDFString "This" }

displayText ∷ PDFString → Text ()
displayText _ = return ()

textStart ∷ ℤ → ℤ → Text ()
textStart _ _ = return ()

getHeight ∷ PDFFont → ℤ
getHeight _ = 0

r2 ∷ a → Draw a
r2 a = do
   s ← get
   traceM (show $ length $ drawStatePolygons s)
   return a

drawText ∷ Text () → Draw ()
drawText _ = r2 ()

data PDFFont = PDFFont Font ℤ
data Font = Times_Roman

setFont ∷ PDFFont → Text ()
setFont _ = return ()

leading ∷ ℤ → Text ()
leading _ = return ()

renderMode ∷ TextStyle → Text ()
renderMode _ = return ()

data TextStyle = FillText

drawXObject ∷ PDFReference PDFXForm → Draw ()
drawXObject _ = r2 ()

fill ∷ Rectangle → Draw ()
fill _ = r2 ()

fillPathEO ∷ Draw ()
fillPathEO  = r2 ()

stroke ∷ Rectangle → Draw ()
stroke _ = r2 ()

addPolygonToPath ∷ Vector Point → Draw ()
addPolygonToPath p = modify it
   where
      it (DrawState s) = DrawState $ s `V.snoc` p

setWidth ∷ Double → Draw ()
setWidth _ = r2 ()

strokeColor ∷ Color → Draw ()
strokeColor _ = r2 ()

drawWithPage ∷ PDFReference PDFPage → Draw () → PDF ()
drawWithPage _ d = fiddle d

fiddle ∷ Draw () → PDF ()
fiddle d = traceM "Bleh" >> ((>>) $! (modify $! it)) $! (traceM $! "hello")
   where
      it (PdfState _ l) = PdfState [drawState d] (l+1)

applyMatrix ∷ Matrix → Draw ()
applyMatrix _ = r2 ()

strokePath ∷ Draw ()
strokePath = r2 ()

addPage ∷ Maybe PDFRect → PDF (PDFReference PDFPage)
addPage _ = return $ PDFReference PDFPage

boxWidth ∷ a
boxWidth = undefined

boxHeight ∷ a
boxHeight = undefined

fillColor ∷ Color → Draw ()
fillColor _ = r2 ()

setFillAlpha ∷ Double → Draw ()
setFillAlpha _ = r2 ()

createPDFXForm ∷ Double → Double → Double → Double 
               → Draw () 
               → PDF (PDFReference PDFXForm)
createPDFXForm _ _ _ _ d = do
   fiddle d
   return $ PDFReference PDFXForm

toPDFString ∷ String → PDFString
toPDFString = PDFString . T.pack

data Color = Rgb Double Double Double

red ∷ Color
red = Rgb 1 0 0

green ∷ Color
green = Rgb 0 1 0

blue ∷ Color
blue = Rgb 0 0 1

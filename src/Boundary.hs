module Boundary()
where

import Data.Text
import Data.Dbase.Parser
import qualified Geometry.Shapefile.Conduit as C

data ShpRec = ShpRec
type Shape = (C.ShapeHeader, ShpRec, DbfRow)

-- todo remove me
shapeFieldByColumnNameRule ∷ (Text → Bool) → DbfRow → Maybe DbfField
shapeFieldByColumnNameRule = C.shapeFieldByColumnNameRule

matchTextDbfField ∷ Text → (Text → Bool) → Shape → Bool
matchTextDbfField = C.matchTextDbfField
matchNumericDbfField ∷ Int → (Text → Bool) → Shape → Bool
matchNumericDbfField = C.matchNumericDbfField



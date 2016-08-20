module Utils
where

import Geometry.Shapefile.Types

bigBoundingBox ∷ Maybe RecBBox → Maybe RecBBox → Maybe RecBBox
bigBoundingBox Nothing a = a
bigBoundingBox a Nothing = a
bigBoundingBox (Just (RecBBox a1 b1 c1 d1)) (Just (RecBBox a2 b2 c2 d2))
    = Just $ RecBBox (min a1 a2) (max b1 b2) (min c1 c2) (max d1 d2)

module Data.Vector(module Data.Vector, module Data.List) where

import Data.List hiding (concat)
import qualified Data.List as L
import Unicode

type Vector = []

toList ∷ Vector a → [a]
toList = const (error "toList")
fromList ∷ [a] → Vector a
fromList = id

empty ∷ [a]
empty = []

snoc ∷ [a] → a → [a]
snoc l a = l ⧺ [a]

concat ∷ [[a]] → [a]
concat = L.concat

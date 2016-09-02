
module Data.Set(module Data.Set, module Data.List) where

import Data.List
import Unicode

type Set = []

toList ∷ Set a → [a]
toList = id

fromList ∷ Ord a ⇒ [a] → Set a
fromList = foldl' (\l e → if e ∈ l then l else e:l) []

module Unicode (
   (⇇),
   (⇉),
   module Prelude.Unicode, 
   module Control.Monad.Unicode)
where

import Prelude.Unicode
import Control.Monad.Unicode hiding ((=≪),(≫=))

infixl 1 ⇉
infixr 1 ⇇

(⇇) ∷ (Monad m) ⇒ (a → m b) → m a → m b 
(⇇) = (=<<)

(⇉) ∷ (Monad m) ⇒ m a → (a → m b) → m b 
(⇉) = (>>=)

module Logic
where

type Predicate a = a -> Bool

(⋀) :: Predicate a -> Predicate a -> a -> Bool
(⋀) p q z = p z && q z

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

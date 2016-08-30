module PolygonReduce (reduce, itWorks)
where

import Prelude.Unicode
import Data.Monoid
import Data.Complex
import Data.Foldable
import Data.Vector hiding (length, foldl', all)
import Prelude (Show, show, error, (+), (>), ($), (<), (*), (^^), sqrt, abs, (-), (/), Int, Double, Bool(..))
import Control.Applicative

type Point = Complex Double

perpendicularDistance ∷ Point → Point → Point → Double
perpendicularDistance (pX :+ pY) (p1x :+ p1y) (p2x :+ p2y) = 
   if (p1x ≡ p2x) 
   then abs (pX - p1x)
   else (abs (slope * pX - pY + intercept)) / (sqrt((slope ^^ (2∷ℤ)) + 1))
   where
      slope = (p2y - p1y) / (p2x - p1x)
      intercept = p1y - (slope * p1x)

reduce ∷ Double → Vector Point → Vector Point
reduce 0 points = points
reduce ε points = if length points < 3 then points else reducedPoints
   where
      firstPoint = head points
      lastPoint = last points
      (dist, ix, _) = foldl' (maxPerpendicularDistance firstPoint lastPoint) (0, 0, 0) points
      pre = take (1+ix) points
      post = drop ix points
      reducedPoints = if dist > ε 
         then mappend r1 (drop 1 r2)
         else pure firstPoint <> pure lastPoint
      r1 = reduce ε pre
      r2 = reduce ε post

maxPerpendicularDistance ∷ Point → Point → 
   (Double, Int, Int) → Point → (Double, Int, Int)
maxPerpendicularDistance firstPoint lastPoint (dist, ix, counter) point = 
   let cDist = perpendicularDistance point firstPoint lastPoint in
   if cDist > dist
   then (cDist, counter, counter + 1)
   else (dist, ix, counter + 1)


test ∷ TestCase → Bool
test testcase = let result = reduce (testEps testcase) (fromList $ original testcase) in
   case fromList (expected testcase) ≡ result of
      True → True
      False → error (show testcase ⧺ " yielded " ⧺ show result)
      
itWorks ∷ Bool
itWorks = all test testCases
   
data TestCase = TestCase { 
   original ∷ [Point], 
   expected ∷ [Point], 
   testEps ∷ Double 
}
   deriving (Show)

testCases ∷ [TestCase]
testCases = [
   -- minimal cases
   TestCase [1 :+ 1, 1 :+ 1] [1 :+ 1, 1 :+ 1] 0.5,
   TestCase [1 :+ 1, 2 :+ 2] [1 :+ 1, 2 :+ 2] 0.5,
   TestCase [1 :+ 1, 2 :+ 2, 3 :+ 3] [1 :+ 1, 3 :+ 3] 0.5,
   -- vary epsilon
   TestCase [0.0 :+ 2.0, 1.0 :+ 1.0, 3.0 :+ 0.0, 5.0 :+ 1.0] 
            [0.0 :+ 2.0, 1.0 :+ 1.0, 3.0 :+ 0.0, 5.0 :+ 1.0] 0.1,
   TestCase [0.0 :+ 2.0, 1.0 :+ 1.0, 3.0 :+ 0.0, 5.0 :+ 1.0] 
            [0.0 :+ 2.0, 3.0 :+ 0.0, 5.0 :+ 1.0] 0.5]

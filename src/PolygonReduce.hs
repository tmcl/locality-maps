module PolygonReduce (reduce, itWorks)
where

import Prelude.Unicode
import Data.Foldable
import Data.Monoid
import Data.Complex

type Point = Complex Double

perpendicularDistance ∷ Point → Point → Point → Double
perpendicularDistance (pX :+ pY) (p1x :+ p1y) (p2x :+ p2y) = 
   if (p1x ≡ p2x) 
   then abs (pX - p1x)
   else (abs (slope * pX - pY + intercept)) / (sqrt((slope ^^ (2∷ℤ)) + 1))
   where
      slope = (p2y - p1y) / (p2x - p1x)
      intercept = p1y - (slope * p1x)

reduce ∷ Double → [Point] → [Point]
reduce 0 points = points
reduce ε points = if length points< 3 then points else reducedPoints
   where
      firstPoint = head points
      lastPoint = last points
      (dist, pre, centre, post) = foldl' (maxPerpendicularDistance firstPoint lastPoint) (0, mempty, Nothing, mempty) points
      reducedPoints = if dist > ε 
         then mappend r1 (tail r2)
         else pure firstPoint <> pure lastPoint
      r1 = reduce ε (combine pre centre)
      r2 = reduce ε (precombine centre post)

maxPerpendicularDistance ∷ Point → Point → 
   (Double, [Point], Maybe Point, [Point]) → Point → (Double, [Point], Maybe Point, [Point])
maxPerpendicularDistance firstPoint lastPoint (dist, pre, it, post) point = 
   let cDist = perpendicularDistance point firstPoint lastPoint in
   if cDist > dist
   then (cDist, (combine pre it) <> post, Just point, mempty)
   else (dist, pre, it, post <> pure point)

combine ∷ (Applicative f, Monoid (f a)) ⇒ f a → Maybe a → f a
combine a Nothing = a
combine a (Just b) = a <> pure b

precombine ∷ (Applicative f, Monoid (f a)) ⇒ Maybe a → f a → f a
precombine Nothing a = a
precombine (Just a) b = pure a <> b


{-
function DouglasPeucker(PointList[], epsilon)
    // Find the point with the maximum distance
    dmax = 0
    index = 0
    end = length(PointList)
    for i = 2 to ( end - 1) {
        d = perpendicularDistance(PointList[i], Line(PointList[1], PointList[end])) 
        if ( d > dmax ) {
            index = i
            dmax = d
        }
    }
    // If max distance is greater than epsilon, recursively simplify
    if ( dmax > epsilon ) {
        // Recursive call
        recResults1[] = DouglasPeucker(PointList[1...index], epsilon)
        recResults2[] = DouglasPeucker(PointList[index...end], epsilon)
 
        // Build the result list
        ResultList[] = {recResults1[1...length(recResults1)-1], recResults2[1...length(recResults2)]}
    } else {
        ResultList[] = {PointList[1], PointList[end]}
    }
    // Return the result
    return ResultList[]
end
-}


test ∷ TestCase → Bool
test testcase = let result = reduce (testEps testcase) (original testcase) in
   case (expected testcase) ≡ result of
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

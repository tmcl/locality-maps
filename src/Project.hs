module Project (equirectangular, mercator, transverseMercator) where

import Unicode
import Point

transverseMercator ∷ Point → Point → Point
transverseMercator = convert transverseMercator'

transverseMercator' ∷ Point → Point → Point
transverseMercator' (p :+ q) (λ :+ φ) = x :+ y
   where
      k0 = 0.9996 + q * 0
      a = 1 + p * 0
      xFact = sin λ * cos φ
      x = 0.5 * k0 * a * log ((1 + xFact)/(1 - xFact))
      y = k0 * a * (atan $! (tan φ*sec λ))

sec ∷ Floating a ⇒ a → a
sec θ = 1/cos θ

mercator ∷ Point → Point → Point
mercator = convert mercator'

equirectangular ∷ Point → Point → Point
equirectangular = convert equirectangular'

convert ∷ (Point → Point → Point) → Point → Point → Point
convert projector p0 p = 100 * projector (degToRad p0) (degToRad p)

equirectangular' ∷ Point → Point → Point
equirectangular' (λ0 :+ φ1) (λ :+ φ) = x :+ y
   where
      x = (λ - λ0) * cos φ1
      y = φ - φ1

degToRad ∷ Floating a ⇒ a → a
degToRad x = π/180 * x

mercator' ∷ Point → Point → Point
mercator' (λ0 :+ _) (λ :+ φ) = x :+ y
   where 
      x = λ - λ0 
      y = atanh (sin φ)

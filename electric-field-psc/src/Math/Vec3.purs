module Math.Vec3 where

import Prelude

import Math
import Data.Foldable

newtype Vec3 = Vec3 { x :: Number, y :: Number, z :: Number }

v3 :: Number -> Number -> Number -> Vec3
v3 x y z = Vec3 $ { x: x, y: y, z: z }

instance showVec3 :: Show Vec3 where
  show (Vec3 v) = "(" ++ show v.x ++ ", " ++ show v.y ++ ", " ++ show v.z ++ ")"

instance semiringVec3 :: Semiring Vec3 where
  add (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x + w.x
                                 , y: v.y + w.y
                                 , z: v.z + w.z }
  
  mul (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x * w.x
                                 , y: v.y * w.y
                                 , z: v.z * w.z }
  
  zero = Vec3 { x: 0.0, y: 0.0, z: 0.0}
  
  one  = Vec3 { x: 1.0, y: 1.0, z: 1.0 }

instance ringVec3 :: Ring Vec3 where
  sub (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x - w.x
                                 , y: v.y - w.y
                                 , z: v.z - w.z }

instance moduleSemiringVec3 :: ModuloSemiring Vec3 where
  div (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x * w.x
                                 , y: v.y * w.y
                                 , z: v.z * w.z }
  
  mod (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x `mod` w.x
                                 , y: v.y `mod` w.y
                                 , z: v.z `mod` w.z }

(.*) :: Vec3 -> Number -> Vec3
(.*) (Vec3 v) n = Vec3 $ { x: v.x * n
                         , y: v.y * n
                         , z: v.z * n }

(./) :: Vec3 -> Number -> Vec3
(./) (Vec3 v) n = Vec3 $ { x: v.x / n
                         , y: v.y / n
                         , z: v.z / n }

length_sq :: Vec3 -> Number
length_sq (Vec3 v) = v.x * v.x + v.y * v.y + v.z * v.z

length :: Vec3 -> Number
length v = sqrt $ length_sq v

norm :: Vec3 -> Vec3
norm v = v ./ length v

distance_to_sq :: Vec3 -> Vec3 -> Number
distance_to_sq v w = length_sq (w - v)

distance_to :: Vec3 -> Vec3 -> Number
distance_to v w = sqrt $ distance_to_sq v w

v3_sum :: Array Vec3 -> Vec3
v3_sum vs = foldl (+) (v3 0.0 0.0 0.0) vs

module LinearAlgebra where

import Math
import Data.Foldable

newtype Vec3 = Vec3 { x :: Number, y :: Number, z :: Number }

v3 :: Number -> Number -> Number -> Vec3
v3 x y z = Vec3 $ { x: x, y: y, z: z }

instance showVec3 :: Show Vec3 where
  show (Vec3 v) = "(" ++ show v.x ++ ", " ++ show v.y ++ ", " ++ show v.z ++ ")"

instance numVec3 :: Num Vec3 where
  (+) (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x + w.x
                                 , y: v.y + w.y
                                 , z: v.z + w.z }
  
  (-) (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x - w.x
                                 , y: v.y - w.y
                                 , z: v.z - w.z }
  
  (*) (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x * w.x
                                 , y: v.y * w.y
                                 , z: v.z * w.z }
  
  (/) (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x / w.x
                                 , y: v.y / w.y
                                 , z: v.z / w.z }
  
  (%) (Vec3 v) (Vec3 w) = Vec3 $ { x: v.x % w.x
                                 , y: v.y % w.y
                                 , z: v.z % w.z }
  
  negate (Vec3 v) = Vec3 $ { x: -v.x, y: -v.y, z: -v.z }

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

v3_sum :: [Vec3] -> Vec3
v3_sum vs = foldl (+) (v3 0 0 0) vs

module Physics.Electricity where

import Math
import Math.Vec3
import Data.Array (map)

type Charge = { value :: Number, position :: Vec3 }

-- vacuum permittivity
vacuum_e0 = 8.854187817e-12

vacuum_k = let x = 4 * pi * vacuum_e0 in 1 / x

charge :: Number -> Vec3 -> Charge
charge val pos = { value: val, position: pos }

electric_field :: Charge -> Vec3 -> Vec3
electric_field charge point =
  d .* (charge.value / dl')
  where
    d   = point - charge.position
    dl  = length_sq d
    dl' = dl * sqrt dl

electric_field_k :: Charge -> Vec3 -> Vec3
electric_field_k c p = electric_field c p .* vacuum_k

electric_field_many :: [Charge] -> Vec3 -> Vec3
electric_field_many cs p = v3_sum $ map ((flip electric_field) p) cs

electric_field_k_many :: [Charge] -> Vec3 -> Vec3
electric_field_k_many cs p = electric_field_many cs p .* vacuum_k

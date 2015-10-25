module Physics.Electricity where

import Prelude

import Math
import Math.Vec3

type Charge = { value :: Number, position :: Vec3 }

-- vacuum permittivity
vacuum_e0 = 8.854187817e-12

vacuum_k = let x = 4.0 * pi * vacuum_e0 in 1.0 / x

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

electric_field_many :: Array Charge -> Vec3 -> Vec3
electric_field_many cs p = v3_sum $ map ((flip electric_field) p) cs

electric_field_k_many :: Array Charge -> Vec3 -> Vec3
electric_field_k_many cs p = electric_field_many cs p .* vacuum_k

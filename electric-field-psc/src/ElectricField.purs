module ElectricField where

import LinearAlgebra

import Data.Array (map)

type Charge = { value :: Number, position :: Vec3 }

charge :: Number -> Vec3 -> Charge
charge val pos = { value: val, position: pos }

electric_field :: Charge -> Vec3 -> Vec3
electric_field charge point =
  norm dist .* (charge.value / length_sq dist)
  where
    dist = point - charge.position

electric_field_many :: [Charge] -> Vec3 -> Vec3
electric_field_many cs p = v3_sum $ map (\c -> electric_field c p) cs

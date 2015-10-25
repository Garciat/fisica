module Main where

import Prelude

import Math.Vec3
import Physics.Electricity

import Control.Monad.Eff.Console (print)

q0 = charge (-9e-6) (v3 0.0 0.0 40.0)

qval = 4e-6

qs = [ charge   qval  (v3   18.0     0.0    0.0)
     , charge (-qval) (v3 (-30.0)    0.0    0.0)
     , charge   qval  (v3    0.0    14.0    0.0)
     , charge (-qval) (v3    0.0  (-19.0)   0.0) ]

main = do
  let field = electric_field_k_many qs q0.position
  print field
  print $ (field .* q0.value)

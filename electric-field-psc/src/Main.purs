module Main where

import Math.Vec3
import Physics.Electricity

import Debug.Trace

q0 = charge (-9e-6) (v3 0 0 40)

qval = 4e-6

qs = [ charge   qval  (v3   18     0    0)
     , charge (-qval) (v3 (-30)    0    0)
     , charge   qval  (v3    0    14    0)
     , charge (-qval) (v3    0  (-19)   0) ]

main = do
  let field = electric_field_k_many qs q0.position
  print field
  print $ (field .* q0.value)

module Main where

import ElectricField
import LinearAlgebra

import Debug.Trace

main = print $ electric_field (charge 1 (v3 0 0 0)) (v3 1 1 0)

import Check exposing (..)
import Shrink exposing (noShrink)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (..)
import Random exposing (..)
import Random.Int exposing (..)
import Random.Float exposing (..)
import List exposing (..)

import Tris exposing (tetrominoGen)

makeTetrominoes = investigator tetrominoGen noShrink

trisSuite = suite "Tris suite" []

result = quickCheck trisSuite

main = display result
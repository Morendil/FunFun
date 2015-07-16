import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)

shape = circle 50

main = collage 200 200
        [filled red <| deform shape]

deform : List (Float,Float) -> List (Float,Float)
deform shape =
    pointe 30 shape

pointe longueur shape =
    case shape of
    ((x,y) :: reste) -> (x+longueur,y) :: pointe (longueur + 2) reste
    [] -> shape
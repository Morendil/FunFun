import Html exposing (text)
import Time exposing (fps)
import Signal

import Array
import Debug

nodes = Array.fromList ["All you can do is wait.", "You die."]

start = { time = 0 }

update dt world =
    {world | time <- world.time + dt}

display world =
    let which = min (floor world.time//1000) ((Array.length nodes)-1)
        (Just value) = Array.get which nodes
    in text value

main =
    let states = Signal.foldp update start (fps 10)
    in Signal.map display states
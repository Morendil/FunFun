import Html exposing (text)
import Time exposing (fps)
import Signal

import Array
import Debug

time = fps 0.2

states = Array.fromList ["All you can do is wait.", "You die."]

update state dt =
    state + dt

display state =
    let which = min (floor state//100) ((Array.length states)-1)
        (Just value) = Array.get which states
    in text value

main =
    let states = Signal.foldp update 0 time
    in Signal.map display states
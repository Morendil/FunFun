import Graphics.Element exposing (..)
import Signal exposing (..)
import Mouse

start = 1

increment counter = counter + 1

clicks : Signal ()
clicks = Mouse.clicks

update : () -> Int -> Int
update _ counter = increment counter

display : Int -> Element
display value = show (toString value)

state : Signal Int 
state = (foldp update start clicks)

main : Signal Element
main = map display state

-- foldp: (a -> b) -> a -> Signal a -> Signal b
-- map : (a -> b) -> Signal a -> Signal b
module Incremental where

import Html exposing (text, div)
import Html.Attributes exposing (class, style)
import Time exposing (fps)
import Signal

import String
import Array
import Debug

nodes = Array.fromList [
        ("All you can do is wait.",3000),
        ("You die.",0)
    ]

start = { time = 0, node = 0 }

update dt world =
    let time' = world.time + dt
        (Just node) = Array.get world.node nodes
        (_,delay) = node
        next = world.node + 1
        limit = (Array.length nodes)-1
        node' = if time' > delay then max next limit else world.node
        time'' = if node' == world.node then time' else 0
    in {world | time <- time'', node <- node'}

display world =
    let (Just value) = Array.get world.node nodes
        wait = snd value
        percent = if wait > 0 then (100 * (1 - (world.time / wait))) else 0
    in div [class "button"] [
        text (fst value),
        div [class "progress", style [("width",String.concat [toString percent, "%"])]] []
    ]

main =
    let states = Signal.foldp update start (fps 30)
    in Signal.map display states
module Incremental where

import Html exposing (text)
import Time exposing (fps)
import Signal

import Array
import Debug

nodes = Array.fromList [
        ("All you can do is wait.",1000),
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
    in {world | time <- world.time + dt, node <- node'}

display world =
    let (Just value) = Array.get world.node nodes
    in text (fst value)

main =
    let states = Signal.foldp update start (fps 10)
    in Signal.map display states
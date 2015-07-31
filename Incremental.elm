module Incremental where

import Html exposing (text, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Time exposing (fps)
import Signal

import String
import Array
import Debug

nodes = Array.fromList [
        {text="All you can do is wait.",wait=3000},
        {text="And wait some more.",wait=3000},
        {text="Done. Start over?",wait=0}
    ]

start = { time = 0, node = 0, count = 0 }

update action world =
    case action of
        Frame dt -> updateFrame dt world
        Goto node -> {world | node <- node, time <- 0, count <- world.count + 1}

updateFrame dt world =
    let time' = world.time + dt
        (Just node) = Array.get world.node nodes
        next = world.node + 1
        limit = (Array.length nodes)-1
        node' = if time' > node.wait then min next limit else world.node
        time'' = if node' == world.node then time' else 0
    in {world | time <- time'', node <- node'}

display world =
    let (Just value) = Array.get world.node nodes
        percent = if value.wait > 0 then (100 * (1 - (world.time / value.wait))) else 0
    in  div []
        [
            text <| String.concat ["You have ",toString world.count," widgets"],
            div [class (if percent > 0 then "button disabled" else "button"),
                onClick actions.address (Goto 0)] [
                text value.text,
                div [
                    class "progress",
                    style [("width",String.concat [toString percent, "%"])]
                ] []
            ]
        ]

type Action = Frame Float | Goto Int

actions = Signal.mailbox (Goto 0)

main =
    let frames = Signal.map Frame (fps 30)
        clicks = actions.signal
        signals = Signal.mergeMany [frames, clicks]
        states = Signal.foldp update start signals
    in Signal.map display states
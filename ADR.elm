module ADR where

import Html exposing (text, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Time exposing (fps)

import ADR.Style exposing (..)

import Signal

import String
import Array
import Debug

-- Model

start = {time = 0, entries = ["the fire is dead.","the room is freezing."]}

-- Update

update u world =
    case u of
        Frame dt -> {world | time <- world.time + dt }
        Action _ -> {world | entries <- "the fire is burning." :: world.entries}
        _ -> world

-- Display

notification string =
    with notificationStyle "notification" [text string]

notifications world =
    with notificationsStyle "notifications" <| List.map notification world.entries

content =
    with contentStyle "content" [
        with outerSliderStyle "outerSlider" [
            with mainStyle "main" [
                with headerStyle "header" [
                    with headerButtonStyle "headerButton" [text "A Dark Room"]
                ],
                with identity "locationSlider" [
                    with buttonStyle "headerButton" [
                        div [onClick clicks.address ()] [text "light fire"]
                    ]
                ]
            ]
        ]
    ]

display world =
    with bodyStyle "body" [
        with wrapperStyle "wrapper" [
            content,
            notifications world,
            text <| toString world.time
        ]
    ]

-- Signals

type Update = Frame Float | Action ()

clicks = Signal.mailbox ()

main =
    let frames = Signal.map Frame (fps 30)
        actions = Signal.map Action clicks.signal
        signals = Signal.mergeMany [frames,actions]
        states = Signal.foldp update start signals
    in Signal.map display states
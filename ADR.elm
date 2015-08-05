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

start = {time = 0}

entries = ["the fire is dead.","the room is freezing."]

-- Update

update u world =
    let (Frame dt) = u
    in {world | time <- world.time + dt }

-- Display

notification string =
    with notificationStyle "notification" [text string]

notifications =
    with notificationsStyle "notifications" <| List.map notification entries

content =
    with contentStyle "content" [
        with outerSliderStyle "outerSlider" [
            with mainStyle "main" [
                with headerStyle "header" [
                    with headerButtonStyle "headerButton" [text "A Dark Room"]
                ],
                with identity "locationSlider" [
                    with buttonStyle "headerButton" [text "light fire"]
                ]
            ]
        ]
    ]

display world =
    with bodyStyle "body" [
        with wrapperStyle "wrapper" [
            content,
            notifications,
            text <| toString world.time
        ]
    ]

-- Signals

type Update = Frame Float

main =
    let frames = Signal.map Frame (fps 30)
        states = Signal.foldp update start frames
    in Signal.map display states
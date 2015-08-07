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

start = {time = 0, entries = ["the fire is dead.","the room is freezing."], fire = 0}

-- Update

update u world =
    case u of
        Frame dt -> {world | time <- world.time + dt }
        Action LightFire ->
            let entries' = List.concat [["the light from the fire spills from the windows, out into the dark.", "the fire is burning."], world.entries]
            in {world | entries <- entries', fire <- 100}
        _ -> world

-- Display

notification string =
    with notificationStyle "notification" [text string]

notifications world =
    with notificationsStyle "notifications" <| List.map notification world.entries

button label action visibleIf =
    (with buttonStyle "headerButton" [
        div [onClick clicks.address action] [text label]
    ], visibleIf)

displayButtons world buttonList =
    let visibleButtons = List.filter (\(_,trueIn) -> trueIn world) buttonList
    in List.map fst visibleButtons

content world =
    with contentStyle "content" [
        with outerSliderStyle "outerSlider" [
            with mainStyle "main" [
                with headerStyle "header" [
                    with headerButtonStyle "headerButton" [text "A Dark Room"]
                ],
                with identity "locationSlider" <| displayButtons world [
                    button "light fire" LightFire (\world -> world.fire == 0),
                    button "stoke fire" StokeFire (\world -> world.fire > 0)
                ]
            ]
        ]
    ]

display world =
    with bodyStyle "body" [
        with wrapperStyle "wrapper" [
            content world,
            notifications world,
            text <| toString world.time
        ]
    ]

-- Signals

type Choice = LightFire | StokeFire
type Update = Frame Float | Action Choice

clicks = Signal.mailbox LightFire

main =
    let frames = Signal.map Frame (fps 30)
        actions = Signal.map Action clicks.signal
        signals = Signal.mergeMany [frames,actions]
        states = Signal.foldp update start signals
    in Signal.map display states
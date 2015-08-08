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

-- Buttons

displayButtons world buttonList =
    let visibleButtons = List.filter (isVisible world) buttonList
        makeButton button = with buttonStyle "headerButton" [div [onClick clicks.address button.action] [text button.label]]
        isVisible world button = button.visibleIf world
    in List.map makeButton visibleButtons

button label action visibleIf =
    {label=label,action=action,visibleIf=visibleIf}

-- Page

notification string =
    with notificationStyle "notification" [text string]

notifications world =
    with notificationsStyle "notifications" <| List.map notification world.entries

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
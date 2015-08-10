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

start = logFire <| logRoom {time = 0, entries = [], fire = 0, log = 100, room = 0}

-- Update

update u world =
    case u of
        Frame dt -> {world | time <- world.time + dt, log <- world.log - dt/200 }
        Action LightFire ->
            let spill = "the light from the fire spills from the windows, out into the dark."
                world' = logFire {world | fire <- 100}
            in log spill world'
        Action StokeFire ->
            logFire {world | log <- 100, fire <- world.fire + 100}
        _ -> world

-- Display

fireLevel world =
    if  | world.fire >= 100 -> "the fire is roaring."
        | world.fire >= 20 && world.fire < 100 -> "the fire is burning."
        | world.fire >= 10 && world.fire < 20 -> "the fire is flickering."
        | world.fire > 0 && world.fire < 10 -> "the fire is smoldering."
        | otherwise -> "the fire is dead."

roomLevel world =
    if | world.room >= 4 -> "the room is hot."
       | world.room == 3 -> "the room is warm."
       | world.room == 2 -> "the room is mild."
       | world.room == 1 -> "the room is cold."
       | otherwise -> "the room is freezing."

logRoom world =
    log (roomLevel world) world

logFire world =
    log (fireLevel world) world

log entry world =
    let entries' = entry :: world.entries
    in {world | entries <- entries'}

-- Buttons

makeButton world button =
    let extra =
            case button.countdown of
                Nothing -> []
                Just countPct ->
                    let percent = countPct world
                    in [adding cooldownStyle "cooldown" [("width",String.concat [toString percent, "%"])] []]
        enabledAction =
            case button.countdown of
                Nothing -> action
                Just countPct -> if countPct world > 0 then [] else action
        enabledStyle =
            case button.countdown of
                Nothing -> buttonStyle
                Just countPct -> if countPct world > 0 then buttonStyle << disabledStyle else buttonStyle
        action = [onClick clicks.address button.action]
        content = text button.label :: extra
    in with enabledStyle "headerButton" [div enabledAction content]

displayButtons world buttonList =
    let visibleButtons = List.filter (isVisible world) buttonList
        isVisible world button = button.visibleIf world
    in List.map (makeButton world) visibleButtons

button label action visibleIf =
    {label=label,action=action,visibleIf=visibleIf,countdown=Nothing}

cooling countdown button =
    {button | countdown <- Just countdown}

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
                    button "stoke fire" StokeFire (\world -> world.fire > 0) |> cooling (.log)
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
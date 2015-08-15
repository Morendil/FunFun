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

start = logFire <| logRoom {time = 0, entries = [], fire = 0, log = 100, room = 0, queue = []}

type Trigger = BuilderEnters | AdjustTemperature | UnlockForest

-- Update

update u world =
    case u of
        Frame dt ->
            let log' = if world.fire <= 0 then world.log else max 0 (world.log - dt/200)
                world' = {world | time <- world.time + dt, log <- log' }
            in updateQueue world' dt
        Action LightFire ->
            let spill = "the light from the fire spills from the windows, out into the dark."
                world' = logFire {world | fire <- 3}
                world'' = log spill world'
            in queueMany [(BuilderEnters,30000),(AdjustTemperature,30000)] world''
        Action StokeFire ->
            logFire {world | log <- 100, fire <- max 4 (world.fire + 1)}
        _ -> world

queueMany triggers world =
    List.foldr queue world triggers

queue trigger world =
    {world | queue <- trigger :: world.queue}

updateQueue world dt =
    let updateDelay dt (action,delay) = (action,delay-dt)
        queue' = List.map (updateDelay dt) world.queue
        (ripe,waiting) = List.partition (\(action,delay) -> delay <= 0) queue'
        ripeActions = List.map (actionFor << fst) ripe
        composeAll = List.foldr (<<) identity
        world' = (composeAll ripeActions) {world | queue <- []}
    in {world' | queue <- List.concat [world'.queue,waiting]}

actionFor trigger =
    case trigger of
        BuilderEnters ->
            log "a ragged stranger stumbles through the door and collapses in the corner."
            << queue (UnlockForest,15000)
        AdjustTemperature -> adjustTemperature
        UnlockForest ->
            log "the wind howls outside."
            << log "the wood is running out."
        _ -> identity

adjustTemperature world =
    let room' = if | world.room > world.fire -> world.room - 1
                   | world.room < world.fire -> world.room + 1
                   | otherwise -> world.room
    in queue (AdjustTemperature,30000) (logRoom {world | room <- room'})

-- Display

fireLevel world =
    if  | world.fire >= 4 -> "the fire is roaring."
        | world.fire == 3 -> "the fire is burning."
        | world.fire == 2 -> "the fire is flickering."
        | world.fire == 1 -> "the fire is smoldering."
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

roomTitle world =
    with headerButtonStyle "headerButton" [text <| if world.fire > 0 then "A Firelit Room" else "A Dark Room"]

content world =
    with contentStyle "content" [
        with outerSliderStyle "outerSlider" [
            with mainStyle "main" [
                with headerStyle "header" [
                    roomTitle world
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
            text <| toString world
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
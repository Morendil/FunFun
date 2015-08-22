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

start = logFire <| logRoom {time = 0, entries = [],
                            wood = 0, fire = 0, log = 100, room = 0, current = 0,
                            event = Just "Nothing",
                            locations = [room], queue = []}

room = {title="A Dark Room", actions=[LightFire, StokeFire], click=GoRoom}
forest = {title="A Silent Forest", actions=[], click=GoOutside}

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
                locations' = [{room | title <- "A Firelit Room"}]
                world' = logFire {world | fire <- 3, locations <- locations'}
                world'' = log spill world'
            in queueMany [(BuilderEnters,30000),(AdjustTemperature,30000)] world''
        Action StokeFire ->
            if world.fire > 0 then logFire {world | log <- 100, fire <- max 4 (world.fire + 1), wood <- world.wood - 1}
            else log "not enough wood." world
        Action EndEvent ->
            {world | event <- Nothing}
        Action GoRoom ->
            {world | current <- 0}
        Action GoOutside ->
            {world | current <- 1}
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
            log "the wood is running out."
            << log "the wind howls outside."
            << addForest
        _ -> identity

addForest world =
    let locations' = List.append world.locations [forest]
    in {world | locations <- locations', wood <- 4}

adjustTemperature world =
    let room' = if | world.room > world.fire -> world.room - 1
                   | world.room < world.fire -> world.room + 1
                   | otherwise -> world.room
        adjust world =
            let world' = {world | room <- room'}
            in if world.room == world'.room then world' else logRoom world'
    in queue (AdjustTemperature,30000) (adjust world)

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

buttonFor action =
    case action of
        LightFire -> button "light fire" LightFire (\world -> world.fire == 0)
        StokeFire -> button "stoke fire" StokeFire (\world -> world.fire > 0) |> cooling (.log)

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

roomTitle selected index location =
    let hereStyle = if selected == index then selectedHeaderStyle else noStyle
        firstStyle = if index == 0 then firstHeaderStyle else laterHeadersStyle
        attrs = if selected == index then [] else [onClick clicks.address location.click]
    in with (hereStyle << firstStyle) "headerButton" [div attrs [text location.title]]

roomTitles world =
    List.indexedMap (roomTitle world.current) world.locations

displayStore world title getter =
    [with rowKeyStyle "row_key" [text title],
     with rowValStyle "row_val" [text <| toString <| getter world],
     with rowClearStyle "clear" []]

storesContainer world =
    with storesContainerStyle "storesContainer" <|
    if List.length world.locations <= 1 then [] else
    [
        with storesStyle "stores" <|
            List.concat [[with legendStyle "legend" [text "stores"]],displayStore world "wood" .wood]
    ]

displayLocations world =
    let displayLocation location =
        with locationStyle "locationPanel" <| displayButtons world (List.map buttonFor location.actions)
    in List.map displayLocation world.locations

content world =
    let offset = world.current * -700
        style = [("left",String.concat [toString offset,"px"])]
    in with contentStyle "content" [
        with outerSliderStyle "outerSlider" [
            with mainStyle "main" [
                with headerStyle "header"
                    <| roomTitles world,
                adding locationSliderStyle "locationSlider" style <|
                    (storesContainer world) :: displayLocations world
            ]
        ]
    ]

eventsPanel world =
    case world.event of
        Nothing -> text ""
        Just title -> with eventPanelStyle "eventPanel" [
            with eventPanelBackingStyle "eventPanelBefore" [text " "],
            with eventTitleStyle "eventTitle" [text title, with eventTitleAfterStyle "eventTitleAfter" [text " "]],
            with noStyle "eventDescription" [text "through the walls, shuffling noises can be heard."],
            with buttonStyle "investigate" [text "investigate"],
            makeButton world <| button "ignore" EndEvent (always True)
        ]

display world =
    with bodyStyle "body" [
        with wrapperStyle "wrapper" [
            content world,
            notifications world,
            eventsPanel world,
            text <| toString world
        ]
    ]

-- Signals

type Choice = LightFire | StokeFire | EndEvent | GoRoom | GoOutside
type Update = Frame Float | Action Choice

clicks = Signal.mailbox LightFire

main =
    let frames = Signal.map Frame (fps 30)
        actions = Signal.map Action clicks.signal
        signals = Signal.mergeMany [frames,actions]
        states = Signal.foldp update start signals
    in Signal.map display states
module ADR where

import Html exposing (text, div, node)
import Html.Events exposing (onClick,onMouseOver,onMouseOut)
import Time exposing (fps,minute,second)

import ADR.Style exposing (..)

import Html.Attributes as Attributes

import Signal

import String
import Array
import Debug

-- Generic

composeMany = List.foldr (<<) identity

-- Model

type alias World = {time:Float, entries: List String, wood: Int, traps:Int,
                    incomes: List Income,
                    log: Float, gather: Float,
                    fire: Int, room: Int, builder: Int, seenForest: Bool, current: Int, event: Maybe String,
                    locations:List Location, queue:List (Trigger, Float)}
type alias Location = {title:String, actions:List Choice, click:Choice}
type alias Income = String

start : World
start = logFire <| logRoom newGame
newGame =   {time = 0, entries = [],
            wood = 0, traps = 0,
            incomes = [],
            log = 100, gather = 0,
            fire = 0, room = 0, builder = 0, seenForest = False,
            current = 0,
            event = Nothing,
            locations = [room], queue = []}

room = {title="A Dark Room", actions=[LightFire, StokeFire], click=GoRoom}
forest = {title="A Silent Forest", actions=[GatherWood, CheckTraps], click=GoOutside}

type Trigger = BuilderEnters | AdjustTemperature | CoolFire | UnlockForest | UpdateBuilder | Income

-- Constants

fireCoolDelay = 5 * minute
roomWarmDelay = 30 * second
builderStateDelay = 30 * second
needWoodDelay = 15 * second
incomeDelay = 10 * second

-- Update

update u =
    case u of
        Frame  dt ->            advanceTime dt
        Action LightFire ->     lightFire
        Action StokeFire ->     stokeFire
        Action GatherWood ->    gatherWood
        Action EndEvent ->      endEvent
        Action GoRoom ->        goRoom
        Action GoOutside ->     goOutside
        _ -> identity

advanceTime dt world =
    let log' = if world.fire <= 0 then world.log else max 0 (world.log - dt/200)
        gather' = max 0 (world.gather - dt/200)
        world' = {world | time <- world.time + dt, log <- log', gather <- gather'}
    in updateQueue world' dt

lightFire world =
    if  | List.length world.locations == 1 -> firstFire world
        | world.wood < 5 -> log "not enough wood to get the fire going." world
        | otherwise -> logFire {world | fire <- 3, wood <- world.wood - 5}

firstFire world =
    let spill = "the light from the fire spills from the windows, out into the dark."
        locations' = {room | title <- "A Firelit Room"} :: List.drop 1 world.locations
        world' = logFire {world | fire <- 3, locations <- locations'}
        world'' = log spill world'
        actions = [(Income,incomeDelay),(BuilderEnters,builderStateDelay),
                   (AdjustTemperature,roomWarmDelay),(CoolFire,fireCoolDelay)]
    in queueMany actions world''

stokeFire world =
    let world' = {world | log <- 100, fire <- min 4 (world.fire + 1), wood <- world.wood - 1}
    in if world.fire > 0 then logFire <| reset (CoolFire,fireCoolDelay) world'
       else log "not enough wood." world

gatherWood world =
    {world | gather <- 100, wood <- world.wood + 10}

endEvent world =
    {world | event <- Nothing}

goRoom world =
    if world.builder >= 2 then updateBuilder {world | current <- 0}
    else {world | current <- 0}
    
goOutside world =
    let world' = {world | current <- 1, seenForest <- True}
    in if world.seenForest then world' else log "the sky is grey and the wind blows relentlessly." world'

queueMany triggers world =
    List.foldr queue world triggers

queue trigger world =
    {world | queue <- trigger :: world.queue}

updateQueue world dt =
    let updateDelay dt (action,delay) = (action,delay-dt)
        queue' = List.map (updateDelay dt) world.queue
        (ripe,waiting) = List.partition (\(action,delay) -> delay <= 0) queue'
        ripeActions = List.map (actionFor << fst) ripe
        world' = (composeMany ripeActions) {world | queue <- []}
    in {world' | queue <- List.concat [world'.queue,waiting]}

reset (trigger,delay) world =
    let thatTrigger (action,_) = action /= trigger
        queue' = (trigger,delay) :: List.filter thatTrigger world.queue
    in {world | queue <- queue'}

actionFor trigger =
    case trigger of
        BuilderEnters ->        builderEnters
        UpdateBuilder ->        updateBuilder
        AdjustTemperature ->    adjustTemperature
        CoolFire ->             coolFire
        UnlockForest ->         unlockForest
        Income ->               distributeIncome
        _ -> identity

builderEnters =
    log "a ragged stranger stumbles through the door and collapses in the corner."
    << queue (UnlockForest,needWoodDelay)
    << queue (UpdateBuilder,builderStateDelay)

updateBuilder world =
    let advanceBuilder world =
          -- todo: should not see these logs immediately if you are outside
          case world.builder of
            0 -> let world' = {world | builder <- 1}
                 in log "the stranger shivers, and mumbles quietly. her words are unintelligible." world'
            1 -> let world' = {world | builder <- 2}
                 in log "the stranger in the corner stops shivering. her breathing calms." world'
            2 -> let world' = {world | builder <- 3, incomes <- ["wood"]}
                 in log "the stranger is standing by the fire. she says she can help. says she builds things." world'
            _ -> world
        world' = if world.room >= 3 then advanceBuilder world else world
    in if world'.builder < 2 then queue (UpdateBuilder,builderStateDelay) world' else world'

adjustTemperature world =
    let room' = if | world.room > world.fire -> world.room - 1
                   | world.room < world.fire -> world.room + 1
                   | otherwise -> world.room
        adjust world =
            let world' = {world | room <- room'}
            in if world.room == world'.room then world' else logRoom world'
    in queue (AdjustTemperature,roomWarmDelay) (adjust world)

coolFire world =
    let adjust world =
            let world' = {world | fire <- max 0 (world.fire-1)}
            in if world.fire == world'.fire then world' else logFire world'
    in queue (CoolFire,fireCoolDelay) (adjust world)

unlockForest =
        log "the wood is running out."
        << log "the wind howls outside."
        << addForest

addForest world =
    let locations' = List.append world.locations [forest]
    in {world | locations <- locations', wood <- 4}

distributeIncome world =
    let applyIncomes = composeMany <| List.map incomeFor world.incomes
        world' = applyIncomes world
    in queue (Income,incomeDelay) world'

incomeFor name world =
    case name of
        "wood" -> {world | wood <- world.wood + 2}
        _ -> world

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
        GatherWood -> button "gather wood" GatherWood (always True) |> cooling (.gather)
        CheckTraps -> button "check traps" CheckTraps (\world -> world.traps > 0)

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
    in with enabledStyle "headerButton" [div (Attributes.key button.label :: enabledAction) content]

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

storesContainer world anti =
    let stores =
          [with storesStyle "stores" <|
          List.concat [[with legendStyle "legend" [text "stores"]],displayStore world "wood" .wood]]
        incomes =
          [with tooltipStyle "tooltip" [
            with rowKeyStyle "row_key" [text "builder"],
            with rowValStyle "row_val" [text "+2 per 10s"],
            with rowClearStyle "clear" [],
            with rowKeyStyle "row_key" [text "total"],
            with rowValStyle "row_val" [text "+2 per 10s"],
            with rowClearStyle "clear" []]]
    in 
        div [] [
            node "style" [] [text "div.tooltip {display:none} *:hover > div.tooltip {display: block;}"],
            adding storesContainerStyle "storesContainer" anti <|
                  if List.length world.locations <= 1 then [] else
                    if List.isEmpty world.incomes then stores else List.append stores incomes]

displayLocations world =
    let displayLocation location =
        with locationStyle "locationPanel" <| displayButtons world (List.map buttonFor location.actions)
    in List.map displayLocation world.locations

content world =
    let offset = world.current * -700
        style = [("left",String.concat [toString offset,"px"])]
        anti = [("right",String.concat [toString <| offset+700,"px"])]
    in with contentStyle "content" [
        with outerSliderStyle "outerSlider" [
            with mainStyle "main" [
                with headerStyle "header"
                    <| roomTitles world,
                adding locationSliderStyle "locationSlider" style <|
                    (storesContainer world anti) :: displayLocations world
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

type Choice = LightFire | StokeFire | EndEvent | GoRoom | GoOutside | GatherWood | CheckTraps
type Update = Frame Float | Action Choice

clicks = Signal.mailbox LightFire

main =
    let frames = Signal.map Frame (fps 30)
        actions = Signal.map Action clicks.signal
        signals = Signal.mergeMany [frames,actions]
        states = Signal.foldp update start signals
    in Signal.map display states
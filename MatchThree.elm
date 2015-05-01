module MatchThree where

import AnimationFrame exposing (..)
import Time exposing (..)

import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)

import Random

import Window
import Signal
import Signal.Extra

import Debug

-- App imports
import Generic exposing (..)

-- Model

size = 12

type Phase = Steady | Matching | Burst | Fall | Swap

start u =
    case u of
        Viewport (w,h) ->
        let (states, seed) = Random.generate (Random.list (size^2) (Random.int 1 4)) (Random.initialSeed 0)
        in {
            view={w=w,h=h},
            states = states,
            next = states,
            phase = Matching,
            swapBack = False,
            selected = (-1,-1),
            swapWith = (-1,-1),
            seed = seed,
            time = 0
        }

index row col = (row-1)*size + (col-1)

holes states row col =
    let state index = at index states
    in length <| filter (\x -> x == 0) (map (\row' -> state (index row' col)) [(row+1)..size])

transpose states =
    let state index = at index states
    in concatMap (\col -> map (\row -> state (index row col)) [1..size]) [1..size]

cleanCol col replacements =
    let filled = filter (\x -> x > 0) col
    in replacements ++ filled

cleanedIn col =
    length <| filter (\x -> x == 0) col

clean states replacements =
    let byCols = transpose states
        cols = map (\row -> take size (drop (size*(row-1)) byCols)) [1..size]
        neededByCol = map cleanedIn cols
        -- so, so ugly...
        replacementsByCol = drop 1 <| map (\(slice,rest) -> slice) (scanl (\count (slice,rest) -> (take count rest, drop count rest)) ([],replacements) neededByCol)
    in transpose (concat (map2 cleanCol cols replacementsByCol))

removeRuns states =
    let rows = map (\row -> take size (drop (size*(row-1)) states)) [1..size]
        runs = map asRuns rows
        dropLongs aRuns = map (\ (n,x) -> if n >= 3 then (n,0) else (n,x)) aRuns
    in concatMap runsToList (map dropLongs runs)

removeBoth states =
    let vertically = transpose (removeRuns (transpose states))
        horizontally = removeRuns states
    in merge horizontally vertically

merge = map2 (\x y -> if x == 0 || y == 0 then 0 else x)

swap (r_one,c_one) (r_two,c_two) states =
    let i_one = min (index r_one c_one) (index r_two c_two)
        i_two = max (index r_one c_one) (index r_two c_two)
        start = take i_one states
        middle = take (i_two-i_one-1) (drop (i_one+1) states)
        end = drop (i_two+1) states
        one = at i_one states
        two = at i_two states
    in start ++ (two :: middle) ++ (one :: end)

runsToList runs =
    case runs of
        [] -> []
        (n,x) :: rs -> (repeat n x) ++ (runsToList rs)

asRuns list = reverse (asRuns' [] list)

asRuns' runs list =
    case list of
        [] -> runs
        x :: xs -> case runs of
            [] -> asRuns' [(1,x)] xs
            (n,x') :: rs -> if x == x' then asRuns' ((n+1,x')::rs) xs
                                       else asRuns' ((1,x)::runs) xs

at index list =
    let rest = drop index list
        (element :: _) = rest
    in element

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Time

burstDuration = 400
fallDuration = 300
swapDuration = 300

update u world =
   case u of
        Click (row,col) -> case world.phase of 
            Steady ->
                let (srow,scol) = world.selected
                    state = at (index row col) world.states
                   in if | state > 0 && world.selected ==  (-1,-1) -> {world | selected <- (row,col)}
                         | state > 0 && ((abs (srow-row))+(abs (scol-col))) == 1 -> {world | time <- 0, phase <- Swap, swapWith <- (row,col)}
                         | otherwise -> {world | selected <- (-1,-1)}
            _ -> world
        Frame dt ->
            case world.phase of
            Matching ->
                let states' = removeBoth world.states
                    matched = any identity (map (\col -> (holes states' 0 col) > (holes world.states 0 col)) [1..size])
                in if matched then {world | time <- 0, next <- states', phase <- Burst}
                    else {world | phase <- Steady}
            Swap ->
                let swapped = swap world.selected world.swapWith world.states
                    swapMatched = removeBoth swapped
                    matched = any identity (map (\col -> (holes swapMatched 0 col) > (holes world.states 0 col)) [1..size])
                in if world.time < swapDuration then {world | time <- world.time + dt}
                   else if matched || world.swapBack then {world | time <- 0, states <- swapped, swapBack <- False, phase <- Matching}
                        else {world | time <- 0, states <- swapped, phase <- Swap, swapBack <- True}
            Burst ->
                if world.time < burstDuration then {world | time <- world.time + dt}
                else {world | time <- 0, states <- world.next, phase <- Fall}
            Fall ->
                let (states', seed') = Random.generate (Random.list (size^2) (Random.int 1 4)) (Random.initialSeed 0)
                in if world.time < fallDuration then {world | time <- world.time + dt}
                else {world | seed <- seed', time <- 0, states <- clean world.states states', phase <- Matching}
            _ -> world
        _ -> world

-- Display

iconSize = 35
iconSpc = 3
iconTotal = iconSize+iconSpc
iconRadius = iconSize/2

displaySquare world row col  =
    clickable (Signal.message locations.address (row,col)) (collage iconSize iconSize [])

displayIcon world row col =
    let its_x row col =  (toFloat col-1)*iconTotal + iconRadius - (size*iconTotal-iconSpc)/2
        its_y row col = -(toFloat row-1)*iconTotal - iconRadius + (size*iconTotal-iconSpc)/2
        x = its_x row col
        y = its_y row col
        state = at (index row col) world.states
        next = at (index row col) world.next
        color state = at (state-1) [green,blue,red,white]
        shape state = at (state-1) [circle iconRadius,ngon 3 iconRadius,rect iconSize iconSize,ngon 5 iconRadius]
        icon = filled (color state) (shape state)
        frame = outlined (solid white) <| rect iconSize iconSize
        phase = Debug.watch "phase" world.phase
    in if state == 0 then move (x,y) <| filled black <| rect 0 0 -- weird bug when selecting some tiles if this is "toForm empty"
        else case world.phase of
        Steady ->
            if world.selected == (row,col) then move (x,y) <| toForm <| collage iconSize iconSize [icon, frame]
            else move (x,y) <| icon
        Swap ->
            let goto (row',col') =
                let to_x = its_x row' col'
                    now_x = x - world.time*(x-to_x)/swapDuration
                    to_y = its_y row' col'
                    now_y = y - world.time*(y-to_y)/swapDuration
                in move (now_x,now_y) <| icon
            in if | (row,col) == world.selected -> goto world.swapWith
                  | (row,col) == world.swapWith -> goto world.selected
                  | otherwise -> move (x,y) <| icon
        Burst ->
            move (x,y) <| if next > 0 then icon else scale (1-(world.time/burstDuration)) icon
        Fall ->
            let tumble = (holes world.states row col) > 0
                to_y = y - (toFloat (holes world.states row col))*iconTotal
                now_y = y-(if tumble then world.time*(y-to_y)/fallDuration else 0)
            in move (x,max to_y now_y) <| icon
        _ ->
            move (x,y) <| icon

rowOfSquares world row =
    let squares = map (displaySquare world row) [1..size]
        between = spacer iconSpc iconSpc
    in flow right (intersperse between squares)

makeSquares world =
    let rows = map (rowOfSquares world) [1..size]
        between = spacer iconSpc iconSpc
    in flow down (intersperse between rows)

makeIcons world =
    let rowOfIcons world row = map (displayIcon world row) [1..size]
    in  collage (size*iconTotal-iconSpc) (size*iconTotal-iconSpc) (concatMap (rowOfIcons world) [1..size])

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = collage world.view.w world.view.h [filled black <| rect w' h']
        squares = makeSquares world
        icons = makeIcons world
    in layers [backdrop, container world.view.w world.view.h middle icons, container world.view.w world.view.h middle squares]

-- Signals

locations = Signal.mailbox (0,0)

frames = Signal.map Frame frame
buttons = Signal.map Click locations.signal
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,buttons,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
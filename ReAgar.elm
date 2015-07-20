module Agar where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Generic exposing (..)
import List exposing (..)
import Color exposing (..)
import Random exposing (..)
import Text exposing (..)
import Easing exposing (..)

import Debug
import String
import Window
import Mouse
import Keyboard
import Signal.Extra

-- Model

start u =
    case u of
        Viewport (w,h) -> updateViewport (w,h) {
                view={w=0,h=0},
                players=[{pos=(60,0),vel=(0,0),mass=1000,free=0},{pos=(0,0),vel=(0,0),mass=1000,free=0},{pos=(30,30),vel=(0,0),mass=2000,free=0}],
                aim=(0,0)
        }

-- Update

type Update = Viewport (Int, Int) | Frame Float | Point (Int, Int) | Eject Bool | Split Bool | Cheat Bool

update u world = 
    case u of
        Viewport vp -> updateViewport vp world
        Point coords ->
            let center = (world.view.w//2,world.view.h//2)
                (x,y) = floatPair (subPair coords center)
            in {world | aim <- (x,-y)}
        Frame dt ->
            let fps = Debug.watch "fps" <| floor (1000/dt)
            in updatePositions world dt
        Split True -> splitCell world
        Cheat True ->
            let doubl player = {player | mass <- 2 * player.mass}
            in {world | players <- map doubl world.players}
        _ -> world

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

maxSpeed = 200

updatePlayerVelocity world dt player =
    let (Just leader) = head world.players
        direction = addPair world.aim (subPair leader.pos player.pos)
        velocity = mapPair (\x -> (/) x player.mass) direction
        original = vecLength direction
        scaling = ease easeInOutCubic Easing.float 0 maxSpeed maxSpeed original
        scaled = if original == 0 then velocity else mapPair ((*) (scaling / original)) velocity
    in {player | vel <- scaled}

updatePlayerPosition world dt others player =
    let apply vel pos = addPair pos (mapPair ((*) dt) vel)
        hit = filter (\x -> vecLength (subPair (apply player.vel player.pos) (apply x.vel x.pos)) < radius x.mass + radius player.mass) others
        overlaps = map (\x -> vecLength (subPair (apply player.vel player.pos) (apply x.vel x.pos)) - (radius x.mass + radius player.mass)) hit
        between = map (\x -> subPair player.pos x.pos) hit
        reactions = map2 (\x y -> if dotProd x player.vel < 0 then vecTimes (normalize x) (-y/dt) else (0,0)) between overlaps
        adjusted = foldr addPair player.vel reactions
    in {player | vel <- adjusted, pos <- apply adjusted player.pos}

updateFreePosition world dt player =
    let pos' = addPair player.pos (mapPair ((*) dt) player.vel)
    in {player | free <- max 0 (player.free-dt), pos <- pos', vel <- vecTimes player.vel 0.75 }

updatePositions world dt =
    let players' = map (updatePlayerVelocity world dt) (filter (\x -> x.free == 0) world.players)
        players'' = mapAllBut (updatePlayerPosition world dt) players'
        freeMoving = map (updateFreePosition world dt) (filter (\x -> x.free > 0) world.players)
    in {world | players <- players'' ++ freeMoving}

splitOne world cell =
    if cell.mass < 3500 then [cell] else
        let vel = if vecLength cell.vel == 0 then (1,1) else mapPair ((/) (vecLength cell.vel)) cell.vel
            halve mass = 100 * (toFloat <| floor (mass/100)//2)
        in [{cell | mass<-halve cell.mass},{cell | vel <- vel, free <- 400, mass<-halve cell.mass}]

splitCell world =
    {world | players <- concatMap (splitOne world) world.players}

-- Display

radius mass = sqrt (mass / 3.14)
spacing = 40

displayMass cell =
    group [filled black <| circle (radius cell.mass), filled red <| circle ((radius cell.mass)-2),
                     text <| fromString <| toString <| cell.mass/100]

displayGrid world player =
    let count = toFloat <| 2 * ((max world.view.w world.view.h) // (spacing*2))
        lines = group <|
                    map (\k -> traced (dotted gray) <| segment (k*spacing,-1000) (k*spacing,1000)) [-count/2..1+count/2] ++
                    map (\k -> traced (dotted gray) <| segment (-1000,k*spacing) (1000,k*spacing)) [-count/2..1+count/2]
        gridOffset coord = -(toFloat (floor coord % spacing))
    in collage world.view.w world.view.h [ move (mapPair gridOffset player.pos) <| lines]

display world =
    let (Just leader) = head world.players
        playerCells = collage world.view.w world.view.h <|
            map (\x -> move (subPair x.pos leader.pos) <| displayMass x) world.players
        playerArrows = collage world.view.w world.view.h <|
            map (\x -> traced (solid black) (segment (subPair x.pos leader.pos) (addPair (mapPair ((*) 1000) x.vel) (subPair x.pos leader.pos)))) world.players
        playerCircles = collage world.view.w world.view.h <|
            map (\x -> move (subPair x.pos leader.pos) <| traced (solid black) (circle (maxSpeed * 1000 / x.mass))) world.players
        grid = displayGrid world leader
    in layers [grid,playerCells]

-- Signals

frames = Signal.map Frame frame
cursors = Signal.map Point Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

grow = Signal.map Cheat (Keyboard.isDown 88)
split = Signal.map Split Keyboard.space

inputs = Signal.mergeMany [dimensions,frames,cursors,grow,split]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
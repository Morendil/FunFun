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
import Window
import Mouse
import Keyboard
import Signal.Extra

-- Model

start u =
    case u of
        Viewport (w,h) -> updateViewport (w,h) {
                view={w=0,h=0},
                players=[{pos=(0,0),vel=(0,0),mass=1000},{pos=(30,30),vel=(0,0),mass=2000}],
                aim=(0,0)
        }

-- Update

type Update = Viewport (Int, Int) | Frame Float | Point (Int, Int)

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
    let scaled = player.vel
        hit = filter (\x -> vecLength (subPair player.pos x.pos) < radius x.mass + radius player.mass) others
        pos' = addPair player.pos (mapPair ((*) dt) scaled)
    in case head hit of
       Just other ->
            let between = subPair player.pos other.pos
                towards = project player.vel between
                escape = project other.vel between
                normal = subPair player.vel towards
                catchup = if vecLength escape > vecLength towards then towards else escape
                go = (dotProd between scaled) > 0
                pos'' = addPair player.pos (mapPair ((*) dt) (addPair normal catchup))
            in {player | vel <- scaled, pos <- if go then pos' else pos''}
       _ -> {player | vel <- scaled, pos <- pos'}

updatePositions world dt =
    let players' = map (updatePlayerVelocity world dt) world.players
        players'' = mapAllBut (updatePlayerPosition world dt) players'
    in {world | players <- players''}

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
        interPlayerArrows = collage world.view.w world.view.h <|
            cartesian (\x y -> traced (solid black) (segment (subPair x.pos leader.pos) (subPair y.pos leader.pos))) world.players world.players
        grid = displayGrid world leader
    in layers [grid,playerCells,playerArrows,playerCircles,interPlayerArrows]

-- Signals

frames = Signal.map Frame frame
cursors = Signal.map Point Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions,frames,cursors]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
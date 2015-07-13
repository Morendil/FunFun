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
            in updatePositions world
        _ -> world

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

updatePlayerPosition world player =
    let (Just leader) = head world.players
        velocity = addPair world.aim (subPair leader.pos player.pos)
        scale upto dist = ease easeInOutQuint Easing.float 0 upto 100 dist
        (ox,oy) = velocity
        (dx,dy) = (scale (1000/player.mass) ox, scale (1000/player.mass) oy)
    in {player | pos <- addPair player.pos (dx,dy)}

updatePositions world =
    let players' = map (updatePlayerPosition world) world.players
    in {world | players <- players'}

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
        grid = displayGrid world leader
    in layers [grid,playerCells]

-- Signals

frames = Signal.map Frame frame
cursors = Signal.map Point Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions,frames,cursors]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
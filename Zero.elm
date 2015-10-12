module Zero where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Color exposing (..)
import Easing exposing (..)
import Generic exposing (..)

import Signal
import Window
import Signal.Extra

import Time exposing (fps)


-- Model

start u =
    case u of
        Viewport (w,h) -> {width=w,height=h,time=0,items=[
            Background {color=Transition white black 5000},
            HalfCircle {color=Transition black white 5000, fill=Transition gray black 5000, angle=Still 0, pos=Transition (0,0) (0,-70) 5000},
            HalfCircle {color=Transition black white 5000, fill=Transition gray black 5000, angle=Still 180, pos=Still (0,70)}]
        }

type Animated a = Still a | Transition a a Float
type Item = Background {color:Animated Color} |
            HalfCircle {color:Animated Color,fill:Animated Color,angle:Animated Float,pos: Animated (Float,Float)} |
            Star

-- Update

update u world =
  case u of
    Viewport (w,h) -> {world | width <- w, height <- h}
    Frame dt -> {world | time <- world.time + dt}
    Action South -> {world | items <- List.append world.items [Star]}
    _ -> world

-- Animate

animate interpolation time animated =
    case animated of
        Still value -> value
        Transition from to duration -> ease Easing.linear interpolation from to duration time

-- Display

halfCircle radius =
    let full = circle radius
        total = List.length full
        oneHalf = List.drop 1 <| List.take (total//2) full
    in polygon oneHalf

north outerColor innerColor radius =
    group [filled innerColor <| halfCircle radius,
           outlined {defaultLine | width <- 14, cap <- Padded, color <- outerColor} <| halfCircle radius]

star time radius =
    let pointy = group [move (radius/1.5,0) <| filled white <| List.map (\(x,y) -> (x,y/2)) <| ngon 3 radius]
        rotated part n = rotate (degrees (n*90)) <| part
        sprite = group <| (filled white <| square (radius/2)) :: List.map (rotated pointy) [0..3]
    in rotate (degrees time/10) sprite

display world =
    let radius = toFloat world.height/3.4
        gap = radius / 14
        gray = (rgb 204 204 204)
        place x = collage world.width world.height [x]
        active x = clickable (Signal.message clicks.address x)
        animateColor = animate Easing.color (max 0 (world.time-5000))
        animatePosition = animate (Easing.pair Easing.float) (max 0 (world.time-5000))
        draw item = case item of
            Background {color} -> place <| filled (animateColor color) <| rect (toFloat world.width) (toFloat world.height)
            HalfCircle {color,fill,angle,pos} -> active South <|
                place <| move (vecTimes (animatePosition pos) (21/100)) <|
                rotate (degrees <| animate Easing.float world.time angle) <| north (animateColor color) (animateColor fill) radius
            Star -> place <| move (gap*5,-gap*5) <| star world.time (radius/15)
    in layers <| List.map draw world.items

-- Signals

clicks = Signal.mailbox South
type Choice = South | North
type Update = Frame Float | Viewport (Int,Int) | Action Choice

main =
    let frames = Signal.map Frame (fps 30)
        dimensions = Signal.map Viewport (Window.dimensions)
        actions = Signal.map Action clicks.signal
        signals = Signal.mergeMany [dimensions,frames,actions]
        states = Signal.Extra.foldp' update start signals
    in Signal.map display states
module Agar where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Generic exposing (..)
import List exposing (..)
import Color exposing (..)
import Random exposing (..)
import Text exposing (..)

import Debug
import Window
import Mouse
import Keyboard
import Signal.Extra

-- Model

start u =
    case u of
        Viewport (w,h) -> makePellets 1000 {
                view={w=w,h=h},
                pos={x=0,y=0},
                aim=(0,0),
                pellets=[],
                others=[],
                mass=1000,
                seed=initialSeed 0
        }

-- Update

type Update = Viewport (Int, Int) | Frame Float | Point (Int, Int) | Eject Bool

minx = -5000
maxx = 5000
miny = -5000
maxy = 5000

makePellets n world =
    let (pellets',seed') = generate (Random.list n <| customGenerator pelletMaker) world.seed
    in {world | seed <- seed', pellets <- pellets'}

pelletMaker seed =
    coordMaker minx maxx miny maxy seed

coordMaker minx maxx miny maxy seed =
    let (x,seed') = generate (float minx maxx) seed
        (y,seed'') = generate (float miny maxy) seed'
    in ({x=x,y=y,c=red,mass=100},seed'')

update u world = 
    case u of
        Viewport vp -> updateViewport vp world
        Point coords -> {world | aim <- addPair (-world.view.w//2,-world.view.h//2) coords}
        Eject _ -> spawn world
        Frame dt ->
            let fps = Debug.watch "fps" <| floor (1000/dt)
            in eat <| slide (glide world dt) dt

spawn world =
    if world.mass < 3500 then world else
        let (x,y) = (toFloat <| fst world.aim, toFloat <| snd world.aim)
            magnitude = sqrt (x^2+y^2)
            (dx,dy) = (if magnitude < 10 then 1 else x/magnitude,if magnitude < 10 then 1 else y/magnitude)
            others' = {x=world.pos.x,y=world.pos.y,dx=dx*3,dy=-dy*3,mass=1200} :: world.others
        in {world | others <- others', mass <- world.mass - 1800}

slideOther world dt {x,y,dx,dy,mass} = 
    {x=x+dx*dt,y=y+dy*dt,dx=max 0 (dx-0.005*dt),dy=max 0 (dy-0.005*dt),mass=mass}

slide world dt =
    let others' = map (slideOther world dt) world.others
    in {world | others <- others'}

glide world dt  =
    let (x,y) = (toFloat <| fst world.aim, toFloat <| snd world.aim)
        magnitude = sqrt (x^2+y^2)
        direction = (if magnitude < 10 then 0 else x/magnitude,if magnitude < 10 then 0 else y/magnitude)
        speed = (min 100 magnitude)/300
        (x',y') = (pin minx maxx (world.pos.x+(fst direction)*speed*dt),pin miny maxy (world.pos.y-(snd direction)*speed*dt))
        pos' = {x=x',y=y'}
    in {world | pos <- pos'}

eat world =
    let distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)
        inRange pellet = distance (pellet.x,pellet.y) (world.pos.x,world.pos.y) < (radius world.mass)
        eatable pellet = (pellet.mass * 1.25) < world.mass
        (eatenPellets,pellets') = partition (inRange `and` eatable) world.pellets
        (eatenOthers,others') = partition (inRange `and` eatable) world.others
        mass' = world.mass + sum (map .mass eatenPellets) + sum (map .mass eatenOthers)
    in {world | pellets <- pellets', others <- others', mass <- mass'}

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

radius mass = sqrt (mass / 3.14)
pradius = 14

displayPellet {x,y,c} =
    move (x,y) <| filled c <| ngon 6 pradius

displayOther {x,y,mass} =
    move (x,y) <| displayMass mass

displayMass mass =
    group [filled black <| circle ((radius mass)+2), filled red <| circle (radius mass),
                     text <| fromString <| toString <| mass/100]

display world =
    let spacing = 40        
        player = collage world.view.w world.view.h [displayMass world.mass]
        pellets = displayOffset <| group <| map displayPellet world.pellets
        others = displayOffset <| group <| map displayOther world.others
        count = toFloat <| 2 * ((max world.view.w world.view.h) // (spacing*2))
        offset coord = -(toFloat (floor coord % spacing))
        displayOffset form = collage world.view.w world.view.h [ move (-world.pos.x,-world.pos.y) form]
        lines = group <|
                    map (\k -> traced (dotted gray) <| segment (k*spacing,-1000) (k*spacing,1000)) [-count/2..1+count/2] ++
                    map (\k -> traced (dotted gray) <| segment (-1000,k*spacing) (1000,k*spacing)) [-count/2..1+count/2]
        grid = collage world.view.w world.view.h [ move (offset world.pos.x,offset world.pos.y) lines]
    in layers [grid,pellets,others,player]

-- Signals

eject = Signal.map Eject (Keyboard.isDown 87)
frames = Signal.map Frame frame
cursors = Signal.map Point Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions,frames,cursors,eject]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
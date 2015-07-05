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
        Viewport (w,h) -> makePellets 1000 <| updateViewport (w,h) {
                view={w=0,h=0},
                players=[{x=0,y=0,mass=1000}],
                aim=(0,0),
                pellets=[],
                nuggets=[],
                seed=initialSeed 0,
                grid = toForm Graphics.Element.empty
        }

-- Update

type Update = Viewport (Int, Int) | Frame Float | Point (Int, Int) | Eject Bool | Split Bool

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

spawnOne (x,y) player =
    if player.mass < 3500 then ([],player) else
    let magnitude = sqrt (x^2+y^2)
        (dx,dy) = (if magnitude < 10 then 1 else x/magnitude,if magnitude < 10 then 1 else y/magnitude)
        nuggets' = [{x=player.x,y=player.y,dx=dx*3,dy=-dy*3,mass=1200}]
        player' = {player | mass <- player.mass - 1800}
    in (nuggets',player')

spawn world =
    let (Just player) = head world.players
        (nuggets',player') = spawnOne (toFloat <| fst world.aim, toFloat <| snd world.aim) player
    in {world | nuggets <- nuggets' ++ world.nuggets, players <- [player']}

slideOther world dt {x,y,dx,dy,mass} = 
    {x=x+dx*dt,y=y+dy*dt,dx=max 0 (dx-0.005*dt),dy=max 0 (dy-0.005*dt),mass=mass}

slide world dt =
    let nuggets' = map (slideOther world dt) world.nuggets
    in {world | nuggets <- nuggets'}

glide world dt  =
    let (x,y) = (toFloat <| fst world.aim, toFloat <| snd world.aim)
        magnitude = sqrt (x^2+y^2)
        direction = (if magnitude < 10 then 0 else x/magnitude,if magnitude < 10 then 0 else y/magnitude)
        speed = (min 100 magnitude)/(6*(sqrt player.mass))
        (x',y') = (pin minx maxx (player.x+(fst direction)*speed*dt),pin miny maxy (player.y-(snd direction)*speed*dt))
        (Just player) = head world.players
        player' = {player | x<-x',y<-y'}
    in {world | players <- [player']}

eat world =
    let distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)
        inRange pellet = distance (pellet.x,pellet.y) (player.x,player.y) < (radius player.mass)
        eatable pellet = (pellet.mass * 1.25) < player.mass
        (eatenPellets,pellets') = partition (inRange `and` eatable) world.pellets
        (eatenNuggets,nuggets') = partition (inRange `and` eatable) world.nuggets
        mass' = player.mass + sum (map .mass eatenPellets) + sum (map .mass eatenNuggets)
        (Just player) = head world.players
        player' = {player | mass <- mass'}
    in {world | pellets <- pellets', nuggets <- nuggets', players <- [player']}

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
        world' = {world | view <- view'}
    in {world' | grid <- initGrid world'}

-- Display

radius mass = sqrt (mass / 3.14)
pradius = 14
spacing = 40

displayPellet {x,y,c} =
    move (x,y) <| filled c <| ngon 6 pradius

displayOther {x,y,mass} =
    move (x,y) <| displayMass mass

displayMass mass =
    group [filled black <| circle ((radius mass)+2), filled red <| circle (radius mass),
                     text <| fromString <| toString <| mass/100]

initGrid world =
    let count = toFloat <| 2 * ((max world.view.w world.view.h) // (spacing*2))
        lines = group <|
                    map (\k -> traced (dotted gray) <| segment (k*spacing,-1000) (k*spacing,1000)) [-count/2..1+count/2] ++
                    map (\k -> traced (dotted gray) <| segment (-1000,k*spacing) (1000,k*spacing)) [-count/2..1+count/2]
    in lines

display world =
    let (Just player) = head world.players
        displayOffset form = collage world.view.w world.view.h [ move (-player.x,-player.y) form]
        playerCells = collage world.view.w world.view.h [displayMass player.mass]
        pellets = displayOffset <| group <| map displayPellet world.pellets
        nuggets = displayOffset <| group <| map displayOther world.nuggets        
        gridOffset coord = -(toFloat (floor coord % spacing))
        grid = collage world.view.w world.view.h [ move (gridOffset player.x,gridOffset player.y) world.grid]
    in layers [grid,pellets,nuggets,playerCells]

-- Signals

split = Signal.map Split Keyboard.space
eject = Signal.map Eject (Keyboard.isDown 87)
frames = Signal.map Frame frame
cursors = Signal.map Point Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions,frames,cursors,eject]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
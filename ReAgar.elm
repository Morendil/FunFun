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
        Viewport (w,h) -> makePellets 1000 <| updateViewport (w,h) {
                view={w=0,h=0},
                players=[{pos=(0,0),vel=(0,0),mass=1000,free=0}],
                aim=(0,0),
                pellets=[],
                nuggets=[],
                seed=initialSeed 0,
                time = 0
        }

-- Update

type Update = Viewport (Int, Int) | Frame Float | Point (Int, Int) | Eject Bool | Split Bool | Cheat Bool

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
    let (x,seed') = generate (Random.float minx maxx) seed
        (y,seed'') = generate (Random.float miny maxy) seed'
    in ({pos=(x,y),c=red,mass=100},seed'')

update u world = 
    case u of
        Viewport vp -> updateViewport vp world
        Point coords ->
            let center = (world.view.w//2,world.view.h//2)
                (x,y) = floatPair (subPair coords center)
            in {world | aim <- (x,-y)}
        Frame dt ->
            let fps = Debug.watch "fps" <| floor (1000/dt)
                world' = updatePositions world dt
                world'' = eat world'
            in {world'' | time <- world.time + dt}
        Eject True -> spawn world
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
        velocity = mapPair (\x -> (/) x (25 * radius player.mass)) direction
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
    in {player | pos <- pos', vel <- vecTimes player.vel 0.75 }

updatePositions world dt =
    let players' = map (updatePlayerVelocity world dt) (filter (\x -> x.free == 0) world.players)
        players'' = mapAllBut (updatePlayerPosition world dt) players'
        freeMoving = map (updateFreePosition world dt) (filter (\x -> x.free > 0) world.players)
        freeMoving' = map (\x -> {x | free <- max 0 (x.free-dt)}) freeMoving
        nuggets' = map (updateFreePosition world dt) world.nuggets
    in {world | players <- players'' ++ freeMoving', nuggets <- nuggets'}

splitOne world cell =
    if cell.mass < 3500 then [cell] else
        let vel = if vecLength cell.vel == 0 then (1,1) else mapPair ((/) (vecLength cell.vel)) cell.vel
            halve mass = 100 * (toFloat <| floor (mass/100)//2)
        in [{cell | mass<-halve cell.mass},{cell | vel <- vel, free <- 400, mass<-halve cell.mass}]

splitCell world =
    {world | players <- concatMap (splitOne world) world.players}

eatOne world player =
    let inRange pellet = vecLength (subPair pellet.pos player.pos) < (radius player.mass)
        eatable pellet = (pellet.mass * 1.25) < player.mass
        (eatenPellets,pellets') = partition (inRange `and` eatable) world.pellets
        (eatenNuggets,nuggets') = partition (inRange `and` eatable) world.nuggets
        mass' = player.mass + sum (map .mass eatenPellets) + sum (map .mass eatenNuggets)
        world' = {world | pellets <- pellets', nuggets <- nuggets'}
        player' = {player | mass <- mass'}
    in (world', player')

eat world =
    let reduce player (world,players) =
        let (world',player') = eatOne world player
        in (world',player' :: players)
        (world',players') = foldr reduce (world,[]) world.players
    in {world' | players <- players'}

spawnOne world player =
    if player.mass < 3500 then (world,player) else
    let direction = if vecLength player.vel == 0 then normalize (1,1) else normalize player.vel
        nuggets' = [{pos=addPair player.pos (vecTimes direction (radius player.mass)),vel=vecTimes direction 0.5,mass=1200}]
        player' = {player | mass <- player.mass - 1800}
        world' = {world | nuggets <- nuggets' ++ world.nuggets}
    in (world', player')

spawn world =
    let reduce player (world,players) =
        let (world',player') = spawnOne world player
        in (world',player' :: players)
        (world',players') = foldr reduce (world,[]) world.players
    in {world' | players <- players'}

-- Display

radius mass = sqrt (mass / 3.14)
spacing = 40
pradius = 14

displayMass time cell =
    group [filled black <| deform time (circle (radius cell.mass)), filled red <| deform time (circle ((radius cell.mass)-2)),
                     text <| fromString <| toString <| cell.mass/100]

displayPellet leader time pellet =
    let {pos,c} = pellet
    in move (subPair pos leader.pos) <| filled c <| deform time (ngon 6 pradius)

displayOther leader time cell =
    let {pos,mass} = cell
    in move (subPair pos leader.pos) <| displayMass time cell

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
            map (\x -> move (subPair x.pos leader.pos) <| displayMass world.time x) world.players
        pellets = collage world.view.w world.view.h <| map (displayPellet leader world.time) world.pellets
        nuggets = collage world.view.w world.view.h <| map (displayOther leader world.time) world.nuggets
        grid = displayGrid world leader
    in layers [grid,pellets,nuggets,playerCells]

deform time shape =
    indexedMap (reduce time) shape

reduce time index (x,y) =
    let findex = toFloat index
        zoom = 1 + (sin (time/32 + findex*2)/32)
    in (x * zoom , y * zoom)

-- Signals

frames = Signal.map Frame frame
cursors = Signal.map Point Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

eject = Signal.map Eject (Keyboard.isDown 87)
grow = Signal.map Cheat (Keyboard.isDown 88)
split = Signal.map Split Keyboard.space

inputs = Signal.mergeMany [dimensions,frames,cursors,grow,split,eject]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
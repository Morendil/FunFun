import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)
import Generic exposing (..)
import Random exposing (..)
import Text exposing (..)

import Window
import Signal.Extra

-- Model

defaultCell = {x=0,y=0,fixed=True}

start u =
    case u of
        Viewport (w,h) -> makeCells {
            view={w=w,h=h},
            cells=[defaultCell],
            seed=initialSeed 0,
            fps = "0"
        }

cellMaker seed =
    let width = 1024
        height = 768
        (x,seed') = generate (int (-width//2) (width//2)) seed
        (y,seed'') = generate (int (-height//2) (height//2)) seed'
    in ({defaultCell|x<-toFloat x,y<-toFloat y,fixed<-False},seed'')

moveMaker seed =
    let (x,seed') = generate (int -6 6) seed
        (y,seed'') = generate (int -6 6) seed'
    in ((x,y),seed'')

-- Update

type Update = Viewport (Int, Int) | Frame Float

makeCells world =
    let (cells', seed') = generate (Random.list 600 <| customGenerator cellMaker) world.seed
    in {world | cells <- defaultCell :: cells', seed <- seed'}

update u world =
    case u of
        Viewport vp -> updateViewport vp world
        Frame dt -> let world' = moveCells <| freezeCells world in {world' | fps <- toString (floor (1000/dt))}

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

offset move cell =
    {cell|x<-cell.x+toFloat (fst move),y<-cell.y+toFloat (snd move)}

moveCells world =
    let count = length world.cells
        (moves,seed') = generate (Random.list count <| customGenerator moveMaker) world.seed
        fixed = filter .fixed world.cells
        cells' = fixed ++ map2 offset moves (filter (not << .fixed) world.cells)
    in {world | cells <- cells', seed <-seed'}

freezeCells world =
    let dist me other = ((me.x-other.x)^2)+((me.y-other.y)^2)
        hit me other = let d = dist me other in (d < 4*radius*radius) && (d > 0) && other.fixed
        maybeFreeze others me = if any (\other -> hit me other) others then {me | fixed <- True} else me
        seeds = filter .fixed world.cells
        cells' = map (maybeFreeze seeds) world.cells
    in {world | cells <- cells'}

-- Display

radius = 10

displayCell w h cell =
    move (cell.x,cell.y) <| filled (if cell.fixed then red else blue) <| circle radius

display world =
    layers [
    collage world.view.w world.view.h <| map (displayCell world.view.w world.view.h) world.cells,
    show world.fps
    ]

-- Signals

dimensions = Signal.map Viewport (Window.dimensions)
frames = Signal.map Frame frame

inputs = Signal.mergeMany [dimensions,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
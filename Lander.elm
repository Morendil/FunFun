import Signal
import Signal.Extra
import Graphics.Element (..)
import Graphics.Collage (..)
import Transform2D
import Color (..)
import Time (..)
import List (..)
import Debug
import Window
import Keyboard
-- App imports
import Generic (..)

-- Model

type alias World = {view : {w:Int,h:Int},zoom:Float,bodies:List {mass:Mass,body:Body}}
type alias Mass = {pos:Vector,vel:Vector,m:Float}
type alias Vector = (Float,Float)
type Body = Ship {heading: Float} | Planet {r:Float}

start : Update -> World
start viewport = case viewport of
    Viewport (w,h) -> {
        view = {w = w, h = h},
        zoom = 1.0,
        bodies = [{mass = {pos=(0,0),    vel=(0,0.06),      m=0.001}, body = Ship {heading=0}},
                  {mass = {pos=(-150,0), vel=(0,0),         m=1},     body = Planet {r = 70}},
                  {mass = {pos=(-80,80), vel=(0.05,-0.05),  m=0.01},  body = Planet {r = 7}}]
    }

-- Update

type Update = Viewport (Int, Int) | Tick Float | Move {x:Int,y:Int}

update s world = case s of
    Viewport dims -> updateViewport dims world
    Tick dt -> let n = 8 in iterate (updateTick (dt/n)) n world
    Move arrows -> updateMove arrows world

updateViewport (w,h) world =
    let wv = world.view
        v' = { wv | w <- w, h <- h}
    in {world | view <- v'}

norm2 (x,y) = x^2+y^2

mapT f (x1,y1) (x2,y2) = (f x1 x2, f y1 y2)
mapS f r (x,y) = (f r x, f r y)
add = mapT (+)
mul = mapT (*)
sub = mapT (-)
div = mapT (/)
mulS = mapS (*)

distance m1 m2 =
    sqrt (norm2 (m1.pos `sub` m2.pos))

integrate dt object1 object2 =
    let vec = object1.pos `sub` object2.pos
        d2 = norm2 vec
        acc = (object1.m / (sqrt(d2) * d2)) `mulS` vec
    in { object2 |
            pos <- object2.pos `add` (dt `mulS` object2.vel) `add` ((dt * dt) `mulS` acc),
            vel <- object2.vel `add` (dt `mulS` acc)
        }

updateZoom dt world =
    let (s :: p :: rest) = world.bodies
        vp = Debug.watch "vp" <| (world.view.w,world.view.h)
        fov = (toFloat (min world.view.w world.view.h))/2
        span = distance s.mass p.mass
        z = Debug.watch "zoom" <| fov/span
    in min 1.0 z

updateTick dt world =
    let integrateAll dt bodies one = foldl (integrate dt) one bodies
        updateMass e m = {e | mass <- m}
        bodies = mapAllBut (integrateAll dt) (map .mass world.bodies)
        zoom' = ((world.zoom * 99) + (updateZoom dt world)) / 100
    in { world | bodies <- map2 updateMass world.bodies bodies, zoom <- min world.zoom zoom'}

updateMove arrows world = 
    let (s :: rest) = world.bodies
        (Ship body) = s.body
        acc = ((toFloat arrows.y) * sin (degrees body.heading) * 0.001, (toFloat arrows.y) * cos (degrees body.heading) * 0.001)
        smass = s.mass
        body' = {body | heading <- body.heading-(toFloat arrows.x)*5}
        mass' = {smass | vel <- s.mass.vel `add` acc }
        s' = {s | body <- Ship body',
                  mass <- mass'}
    in {world | bodies <- s' :: rest}

-- Display

displayBody {mass,body} =
    case body of
        Ship {heading} -> move mass.pos <| rotate (degrees heading)
                                                <| group [rotate (degrees -30) (outlined (solid white) (ngon 3 25)), outlined (solid white) (rect 3 25)]
        Planet {r} -> move mass.pos <| outlined (solid white) (circle r)

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        sky = [filled black (rect w' h')]
        bodies = sky ++ [groupTransform (Transform2D.scale world.zoom) (map displayBody world.bodies)]
    in collage world.view.w world.view.h bodies

-- Signals

window = Signal.map Viewport Window.dimensions
ticks = Signal.map Tick (fps 30)
keys = Signal.sampleOn (fps 30) (Signal.map Move Keyboard.arrows)

inputs = Signal.mergeMany [window, ticks, keys]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
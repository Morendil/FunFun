import Color (..)
import Debug
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time (..)
import List (..)
import Window
import Signal.Extra
import Signal.Time

-- MODEL

type alias World = List Character
type alias Pending = List (Float, Keys)

type Tile = Moving Figure | Static Terrain
type Character = Active Figure | Sleeping Figure Pending | Ghost Figure Pending Pending

type alias Figure = 
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    }

type alias Terrain = 
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }

right : Tile -> Float
right t = case t of
  Moving f -> f.x+f.w/2
  Static p -> p.x+p.w

left : Tile -> Float
left t = case t of
  Moving f -> f.x-f.w/2
  Static p -> p.x

bottom : Tile -> Float
bottom t = case t of
  Moving f -> f.y
  Static p -> p.y

top : Tile -> Float
top t = case t of
  Moving f -> f.y+f.h
  Static p -> p.y+p.h

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }

mario : Figure
mario =
  { x = 0 , y = 0 , w = 16 , h = 26 , vx = 0 , vy = 0 , dir = Right }

marios : World
marios = [Active mario, Sleeping mario []]

decor : List Terrain
decor = [ {w = 30, h = 50, x = 20, y = 0},
          {w = 30, h = 20, x = 20, y = 85},
          {w = 30, h = 40, x = 55, y = 0},
          {w = 99999, h = 48, x = -9999, y = -48}, -- the ground
          {w = 0, h = 9999, x = 9999, y = 0}, -- the far right
          {w = 0, h = 9999, x = -9999, y = 0} -- the far left
        ]

-- GENERAL

and : (a -> Bool) -> (a -> Bool) -> a -> Bool
and f g x =
  f x && g x

between : number -> number -> number -> Bool
between min max x =
  x >= min && x < max

iterate : (a -> a) -> Int -> a -> a
iterate f n =
  foldr (<<) identity (repeat n f)

-- mapAllBut is a map with the list itself, without the element mapped
-- we don't want self-collisions
mapAllBut : (List a -> a -> a) -> List a -> List a
mapAllBut f l =
  map2 f (dropEach l) l

dropEach : List a -> List (List a)
dropEach l =
  case l of
    [] -> []
    x :: [] -> [[]]
    x :: xs -> append [xs] (map (append [x]) (dropEach xs))

-- UPDATE

type Update = Spawn Bool | Move (Float, Keys)

updateWorld : Update -> World -> World
updateWorld u world =
  case u of 
    Spawn true -> (head world) :: (append (map cycle (tail world)) [Sleeping mario []])
    Move move -> mapAllBut (update move) world
    _ -> world

cycle : Character -> Character
cycle x = case x of
            Sleeping m x -> Ghost m x x
            Ghost m old sav -> Ghost mario sav sav
            _ -> x

update : (Float, Keys) -> World -> Character -> Character
update (dt, keys) world mario' =
    case mario' of 
      Active m -> Active (updateActive (dt,keys) m world)      
      Sleeping m x -> Sleeping m (append x [(dt,keys)])
      Ghost m [] sav -> Ghost (updateActive (dt,{x=0,y=0}) m world) [] sav
      Ghost m x sav -> Ghost (updateActive (head x) m world) (tail x) sav

updateActive (dt,keys) mario world =
    let n = 6 in
    mario
        |> jump keys
        |> walk keys
        |> iterate (physics world (dt/n)) n        
        |> Debug.watch "mario"

jump : Keys -> Figure -> Figure
jump keys mario =
    if keys.y > 0 && mario.vy == 0
      then { mario | vy <- 4.0 }
      else mario

figure : Character -> Tile
figure c = case c of
  Active f -> Moving f
  Sleeping f _ -> Moving {f|x<-8888}
  Ghost f _ _ -> Moving f

physics : World -> Float -> Figure -> Figure
physics world dt mario =
  let candidates = append (map Static decor) [] -- should be :(map figure world)
      newx = mario.x + dt * mario.vx
      newy = mario.y + dt * mario.vy
      xymario = { mario | x <- newx, y <- newy }
      y_mario = { mario | y <- newy - 0.01} -- test whether Mario is supported
      x_mario = { mario | x <- newx }
      newv = if any (collidingWith (Moving y_mario)) candidates then 0 else mario.vy - dt/8
      next = nonColliding candidates [xymario, y_mario, x_mario]
      newmario = head <| append next [mario]
  in
     {newmario | vy <- newv}

-- maybe not ideal, Elm being non-lazy
nonColliding : List Tile -> List Figure -> List Figure
nonColliding others list =
  filter (\ mario -> not (colliding others mario)) list

colliding others mario =
  any (collidingWith (Moving mario)) others

collidingWith : Tile -> Tile -> Bool
collidingWith mario pl =
  sameLevelAs mario pl && sameColumnAs mario pl

sameColumnAs : Tile -> Tile -> Bool
sameColumnAs mario pl =
  (left pl, right pl) `intersects` (left mario, right mario)

sameLevelAs : Tile -> Tile -> Bool
sameLevelAs mario pl =
  (bottom pl, top pl) `intersects` (bottom mario, top mario)

intersects (min1, max1) (min2, max2) =
  between min1 max1 min2 ||
  between min2 max2 min1

walk : Keys -> Figure -> Figure
walk keys mario =
    { mario |
        vx <- toFloat keys.x,
        dir <-
          if  | keys.x < 0 -> Left
              | keys.x > 0 -> Right
              | otherwise  -> mario.dir
    }

-- VIEW

viewWorld : (Int, Int) -> World -> Element
viewWorld (w, h) world =
  let 
    dims = (toFloat w, toFloat h)
    moving = map (view dims) world
    static = displayDecor dims
  in
  collage w h <|
    append static (reverse moving)

view : (Float, Float) -> Character -> Form
view dims mario' =
  case mario' of
    Active mario -> Debug.trace "mario" (viewActive dims "mario" mario)
    Ghost mario _ _ -> viewActive dims "ghost" mario
    Sleeping _ _ -> toForm empty

viewActive : (Float, Float) -> String -> Figure -> Form
viewActive (w,h) who mario =
  let verb =
        if  | abs mario.vy > 0 -> "jump"
            | mario.vx /= 0    -> "walk"
            | otherwise        -> "stand"

      dir =
        case mario.dir of
          Left -> "left"
          Right -> "right"

      src = "imgs/" ++ who ++ "/"++ verb ++ "/" ++ dir ++ ".gif"

      marioImage = image 35 35 src
  in
    marioImage
       |> toForm
       |> position (w,h) (Moving mario)

displayDecor : (Float, Float) -> List Form
displayDecor (w,h) =
          append 
          [ rect w h |> filled (rgb 174 238 238) ]
          (map (displayPlatform (w,h)) decor)

displayPlatform : (Float, Float) -> Terrain -> Form
displayPlatform (w,h) platform =
              rect platform.w platform.h
              |> filled (if platform.y < 0 then green else red)
              |> position (w,h) (Static platform)

position : (Float, Float) -> Tile -> Form -> Form
position (w,h) tile =
  case tile of
    Static platform -> move (platform.x+platform.w/2, platform.y+platform.h/2-h/2+base) 
    Moving mario -> move (mario.x, mario.y+mario.h/2-h/2+base)

base = 50

-- SIGNALS

main : Signal Element
main =
  let states = Signal.foldp updateWorld marios input
  in
     Signal.map2 viewWorld Window.dimensions states

input : Signal Update
input =
  let delta = Signal.map (\t -> t/20) (fps 30)
      deltaArrows = Signal.map2 (,) delta Keyboard.arrows
      moves = Signal.map Move (Signal.sampleOn delta deltaArrows)
      spawns = Signal.map Spawn (Signal.Time.dropWithin second Keyboard.space)
  in
      Signal.merge moves spawns
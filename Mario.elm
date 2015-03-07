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

-- MODEL

type alias World = List Character
type alias Pending = List (Float, Keys)

type Character = Active Figure | Sleeping Figure Pending | Ghost Figure Pending

type alias Figure = Tile
    { vx : Float
    , vy : Float
    , dir : Direction
    }

type alias Terrain = Tile {}

type alias Tile a =
    { a |
      w : Float
    , h : Float
    , x : Float
    , y : Float
    }

left : Tile a -> Float
left = .x

right : Tile a -> Float
right t = t.x + t.w

top : Tile a -> Float
top t = t.y + t.h

bottom = .y

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }

mario : Figure
mario =
  { x = 0 , y = 0 , w = 16 , h = 20 , vx = 0 , vy = 0 , dir = Right }

marios : World
marios = [Active mario, Sleeping mario []]

decor : List Terrain
decor = [ {w = 30, h = 50, x = 20, y = 0},
        {w = 30, h = 20, x = 20, y = 85},
        {w = 30, h = 40, x = 55, y = 0}
        -- the ground
        ,{w = 99999, h = 48, x = -9999, y = -48}
        -- the far right
        ,{w = 0, h = 9999, x = 9999, y = 0}
        -- the far left
        ,{w = 0, h = 9999, x = -9999, y = 0}
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

-- UPDATE

updateWorld : (Float, Keys, Bool) -> World -> World
updateWorld =
  map << update

update : (Float, Keys, Bool) -> Character -> Character
update (dt, keys, sp) mario' =
    case mario' of 
      Active m -> Active (updateActive (dt,keys) m)
      Sleeping m x -> if sp then Ghost m x else Sleeping m (append x [(dt,keys)])
      Ghost m [] -> Ghost (updateActive (dt,{x=0,y=0}) m) []
      Ghost m x -> Ghost (updateActive (head x) m) (tail x)

updateActive (dt,keys) mario =
    let n = 6 in
    mario
        |> jump keys
        |> walk keys
        |> iterate (physics (dt/n)) n        
        |> Debug.watch "mario"

jump : Keys -> Figure -> Figure
jump keys mario =
    if keys.y > 0 && mario.vy == 0
      then { mario | vy <- 4.0 }
      else mario

physics dt mario =
  let newx = mario.x + dt * mario.vx
      newy = mario.y + dt * mario.vy
      xymario = { mario | x <- newx, y <- newy }
      y_mario = { mario | y <- newy - 0.01} -- test whether Mario is supported
      x_mario = { mario | x <- newx }
      support = Debug.watch "floor" <| filter (collidingWith y_mario) decor
      newv = if any (collidingWith y_mario) decor then 0 else mario.vy - dt/8
      newmario = firstNonColliding [xymario, y_mario, x_mario, mario]
  in
     {newmario | vy <- newv}

-- maybe not ideal, Elm being non-lazy
firstNonColliding : List Figure -> Figure
firstNonColliding list =
  head <| filter (\ mario -> not (colliding mario)) list

colliding mario =
  any (collidingWith mario) decor

collidingWith : Tile a -> Tile b -> Bool
collidingWith mario pl =
  sameLevelAs mario pl && sameColumnAs mario pl 

sameColumnAs : Tile a -> Tile b -> Bool
sameColumnAs mario pl =
  (left pl, right pl) `intersects` (mario.x - mario.w/2, mario.x + mario.w/2)

sameLevelAs : Tile a -> Tile b -> Bool
sameLevelAs mario pl =
  (bottom pl, top pl) `intersects` (bottom mario, top mario)

intersects (min1, max1) (min2, max2) =
  between min1 max1 min2 ||
  between min2 max2 min1

rightOf : Tile a -> Tile b -> Bool
rightOf mario pl =
  mario.x <= left pl       

leftOf : Tile a -> Tile b -> Bool
leftOf mario pl =
  mario.x >= right pl

lowerThan mario pl =
  mario.y >= top pl

rightObstacles : Figure -> List Terrain
rightObstacles mario =
  filter (sameLevelAs mario `and` rightOf mario) (sortBy left decor)

leftObstacles : Figure -> List Terrain
leftObstacles mario =
  filter (sameLevelAs mario `and` leftOf mario) (sortBy right decor)

lowObstacles : Figure -> List Terrain
lowObstacles mario =
  filter (sameColumnAs mario `and` lowerThan mario) (sortBy top decor)

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
viewWorld dims world =
  layers (map (view dims) world) 

view : (Int, Int) -> Character -> Element
view (w',h') mario' =
  case mario' of
    Active mario -> viewActive (w',h') "mario" mario
    Ghost mario _ -> viewActive (w',h') "ghost" mario
    Sleeping _ _ -> empty

viewActive : (Int, Int) -> String -> Figure -> Element
viewActive (w',h') who mario =
  let (w,h) = (toFloat w', toFloat h')

      verb =
        if  | abs mario.vy > 0 -> "jump"
            | mario.vx /= 0    -> "walk"
            | otherwise        -> "stand"

      dir =
        case mario.dir of
          Left -> "left"
          Right -> "right"

      src = "imgs/" ++ who ++ "/"++ verb ++ "/" ++ dir ++ ".gif"

      marioImage = image 35 35 src

      -- 50 is "real" ground height, 5 is margin below Mario's feet, 35/2 is half his height
      groundY = 50 - 5 + 35/2 - h/2 

      position = (mario.x, mario.y + groundY)
  in
      collage w' h' <|
        [ marioImage
               |> toForm
               |> Debug.trace "mario"
               |> move position
        ]

viewDecor : (Int, Int) -> Element
viewDecor (w',h') =
  let (w,h) = (toFloat w', toFloat h')
  in collage w' h' <| displayDecor (w,h)

displayDecor : (Float, Float) -> List Form
displayDecor (w,h) =
          append 
          [ rect w h |> filled (rgb 174 238 238) ]
          (displayPlatforms (w,h))

displayPlatforms : (Float, Float) -> List Form
displayPlatforms (w,h) = map (displayPlatform (w,h)) decor

displayPlatform : (Float, Float) -> Terrain -> Form
displayPlatform (w,h) platform =
              rect platform.w platform.h
              |> filled (if platform.y < 0 then green else red)
              |> move (platform.x + platform.w/2, platform.y+platform.h/2-h/2+49)

-- SIGNALS

main : Signal Element
main =
  let states = Signal.foldp updateWorld marios input
      view1 = Signal.map viewDecor Window.dimensions
      view2 = Signal.map2 viewWorld Window.dimensions states
  in
     Signal.Extra.mapMany layers [view1, view2]

input : Signal (Float, Keys, Bool)
input =
  let delta = Signal.map (\t -> t/20) (fps 30)
      deltaArrows = Signal.map3 (,,) delta Keyboard.arrows Keyboard.space
  in
      Signal.sampleOn delta deltaArrows

import Keyboard
import Window
import Debug
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Time (fps)
import Signal
import List (..)

-- MODEL

type alias World = List Sprite

type alias Sprite =
    { x   : Float
    , y   : Float
    , w   : Float
    , h   : Float
    , vx  : Float
    , vy  : Float
    , dir : Direction
    }

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }

start_state : World
start_state = [
    { x = 0
    , y = 0 
    , w = 16
    , h = 26
    , vx = 0
    , vy = 0
    , dir = Right
    },
    -- the floor
    { x = -999
    , y = 0 
    , w = 2*999
    , h = 50
    , vx = 0
    , vy = 0
    , dir = Right
    }]


-- UPDATE

step : (Float, Keys) -> World -> World
step move world =
  map (stepOne move) world

stepOne : (Float, Keys) -> Sprite -> Sprite
stepOne (dt, keys) mario =
    mario
        |> jump keys
        |> walk keys
        |> physics dt
        |> Debug.watch "mario"

jump : Keys -> Sprite -> Sprite
jump keys mario =
    if keys.y > 0 && mario.vy == 0 then { mario | vy <- 6.0 } else mario

physics : Float -> Sprite -> Sprite
physics dt mario =
    { mario |
        x <- mario.x + dt * mario.vx,
        y <- max 0 (mario.y + dt * mario.vy),
        vy <- if mario.y > 0 then mario.vy - dt/4 else 0
    }

walk : Keys -> Sprite -> Sprite
walk keys mario =
    { mario |
        vx <- toFloat keys.x,
        dir <- if | keys.x < 0 -> Left
                  | keys.x > 0 -> Right
                  | otherwise  -> mario.dir
    }


-- DISPLAY

display : (Int, Int) -> World -> Element
display (w',h') world =
  collage w' h' (concatMap (displayOne (w',h')) (reverse world))

displayOne : (Int, Int) -> Sprite -> List Form
displayOne (w',h') mario =
  let (w,h) = (toFloat w', toFloat h')

      verb = if | mario.y  >  0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"

      dir = case mario.dir of
              Left -> "left"
              Right -> "right"

      src  = "imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"

      marioImage = image 35 35 src

      base = 50
  in
          [ rect w h
              |> filled (rgb 174 238 238)
          , rect w 50
              |> filled (rgb 74 167 43)
              |> move (0, 24 - h/2)
          , marioImage
              |> toForm
              |> Debug.trace "mario"
              |> move (mario.x, mario.y+mario.h/2-h/2+base)
          ]


-- SIGNALS

main : Signal Element
main =
  let states = Signal.foldp step start_state input
  in
    Signal.map2 display Window.dimensions states

input : Signal (Float, Keys)
input =
  let delta = Signal.map (\t -> t/20) (fps 30)
      deltaArrows = Signal.map2 (,) delta Keyboard.arrows
  in
      Signal.sampleOn delta deltaArrows
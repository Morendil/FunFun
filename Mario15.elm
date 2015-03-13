import Keyboard
import Window
import Debug
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import List exposing (foldr,repeat,drop,map,filter,isEmpty,concatMap,reverse)

-- GENERIC

between : number -> number -> number -> Bool
between min max x =
  x >= min && x < max

intersects (min1, max1) (min2, max2) =
  between min1 max1 min2 ||
  between min2 max2 min1

iterate : (a -> a) -> Int -> a -> a
iterate f n =
  foldr (<<) identity (repeat n f)

-- MODEL

type alias World = List Sprite

type Sprite = Sky | Platform PlatformSprite | Player BasicSprite

type alias BasicSprite =
    { x   : Float
    , y   : Float
    , w   : Float
    , h   : Float
    , vx  : Float
    , vy  : Float
    , dir : Direction
    }

type alias PlatformSprite =
    { x   : Float
    , y   : Float
    , w   : Float
    , h   : Float
    , c   : Color
    }

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }

start_mario = Player { x = 0
    , y = 0 
    , w = 16
    , h = 26
    , vx = 0
    , vy = 0
    , dir = Right
    }

start_state : World
start_state = [
    start_mario,
    Platform { x = 40 , y = 20 , w = 20 , h = 20 , c = red },
    Platform { x = 60 , y = 30 , w = 20 , h = 4 , c = blue },
    -- the floor
    Platform { x = 0 , y = 0 , w = 9999 , h = 50 , c = rgb 74 167 43 },
    Sky ]

-- UPDATE

step : (Float, Keys) -> World -> World
step move world =
  map (stepOne move world) world
  
stepOne : (Float, Keys) -> World -> Sprite -> Sprite
stepOne dims world sprite = case sprite of
  Player sprite' -> Player (stepPlayer dims world sprite')
  _ -> sprite

stepPlayer : (Float, Keys) -> World -> BasicSprite -> BasicSprite
stepPlayer (dt, keys) world mario =
    let n = 6 in
    mario
        |> jump keys
        |> walk keys
        |> iterate (physics (dt/n) world) n

jump : Keys -> BasicSprite -> BasicSprite
jump keys mario =
    if keys.y > 0 && mario.vy == 0 then { mario | vy <- 4.0 } else mario

blocks mario p =
  let mlft = mario.x-mario.w/2
      mrgt = mario.x+mario.w/2
      mtop = mario.y+mario.h
      mbot = mario.y
  in case p of
    Platform pl ->
      let plft = pl.x-pl.w/2
          prgt = pl.x+pl.w/2
          ptop = pl.y
          pbot = pl.y-pl.h
      in
        if pl.c == blue then intersects (plft,prgt) (mlft,mrgt) && intersects (ptop-2,ptop) (mbot,mbot+2) && mario.vy <= 0
                        else intersects (plft,prgt) (mlft,mrgt) && intersects (pbot,ptop) (mbot,mtop)
    _ -> False

physics : Float -> World -> BasicSprite -> BasicSprite
physics dt world mario =
    let newx = mario.x + dt * mario.vx
        newy = mario.y + dt * (mario.vy - 0.01) -- fudge factor to "test" our feet
        support = filter (blocks {mario | y <- newy}) world
        blockers = filter (blocks {mario | x <- newx}) world
        newv = if (isEmpty support) then mario.vy - dt/8 else 0
    in
    Debug.watch "mario" <| 
    { mario |
        x <- if (isEmpty blockers) then newx else mario.x,
        y <- if (isEmpty support) then newy else mario.y,
        vy <- newv
    }

walk : Keys -> BasicSprite -> BasicSprite
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
  let (w,h) = (toFloat w', toFloat h')
  in collage w' h' (concatMap (displayOne (w,h)) (reverse world))

displayOne : (Float, Float) -> Sprite -> List Form
displayOne dims sprite = case sprite of
  Player basic -> displayPlayer dims basic
  Platform basic -> displayPlatform dims basic
  Sky -> displaySky dims

displayPlayer : (Float, Float) -> BasicSprite -> List Form
displayPlayer (w,h) mario =
  let verb = if | (abs mario.vy) > 0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"

      dir = case mario.dir of
              Left -> "left"
              Right -> "right"

      src  = "imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"

      marioImage = image 35 35 src

      base = 50
  in [marioImage
        |> toForm
        |> Debug.trace "mario"
        |> move (mario.x, mario.y + mario.h/2 + base - h/2) ]

displayPlatform : (Float, Float) -> PlatformSprite -> List Form
displayPlatform (w,h) pl =
  let pw = min w pl.w
      ph = min h pl.h
      base = 50
  in
          [ rect pw ph
              |> filled pl.c
              |> move (pl.x, pl.y - ph/2 + base - h/2) ]

displaySky : (Float, Float) -> List Form
displaySky (w,h) =
  [ rect w h |> filled (rgb 174 238 238) ]          

-- SIGNALS

main : Varying Element
main =
  let states = Stream.fold step start_state inputs
  in
    Varying.map2 display Window.dimensions states

inputs : Stream (Float, Keys)
inputs =
  let delta = Stream.map (\t -> t/20) (fps 30)
  in Stream.sample (\ x y -> (y,x)) Keyboard.arrows delta
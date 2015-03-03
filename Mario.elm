import Color (..)
import Debug
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time (..)
import List (..)
import Window


-- MODEL

type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    }

type alias Terrain =
    { w : Float
    , h : Float
    , x : Float
    , y : Float
    }

left : Terrain -> Float
left = .x

right : Terrain -> Float
right t = t.x + t.w

top : Terrain -> Float
top t = t.y + t.h

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }


mario : Model
mario =
    { x = 0
    , y = 0 
    , vx = 0
    , vy = 0
    , dir = Right
    }

decor : List Terrain
decor = [
        {w = 30,
        h = 50,
        x = 0,
        y = 0},
        {w = 30,
        h = 40,
        x = 0,
        y = 70}
        -- the ground
        ,{w = 99999,
        h = 0,
        x = -9999,
        y = 0}
        -- the far right
        ,{w = 0,
        h = 9999,
        x = 9999,
        y = 0}
        -- the far left
        ,{w = 0,
        h = 9999,
        x = -9999,
        y = 0}
        ]

-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) mario =
    mario
        |> gravity dt
        |> jump keys
        |> walk keys
        |> physics dt
        |> Debug.watch "mario"


jump : Keys -> Model -> Model
jump keys mario =
    if keys.y > 0 && mario.vy == 0
      then { mario | vy <- 4.0 }
      else mario


gravity : Float -> Model -> Model
gravity dt mario =
  let miny = top <| head <| reverse <| lowObstacles mario
    in
    { mario |
        vy <- if mario.y > miny then mario.vy - dt/8 else 0
    }

locate : Model -> Terrain
locate mario = head (filter (marioOver mario) decor)

marioOver : Model -> Terrain -> Bool
marioOver mario platform =
  mario.x > platform.x && mario.x < platform.x+platform.w 

physics : Float -> Model -> Model
physics dt mario =
    let maxx = left <| head <| Debug.watch "right" <| rightObstacles mario
        minx = right <| head <| reverse <| leftObstacles mario
        orgx = mario.x + dt * mario.vx
        maxy = 9999
        miny = top <| head <| reverse <| lowObstacles mario
        orgy = mario.y + dt * mario.vy
    in
    { mario |
        x <- max minx (min maxx orgx),
        y <- max miny orgy
    }

rightObstacles : Model -> List Terrain
rightObstacles mario =
  filter (\ pl -> mario.y >= pl.y && mario.y+2 < top pl && mario.x <= left pl) (sortBy left decor)

leftObstacles : Model -> List Terrain
leftObstacles mario =
  filter (\ pl -> mario.y >= pl.y && mario.y+2 < top pl && mario.x >= right pl) (sortBy right decor)

lowObstacles : Model -> List Terrain
lowObstacles mario =
  filter (\ pl -> mario.x >= pl.x && mario.x < right pl && mario.y >= top pl) (sortBy top decor)

walk : Keys -> Model -> Model
walk keys mario =
    { mario |
        vx <- toFloat keys.x,
        dir <-
          if  | keys.x < 0 -> Left
              | keys.x > 0 -> Right
              | otherwise  -> mario.dir
    }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') mario =
  let (w,h) = (toFloat w', toFloat h')

      verb =
        if  | mario.y  >  0 -> "jump"
            | mario.vx /= 0 -> "walk"
            | otherwise     -> "stand"

      dir =
        case mario.dir of
          Left -> "left"
          Right -> "right"

      src = "imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"

      marioImage = image 35 35 src

      groundY = 50 - 5 + 35/2 - h/2

      position = (mario.x, mario.y + groundY)
  in
      collage w' h' (append (displayDecor (w,h))
          [ marioImage
              |> toForm
              |> Debug.trace "mario"
              |> move position
          ])

displayDecor : (Float, Float) -> List Form
displayDecor (w,h) =
          append 
          [ rect w h
              |> filled (rgb 174 238 238)
          , rect w 50
              |> filled (rgb 74 167 43)
              |> move (0, 24 - h/2)
          ]
          (displayPlatforms (w,h))

displayPlatforms : (Float, Float) -> List Form
displayPlatforms (w,h) = map (displayPlatform (w,h)) decor

displayPlatform : (Float, Float) -> Terrain -> Form
displayPlatform (w,h) platform =
              rect platform.w platform.h
              |> filled red
              |> move (platform.x + platform.w/2, platform.y+platform.h/2-h/2+49)

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update mario input)


input : Signal (Float, Keys)
input =
  let delta = Signal.map (\t -> t/20) (fps 30)
      deltaArrows =
          Signal.map2 (,) delta (Signal.map (Debug.watch "arrows") Keyboard.arrows)
  in
      Signal.sampleOn delta deltaArrows


-- todo:
-- jump on top of platforms - OKish
-- get blocked by platforms on the sides - OK
-- BUG(?) - with two stacked platforms only the lower constrains movement
  -- actually no - just that platforms only constrain Mario's feet!
-- BUG - can't jump from the top of a high platform -- fixed by rewriting gravity
import Keyboard
import Window
import Debug
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Time (fps,second)
import Signal
import Signal.Time
import List (..)
-- App imports
import Generic (..)

-- MODEL

type alias World = List Sprite
type alias History = List (Float,Keys)
type alias Past = List BasicSprite
type alias Behavior = (Float, Keys) -> World -> PlatformSprite -> PlatformSprite

type Sprite = Sky | Platform PlatformSprite Behavior | Cloud PlatformSprite Behavior | Player BasicSprite History Past | Ghost BasicSprite History History

type alias Common a =
    { a |
      x   : Float
    , y   : Float
    , w   : Float
    , h   : Float
    , vx  : Float
    , vy  : Float
    }

type alias BasicSprite =
    Common { dir : Direction }

type alias PlatformSprite =
    Common { t : Float , c : Color }

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }

column common = (common.x-common.w/2,common.x+common.w/2)
row common = (common.y,common.y+common.h)
feet common = (common.y+common.h-2,common.y+common.h)
hair common = (common.y,common.y+2)

start_mario = { x = 0 , y = 0 , w = 16 , h = 26 , vx = 0 , vy = 0 , dir = Right }

pink = rgb 150 10 50

start_state : World
start_state = [    
    Player start_mario [] [],
    Platform { x = -55 , y = 80 , w = 20 , h = 20 , c = red , t = 0, vx = 0, vy = 0} nothing,
    Platform { x = -80 , y = 60 , w = 20 , h = 20 , c = red , t = 0, vx = 0, vy = 0} nothing,
    Cloud { x = 0 , y = 70 , w = 20 , h = 20 , c = red , t = 0, vx = 0, vy = 0} sway,
    Platform { x = -80 , y = 60 , w = 20 , h = 20 , c = red , t = 0, vx = 0, vy = 0} nothing,
    Platform { x = 40 , y = 0 , w = 20 , h = 20 , c = blue , t = 0, vx = 0, vy = 0} nothing,
    Platform { x = 40 , y = 160 , w = 20 , h = 20 , c = green , t = 0, vx = 0, vy = 0} nothing,

    Cloud { x = -40 , y = 120 , w = 20 , h = 20 , c = purple , t = 0, vx = 0, vy = 0} lift,
    Platform { x = -60 , y = 26 , w = 20 , h = 4 , c = blue , t = 0, vx = 0, vy = 0} change,
    -- the floor
    Platform { x = 0 , y = -50 , w = 9999 , h = 50 , c = rgb 74 167 43 , t = 0, vx = 0, vy = 0} nothing,
    Sky ]

-- UPDATE

type Update = Rewind Bool | Spawn Bool | Move (Float, Keys)

step : Update -> World -> World
step u world =
  let ghost one = case one of
        Player _ history _ -> let backward = reverse history in Ghost start_mario backward backward
        _ -> one
      rewind one = case one of
        Player sprite (h::hs) (x::xs) -> Player x hs xs
        _ -> one
      reset one = case one of
        Player _ _ _ -> Player start_mario [] []
        Ghost _ history copy -> Ghost start_mario copy copy
        _ -> one
      player = head world
  in
  case u of
    Spawn True -> reset player :: ghost player :: map reset (tail world)
    Rewind True -> rewind player :: tail world
    Move move -> mapAllBut (stepOne move) world
    _ -> world

nothing _ _ sprite = sprite

sway (dt,_) world sprite = {sprite | t <- sprite.t+dt, x <- sprite.x + dt * sprite.vx, vx <- sin(sprite.t/50)}
lift (dt,_) world sprite = {sprite | t <- sprite.t+dt, y <- sprite.y + dt * sprite.vy, vy <- sin(sprite.t/50)}

change fff world sprite =
  let t = Debug.watch "time" sprite.t
  in if sprite.t > 100 then lift fff world sprite else sway fff world sprite

stepOne : (Float, Keys) -> World -> Sprite -> Sprite
stepOne move world sprite = case sprite of
  Player sprite' h past -> Player (stepPlayer move world sprite') (move :: h) (sprite' :: past)
  Ghost sprite' [] copy -> let (dt,keys) = move in Ghost (stepPlayer (dt,{x=0,y=0}) world sprite') [] copy
  Ghost sprite' (h::hs) copy -> Ghost (stepPlayer h world sprite') hs copy
  Platform sprite' behavior -> Platform (behavior move world sprite') behavior
  Cloud sprite' behavior -> Cloud (behavior move world sprite') behavior
  _ -> sprite

stepPlayer : (Float, Keys) -> World -> BasicSprite -> BasicSprite
stepPlayer (dt, keys) world mario =
    let n = 6 in
    mario
        |> jump keys
        |> walk keys
        |> iterate (physics (dt/n) world) n

jump : Keys -> BasicSprite -> BasicSprite
jump keys mario=
    if keys.y > 0 && mario.vy == 0 then { mario | vy <- 4.0} else mario

walk : Keys -> BasicSprite -> BasicSprite
walk keys mario =
    { mario |
        vx <- toFloat keys.x,
        dir <- if | keys.x < 0 -> Left
                  | keys.x > 0 -> Right
                  | otherwise  -> mario.dir
    }

blocks mario p =
  case p of
    Platform g _ -> intersects (column g) (column mario) && intersects (row g) (row mario)
    Cloud g _ -> intersects (column g) (column mario) && intersects (feet g) (hair mario) && mario.vy <= 0
    Ghost g _ _ -> intersects (column g) (column mario) && intersects (feet g) (hair mario) && mario.vy <= 0
    Player g _ _ -> intersects (column g) (column mario) && intersects (feet g) (hair mario) && mario.vy <= 0
    _ -> False

physics : Float -> World -> BasicSprite -> BasicSprite
physics dt world mario =
    let newx = mario.x + dt * mario.vx
        newy = mario.y + dt * (mario.vy - 0.01) -- fudge factor to "test" our feet
        support = filter (blocks {mario | y <- newy}) world
        newvy = if (isEmpty support) then mario.vy - dt/8 else 0
        extrax = if (isEmpty support) || (mario.vx /= 0) then 0 else
          case head support of
            Player pl _ _ -> dt * pl.vx
            Platform pl _ -> dt * pl.vx
            Cloud pl _ -> dt * pl.vx
            _ -> 0
        extray = if (isEmpty support) || (mario.vy /= 0) then 0 else
          case head support of
            Player pl _ _ -> dt * pl.vy
            Platform pl _ -> dt * pl.vy
            Cloud pl _ -> dt * pl.vy
            _ -> 0
        blockers = filter (blocks {mario | x <- newx + extrax}) world
    in    
    { mario |
        x <- if (isEmpty blockers) then newx+extrax else mario.x,
        y <- if (isEmpty support) then newy else mario.y+extray,
        vy <- newvy
    }

-- DISPLAY

display : (Int, Int) -> World -> Element
display (w',h') world =
  let (w,h) = (toFloat w', toFloat h')
  in collage w' h' (map (displayOne (w,h)) (reverse world))

displayOne : (Float, Float) -> Sprite -> Form
displayOne dims sprite = case sprite of
  Player shape _ _ -> Debug.trace "mario" <| displayPlayer dims "ghost" (Debug.watch "mario" shape)
  Ghost shape _ _ -> displayPlayer dims "ghost" shape
  Platform shape _ -> displayPlatform dims shape
  Cloud shape _ -> displayPlatform dims shape
  Sky -> displaySky dims

displayPlayer : (Float, Float) -> String -> BasicSprite -> Form
displayPlayer dims who mario =
  let verb = if | (abs mario.vy) > 0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"

      dir = case mario.dir of
              Left -> "left"
              Right -> "right"

      src  = "imgs/" ++ who ++ "/"++ verb ++ "/" ++ dir ++ ".gif"

      marioImage = image 35 35 src
  in
    marioImage
      |> toForm
      |> move (displayCoords dims mario)

displayPlatform : (Float, Float) -> PlatformSprite -> Form
displayPlatform (w,h) pl =
  let pw = min w pl.w
      ph = min h pl.h
  in
    rect pw ph
      |> filled pl.c
      |> move (displayCoords (w,h) pl)

displaySky : (Float, Float) -> Form
displaySky (w,h) =
  rect w h |> filled (rgb 174 238 238)

displayCoords (w,h) common = let base = 50 in (common.x, common.y + common.h/2 + base - h/2)

-- SIGNALS

main : Signal Element
main =
  let states = Signal.foldp step start_state input
  in
    Signal.map2 display Window.dimensions states

input : Signal Update
input =
  let delta = Signal.map (\t -> t/20) (fps 30)
      deltaArrows = Signal.map2 (,) delta Keyboard.arrows
      moves = Signal.map Move (Signal.sampleOn delta deltaArrows)
      spawns = Signal.map Spawn (Signal.Time.dropWithin second Keyboard.space)
      rewind = Signal.map Rewind (Signal.keepWhen Keyboard.shift True (Signal.sampleOn delta Keyboard.shift))
  in
      Signal.mergeMany [rewind, spawns, moves]
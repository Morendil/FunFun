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

-- GENERIC

between min max x =
  x >= min && x < max

intersects (min1, max1) (min2, max2) =
  between min1 max1 min2 ||
  between min2 max2 min1

iterate : (a -> a) -> Int -> a -> a
iterate f n =
  foldr (<<) identity (repeat n f)

mapAllBut : (List a -> a -> a) -> List a -> List a
mapAllBut f l =
  map2 f (dropEach l) l

dropEach : List a -> List (List a)
dropEach l =
  case l of
    [] -> []
    x :: [] -> [[]]
    x :: xs -> append [xs] (map (append [x]) (dropEach xs))

-- MODEL

type alias World = List Sprite
type alias History = List (Float,Keys)
type alias Behavior = (Float, Keys) -> World -> PlatformSprite -> PlatformSprite

type Sprite = Sky | Platform PlatformSprite Behavior | Player BasicSprite History | Ghost BasicSprite History History

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

start_mario = { x = 0 , y = 0 , w = 16 , h = 26 , vx = 0 , vy = 0 , dir = Right }

start_state : World
start_state = [
    Player start_mario [],
    Platform { x = 40 , y = 20 , w = 20 , h = 20 , c = red , t = 0, vx = 0, vy = 0} nothing,
    Platform { x = -80 , y = 80 , w = 20 , h = 20 , c = red , t = 0, vx = 0, vy = 0} nothing,
    Platform { x = -60 , y = 30 , w = 20 , h = 4 , c = blue , t = 0, vx = 0, vy = 0} sway,
    -- the floor
    Platform { x = 0 , y = 0 , w = 9999 , h = 50 , c = rgb 74 167 43 , t = 0, vx = 0, vy = 0} nothing,
    Sky ]

-- UPDATE

type Update = Spawn Bool | Move (Float, Keys)

step : Update -> World -> World
step u world =
  let ghost = case (head world) of
        Player _ history -> Ghost start_mario (reverse history) (reverse history)
      reset one = case one of
        Player _ _ -> Player start_mario []
        Ghost _ history copy -> Ghost start_mario copy copy
        _ -> one
  in
  case u of
    Spawn True -> reset (head world) :: ghost :: map reset (tail world)
    Move move -> mapAllBut (stepOne move) world
    _ -> world

nothing _ _ sprite = sprite
sway (dt,_) world sprite = {sprite | t <- sprite.t+dt, x <- sprite.x + dt * sprite.vx, vx <- sin(sprite.t/50)}

stepOne : (Float, Keys) -> World -> Sprite -> Sprite
stepOne move world sprite = case sprite of
  Player sprite' h -> Player (stepPlayer move world sprite') (move :: h)
  Ghost sprite' [] copy -> let (dt,keys) = move in Ghost (stepPlayer (dt,{x=0,y=0}) world sprite') [] copy
  Ghost sprite' h copy -> Ghost (stepPlayer (head h) world sprite') (tail h) copy
  Platform sprite behavior -> Platform (behavior move world sprite) behavior
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
      collides g =
        let glft = g.x-g.w/2
            grgt = g.x+g.w/2
            gtop = g.y+g.h
        in
          intersects (glft,grgt) (mlft,mrgt) && intersects (gtop-2,gtop) (mbot,mbot+2) && mario.vy <= 0        
  in case p of
    Platform pl _ ->
      let plft = pl.x-pl.w/2
          prgt = pl.x+pl.w/2
          ptop = pl.y
          pbot = pl.y-pl.h
      in
        if pl.c == blue then intersects (plft,prgt) (mlft,mrgt) && intersects (ptop-2,ptop) (mbot,mbot+2) && mario.vy <= 0
                        else intersects (plft,prgt) (mlft,mrgt) && intersects (pbot,ptop) (mbot,mtop)
    Ghost g _ _ -> collides g
    Player p _ -> collides p
    _ -> False

physics : Float -> World -> BasicSprite -> BasicSprite
physics dt world mario =
    let newx = mario.x + dt * mario.vx
        newy = mario.y + dt * (mario.vy - 0.01) -- fudge factor to "test" our feet
        support = filter (blocks {mario | y <- newy}) world
        newvy = if (isEmpty support) then mario.vy - dt/8 else 0
        extrax = if (isEmpty support) || (mario.vx /= 0) then 0 else
          case head support of
            Player pl _ -> dt * pl.vx
            Platform pl _ -> dt * pl.vx
            _ -> 0
        blockers = filter (blocks {mario | x <- newx + extrax}) world
    in
    Debug.watch "mario" <| 
    { mario |
        x <- if (isEmpty blockers) then newx+extrax else mario.x,
        y <- if (isEmpty support) then newy else mario.y,
        vy <- newvy
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
  in collage w' h' (map (displayOne (w,h)) (reverse world))

displayOne : (Float, Float) -> Sprite -> Form
displayOne dims sprite = case sprite of
  Player shape _ -> Debug.trace "mario" <| displayPlayer dims "mario" shape
  Ghost shape _ _ -> displayPlayer dims "ghost" shape
  Platform shape _ -> displayPlatform dims shape
  Sky -> displaySky dims

displayPlayer : (Float, Float) -> String -> BasicSprite -> Form
displayPlayer (w,h) who mario =
  let verb = if | (abs mario.vy) > 0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"

      dir = case mario.dir of
              Left -> "left"
              Right -> "right"

      src  = "imgs/" ++ who ++ "/"++ verb ++ "/" ++ dir ++ ".gif"

      marioImage = image 35 35 src

      base = 50
  in
    marioImage
      |> toForm
      |> move (mario.x, mario.y + mario.h/2 + base - h/2)

displayPlatform : (Float, Float) -> PlatformSprite -> Form
displayPlatform (w,h) pl =
  let pw = min w pl.w
      ph = min h pl.h
      base = 50
  in
    rect pw ph
      |> filled pl.c
      |> move (pl.x, pl.y - ph/2 + base - h/2)

displaySky : (Float, Float) -> Form
displaySky (w,h) =
  rect w h |> filled (rgb 174 238 238)

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
  in
      Signal.merge moves spawns
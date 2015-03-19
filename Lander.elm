import Signal
import Signal.Extra
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Debug
import Window

-- Model

start (w,h) = {w = w, h = h}

-- Update

update (w,h) world = 
    let wh = Debug.watch "dims" (w,h)
    in {world | w<-w, h<-h}

-- Display

display world =
    let (w',h') = (toFloat world.w, toFloat world.h)
    in collage world.w world.h [filled black (rect w' h')]

-- Signals

inputs = Window.dimensions

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
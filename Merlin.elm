import Graphics.Collage (..)
import Color (..)

import Signal
import Signal.Extra
import Window

-- Model

start (w,h) =
    {view={w=w,h=h}}

-- Update

update (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

display world =
    collage world.view.w world.view.h <| [filled black <| rect (toFloat world.view.w) (toFloat world.view.h)]

-- Signals

inputs = Window.dimensions

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states

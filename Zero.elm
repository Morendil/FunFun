module Zero where

import Graphics.Collage exposing (..)

import Signal
import Time exposing (fps)


-- Model

start = {}

-- Update

update u =
  identity

-- Display

display _ =
    collage 0 0 []

-- Signals

type Update = Frame Float

main =
    let frames = Signal.map Frame (fps 30)
        states = Signal.foldp update start frames
    in Signal.map display states
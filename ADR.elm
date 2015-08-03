module ADR where

import Html exposing (text, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Time exposing (fps)

import Css.Position exposing (..)
import Css.Dimension exposing (..)
import Css.Font exposing (family)

import Signal

import String
import Array
import Debug

entries = ["the room is freezing.","the fire is dead."]

notification string =
    div [] [text string]

notifications =
    div [style <| notificationsStyle []] <| List.map notification entries

notificationsStyle =
    position Absolute
    << top 20
    << width 200

bodyStyle =
    family "'Times New Roman', Times, serif"

main =
    div [style <| bodyStyle []] [notifications]
module ADR where

import Html exposing (text, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Time exposing (fps)

import Css.Position as Position exposing (position,top)
import Css.Padding as Padding
import Css.Dimension exposing (..)
import Css.Font exposing (family)
import Css.Margin exposing (bottom)

import Signal

import String
import Array
import Debug

entries = ["the room is freezing.","the fire is dead."]

notification string =
    div [style <| notificationStyle []] [text string]

notifications =
    div [style <| notificationsStyle []] <| List.map notification entries

notificationStyle =
    bottom 10

notificationsStyle =
    position Position.Absolute
    << top 20
    << width 200

bodyStyle =
    family "'Times New Roman', Times, serif"

wrapperStyle =
    width 700
    << Padding.all 20 0 0 220
    << position Position.Relative

main =
    div [style <| bodyStyle []] [
        div [style <| wrapperStyle []] [
            notifications
        ]
    ]
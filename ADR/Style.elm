module ADR.Style where

import Html exposing (div)

import Html.Attributes as Attributes

import Css.Padding as Padding
import Css.Float as Float exposing (float)
import Css.Position as Position exposing (position,overflow,top,left)
import Css.Dimension exposing (..)
import Css.Font exposing (family)
import Css.Margin exposing (bottom)

notificationStyle =
    bottom 10

notificationsStyle =
    position Position.Absolute
    << left 0
    << top 20
    << width 200
    << height 700

mainStyle = 
  position Position.Relative
  << float Float.Left
  << width 700
  << height 700
  << overflow Position.Hidden

outerSliderStyle =
  position Position.Absolute

contentStyle =
  position Position.Relative
  << overflow Position.Hidden
  << height 700

wrapperStyle x =
    (width 700
    << Padding.all 20 0 0 220
    << position Position.Relative) (("margin","auto") :: x)

bodyStyle =
    family "'Times New Roman', Times, serif"

with styling id =
    div [Attributes.style <| styling [], Attributes.id id]
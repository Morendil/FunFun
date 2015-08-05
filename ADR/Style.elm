module ADR.Style where

import Html exposing (div)

import Html.Attributes as Attributes

import Css.Dimension exposing (..)

import Css.Position as Position exposing (position,overflow,top,left)
import Css.Float as Float exposing (float)
import Css.Cursor as Cursor exposing (cursor)

import Css.Border as Border
import Css.Border.Style as Style
import Css.Text as Text
import Css.Font as Font
import Css.Padding as Padding
import Css.Margin as Margin

notificationStyle =
    Margin.bottom 10

notificationsStyle =
    position Position.Absolute
    << left 0
    << top 20
    << width 200
    << height 700

buttonStyle =
  position Position.Relative
  << Text.align Text.Center
  << Border.width 1 1 1 1
  << Border.style Style.Solid
  << width 80
  << Margin.bottom 5
  << Padding.all 5 10 5 10
  << cursor Cursor.Pointer

headerButtonStyle =
  Font.size 18
  << float Float.Left
  << cursor Cursor.Pointer
  << Margin.left 0
  << Padding.left 0
  << Text.decoration Text.Underline

headerStyle =
  Padding.bottom 20
  << height 20

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
    Font.family "'Times New Roman', Times, serif"

with styling class =
    div [Attributes.style <| styling [], Attributes.class class]
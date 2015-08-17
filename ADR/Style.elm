module ADR.Style where

import Html exposing (div)

import Html.Attributes as Attributes

import Css.Dimension exposing (..)
import Color exposing (..)

import Css.Position as Position exposing (position,overflow,top,left)
import Css.Float as Float exposing (float)
import Css.Cursor as Cursor exposing (cursor)

import Css.Background as Background
import Css.Border as Border
import Css.Border.Left as BorderLeft
import Css.Border.Style as Style
import Css.Text as Text
import Css.Font as Font
import Css.Padding as Padding
import Css.Margin as Margin

noStyle =
  identity

notificationStyle =
    Margin.bottom 10

notificationsStyle =
    position Position.Absolute
    << left 0
    << top 20
    << width 200
    << height 700

disabledStyle =
  cursor Cursor.Default
  << Border.color (rgb 178 178 178)
  << Text.color (rgb 178 178 178)
  << Text.decoration Text.NoDecoration

cooldownStyle x =
  (position Position.Absolute
  << top 0
  << left 0
  << Position.zIndex -1
  << Background.color (rgb 221 221 221)) (("height","100%") :: x)

buttonStyle =
  position Position.Relative
  << Text.align Text.Center
  << Border.width 1 1 1 1
  << Border.style Style.Solid
  << width 80
  << Margin.bottom 5
  << Padding.all 5 10 5 10
  << cursor Cursor.Pointer

selectedHeaderStyle =
  Text.decoration Text.Underline

firstHeaderStyle =
  Font.size 18
  << float Float.Left
  << cursor Cursor.Pointer
  << Margin.left 0
  << Padding.left 0

laterHeadersStyle =
  Font.size 18
  << float Float.Left
  << cursor Cursor.Pointer
  << BorderLeft.style Style.Solid
  << BorderLeft.width 1
  << Margin.left 10
  << Padding.left 10

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

adding styling class styles =
    div [Attributes.style <| styling styles, Attributes.class class]
module ButtonBug where

import Html exposing (text, div)
import Html.Events exposing (onClick)

import Signal exposing (..)
import Time exposing (..)

type Action = Wait Time | Click Int

click : Mailbox Int
click = Signal.mailbox 1

time : Signal Time
time = every (7 * second)

page : Action -> Html.Html
page action =
    case action of
        Wait time -> div [onClick click.address 1] [text (toString time)]
        Click 1 -> div [onClick click.address 2] [text "clic!"]
        Click 2 -> div [] [text "clac!"]

input : Signal Action
input = merge (map Wait time) (map Click click.signal)

main : Signal Html.Html
main = map page input
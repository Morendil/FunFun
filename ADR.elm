module ADR where

import Html exposing (text, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Time exposing (fps)

import ADR.Style exposing (..)

import Signal

import String
import Array
import Debug

entries = ["the fire is dead.","the room is freezing."]

notification string =
    with notificationStyle "notification" [text string]

notifications =
    with notificationsStyle "notifications" <| List.map notification entries

content =
    with contentStyle "content" [
        with outerSliderStyle "outerSlider" [
            with mainStyle "main" [
                div [] [text "A Dark Room"]
            ]
        ]
    ]

main =
    with bodyStyle "body" [
        with wrapperStyle "wrapper" [
            content,
            notifications
        ]
    ]
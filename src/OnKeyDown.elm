module OnKeyDown exposing (onKeyDown)

import Html.Events exposing (on)

import Json.Decode exposing (Decoder, oneOf, field, int, map)

-- Decoder

keyCode : Decoder Int
keyCode =
  oneOf [ field "which" int -- Firefox support
        , field "keyCode" int ]

onKeyDown tagger =
  on "keydown" (map tagger keyCode)

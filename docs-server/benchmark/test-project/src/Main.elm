module Main exposing (main)

import Browser
import Bytes
import File
import Html exposing (Html, text)
import Http
import Json.Decode
import Json.Encode
import Parser
import Random
import Regex
import Svg
import Time
import Url


main : Html msg
main =
    text "benchmark"

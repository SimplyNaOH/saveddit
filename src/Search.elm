module Search exposing (..)

import Html exposing (Html)

type alias MatchedString = List (Bool, String)

type alias SearchResult a = (MatchedString, a)

decomposeMatch : String -> String -> MatchedString
decomposeMatch str source=
  let
    indices = String.indices str source
  in
    case indices of
      [] -> [(False, source)]
      x::_ -> [(False, String.slice 0 x source), (True, String.slice x (x + String.length str) source), (False, String.dropLeft (x + String.length str) source)]

search : String -> (a -> String) -> List a -> List (SearchResult a)
search str f elems =
  let
    readyToMatch = List.map (\elem -> (String.toLower <| f elem, elem)) elems
    matches = List.filter (\(toMatch, _) -> String.contains (String.toLower str) toMatch) readyToMatch
  in
    if String.isEmpty str then
      List.map (\elem -> ([(True, f elem)], elem)) elems
    else
      List.map (\(matched, elem) -> (decomposeMatch str matched, elem)) matches

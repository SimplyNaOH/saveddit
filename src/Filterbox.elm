module Filterbox exposing (..)

import Html exposing (Html, div, span, h1, a, input, text, li, i)
import Html.Keyed exposing (ul, ol)
import Html.Attributes exposing (id, class, classList, placeholder, href, value, style)
import Html.Events exposing (onInput, onClick, onBlur, onFocus, onMouseLeave, onMouseEnter)

import Dom exposing (focus, blur)

import Task

import OnKeyDown exposing (onKeyDown)

import Search exposing (SearchResult)

-- Model

type SubOver18 = SFW | Partial | NSFW

type alias SubredditInfo =
  { subreddit : String
  , numberOfItems : Int
  , over18 : SubOver18
  }

type alias ResultInfo =
  { selected : Int
  , results : List (SearchResult SubredditInfo)
  }

type alias MenuState =
  { mouseOver : Bool
  , open : Bool
}

type alias Model =
  { subreddits : List SubredditInfo
  , active : List String
  , searchString : String
  , results : ResultInfo
  , showNSFW : Bool
  , menuState : MenuState
  }

initialModel = Model [] [] "" (ResultInfo 0 []) False (MenuState False False)

-- Update

type Msg = SetSubreddits (List SubredditInfo)
         | ToggleSub String
         | ClearActive
         | Search String
         | KeyDown Int
         | ToggleNSFW
         | SetMouseOnMenu Bool
         | LostFocus
         | CloseMenu
         | OpenMenu
         | NoOp


(!!) (model, cmd1) cmd2 = model ! [cmd1, cmd2]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    checkNSFW = List.filter (\sub -> not (sub.over18 == NSFW) || model.showNSFW)
    results str = ResultInfo 0 << Search.search str (.subreddit) << checkNSFW <| model.subreddits
  in
    case msg of
    SetSubreddits subs -> ({ model | subreddits = subs}, Cmd.none)
    ToggleSub sub ->
      let newActive =
        if List.member sub model.active
        then List.filter ((/=) sub) model.active
        else model.active ++ [sub]
      in
        update (Search "") { model | active = newActive }
    ClearActive ->
      update (Search "") { model | active = [] }
    Search text ->
      ({ model | searchString = text, results = results text }, Cmd.none)
    KeyDown keycode ->
      let
        selectNext results =
          { results | selected = min (results.selected + 1) (List.length results.results - 1) }
        selectPrev results =
          { results | selected = max (results.selected - 1) 0 }
        selectedSub results =
          case List.take 1 <| List.drop results.selected results.results of
            [] -> "" -- should never happen!
            (_,x)::xs -> x.subreddit
      in
        case keycode of
          -- up-arrow
          38 -> ({ model | results = selectPrev model.results }, Cmd.none)
          -- down-arrow
          40 -> ({ model | results = selectNext model.results }, Cmd.none)
          -- enter
          13 -> update (ToggleSub <| selectedSub model.results) model !! Task.attempt (\x -> NoOp) (focus "active-box-textbox")
          -- esc
          27 -> (update CloseMenu << (\(a,b) -> a) << update (Search "") <| model) !! Task.attempt (\x -> NoOp) (blur "active-box-textbox")
          _ -> (model, Cmd.none)
    ToggleNSFW -> update (Search "") { model | showNSFW = not model.showNSFW }
    SetMouseOnMenu isOnMenu -> let oldMenuState = model.menuState
      in ({ model | menuState = {oldMenuState | mouseOver = isOnMenu} }, Cmd.none)
    LostFocus -> let oldMenuState = model.menuState
      in ({ model | menuState = {oldMenuState | open = if oldMenuState.mouseOver then True else False}}, Cmd.none)
    CloseMenu -> let oldMenuState = model.menuState
      in ({ model | menuState = {oldMenuState | open = False} }, Cmd.none)
    OpenMenu -> let oldMenuState = model.menuState
      in ({ model | menuState = {oldMenuState | open = True} }, Cmd.none)
    NoOp -> (model, Cmd.none)

-- View

activeView sub = [(sub, li [] [a [href "#0", class "active-box__sub", onClick <| ToggleSub sub, onMouseEnter <| SetMouseOnMenu True, onMouseLeave <| SetMouseOnMenu False] [text sub]]), (toString (sub ++ "-sep"), text " ")]

subView : Model -> Bool -> List (String, Html Msg)
subView model showNSFW=
  let
    checkNSFW = List.filter (\sub -> not (sub.over18 == NSFW) || model.showNSFW) model.subreddits
    classes i sub = classList [ ("subreddits-box__subreddit", True)
                        , ("subreddits-box--selected", i == model.results.selected && (not <| String.isEmpty model.searchString))
                        , ("subreddits-box--active", List.member sub.subreddit model.active)
                        , ("subreddits-box--partial", model.showNSFW && sub.over18 == Partial)
                        , ("subreddits-box--nsfw", model.showNSFW && sub.over18 == NSFW)
                        ]
    view i sub = (sub.subreddit, li [] [a [href "#0", classes i sub, onClick <| ToggleSub sub.subreddit, onBlur LostFocus] [text (sub.subreddit ++ " (" ++ toString sub.numberOfItems ++ ")")]])
    matchedView results = List.map (\(isMatch, str) -> if not isMatch then text str else span [class "match"] [text str]) results
    viewResult : Int -> (Search.MatchedString, SubredditInfo) -> (String, Html Msg)
    viewResult i (res, sub) = (sub.subreddit, li [] [a [href "#0", classes i sub, onClick <| ToggleSub sub.subreddit, onBlur LostFocus] ( matchedView res ++ [text (" (" ++ toString sub.numberOfItems ++ ")")] ) ])
  in
    case model.searchString of
      "" -> List.indexedMap view checkNSFW
      _ -> List.indexedMap viewResult model.results.results

plus = i [class "fa fa-plus-square"] []
minus = i [class "fa fa-minus-square"] []

plusminus model = if model.menuState.open
                  then a [href "#0", onBlur LostFocus, onClick CloseMenu] [minus]
                  else a [href "#0", onBlur LostFocus, onClick OpenMenu] [plus]


view model showNSFW =
  let
    textbox = input [ id "active-box-textbox"
                    , class "active-box__input"
                    , onInput Search
                    , onBlur LostFocus
                    , onFocus OpenMenu
                    , onKeyDown KeyDown
                    , placeholder "  Add subreddit"
                    , value model.searchString] []
  in
    div [class <| "filter-box"] <|
      [ h1 [] [text "Filter by subreddit"]
      , span [] [a [classList [("filter-box__clear-all-button", True), ("filter-box__clear-all-button--hidden", List.isEmpty model.active)], href "#0", onClick ClearActive] [text "clear-all"]]
      ,  ol [classList [("active-box", True), ("active-box--no-active", List.isEmpty model.active)]]
             (List.concatMap activeView model.active ++ [("inputTextBox", li [] [div [class "active-box__input-container"] [textbox]])])
         , div [class "filter-box__open-button"] [plusminus model]
         , ul [ classList [("subreddits-box", True), ("subreddits-box--collapsed", not model.menuState.open)], onMouseEnter <| SetMouseOnMenu True, onMouseLeave <| SetMouseOnMenu False]
              ( (subView model showNSFW) ++ [("close-button", li [class "subreddits-box__closeButton"] [a [href "#firstitem", onClick CloseMenu] [i [class "fa fa-times-circle"] []]])])

      ]

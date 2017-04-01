module App.FiltersMenu exposing (..)

import Material.Button as Button
import Material.Options as Options

import Html exposing (div, ul, li, text)
import Html.Attributes exposing (class, classList)
import Html.Keyed as Keyed

import Task
import Process
import Time

-- Small state to help with the view
type alias State =
  { isOpen : Bool
  , hideScrollBar : Bool
  , hideButtons : Bool
  }

initialState = State False True True

type Msg
    = HideFilters
    | ShowMenuScrollBar
    | CloseMenu
    | OpenMenu


update msg state = case msg of
    HideFilters ->
      ( { state | hideButtons = True }, Cmd.none)

    ShowMenuScrollBar ->
      ( { state | hideScrollBar = False }, Cmd.none)

    CloseMenu ->
      ( { state | isOpen = False, hideScrollBar = True }, Task.perform (always HideFilters) (Task.sequence [Process.sleep (Time.second), Task.succeed ()]) )

    OpenMenu ->
      ( { state | isOpen = True, hideButtons = False }, Task.perform (always ShowMenuScrollBar) (Task.sequence [Process.sleep (Time.second), Task.succeed ()]) )



menuItemView mdlMsg buttonMsg model idx sub =
  ( sub.subreddit
  , li [ class "filters-menu__item" ]
       [ Button.render mdlMsg [1, idx] model.mdl
          [ Button.colored |> Options.when (List.member sub.subreddit model.filters)
          , Options.onClick (buttonMsg sub.subreddit)
          ]
          [ text sub.subreddit]
       ]
  )

view wrapMsg mdlMsg buttonMsg state model subs =
    Keyed.ul [ classList [("filters-menu", True), ("filters-menu--closed", not state.isOpen), ("filters-menu--showScrollBar", not state.hideScrollBar)] ] <|
      List.indexedMap (menuItemView mdlMsg buttonMsg model) (if not state.hideButtons then subs else []) ++
          [ ("close button", li [class "filters-menu__item filters-menu__item--close-button"]
              [Button.render mdlMsg [1, -1] model.mdl
               [ Button.accent
               , Options.onClick (wrapMsg CloseMenu)
               ]
               [ text "Close"]
               ]
             )
          ]

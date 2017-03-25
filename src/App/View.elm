{-
   Copyright (C) 2017  SimplyNaOH

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module App.View exposing (..)

import Html exposing (div, ul, li, span, a, h1, p, text, img, i)
import Html.Attributes exposing (class, href, target, src, attribute, id, classList, placeholder)
import Html.Events exposing (onClick, onBlur, onFocus)
import Html.Keyed as Keyed
import App.FiltersMenu as Menu
import App.Model exposing (Model, filtered, SubOver18(..))
import App.Update exposing (Msg(..))
import App.ItemListView exposing (itemsView)
import List exposing (length)


-- View


pagination model =
    div [ class "pagination" ]
        [ a [ class "pagination__prev-button", onClick PrevSlice] [ text "<" ]
        , span [ class "pagination__counter" ]
            [ text <| (toString <| model.page + 1) ++ "/" ++ (toString <| 1 + length (filtered model) // model.sliceLength) ]
        , a [ class "pagination__next-button", onClick NextSlice] [ text ">" ]
        ]


filtersView model =
    let
        checkNSFW sub =
            not <| not model.showNSFW && (sub.over18 == NSFW)

        filterView sub =
            [ ( sub
              , li []
                    [ a
                        [ href "#0"
                        , class "active-box__sub"
                        , onClick <| ToggleSubreddit sub
                          --, onMouseEnter <| MenuMsg <| Menu.SetMouseOver True
                          --, onMouseLeave <| MenuMsg <| Menu.SetMouseOver False
                        ]
                        [ text sub ]
                    ]
              )
            , ( toString (sub ++ "-sep"), text " " )
            ]

    in
        div [ class <| "filter-box" ] <|
              [ h1 [] [ text "Filter by subreddit" ]
              , span [] [ a [ classList [ ( "filter-box__clear-all-button", True ), ( "filter-box__clear-all-button--hidden", List.isEmpty model.filters ) ], href "#0", onClick (MenuMsg Menu.OpenMenu) ] [ text "clear-all" ] ]
              , Keyed.ul [ classList [ ( "active-box", True ), ( "active-box--no-active", List.isEmpty model.filters ) ] ]
                  (List.concatMap filterView model.filters)
              ]
                ++ [Menu.view MenuMsg Mdl ToggleSubreddit model.menu model  <| List.filter (checkNSFW) model.subreddits ]


view : Model -> Html.Html Msg
view model =
    let
        itemView : { a | subreddit : String } -> List (Html.Html Never)
        itemView =
            flip (::) [] << text << .subreddit

        --config = { openDivClass = "active-box", closedDivClass = "active-box subreddits-box--collapsed", olClass = "subreddits-box", liClass = "subreddits-box__subreddit", liView = itemView, textboxClass = "active-box__input", textboxId = "myId", textboxPlaceholder = "Search subreddits", toId = .subreddit}
    in
        div [ class "app" ] [ filtersView model, pagination model, itemsView model, pagination model ]

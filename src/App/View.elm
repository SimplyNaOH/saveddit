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

import Material.Chip as Chip
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Options as Options
import Material.Button as Button
import Material.Icon as Icon

import Html exposing (div, ul, li, span, a, h1, h6, p, text, img, i)
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

        activeView = ul [ classList [ ( "filter-box__active-box", True ), ( "filter-box__active-box--empty", List.isEmpty model.filters ) ] ] <|
            List.map (\sub ->
              Chip.span
                [ Chip.deleteIcon "cancel"
                , Chip.deleteClick <| ToggleSubreddit sub
                , Options.css "margin" "5px 5px"
                ]
                [ Chip.contact Html.span
                    [ Color.background Color.primary
                    , Color.text Color.white
                    ]
                    [ text "/r/" ]
                , Chip.content []
                    [ text sub ]
                ]
              )
              model.filters ++
                [ Button.render Mdl [0] model.mdl
                      [ Button.icon
                      , Options.onClick <| if model.menu.isOpen
                                           then MenuMsg Menu.CloseMenu
                                           else MenuMsg Menu.OpenMenu
                      , Options.cs "filter-box__add-button"
                      , if model.menu.isOpen then Options.cs "filter-box__add-button--rotated" else Options.nop
                      ]
                      [ Icon.i "add_cirlce"]
                ]

    in
        Options.div [ Elevation.e2 ] <|
              [ h6 [ class "filter-box__title" ] [ text "Active filters:" ]
              , Button.render Mdl [0] model.mdl
                [ Button.accent
                , Button.ripple
                , Options.onClick ClearFilters
                , Options.cs "filter-box__clear-all-button"
                , if List.isEmpty model.filters then Options.cs "filter-box__clear-all-button--hidden" else Options.nop
                ]
                [ text "clear-all"]
              , activeView
              ]
                ++ [Menu.view MenuMsg Mdl ToggleSubreddit model.menu model  <| List.filter (checkNSFW) model.subreddits ]


view : Model -> Html.Html Msg
view model =
    div [ class "app" ] [ filtersView model, pagination model, itemsView model, pagination model ]

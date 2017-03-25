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

module App.ItemListView exposing (..)

import Material.List as Lists
import Material.Options as Options
import Material.Button as Button
import Material.Icon as Icon

import Html exposing (div, ul, li, span, a, h1, p, text, img, i)
import Html.Attributes exposing (class, href, target, src, attribute, id)
import Html.Keyed as Keyed
import RedditAPI.Types exposing (Item (..), Subreddit)
import App.Model exposing (filtered)
import App.Update exposing (Msg (..))


-- View


subredditButton sub =
    p [ class "item__subreddit" ]
        [ a [ href <| "https://reddit.com/r/" ++ sub, target "_blank" ] <|
            [ span [ class "item__subreddit-prefix" ]
                [ text "/r/" ]
            , text sub
            ]
        ]


linkView link =
    [ div [ class "link__preview-container" ]
        [ a [ href link.url, target "_blank" ]
            [ img [ class "link__preview-img", src link.previewURL ] [] ]
        ]
    , div [ class "link-content" ]
        [ subredditButton link.subreddit
        , h1 [ class "link__title" ]
            [ a [ href link.url, target "_blank" ] <|
                [ text link.title ]
                    ++ if link.over18 then
                        [ span [ class "nsfw-tagged" ] [] ]
                       else
                        []
            ]
        ]
    , ul [ class "link__button-bar" ]
        [ li []
            [ a [ href link.commentsURL, target "_blank" ]
                [ text <| toString link.numComments
                , i [ class "fa fa-comments", attribute "aria-hidden" "true" ] []
                ]
            ]
        , li []
            [ a [ href link.url, target "_blank" ]
                [ text "GoTo"
                , i [ class "fa fa-external-link-square", attribute "aria-hidden" "true" ] []
                ]
            ]
        ]
    ]


commentView comment =
    [ div [ class "comment__title-bar" ]
        [ subredditButton comment.subreddit
        , h1 [ class "comment__title" ]
            [ a [ href comment.linkURL ] <|
                [ text comment.title ]
                    ++ if comment.over18 then
                        [ span [ class "nsfw-tagged" ] [] ]
                       else
                        []
            ]
        , span [] [ i [ class "fa fa-commenting", attribute "aria-hidden" "true" ] [ a [ href comment.url ] [] ] ]
        ]
    , div [ class "comment__body" ] [ text comment.body ]
    , p [ class "comment__goToReddit-button" ]
        [ a [ href comment.url, target "_blank" ]
            [ text "GoTo"
            , i [ class "fa fa-external-link-square", attribute "aria-hidden" "true" ] []
            ]
        ]
    ]


itemView i item =
    let
        baseclass =
            if isNSFW item then
                "item-nsfw "
            else
                "item "

        idWithClass c =
            if i == 0 then
                [ class <| baseclass ++ c, id "firstitem" ]
            else
                [ class <| baseclass ++ c ]
    in
        case item of
            Link link ->
                ( link.id, li (idWithClass "link") <| linkView link )

            Comment comment ->
                ( comment.id, li (idWithClass "comment") <| commentView comment )

itemsView model =
  let
    itemView idx item =
      case item of
        Link info ->
          Lists.li []
              [ Lists.content []
                  [ Lists.avatarImage info.previewURL []
                  , text info.title
                  ]
              , Button.render Mdl [idx] model.mdl
                  [ Button.icon
                  , Button.link info.url
                  , Options.attribute <| Html.Attributes.target "_blank"
                  ]
                  [ Icon.i "open_in_new" ]
              ]
        Comment info ->
          Lists.li [ Lists.withBody, Options.cs "comment" ] -- NB! Required on every Lists.li containing body.
              [ Lists.content []
                  [ Lists.avatarIcon "comment" []
                  , text info.title
                  , Lists.body [] [ text info.body ]
                  ]
              ]


  in
    Lists.ul [] <|
        List.indexedMap itemView <|
            List.take model.sliceLength <|
                List.drop (model.page * model.sliceLength) <|
                    filtered model


-- Helper functions for items:


isNSFW : Item -> Bool
isNSFW item =
    case item of
        Link link ->
            link.over18

        Comment comment ->
            comment.over18


subreddit : Item -> Subreddit
subreddit item =
    case item of
        Link link ->
            link.subreddit

        Comment comment ->
            comment.subreddit

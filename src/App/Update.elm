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

module App.Update exposing (..)

import App.Model exposing (Model, SubOver18(..), SubredditInfo, filtered)
import RedditAPI.Types exposing (isNSFW, subreddit, Item, Subreddit)
import SearchableMenu as Menu
import List exposing (length)


-- Update


type Msg
    = AddItems (List Item)
    | UpdateSubreddits
      -- summarize the subreddits from the items in the model
    | ToggleSubreddit Subreddit
    | ClearFilters
    | ToggleNSFW
    | NextSlice
    | PrevSlice
    | SetSliceLength Int
    | MenuMsg Menu.Msg
    | NoOp
    | SetPage Int
    | SetFilters (List Subreddit)



{- | This is used in the UpdateSubreddits Msg to generate the SubredditInfo for
   each unique subreddit. To do this we must fold over the items, updating the
   corresponding SubredditInfo as we go.
-}


subsFromItems : List Item -> List SubredditInfo
subsFromItems items =
    let
        resolveNSFW sub item =
            case sub.over18 of
                SFW ->
                    if isNSFW item then
                        Partial
                    else
                        SFW

                NSFW ->
                    if not <| isNSFW item then
                        Partial
                    else
                        NSFW

                Partial ->
                    Partial

        currentSub item subs =
            List.filter (\sub -> sub.subreddit == subreddit item) subs

        update item subs =
            case currentSub item subs of
                [] ->
                    { subreddit = subreddit item
                    , over18 =
                        if isNSFW item then
                            NSFW
                        else
                            SFW
                    , numberOfItems = 1
                    }
                        :: subs

                x :: [] ->
                    { x | over18 = resolveNSFW x item, numberOfItems = x.numberOfItems + 1 }
                        :: (List.filter (\sub -> sub.subreddit /= x.subreddit) subs)

                x :: xs ->
                    []
    in
        List.sortBy (String.toLower << .subreddit) <| List.foldr update [] items


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        andThen : (msg -> model -> ( model, Cmd msg )) -> msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
        andThen update msg ( model, cmd ) =
            let
                ( model_, cmd_ ) =
                    update msg model
            in
                ( model_, Cmd.batch [ cmd, cmd_ ] )
    in
        case msg of
            AddItems items ->
                ( { model | items = model.items ++ items }
                , Cmd.none
                )
                    |> andThen update UpdateSubreddits

            UpdateSubreddits ->
                ( { model | subreddits = subsFromItems model.items }
                , Cmd.none
                )

            ToggleSubreddit sub ->
                ( { model
                    | filters =
                        if List.member sub model.filters then
                            List.filter ((/=) sub) model.filters
                        else
                            model.filters ++ [ sub ]
                  }
                , Cmd.none
                )

            ClearFilters ->
                ( { model | filters = [] }
                , Cmd.none
                )

            ToggleNSFW ->
                ( { model | showNSFW = not model.showNSFW }
                , Cmd.none
                )

            NextSlice ->
                let
                    totalPages =
                        ceiling <|
                            toFloat (length (filtered model))
                                / toFloat model.sliceLength
                in
                    ( { model
                        | page =
                            min (model.page + 1) totalPages
                      }
                    , Cmd.none
                    )

            PrevSlice ->
                ( { model
                    | page =
                        max (model.page - 1) 0
                  }
                , Cmd.none
                )

            SetSliceLength len ->
                ( { model
                    | sliceLength = len
                    , page = model.page * model.sliceLength // len
                  }
                , Cmd.none
                )

            MenuMsg msg ->
                let
                    config =
                        { textboxId = "myId", onSelectMsg = ToggleSubreddit, toId = .subreddit }

                    ( updatedMenu, menuCmd, maybeMsg ) =
                        Menu.update config msg model.menu model.subreddits
                in
                    case maybeMsg of
                        Nothing ->
                            ( { model | menu = updatedMenu }, Cmd.none )

                        Just msg ->
                            update msg model

            SetPage page ->
                let
                    totalPages =
                        ceiling <|
                            toFloat (length (filtered model))
                                / toFloat model.sliceLength
                in
                    ( { model
                        | page =
                            clamp 0 totalPages page
                      }
                    , Cmd.none
                    )

            SetFilters filters ->
                ( { model
                    | filters =
                        List.filter
                            (\sub -> List.member sub (List.map .subreddit model.subreddits))
                            filters
                  }
                , Cmd.none
                )

            NoOp ->
                ( model, Cmd.none )

module App exposing (..)

import AOS exposing (..)

import List exposing (length)

import SearchableMenu as Menu

import Html exposing (h1, p, div, span, img, a, ul, li, text, button, i)
import Html.Attributes exposing (class, classList, id, src, href, target, attribute, style, placeholder)
import Html.Events exposing (onClick, onBlur, onFocus, onMouseEnter, onMouseLeave)
import Html.Keyed as Keyed

-- Model
type alias LinkInfo =
  { id: String
  , title : String
  , url : String
  , commentsURL : String
  , previewURL : String
  , subreddit : String
  , numComments : Int
  , over18 : Bool
  }

type alias CommentInfo =
  { id : String
  , title : String
  , url : String
  , linkURL : String
  , body : String
  , subreddit : String
  , over18 : Bool
  }

type Item = Link LinkInfo | Comment CommentInfo

type SubOver18 = SFW | Partial | NSFW

type alias Subreddit = String

type alias SubredditInfo =
  { subreddit : Subreddit
  , numberOfItems : Int
  , over18 : SubOver18
  }

type alias Model =
  { items : List Item
  , subreddits : List SubredditInfo
  , filters : List Subreddit
  , showNSFW : Bool
  , startAt : Int
  , sliceLength : Int
  , menu : Menu.Model
  }

initialModel = Model [] [] [] False 0 15 Menu.initialModel


-- Helper functions for items:
isNSFW : Item -> Bool
isNSFW item = case item of
  Link link -> link.over18
  Comment comment -> comment.over18
subreddit : Item -> String
subreddit item = case item of
  Link link -> link.subreddit
  Comment comment -> comment.subreddit

-- Update

type Msg = AddItems (List Item)
         | UpdateSubreddits
         | ToggleSubreddit Subreddit
         | ClearFilters
         | ToggleNSFW
         | NextSlice
         | PrevSlice
         | SetSliceLength Int
         | MenuMsg Menu.Msg
         | NoOp

subsFromItems : List Item -> List SubredditInfo
subsFromItems items =
  let
    resolveNSFW sub item = case sub.over18 of
      SFW -> if isNSFW item then Partial else SFW
      NSFW -> if not <| isNSFW item then Partial else NSFW
      Partial -> Partial
    currentSub item subs = List.filter (\sub -> sub.subreddit == subreddit item) subs
    update item subs = case currentSub item subs of
      [] -> { subreddit = subreddit item
            , over18 = if isNSFW item then NSFW else SFW
            , numberOfItems = 1} :: subs
      x::[] -> { x | over18 = resolveNSFW x item, numberOfItems = x.numberOfItems + 1}
              :: (List.filter (\sub -> sub.subreddit /= x.subreddit) subs)
      x::xs -> []
  in
    List.sortBy (String.toLower << .subreddit) <| List.foldr update [] items

fst (a,b) = a

update : Msg -> Model -> (Model, Cmd Msg)
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
  AddItems items -> ({ model | items = model.items ++ items }
                    , refreshAOS ()) |> andThen update UpdateSubreddits
  UpdateSubreddits ->
    ( { model | subreddits = subsFromItems model.items }
    , Cmd.none )
  ToggleSubreddit sub ->
    ( { model | filters =
        if List.member sub model.filters
          then List.filter ((/=) sub) model.filters
          else model.filters ++ [sub]
      }
    , Cmd.none)
  ClearFilters ->
    ( { model | filters = [] }
    , Cmd.none )
  ToggleNSFW ->
    ({ model | showNSFW = not model.showNSFW }
    , Cmd.none)
  NextSlice -> ({ model |
                 startAt = if (model.startAt + model.sliceLength) > length (filtered model) - 1
                           then model.startAt
                           else model.startAt + model.sliceLength }
               , refreshAOS ())
  PrevSlice -> ({ model |
                 startAt = max 0 <| model.startAt - model.sliceLength}
               , refreshAOS ())
  SetSliceLength len -> ({ model | sliceLength = len
                                , startAt = model.startAt % len}
                        , refreshAOS ())
  MenuMsg msg ->
    let
      config = { textboxId = "myId", onSelectMsg = ToggleSubreddit, toId = .subreddit }
      (updatedMenu, menuCmd, maybeMsg) = Menu.update config msg model.menu model.subreddits
    in
      case maybeMsg of
        Nothing ->
          ( {model | menu = updatedMenu}, Cmd.none )
        Just msg ->
          update msg model
  NoOp -> (model, Cmd.none)

-- View

filtered model =
  let
    isInFilteredSub item =
      if List.isEmpty model.filters then True
      else List.member (subreddit item) model.filters
    checkNSFW item = not <| not model.showNSFW && (isNSFW item)
  in
    List.filter (\item -> isInFilteredSub item && checkNSFW item) model.items

linkView link =
  [ div [class "link__preview-container"]
        [a [href link.url, target "_blank"]
           [img [class "link__preview-img", src link.previewURL] []]
        ]
  , div [class "link-content"]
        [ p [class "item__subreddit"] [text link.subreddit]
        , h1 [class "link__title"] [a [href link.url, target "_blank"]
                                      [text link.title]]
        ]
  , ul [class "link__button-bar"]
       [ li []
            [a [href link.commentsURL, target "_blank"]
               [ text <| toString link.numComments
               , i [class "fa fa-comments", attribute "aria-hidden" "true"] []
               ]
            ]
       , li []
            [a [href link.url, target "_blank"]
               [ text "GoTo"
               , i [class "fa fa-external-link-square", attribute "aria-hidden" "true"] []
               ]
            ]
       ]
  ]

commentView comment =
  [ div [class "comment__title-bar"]
        [ span [] [i [class "fa fa-commenting", attribute "aria-hidden" "true"] [a [href comment.url] []]]
        , div [] [ p [class "item__subreddit"] [text comment.subreddit]
                 , h1 [class "comment__title"]
                      [a [href comment.linkURL] [text comment.title]]
                 ]
        ]
  , div [class "comment__body"] [text comment.body]
  , p [class "comment__goToReddit-button"]
      [ a [href comment.url, target "_blank"]
          [ text "GoTo"
          , i [class "fa fa-external-link-square", attribute "aria-hidden" "true"] []
          ]
      ]
  ]


itemView i item =
  let
    idWithClass c = if i == 0 then [class <| "item " ++ c, id "firstitem"]
                              else [class <| "item " ++ c]
  in
    case item of
      Link link -> (link.id, li (idWithClass "link") <| linkView link)
      Comment comment -> (comment.id, li (idWithClass "comment") <| commentView comment)

itemsView model = Keyed.ul [class "item-container"] <| List.indexedMap itemView <|
  List.take model.sliceLength <| List.drop model.startAt <| filtered model

pagination model =
  div [class "pagination", attribute "data-aos" "zoom-out", attribute "data-aos-anchor-placement" "center-bottom"]
  [ a [ class "pagination__prev-button", onClick PrevSlice, href "#0"] [text "<"]
  , span [class "pagination__counter"]
    [text <| (toString <| model.startAt // model.sliceLength + 1) ++ "/" ++ (toString <| 1+ length (filtered model) // model.sliceLength )]
  ,  a [ class "pagination__next-button", onClick NextSlice, href "#0"] [text ">"]
  ]

filtersView model =
  let
    filterView sub =
      [(sub, li [] [a [href "#0"
        , class "active-box__sub"
        , onClick <| ToggleSubreddit sub
        , onMouseEnter <| MenuMsg <| Menu.SetMouseOver True
        , onMouseLeave <| MenuMsg <| Menu.SetMouseOver False
        ]
        [text sub]]
        )
      , (toString (sub ++ "-sep"), text " ")
      ]

    config = { toId = .subreddit
      , div = \isOpen -> if isOpen then [class "subreddits-menu"] else [ class "subreddits-menu subreddits-menu--collapsed"]
      , ul = [class "subreddits-menu__list"]
      , li = \isSelected result -> Menu.HtmlDetails [] [a ((if isSelected then class "subreddits-menu__subreddit subreddits-menu--selected" else class "subreddits-menu__subreddit") :: [href "#0", onClick <| Menu.Select (.subreddit << Tuple.second <| result), onBlur Menu.LostFocus, onFocus Menu.Open]) (Menu.simpleSpanView [class "subreddits-menu__match"] result) ]
      , input = [class "subreddits-menu__input", placeholder "Add a subreddit"]
      , prepend = Nothing
      , append = Just <| {
          attributes = [class "subreddits-box__close-button"]
        , children = [a [href "#0", onClick <| Menu.Close] [i [class "fa fa-times-circle"] []]]
        }
      }
  in
    div [class <| "filter-box"] <|
      [ h1 [] [text "Filter by subreddit"]
      , span [] [a [classList [("filter-box__clear-all-button", True), ("filter-box__clear-all-button--hidden", List.isEmpty model.filters)], href "#0", onClick ClearFilters] [text "clear-all"]]
      ,  Keyed.ul [classList [("active-box", True), ("active-box--no-active", List.isEmpty model.filters)]]
             (List.concatMap filterView model.filters)
      ] ++ [Html.map MenuMsg <| Menu.view config model.menu model.subreddits]


view : Model -> Html.Html Msg
view model =
  let
    debugString = "Loaded " ++ (toString <| List.length model.items) ++ " posts. Filtered " ++ (toString <| List.length <| filtered model) ++ "."

    itemView : { a | subreddit : String }  -> List (Html.Html Never)
    itemView = flip (::) [] << text << .subreddit
    --config = { openDivClass = "active-box", closedDivClass = "active-box subreddits-box--collapsed", olClass = "subreddits-box", liClass = "subreddits-box__subreddit", liView = itemView, textboxClass = "active-box__input", textboxId = "myId", textboxPlaceholder = "Search subreddits", toId = .subreddit}
  in
      div [class "app"] [text debugString, filtersView model , pagination model, itemsView model, pagination model]

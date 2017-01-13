module App exposing (..)

import AOS exposing (..)

import List exposing (length)

import Filterbox exposing (SubOver18 (..))

import Html exposing (h1, p, div, span, img, a, ul, li, text, button, i)
import Html.Attributes exposing (class, classList, id, src, href, target, attribute, style)
import Html.Events exposing (onClick)
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

type alias Model =
  { items : List Item
  , filterBox : Filterbox.Model
  , showNSFW : Bool
  , startAt : Int
  , sliceLength : Int
  }

initialModel = Model [] Filterbox.initialModel False 0 15


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
         | NextSlice
         | PrevSlice
         | SetSliceLength Int
         | ToggleNSFW
         | FilterboxMsg Filterbox.Msg
         | NoOp

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
update msg model = case msg of
  AddItems items -> ({ model | items = model.items ++ items
                            , filterBox = fst <| Filterbox.update (Filterbox.SetSubreddits <| subsFromItems <| model.items ++ items) model.filterBox }
                    , refreshAOS ())
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
  ToggleNSFW -> ({ model | showNSFW = not model.showNSFW, filterBox = (\(a,b) -> a) <| Filterbox.update Filterbox.ToggleNSFW model.filterBox}, Cmd.none)
  FilterboxMsg msg ->
    let (updatedFilterbox, filterboxCmd) = Filterbox.update msg model.filterBox
    in
    case msg of
      Filterbox.ToggleSub _ ->( {model | filterBox = updatedFilterbox, startAt = 0 }, Cmd.map FilterboxMsg filterboxCmd)
      _ -> ( {model | filterBox = updatedFilterbox }, Cmd.map FilterboxMsg filterboxCmd)
  NoOp -> (model, Cmd.none)

-- View

filtered model =
  let
    isInFilteredSub item =
      if List.isEmpty model.filterBox.active then True
      else List.member (subreddit item) model.filterBox.active
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
{-
prevButton = a [class "nav-button nav-button--prev", onClick PrevSlice, href "#0"] [text "prev"]
nextButton = a [class "nav-button nav-button--next"
     , href "#firstitem"
     , attribute "data-aos" "zoom-out"
     , attribute "data-aos-anchor-placement" "center-bottom"
     , onClick NextSlice]
     [text "next"]
-}
view : Model -> Html.Html Msg
view model =
  let
    debugString = "Loaded " ++ (toString <| List.length model.items) ++ " posts. Filtered " ++ (toString <| List.length <| filtered model) ++ "."
  in
      div [class "app"] [text debugString, Html.map FilterboxMsg <| Filterbox.view model.filterBox model.showNSFW, pagination model, itemsView model, pagination model]

module Main exposing (..)

import App
import Html exposing (Html, button, input, div, body, text, img, a, ul, li, p, span, select, option, h1, i, h2)
import Html.Attributes exposing (style, class, classList, src, height, width, href, target, attribute, id)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json
import Json.Decode exposing (field, string, bool, list, oneOf, int)
import Navigation
import Regex exposing (regex)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Time exposing (Time)
import Http


-- Model


type alias Token =
    String


type alias LoginData =
    { token : Token
    , expire : Time
    , username : String
    }


type LoginState
    = NoLogin
    | GotToken LoginData
    | LoggedIn LoginData


type alias ErrorCode =
    Int


accessDenied =
    0


networkError =
    1


unhandledError =
    2


type alias Model =
    { app : App.Model
    , loginState : LoginState
    , loadedData : Bool
    , errors : List ( ErrorCode, String )
    }


initialModel =
    Model App.initialModel NoLogin False []


err model errorCode error =
    { model | errors = ( errorCode, error ) :: model.errors }


type alias RedditResponse =
    { headers : Dict String String
    , items : List App.Item
    , after : String
    }


-- UPDATE


type Msg
    = RequestResponse (Result Http.Error RedditResponse)
    | MakeRequest
    | UsernameResponse (Result Http.Error String)
    | AppMsg App.Msg
    | LocChange Navigation.Location
    | NoOp


processLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
processLocation location model =
    let
        findOne =
            Regex.AtMost 1

        parameter name =
            Maybe.map (String.dropLeft (String.length name + 1)) <|
                Maybe.map .match <|
                    List.head <|
                        Regex.find findOne
                            (regex <| name ++ "=[^&]*")
                            location.hash

        maybeError =
            parameter "error"

        maybeToken =
            parameter "token"

        maybeExpire =
            Maybe.map toFloat <|
                Maybe.andThen (Result.toMaybe << Json.decodeString Json.int) <|
                    parameter "expires_in"

        maybeState =
            parameter "state"

        newModel token expire =
            { model
                | loginState =
                    GotToken <|
                        { token = token
                        , expire = expire
                        , username = ""
                        }
            }

        getUsername token =
            Http.send UsernameResponse (usernameRequest token)

        login =
            Maybe.map2
                (\token expire ->
                    ( newModel token expire, getUsername token )
                )
                maybeToken
                maybeExpire
    in
        if model.loadedData then
            ( model, Cmd.none )
        else
            case model.loginState of
                LoggedIn _ ->
                    ( model, Cmd.none )

                GotToken _ ->
                    ( model, Cmd.none )

                NoLogin ->
                    case login of
                        Nothing ->
                            case maybeError of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just "access_denied" ->
                                    ( err model
                                        accessDenied
                                        "Access to your reddit account was denied"
                                    , Cmd.none
                                    )

                                Just _ ->
                                    ( err model
                                        unhandledError
                                        "Error while trying to login with reddit"
                                    , Cmd.none
                                    )

                        Just newState ->
                            newState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestResponse (Ok response) ->
            let
                ( updatedApp, appCmd ) =
                    App.update (App.AddItems response.items) model.app

                ( newModel, newRequestCmd ) =
                    case model.loginState of
                        NoLogin ->
                            ( err model
                                unhandledError
                                "Error while trying to load data (Lost login data)"
                            , Cmd.none
                            )

                        GotToken _ ->
                            ( err model
                                unhandledError
                                "Error while trying to load data (Lost login data)"
                            , Cmd.none
                            )

                        LoggedIn data ->
                            if List.length response.items == 100 then
                                ( model
                                , Http.send RequestResponse <|
                                    savedRequest data.token data.username response.after
                                )
                            else
                                ( { model | loadedData = True }, Cmd.none )
            in
                -- TODO check (Dict.get "x-ratelimit-remaining" response.headers)
                { newModel | app = updatedApp } ! [ Cmd.map AppMsg appCmd, newRequestCmd ]

        RequestResponse (Err htmlError) ->
            ( err model networkError (toString htmlError), Cmd.none )

        MakeRequest ->
            case model.loginState of
                NoLogin ->
                    ( err model unhandledError "Tried to retrieve data with no login.", Cmd.none )

                GotToken _ ->
                    ( err model unhandledError "Tried to retrieve data with no username.", Cmd.none )

                LoggedIn data ->
                    ( model
                    , Http.send RequestResponse <|
                        savedRequest data.token data.username ""
                    )

        UsernameResponse (Ok username) ->
            case model.loginState of
                GotToken data ->
                    ( { model | loginState = LoggedIn { data | username = username } }
                    , if not model.loadedData then
                        Http.send RequestResponse <|
                            savedRequest data.token username ""
                      else
                        Cmd.none
                    )

                _ ->
                    ( err model
                        unhandledError
                        "Trying to process UsernameResponse without token"
                    , Cmd.none
                    )

        UsernameResponse (Err htmlError) ->
            ( err model networkError (toString htmlError), Cmd.none )

        AppMsg msg ->
            let
                ( updatedApp, appCmd ) =
                    App.update msg model.app
            in
                ( { model | app = updatedApp }, Cmd.map AppMsg appCmd )

        LocChange location ->
            processLocation location model

        NoOp ->
            ( model, Cmd.none )



-- View


accessRequestUrl =
    "https://www.reddit.com/api/v1/authorize?response_type=token&client_id=jr6Xea0v9ERhNQ&redirect_uri=http%3A%2F%2F127.0.0.1:8000%2Findex.html&scope=history,identity&state=asdfghjkl"


username model =
    case model.loginState of
        LoggedIn data ->
            data.username

        _ ->
            ""


isLoggedIn model =
    case model.loginState of
        LoggedIn _ ->
            True

        _ ->
            False


gotToken model =
    case model.loginState of
        GotToken _ ->
            True

        _ ->
            False


loginPrompt model =
    div
        [ classList
            [ ( "login-prompt", True )
            , ( "login-prompt--access-denied", List.any (\(code, _) -> code == accessDenied) model.errors)
            , ( "login-prompt--got-token", gotToken model || isLoggedIn model )
            , ( "login-prompt--logged-in", isLoggedIn model )
            , ( "login-prompt--hidden", model.loadedData )
            ]
        ]
        [ img [ class "login-prompt__logo", src "resources/logo.png" ] []
        , h1 [ class "login-prompt__login-button" ] [ a [ href accessRequestUrl ] [ text "Login with Reddit" ] ]
                , h2 [ class "login-prompt__denied"]
                  [ text "Access to reddit was denied"]
        , h2 [ class "login-prompt__welcome" ]
            [ text "Hello "
            , span [ class "login-prompt__username" ]
                [ text <| username model ]
            , text "!"
            ]
        , i [ class "login-prompt__loading-animation fa fa-spinner fa-pulse fa-3x" ] []
        , p [ class "login-prompt__loading" ] [ text <| "Loading data... " ++ (toString <| List.length model.app.items) ++ " posts." ]

        ]


view : Model -> Html.Html Msg
view model =
    div [ class "main-div" ] <|
        [ div [ class "top-bar" ]
            [ img [ src "resources/logo.png" ] []
            , span
                [ classList
                    [ ( "top-bar__username", True )
                    , ( "top-bar__username--hiden", isLoggedIn model )
                    ]
                ]
                [ text <| "Logged in as " ++ username model ]
            , select [ onInput <| (\i -> AppMsg (App.SetSliceLength i)) << Result.withDefault 10 << (Json.decodeString int) ]
                [ option [ onClick <| AppMsg (App.SetSliceLength 5) ] [ text "5" ]
                , option [] [ text "10" ]
                , option [] [ text "15" ]
                , option [] [ text "25" ]
                ]
            , a [ href "#0", onClick <| AppMsg App.ToggleNSFW ] [ text "Toggle NSFW" ]
            ]
        , loginPrompt model
        , Html.map AppMsg (App.view model.app)
        ]



-- Commands


decodeComment =
    field "data" <|
        Json.map7 App.CommentInfo
            (field "id" string)
            (field "link_title" string)
            (Json.map4
                (\a b c d ->
                    "https://www.reddit.com/r/"
                        ++ a
                        ++ "/comments/"
                        ++ (String.dropLeft 3 b)
                        ++ "/"
                        ++ c
                        ++ "/"
                        ++ d
                        ++ "/"
                )
                (field "subreddit" string)
                (field "link_id" string)
                (field "link_title" string)
                (field "id" string)
            )
            (field "link_url" string)
            (field "body" string)
            (field "subreddit" string)
            (field "over_18" bool)


decodeThumbnail thumbnail =
    if String.startsWith "https://" thumbnail then
        Json.succeed thumbnail
    else
        case thumbnail of
            "self" ->
                Json.succeed "resources/self.png"

            _ ->
                Json.oneOf [ Json.at [ "preview", "images", "0", "resolutions", "0", "url" ] string, Json.succeed "resources/noimage.png" ]


decodeLink =
    field "data" <|
        Json.map8 App.LinkInfo
            (field "id" string)
            (field "title" string)
            (field "url" string)
            (Json.map ((++) "https://www.reddit.com") (field "permalink" string))
            (field "thumbnail" string |> Json.andThen decodeThumbnail)
            {- (oneOf
                [ Json.at ["preview", "images", "0", "source", "url"] string
                , field "thumbnail" string]
               )
            -}
            (field "subreddit" string)
            (field "num_comments" int)
            (field "over_18" bool)


decodeItem =
    field "kind" string
        |> (Json.andThen
                (\kind ->
                    case kind of
                        "t1" ->
                            Json.map App.Comment decodeComment

                        "t3" ->
                            Json.map App.Link decodeLink

                        _ ->
                            Json.fail "kind is not t1 or t3"
                )
           )


decodeItems =
    Json.at [ "data", "children" ] (list decodeItem)



{-
   loadPosts =
     Cmd.batch
       [ Http.send LoadedData (Http.get "1.json" decodeItems)
       , Http.send LoadedData (Http.get "2.json" decodeItems)
       , Http.send LoadedData (Http.get "3.json" decodeItems)
       , Http.send LoadedData (Http.get "4.json" decodeItems)
       , Http.send LoadedData (Http.get "5.json" decodeItems)
       ]
-}
-- TODO: Handle error here!


htmlResponseToRedditResponse response =
    Ok
        { headers = response.headers
        , items = Result.withDefault [] <| Json.decodeString decodeItems response.body
        , after = Result.withDefault "" <| Json.decodeString (Json.at [ "data", "after" ] string) response.body
        }


savedRequest token username after =
    Http.request
        { method = "GET"
        , headers =
            [
              Http.header "Authorization" ("bearer " ++ token)
            ]
        , url =
            "https://oauth.reddit.com/user/"
                ++ username
                ++ "/saved.json?raw_json=1&limit=100"
                ++ if after /= "" then
                    "&after=" ++ after
                   else
                    ""
        , body = Http.emptyBody
        , expect = Http.expectStringResponse htmlResponseToRedditResponse
        , timeout = Nothing
        , withCredentials = False
        }


usernameRequest token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("bearer " ++ token) ]
        , url = "https://oauth.reddit.com/api/v1/me"
        , body = Http.emptyBody
        , expect = Http.expectJson (field "name" string)
        , timeout = Nothing
        , withCredentials = False
        }


initialCmd =
    Cmd.none



-- app


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    processLocation location initialModel


main =
    Navigation.program (LocChange)
        { init = init
        , subscriptions = \x -> Sub.none
        , view = view
        , update = update
        }

module Main exposing (main)

{-| WebSocketClient Example
-}

import Browser
import Browser.Events
import Cmd.Extra exposing (withCmd, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (checked, disabled, size, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (Value)
import Platform.Sub
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Time exposing (Posix, millisToPosix, posixToMillis)


handlers : List (Handler Model Msg)
handlers =
    [ WebSocketHandler socketHandler
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Platform.Sub.batch
        [ PortFunnels.subPort Process
        , Browser.Events.onAnimationFrame NewClock
        ]


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers PortFunnels.cmdPort



-- MODEL


defaultUrl : String
defaultUrl =
    "ws://localhost:9161"


type alias Model =
    { send : String
    , log : List String
    , url : String
    , wasLoaded : Bool
    , state : State
    , key : String
    , error : Maybe String
    , frameCount : Maybe Int
    , status : String
    , frames : Dict Int String
    , clock : Posix
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    { send = "Hello World!"
    , log = []
    , url = defaultUrl
    , wasLoaded = False
    , state = PortFunnels.initialState
    , key = "socket"
    , error = Nothing
    , frameCount = Nothing
    , status = "Not connected"
    , frames = Dict.empty
    , clock = millisToPosix 0
    }
        |> withNoCmd



-- UPDATE


type Msg
    = UpdateUrl String
    | ToggleAutoReopen
    | Connect
    | Close
    | Process Value
    | NewClock Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUrl url ->
            { model | url = url } |> withNoCmd

        ToggleAutoReopen ->
            let
                state =
                    model.state

                socketState =
                    state.websocket

                autoReopen =
                    WebSocket.willAutoReopen model.key socketState
            in
            { model
                | state =
                    { state
                        | websocket =
                            WebSocket.setAutoReopen
                                model.key
                                (not autoReopen)
                                socketState
                    }
            }
                |> withNoCmd

        Connect ->
            model
                |> logMessage ("Connecting to " ++ model.url)
                |> withCmd (WebSocket.makeOpenWithKey model.key model.url |> send)

        Close ->
            { model | log = "Closing" :: model.log }
                |> withCmd (WebSocket.makeClose model.key |> send)

        Process value ->
            case
                PortFunnels.processValue funnelDict value model.state model
            of
                Err error ->
                    { model | error = Just error } |> withNoCmd

                Ok res ->
                    res

        NewClock clock ->
            { model | clock = clock } |> withNoCmd


send : WebSocket.Message -> Cmd Msg
send message =
    WebSocket.send PortFunnels.cmdPort message


doIsLoaded : Model -> Model
doIsLoaded model =
    if not model.wasLoaded && WebSocket.isLoaded model.state.websocket then
        { model | wasLoaded = True }

    else
        model


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            doIsLoaded
                { mdl
                    | state = state
                    , error = Nothing
                }
    in
    case response of
        WebSocket.MessageReceivedResponse { message } ->
            ( case String.lines message of
                [ "status", status ] ->
                    { model | status = status }

                [ "frame_count", n ] ->
                    { model | frameCount = String.toInt n }

                [ "frame", n, svg ] ->
                    let
                        nth =
                            Maybe.withDefault 0 (String.toInt n)
                    in
                    { model | frames = Dict.insert nth svg model.frames }

                _ ->
                    model
                        |> logMessage ("Received \"" ++ message ++ "\"")
            , Cmd.none
            )

        WebSocket.ConnectedResponse r ->
            model
                |> logMessage ("Connected: " ++ r.description)
                |> withNoCmd

        WebSocket.ClosedResponse { code, wasClean, expected } ->
            { model | frameCount = Nothing }
                |> logMessage ("Closed, " ++ closedString code wasClean expected)
                |> withNoCmd

        WebSocket.ErrorResponse error ->
            { model | frameCount = Nothing }
                |> logMessage (WebSocket.errorToString error)
                |> withNoCmd

        _ ->
            ( case WebSocket.reconnectedResponses response of
                [] ->
                    model

                [ ReconnectedResponse r ] ->
                    logMessage ("Reconnected: " ++ r.description) model

                list ->
                    logMessage (Debug.toString list) model
            , Cmd.none
            )


logMessage : String -> Model -> Model
logMessage msg model =
    { model | log = msg :: model.log }


closedString : WebSocket.ClosedCode -> Bool -> Bool -> String
closedString code wasClean expected =
    "code: "
        ++ WebSocket.closedCodeToString code
        ++ ", "
        ++ (if wasClean then
                "clean"

            else
                "not clean"
           )
        ++ ", "
        ++ (if expected then
                "expected"

            else
                "NOT expected"
           )



-- VIEW


b : String -> Html Msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    let
        isConnected =
            WebSocket.isConnected model.key model.state.websocket

        frameCount =
            Maybe.withDefault 1 model.frameCount

        now =
            (posixToMillis model.clock * 60) // 1000

        thisFrame =
            modBy frameCount now

        bestFrame =
            List.head (List.reverse (Dict.values (Dict.filter (\x _ -> x <= thisFrame) model.frames)))
    in
    div
        [ style "width" "40em"
        , style "margin" "auto"
        , style "margin-top" "1em"
        , style "padding" "1em"
        , style "border" "solid"
        ]
        [ h1 [] [ text "Reanimate: elm viewer" ]
        , p []
            [ b "url: "
            , input
                [ value model.url
                , onInput UpdateUrl
                , size 30
                , disabled isConnected
                ]
                []
            , text " "
            , if isConnected then
                button [ onClick Close ]
                    [ text "Close" ]

              else
                button [ onClick Connect ]
                    [ text "Connect" ]
            , br
            , b "Frame: "
            , text (String.fromInt thisFrame)
            , br
            , case bestFrame of
                Nothing ->
                    b "no frames yet"

                Just path ->
                    img [ src path ] []
            , b "Status: "
            , text model.status
            , br
            , b "Fetched frames: "
            , text (String.fromInt (Dict.size model.frames))
            , br
            , b "frames: "
            , text (Maybe.withDefault "no frames" (Maybe.map String.fromInt model.frameCount))
            , br
            , b "auto reopen: "
            , input
                [ type_ "checkbox"
                , onClick ToggleAutoReopen
                , checked <|
                    WebSocket.willAutoReopen
                        model.key
                        model.state.websocket
                ]
                []
            ]
        , p [] <|
            List.concat
                [ [ b "Log:"
                  , br
                  ]
                , List.intersperse br (List.map text model.log)
                ]
        ]

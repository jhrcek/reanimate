module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes as Attr exposing (size, src, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Platform.Sub
import Ports
import Time exposing (Posix, millisToPosix, posixToMillis)
import WebSocket


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Platform.Sub.batch
        [ Ports.receiveSocketMsg (WebSocket.receive MessageReceived)
        , Browser.Events.onAnimationFrame TimestampReceived
        ]


type alias Model =
    { log : List String
    , url : String
    , wasLoaded : Bool
    , key : String
    , error : Maybe String
    , frameCount : Maybe Int
    , status : String
    , frames : Dict Int String
    , clock : Posix
    }


defaultUrl : String
defaultUrl =
    "ws://localhost:9161"


socketName : String
socketName =
    "TheSocket"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { log = []
      , url = defaultUrl
      , wasLoaded = False
      , key = "socket"
      , error = Nothing
      , frameCount = Nothing
      , status = "Not connected"
      , frames = Dict.empty
      , clock = millisToPosix 0
      }
    , Cmd.none
    )


connectCmd : String -> Cmd msg
connectCmd url =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Connect
            { name = socketName
            , address = url
            , protocol = ""
            }


closeCmd : Cmd msg
closeCmd =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Close { name = socketName }


type Msg
    = UrlUpdated String
    | MessageReceived (Result Json.Decode.Error WebSocket.WebSocketMsg)
    | ConnectClicked
    | CloseClicked
    | TimestampReceived Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdated url ->
            ( { model | url = url }, Cmd.none )

        ConnectClicked ->
            ( model |> logMessage ("Connecting to " ++ model.url)
            , connectCmd model.url
            )

        CloseClicked ->
            ( { model | frames = Dict.empty, status = "Not connected" }
                |> logMessage "Closing"
            , closeCmd
            )

        TimestampReceived clock ->
            ( { model | clock = clock }, Cmd.none )

        MessageReceived result ->
            processResult result model


processResult : Result Json.Decode.Error WebSocket.WebSocketMsg -> Model -> ( Model, Cmd Msg )
processResult result model =
    case result of
        Err decodeError ->
            ( { model | error = Just <| Json.Decode.errorToString decodeError }, Cmd.none )

        Ok wsMsg ->
            case wsMsg of
                WebSocket.Error { error } ->
                    ( { model | error = Just error }, Cmd.none )

                WebSocket.Data { data } ->
                    ( processMessage data model, Cmd.none )


processMessage : String -> Model -> Model
processMessage data model =
    case String.lines data of
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
                |> logMessage ("Message not recognized \"" ++ data ++ "\"")


logMessage : String -> Model -> Model
logMessage msg model =
    { model | log = msg :: model.log }


bold : String -> Html msg
bold string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    let
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
            [ bold "url: "
            , input
                [ value model.url
                , onInput UrlUpdated
                , size 30

                -- , disabled isConnected
                ]
                []
            , text " "
            , button [ onClick ConnectClicked ] [ text "Connect" ]

            -- TODO add WS state management and show just one button at time
            , button [ onClick CloseClicked ] [ text "Close" ]
            , br
            , bold "Frame: "
            , text (String.fromInt thisFrame)
            , br
            , case bestFrame of
                Nothing ->
                    bold "no frames yet"

                Just path ->
                    img [ src path ] []
            , bold "Status: "
            , text model.status
            , br
            , progressIndicator model
            ]
        , p [] <|
            bold "Log:"
                :: List.intersperse br (List.map text model.log)
        ]


progressIndicator : Model -> Html msg
progressIndicator { frames, frameCount } =
    case frameCount of
        Nothing ->
            bold "No frames available"

        Just frameCnt ->
            if Dict.size frames == frameCnt then
                bold (String.fromInt frameCnt ++ " frames loaded")

            else
                Html.div []
                    [ bold "Loading frames "
                    , Html.progress
                        [ Attr.max (String.fromInt frameCnt)
                        , value (String.fromInt <| Dict.size frames)
                        ]
                        []
                    ]

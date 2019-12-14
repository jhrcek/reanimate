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
import Task
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
subscriptions model =
    Platform.Sub.batch
        [ Ports.receiveSocketMsg (WebSocket.receive MessageReceived)
        , Browser.Events.onAnimationFrame TimestampReceived
        , case model.connectionStatus of
            TryingToConnect ->
                Time.every 1000 (always TryToConnect)

            _ ->
                Sub.none
        ]


type ConnectionStatus
    = TryingToConnect
    | Connected
    | Disconnected
    | Compiling
    | ReceivingFrames Int
    | Animating


type alias Model =
    { log : List String
    , error : Maybe String
    , frameCount : Maybe Int
    , frames : Dict Int String
    , clock : Posix
    , connectionStatus : ConnectionStatus
    }


sockerUrl : String
sockerUrl =
    "ws://localhost:9161"


socketName : String
socketName =
    "TheSocket"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { log = []
      , error = Nothing
      , frameCount = Nothing
      , frames = Dict.empty
      , clock = millisToPosix 0
      , connectionStatus = TryingToConnect
      }
    , connectCmd sockerUrl
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
    = MessageReceived (Result Json.Decode.Error WebSocket.WebSocketMsg)
    | ConnectClicked
    | CloseClicked
    | TimestampReceived Posix
      -- TODO what happened?
    | TryToConnect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TryToConnect ->
            ( { model | connectionStatus = Connected }, connectCmd sockerUrl )

        ConnectClicked ->
            ( model |> logMessage ("Connecting to " ++ sockerUrl)
            , connectCmd sockerUrl
            )

        CloseClicked ->
            ( { model | frames = Dict.empty, connectionStatus = Disconnected }
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
            case status of
                "Compiling" ->
                    { model | connectionStatus = Compiling }

                "Done" ->
                    { model | connectionStatus = Animating }

                -- TODO deal with this somehow
                other ->
                    model

        [ "frame_count", n ] ->
            { model
                | connectionStatus = ReceivingFrames <| Maybe.withDefault 0 <| String.toInt n
                , frameCount = String.toInt n
            }

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
    case model.connectionStatus of
        TryingToConnect ->
            Html.text "Trying to connect"

        Disconnected ->
            Html.div [] [ Html.button [ onClick ConnectClicked ] [ text "Connect" ] ]

        Connected ->
            Html.text "Connected"

        Compiling ->
            Html.text "Compiling"

        ReceivingFrames totalFrames ->
            progressIndicator (Dict.size model.frames) totalFrames

        Animating ->
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
                [ style "width" "100%" ]
                [ button [ onClick CloseClicked ] [ text "Close" ]
                , br
                , bold "Frame: "
                , text (String.fromInt thisFrame)
                , br
                , case bestFrame of
                    Nothing ->
                        bold "no frames yet"

                    Just path ->
                        img [ src path ] []
                , Html.div []
                    (bold "Log:"
                        :: List.intersperse br (List.map text model.log)
                    )
                ]


progressIndicator : Int -> Int -> Html msg
progressIndicator receivedFrames totalFrames =
    Html.div []
        [ bold "Loading frames "
        , Html.progress
            [ value (String.fromInt receivedFrames)
            , Attr.max (String.fromInt totalFrames)
            ]
            []
        ]

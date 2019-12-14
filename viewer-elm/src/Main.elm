module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr exposing (src, style, value)
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
subscriptions model =
    Platform.Sub.batch
        [ Ports.receiveSocketMsg (WebSocket.receive MessageReceived)
        , Browser.Events.onAnimationFrame TimestampReceived
        , case model.status of
            SomethingWentWrong ConnectionFailed ->
                Time.every 1000 (always AttemptReconnect)

            _ ->
                Sub.none
        ]


type Status
    = Disconnected
    | Connected
    | Compiling
    | ReceivingFrames Int Frames
    | Animating Int Frames
    | SomethingWentWrong Problem


type Problem
    = CompilationError String
    | ConnectionFailed
    | DoneWithoutFrames
    | PortMessageDecodeFailure Json.Decode.Error
    | UnexpectedMessage String


type alias Model =
    { status : Status
    , clock : Posix
    }


type alias Frames =
    Dict Int String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Disconnected
      , clock = millisToPosix 0
      }
    , connectCmd
    )


connectCmd : Cmd msg
connectCmd =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Connect
            { name = "TheSocket"
            , address = "ws://localhost:9161"
            , protocol = ""
            }


type Msg
    = MessageReceived (Result Json.Decode.Error WebSocket.WebSocketMsg)
    | TimestampReceived Posix
    | AttemptReconnect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptReconnect ->
            ( model
            , connectCmd
            )

        TimestampReceived clock ->
            ( { model | clock = clock }, Cmd.none )

        MessageReceived result ->
            processResult result model


processResult : Result Json.Decode.Error WebSocket.WebSocketMsg -> Model -> ( Model, Cmd Msg )
processResult result model =
    case result of
        Err decodeError ->
            ( { model | status = SomethingWentWrong (PortMessageDecodeFailure decodeError) }
            , Cmd.none
            )

        Ok wsMsg ->
            case wsMsg of
                WebSocket.Error { error } ->
                    ( { model | status = SomethingWentWrong (UnexpectedMessage error) }, Cmd.none )

                WebSocket.Data { data } ->
                    ( processMessage data model, Cmd.none )


processMessage : String -> Model -> Model
processMessage data model =
    case String.lines data of
        [ "connection established" ] ->
            { model | status = Connected }

        [ "connection failed" ] ->
            somethingWentWrong ConnectionFailed model

        [ "status", status ] ->
            case status of
                "Compiling" ->
                    { model | status = Compiling }

                "Done" ->
                    case model.status of
                        ReceivingFrames frameCount frames ->
                            { model | status = Animating frameCount frames }

                        _ ->
                            somethingWentWrong DoneWithoutFrames model

                _ ->
                    somethingWentWrong (UnexpectedMessage ("Unknown status: '" ++ status ++ "'")) model

        "error" :: errorLines ->
            somethingWentWrong (CompilationError (String.join "\n" errorLines)) model

        [ "frame_count", n ] ->
            case String.toInt n of
                Just frameCount ->
                    { model | status = ReceivingFrames frameCount Dict.empty }

                Nothing ->
                    somethingWentWrong (UnexpectedMessage ("frame_count wasn't number, but '" ++ n ++ "'")) model

        [ "frame", n, svgUrl ] ->
            case String.toInt n of
                Just frameIndex ->
                    case model.status of
                        ReceivingFrames frameCount frames ->
                            { model | status = ReceivingFrames frameCount (Dict.insert frameIndex svgUrl frames) }

                        _ ->
                            somethingWentWrong (UnexpectedMessage "Got 'frame' message while not ReceivingFrames") model

                Nothing ->
                    somethingWentWrong (UnexpectedMessage ("Frame index wasn't number, but '" ++ n ++ "'")) model

        _ ->
            somethingWentWrong (UnexpectedMessage data) model


somethingWentWrong : Problem -> Model -> Model
somethingWentWrong what model =
    { model | status = SomethingWentWrong what }


bold : String -> Html msg
bold string =
    Html.b [] [ Html.text string ]


view : Model -> Html Msg
view model =
    case model.status of
        Disconnected ->
            Html.text "Disconnected"

        Connected ->
            Html.text "Connected"

        Compiling ->
            Html.text "Compiling"

        SomethingWentWrong problem ->
            problemView problem

        ReceivingFrames frameCount frames ->
            Html.div []
                [ progressView (Dict.size frames) frameCount
                , animationView frameCount frames model.clock
                ]

        Animating frameCount frames ->
            animationView frameCount frames model.clock


animationView : Int -> Frames -> Posix -> Html msg
animationView frameCount frames clock =
    let
        now =
            (posixToMillis clock * 60) // 1000

        thisFrame =
            modBy frameCount now

        bestFrame =
            List.head (List.reverse (Dict.values (Dict.filter (\x _ -> x <= thisFrame) frames)))
    in
    Html.div [ style "width" "100%", style "height" "100%" ]
        [ bold "Frame: "
        , Html.text (String.fromInt thisFrame ++ " / " ++ String.fromInt frameCount)
        , Html.div []
            [ case bestFrame of
                Nothing ->
                    Html.text "No frames available"

                Just path ->
                    Html.img [ src path ] []
            ]
        ]


progressView : Int -> Int -> Html msg
progressView receivedFrames frameCount =
    Html.label []
        [ Html.text "Loading frames "
        , Html.progress
            [ value (String.fromInt receivedFrames)
            , Attr.max (String.fromInt frameCount)
            ]
            []
        ]


problemView : Problem -> Html msg
problemView problem =
    case problem of
        CompilationError error ->
            Html.div []
                [ Html.h1 [] [ Html.text "Compilation failed" ]
                , Html.pre [] [ Html.text error ]
                ]

        ConnectionFailed ->
            Html.div []
                [ Html.text "Failed to establish connection. Possible causes include: "
                , Html.ul []
                    [ Html.li [] [ Html.text "The reanimate script is not running" ]
                    , Html.li [] [ Html.text "At most one viewer window can connect at time. Maybe there's another browser window/tab already connected." ]
                    ]
                ]

        DoneWithoutFrames ->
            Html.text "Received 'done' message, but I was not receiving frames!"

        PortMessageDecodeFailure decodeError ->
            Html.text ("Failed to decode Port message. The error was: " ++ Json.Decode.errorToString decodeError)

        UnexpectedMessage problemDescription ->
            Html.text ("Unexpected message: " ++ problemDescription)

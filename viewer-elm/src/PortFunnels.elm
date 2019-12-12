----------------------------------------------------------------------
--
-- PortFunnels.elm
-- Most of the support needed for a PortFunnel application
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module PortFunnels exposing
    ( FunnelDict
    , Handler(..)
    , State
    , cmdPort
    , initialState
    , makeFunnelDict
    , processValue
    , subPort
    )

{-| A copy of the PortFunnels.elm example module, modified for `PortFunnel.WebSocket`.

You will usually copy this file into your application's source directory, and, if you use other `PortFunnel` modules, modify it to support all of them.

Note that this is a `port module`, and it defines the two ports that are used by `site/index.html`, `cmdPort` and `subPort`.

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import PortFunnel
    exposing
        ( FunnelSpec
        , GenericMessage
        , StateAccessors
        )
import PortFunnel.WebSocket as WebSocket


{-| Add a property to this type for each funnel module you use.
-}
type alias State =
    { websocket : WebSocket.State
    }


{-| Create the initial state record.

Some modules have parameters to their `initialState` functions.

In that case, you may have make those parameters be parameters to `initialState`.

-}
initialState : State
initialState =
    { websocket = WebSocket.initialState
    }


{-| Make a `StateAccessors` instance for each funnel module.
-}
websocketAccessors : StateAccessors State WebSocket.State
websocketAccessors =
    StateAccessors .websocket (\substate state -> { state | websocket = substate })


{-| A `Funnel` tags a module-specific `FunnelSpec`.

Add a tag here for each funnel module you use.

-}
type Funnel model msg
    = WebSocketFunnel (FunnelSpec State WebSocket.State WebSocket.Message WebSocket.Response model msg)


{-| A `Handler` tags a function to handle responses from one funnel module.

Add a tag in this type for each funnel module you use.

-}
type Handler model msg
    = WebSocketHandler (WebSocket.Response -> State -> model -> ( model, Cmd msg ))


{-| This packages up everything necessary to dispatch for each module.

Add a clause for each funnel module you use.

-}
handlerToFunnel : Handler model msg -> ( String, Funnel model msg )
handlerToFunnel handler =
    case handler of
        WebSocketHandler websocketHandler ->
            ( WebSocket.moduleName
            , FunnelSpec websocketAccessors WebSocket.moduleDesc WebSocket.commander websocketHandler
                |> WebSocketFunnel
            )


{-| This is called from `AppFunnel.processValue`.

It unboxes the `Funnel` arg, and calls `PortFunnel.appProcess`.

-}
appTrampoline : (Value -> Cmd msg) -> GenericMessage -> Funnel model msg -> State -> model -> Result String ( model, Cmd msg )
appTrampoline portGetter genericMessage funnel state model =
    -- Dispatch on the `Funnel` tag.
    -- This example has only one possibility.
    case funnel of
        WebSocketFunnel appFunnel ->
            PortFunnel.appProcess portGetter
                genericMessage
                appFunnel
                state
                model


port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


{-| A `Dict` that maps a module name to a concretized `FunnelSpec`.

Create one with `makeFunnelDict`. Pass it to `processValue`.

-}
type alias FunnelDict model msg =
    ( Dict String (Funnel model msg), Value -> Cmd msg )


{-| Make a `Dict` mapping `moduleName` to tagged concrete `FunnelSpec`.
-}
makeFunnelDict : List (Handler model msg) -> (Value -> Cmd msg) -> FunnelDict model msg
makeFunnelDict handlers portGetter =
    ( List.map handlerToFunnel handlers |> Dict.fromList
    , portGetter
    )


{-| Process a value coming in through the `subPort`.

The `FunnelDict` is the result of calling `makeFunnelDict`.

-}
processValue : FunnelDict model msg -> Value -> State -> model -> Result String ( model, Cmd msg )
processValue ( funnelDict, portGetter ) value state model =
    PortFunnel.processValue funnelDict (appTrampoline portGetter) value state model

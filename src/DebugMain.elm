port module DebugMain exposing (..)

import Base64
import Ports
import Proto.Google.Protobuf.Compiler exposing (CodeGeneratorRequest, decodeCodeGeneratorRequest, defaultCodeGeneratorRequest, defaultCodeGeneratorResponse, encodeCodeGeneratorResponse)
import Protobuf.Decode as PD
import Protobuf.Encode as PE


port debug : String -> Cmd msg


type alias Msg =
    String


main : Program () () String
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> a -> ( a, Cmd Msg )
update request model =
    ( model, process request )


subscriptions : a -> Sub String
subscriptions _ =
    Ports.request identity


process : String -> Cmd msg
process base64 =
    let
        request =
            base64
                |> Base64.toBytes
                |> Maybe.andThen (PD.decode decodeCodeGeneratorRequest)
                |> Maybe.withDefault defaultCodeGeneratorRequest

        response =
            defaultCodeGeneratorResponse
    in
    Cmd.batch
        [ encodeCodeGeneratorResponse response
            |> PE.encode
            |> Base64.fromBytes
            |> Maybe.withDefault ""
            |> Ports.response
        , debug <| Debug.toString request
        ]

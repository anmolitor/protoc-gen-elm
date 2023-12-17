module Main exposing (main)

import Base64
import Generator
import Options
import Platform
import Ports
import Proto.Google.Protobuf.Compiler exposing (CodeGeneratorRequest, CodeGeneratorResponse, Version, decodeCodeGeneratorRequest, encodeCodeGeneratorResponse)
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Protobuf.Types.Int64


main : Program Versions Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Versions =
    { plugin : String
    , library : String
    }


type alias Model =
    { requestCount : Int
    , versions : Versions
    }


init : Versions -> ( Model, Cmd Msg )
init libraryVersion =
    ( Model 0 libraryVersion, Cmd.none )


type Msg
    = Request String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Request base64 ->
            ( { model | requestCount = model.requestCount + 1 }, process model base64 )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.request Request



-- WORKER


process : Model -> String -> Cmd msg
process model base64 =
    let
        request =
            base64
                |> Base64.toBytes
                |> Maybe.andThen (Decode.decode decodeCodeGeneratorRequest)

        response =
            Maybe.map (map model.versions) request
                |> Maybe.withDefault (fail <| "Failed parsing request from protoc. Here is the request in base64: " ++ base64)
    in
    Cmd.batch
        [ encodeCodeGeneratorResponse response
            |> Encode.encode
            |> Base64.fromBytes
            |> Maybe.withDefault ""
            |> Ports.response
        ]


map : Versions -> CodeGeneratorRequest -> CodeGeneratorResponse
map versions request =
    let
        allVersions =
            { plugin = versions.plugin
            , library = versions.library
            , compiler = Maybe.withDefault "unknown version" (Maybe.map version request.compilerVersion)
            }
    in
    Generator.requestToResponse allVersions (Options.parse request.parameter) request


version : Version -> String
version v =
    String.join "." [ String.fromInt v.major, String.fromInt v.minor, String.fromInt v.patch ] ++ v.suffix


fail : String -> CodeGeneratorResponse
fail err =
    { error = err, supportedFeatures = Protobuf.Types.Int64.fromInts 0 3, file = [] }

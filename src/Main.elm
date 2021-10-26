module Main exposing (main)

import Base64
import Bytes
import Generator
import Internal.Google.Protobuf.Compiler exposing (CodeGeneratorRequest, CodeGeneratorResponse, CodeGeneratorResponseFile, Version, codeGeneratorRequestDecoder, toCodeGeneratorResponseEncoder)
import Mapper
import Platform
import Ports
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode


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
                |> Maybe.andThen (Decode.decode codeGeneratorRequestDecoder)

        response =
            Maybe.map (map model.versions) request
                |> Maybe.withDefault (fail "Failed parsing request from protoc")
    in
    toCodeGeneratorResponseEncoder response
        |> Encode.encode
        |> Base64.fromBytes
        |> Maybe.withDefault ""
        |> Ports.response


map : Versions -> CodeGeneratorRequest -> CodeGeneratorResponse
map versions request =
    let
        allVersions =
            { plugin = versions.plugin
            , library = versions.library
            , compiler = Maybe.withDefault "unknown version" (Maybe.map version request.compilerVersion)
            }

        files : List CodeGeneratorResponseFile
        files =
            Mapper.map request.fileToGenerate request.protoFile
                |> List.map
                    (\pkg ->
                        { name = packageFile pkg.name
                        , insertionPoint = ""
                        , content = Generator.generate allVersions pkg
                        , generatedCodeInfo = Nothing
                        }
                    )
    in
    CodeGeneratorResponse "" 3 files


version : Version -> String
version v =
    String.join "." [ String.fromInt v.major, String.fromInt v.minor, String.fromInt v.patch ] ++ v.suffix


packageFile : String -> String
packageFile =
    (\f -> f ++ ".elm") << String.replace "." "/"


fail : String -> CodeGeneratorResponse
fail err =
    CodeGeneratorResponse err 3 []

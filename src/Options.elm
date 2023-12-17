module Options exposing (..)


type alias Options =
    { grpc : Bool
    , json : JsonOpt
    , grpcDevTools : Bool
    }


default : Options
default =
    { grpc = True
    , json = None
    , grpcDevTools = False
    }


type JsonOpt
    = None
    | Encode
    | Decode
    | All


parse : String -> Options
parse parameters =
    let
        parseParam param opts =
            case String.split "=" param of
                [ "grpc" ] ->
                    { opts | grpc = True }

                [ "grpc", "false" ] ->
                    { opts | grpc = False }

                [ "grpc", "true" ] ->
                    { opts | grpc = True }

                [ "json" ] ->
                    { opts | json = All }

                [ "json", "all" ] ->
                    { opts | json = All }

                [ "json", "encode" ] ->
                    { opts | json = Encode }

                [ "json", "decode" ] ->
                    { opts | json = Decode }

                [ "json", "none" ] ->
                    { opts | json = None }

                [ "grpcDevTools" ] ->
                    { opts | grpcDevTools = True }

                [ "grpcDevTools", "true" ] ->
                    { opts | grpcDevTools = True }

                [ "grpcDevTools", "false" ] ->
                    { opts | grpcDevTools = False }

                _ ->
                    opts
    in
    String.split "," parameters
        |> List.foldl parseParam default

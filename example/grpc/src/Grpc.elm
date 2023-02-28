module Grpc exposing (ErrCode(..), Error(..), Rpc, send)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Http exposing (Response(..), header)
import Protobuf.Decode exposing (Decoder, decode)
import Protobuf.Encode exposing (Encoder, encode)


type alias Rpc =
    { service : String, package : String, rpcName : String }


send : Rpc -> (req -> Encoder) -> Decoder res -> req -> (Result Error res -> msg) -> Cmd msg
send rpc encoder decoder request msg =
    let
        body =
            encode (encoder request)
                |> frameRequest
                |> Http.bytesBody grpcContentType

        handleResponse httpResponse =
            case httpResponse of
                GoodStatus_ { headers } bytes ->
                    let
                        status =
                            Dict.get "grpc-status" headers
                                |> Maybe.withDefault "0"

                        errMessage =
                            Dict.get "grpc-message" headers |> Maybe.withDefault ""
                    in
                    case errCodeFromString status of
                        Just (Ok ()) ->
                            Bytes.Decode.decode responseDecoder bytes
                                |> Maybe.andThen (\response -> decode decoder response.message)
                                |> Result.fromMaybe (BadBody bytes)

                        Just (Err errCode) ->
                            Err <| BadGrpcStatus errCode errMessage

                        Nothing ->
                            Err <| UnknownGrpcStatus status

                BadUrl_ badUrl ->
                    Err <| BadUrl badUrl

                Timeout_ ->
                    Err Timeout

                NetworkError_ ->
                    Err NetworkError

                BadStatus_ metadata bytes ->
                    Err <| BadHttpStatus metadata bytes
    in
    Http.request
        { method = "POST"
        , headers = [ header "accept" grpcContentType ]
        , url = rpcToUrl rpc
        , body = body
        , expect = Http.expectBytesResponse msg handleResponse
        , timeout = Nothing
        , tracker = Nothing
        }



-- Errors


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadHttpStatus Http.Metadata Bytes
    | BadBody Bytes
    | BadGrpcStatus ErrCode String
    | UnknownGrpcStatus String


type ErrCode
    = -- The operation was cancelled.
      Cancelled
    | -- Unknown error.
      Unknown
    | -- Client specified an invalid argument.
      InvalidArgument
    | -- Deadline expired before operation could complete.
      DeadlineExceeded
    | -- Some requested entity was not found.
      NotFound
    | -- Some entity that we attempted to create already exists.
      AlreadyExists
    | -- The caller does not have permission to execute the specified operation.
      PermissionDenied
    | -- Some resource has been exhausted.
      ResourceExhausted
    | -- The system is not in a state required for the operation's execution.
      FailedPrecondition
    | -- The operation was aborted.
      Aborted
    | -- Operation was attempted past the valid range.
      OutOfRange
    | -- Operation is not implemented or not supported.
      Unimplemented
    | -- Internal error.
      Internal
    | -- The service is currently unavailable.
      Unavailable
    | -- Unrecoverable data loss or corruption.
      DataLoss
    | -- The request does not have valid authentication credentials
      Unauthenticated



-- INTERNALS


grpcContentType : String
grpcContentType =
    "application/grpc-web+proto"


rpcToUrl : Rpc -> String
rpcToUrl { service, package, rpcName } =
    "/"
        ++ (if String.isEmpty package then
                ""

            else
                package ++ "."
           )
        ++ service
        ++ "/"
        ++ rpcName


requestEncoder : Bytes -> Bytes.Encode.Encoder
requestEncoder message =
    let
        messageLength =
            Bytes.width message
    in
    Bytes.Encode.sequence
        [ Bytes.Encode.unsignedInt8 0
        , Bytes.Encode.unsignedInt32 Bytes.BE messageLength
        , Bytes.Encode.bytes message
        ]


type alias Response =
    { message : Bytes
    }


responseDecoder : Bytes.Decode.Decoder Response
responseDecoder =
    Bytes.Decode.map2 (\_ -> Response)
        (Bytes.Decode.bytes 1)
        (Bytes.Decode.unsignedInt32 Bytes.BE
            |> Bytes.Decode.andThen Bytes.Decode.bytes
        )


frameRequest : Bytes -> Bytes
frameRequest binaryData =
    requestEncoder binaryData
        |> Bytes.Encode.encode


errCodeFromString : String -> Maybe (Result ErrCode ())
errCodeFromString str =
    String.toInt str |> Maybe.andThen errCodeFromInt


errCodeFromInt : Int -> Maybe (Result ErrCode ())
errCodeFromInt n =
    case n of
        0 ->
            Just <| Ok ()

        1 ->
            Just <| Err Cancelled

        2 ->
            Just <| Err Unknown

        3 ->
            Just <| Err InvalidArgument

        4 ->
            Just <| Err DeadlineExceeded

        5 ->
            Just <| Err NotFound

        6 ->
            Just <| Err AlreadyExists

        7 ->
            Just <| Err PermissionDenied

        8 ->
            Just <| Err ResourceExhausted

        9 ->
            Just <| Err FailedPrecondition

        10 ->
            Just <| Err Aborted

        11 ->
            Just <| Err OutOfRange

        12 ->
            Just <| Err Unimplemented

        13 ->
            Just <| Err Internal

        14 ->
            Just <| Err Unavailable

        15 ->
            Just <| Err DataLoss

        16 ->
            Just <| Err Unauthenticated

        _ ->
            Nothing

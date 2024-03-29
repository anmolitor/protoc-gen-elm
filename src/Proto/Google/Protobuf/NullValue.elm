{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.NullValue exposing (NullValue(..), decodeNullValue, defaultNullValue, encodeNullValue, fieldNumbersNullValue, jsonDecodeNullValue, jsonEncodeNullValue)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 4.0.0
- `protoc` 3.19.4
- the following specification files: `google/protobuf/struct.proto`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version 1.2.0 or higher.

@docs NullValue, decodeNullValue, defaultNullValue, encodeNullValue, fieldNumbersNullValue, jsonDecodeNullValue, jsonEncodeNullValue

-}

import Json.Decode
import Json.Encode
import Protobuf.Decode
import Protobuf.Encode


{-| Decode a `NullValue` from JSON. Uses the canonical encoding described here: https://protobuf.dev/programming-guides/proto3/#json

-}
jsonDecodeNullValue : Json.Decode.Decoder NullValue
jsonDecodeNullValue =
    Json.Decode.null defaultNullValue


{-| Encode a `NullValue` to JSON. Uses the canonical encoding described here: https://protobuf.dev/programming-guides/proto3/#json

-}
jsonEncodeNullValue : NullValue -> Json.Encode.Value
jsonEncodeNullValue _ =
    Json.Encode.null


{-| The field numbers for the fields of `NullValue`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersNullValue : NullValue -> Int
fieldNumbersNullValue n_ =
    case n_ of
        NULLVALUE ->
            0

        NullValueUnrecognized_ m_ ->
            m_


{-| Default for NullValue. Should only be used for 'required' decoders as an initial value.

-}
defaultNullValue : NullValue
defaultNullValue =
    NULLVALUE


{-| Declares how to encode a `NullValue` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeNullValue : NullValue -> Protobuf.Encode.Encoder
encodeNullValue value =
    Protobuf.Encode.int32 <|
        case value of
            NULLVALUE ->
                0

            NullValueUnrecognized_ i ->
                i


{-| Declares how to decode a `NullValue` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeNullValue : Protobuf.Decode.Decoder NullValue
decodeNullValue =
    Protobuf.Decode.int32
        |> Protobuf.Decode.map
            (\i ->
                case i of
                    0 ->
                        NULLVALUE

                    _ ->
                        NullValueUnrecognized_ i
            )


{-| `NullValue` enumeration

-}
type NullValue
    = NULLVALUE
    | NullValueUnrecognized_ Int

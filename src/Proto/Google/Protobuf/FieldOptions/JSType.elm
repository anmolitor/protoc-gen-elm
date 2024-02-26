{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.FieldOptions.JSType exposing (JSType(..), decodeJSType, defaultJSType, encodeJSType, fieldNumbersJSType, jsonDecodeJSType, jsonEncodeJSType)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 4.0.0-beta.0
- `protoc` 3.19.4
- the following specification files: `google/protobuf/descriptor.proto`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version 1.2.0 or higher.

@docs JSType, decodeJSType, defaultJSType, encodeJSType, fieldNumbersJSType, jsonDecodeJSType, jsonEncodeJSType

-}

import Json.Decode
import Json.Encode
import Protobuf.Decode
import Protobuf.Encode
import Protobuf.Utils.Int32


{-| Decode a `JSType` from JSON. Uses the canonical encoding described here: https://protobuf.dev/programming-guides/proto3/#json

-}
jsonDecodeJSType : Json.Decode.Decoder JSType
jsonDecodeJSType =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.map
                (\i ->
                    case i of
                        "JS_NORMAL" ->
                            JSNORMAL

                        "JS_STRING" ->
                            JSSTRING

                        "JS_NUMBER" ->
                            JSNUMBER

                        _ ->
                            JSNORMAL
                )
        , Protobuf.Utils.Int32.int32JsonDecoder
            |> Json.Decode.map
                (\i ->
                    case i of
                        0 ->
                            JSNORMAL

                        1 ->
                            JSSTRING

                        2 ->
                            JSNUMBER

                        _ ->
                            JSNORMAL
                )
        ]


{-| Encode a `JSType` to JSON. Uses the canonical encoding described here: https://protobuf.dev/programming-guides/proto3/#json

-}
jsonEncodeJSType : JSType -> Json.Encode.Value
jsonEncodeJSType value =
    Json.Encode.string <|
        case value of
            JSNORMAL ->
                "JS_NORMAL"

            JSSTRING ->
                "JS_STRING"

            JSNUMBER ->
                "JS_NUMBER"


{-| The field numbers for the fields of `JSType`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersJSType : JSType -> Int
fieldNumbersJSType n_ =
    case n_ of
        JSNORMAL ->
            0

        JSSTRING ->
            1

        JSNUMBER ->
            2


{-| Default for JSType. Should only be used for 'required' decoders as an initial value.

-}
defaultJSType : JSType
defaultJSType =
    JSNORMAL


{-| Declares how to encode a `JSType` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeJSType : JSType -> Protobuf.Encode.Encoder
encodeJSType value =
    Protobuf.Encode.int32 <|
        case value of
            JSNORMAL ->
                0

            JSSTRING ->
                1

            JSNUMBER ->
                2


{-| Declares how to decode a `JSType` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeJSType : Protobuf.Decode.Decoder JSType
decodeJSType =
    Protobuf.Decode.int32
        |> Protobuf.Decode.map
            (\i ->
                case i of
                    0 ->
                        JSNORMAL

                    1 ->
                        JSSTRING

                    2 ->
                        JSNUMBER

                    _ ->
                        JSNORMAL
            )


{-| `JSType` enumeration

-}
type JSType
    = JSNORMAL
    | JSSTRING
    | JSNUMBER

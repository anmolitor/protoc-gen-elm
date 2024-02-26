{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.FieldOptions.CType exposing (CType(..), decodeCType, defaultCType, encodeCType, fieldNumbersCType, jsonDecodeCType, jsonEncodeCType)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 4.0.0-beta.0
- `protoc` 3.19.4
- the following specification files: `google/protobuf/descriptor.proto`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version 1.2.0 or higher.

@docs CType, decodeCType, defaultCType, encodeCType, fieldNumbersCType, jsonDecodeCType, jsonEncodeCType

-}

import Json.Decode
import Json.Encode
import Protobuf.Decode
import Protobuf.Encode
import Protobuf.Utils.Int32


{-| Decode a `CType` from JSON. Uses the canonical encoding described here: https://protobuf.dev/programming-guides/proto3/#json

-}
jsonDecodeCType : Json.Decode.Decoder CType
jsonDecodeCType =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.map
                (\i ->
                    case i of
                        "STRING" ->
                            STRING

                        "CORD" ->
                            CORD

                        "STRING_PIECE" ->
                            STRINGPIECE

                        _ ->
                            STRING
                )
        , Protobuf.Utils.Int32.int32JsonDecoder
            |> Json.Decode.map
                (\i ->
                    case i of
                        0 ->
                            STRING

                        1 ->
                            CORD

                        2 ->
                            STRINGPIECE

                        _ ->
                            STRING
                )
        ]


{-| Encode a `CType` to JSON. Uses the canonical encoding described here: https://protobuf.dev/programming-guides/proto3/#json

-}
jsonEncodeCType : CType -> Json.Encode.Value
jsonEncodeCType value =
    Json.Encode.string <|
        case value of
            STRING ->
                "STRING"

            CORD ->
                "CORD"

            STRINGPIECE ->
                "STRING_PIECE"


{-| The field numbers for the fields of `CType`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersCType : CType -> Int
fieldNumbersCType n_ =
    case n_ of
        STRING ->
            0

        CORD ->
            1

        STRINGPIECE ->
            2


{-| Default for CType. Should only be used for 'required' decoders as an initial value.

-}
defaultCType : CType
defaultCType =
    STRING


{-| Declares how to encode a `CType` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeCType : CType -> Protobuf.Encode.Encoder
encodeCType value =
    Protobuf.Encode.int32 <|
        case value of
            STRING ->
                0

            CORD ->
                1

            STRINGPIECE ->
                2


{-| Declares how to decode a `CType` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeCType : Protobuf.Decode.Decoder CType
decodeCType =
    Protobuf.Decode.int32
        |> Protobuf.Decode.map
            (\i ->
                case i of
                    0 ->
                        STRING

                    1 ->
                        CORD

                    2 ->
                        STRINGPIECE

                    _ ->
                        STRING
            )


{-| `CType` enumeration

-}
type CType
    = STRING
    | CORD
    | STRINGPIECE

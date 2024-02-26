{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.Compiler.CodeGeneratorResponse.Feature exposing (Feature(..), decodeFeature, defaultFeature, encodeFeature, fieldNumbersFeature, jsonDecodeFeature, jsonEncodeFeature)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 4.0.0-beta.0
- `protoc` 3.19.4
- the following specification files: `google/protobuf/compiler/plugin.proto`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version 1.2.0 or higher.

@docs Feature, decodeFeature, defaultFeature, encodeFeature, fieldNumbersFeature, jsonDecodeFeature, jsonEncodeFeature

-}

import Json.Decode
import Json.Encode
import Protobuf.Decode
import Protobuf.Encode
import Protobuf.Utils.Int32


{-| Decode a `Feature` from JSON. Uses the canonical encoding described here: https://protobuf.dev/programming-guides/proto3/#json

-}
jsonDecodeFeature : Json.Decode.Decoder Feature
jsonDecodeFeature =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.map
                (\i ->
                    case i of
                        "FEATURE_NONE" ->
                            FEATURENONE

                        "FEATURE_PROTO3_OPTIONAL" ->
                            FEATUREPROTO3OPTIONAL

                        _ ->
                            FEATURENONE
                )
        , Protobuf.Utils.Int32.int32JsonDecoder
            |> Json.Decode.map
                (\i ->
                    case i of
                        0 ->
                            FEATURENONE

                        1 ->
                            FEATUREPROTO3OPTIONAL

                        _ ->
                            FEATURENONE
                )
        ]


{-| Encode a `Feature` to JSON. Uses the canonical encoding described here: https://protobuf.dev/programming-guides/proto3/#json

-}
jsonEncodeFeature : Feature -> Json.Encode.Value
jsonEncodeFeature value =
    Json.Encode.string <|
        case value of
            FEATURENONE ->
                "FEATURE_NONE"

            FEATUREPROTO3OPTIONAL ->
                "FEATURE_PROTO3_OPTIONAL"


{-| The field numbers for the fields of `Feature`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersFeature : Feature -> Int
fieldNumbersFeature n_ =
    case n_ of
        FEATURENONE ->
            0

        FEATUREPROTO3OPTIONAL ->
            1


{-| Default for Feature. Should only be used for 'required' decoders as an initial value.

-}
defaultFeature : Feature
defaultFeature =
    FEATURENONE


{-| Declares how to encode a `Feature` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeFeature : Feature -> Protobuf.Encode.Encoder
encodeFeature value =
    Protobuf.Encode.int32 <|
        case value of
            FEATURENONE ->
                0

            FEATUREPROTO3OPTIONAL ->
                1


{-| Declares how to decode a `Feature` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeFeature : Protobuf.Decode.Decoder Feature
decodeFeature =
    Protobuf.Decode.int32
        |> Protobuf.Decode.map
            (\i ->
                case i of
                    0 ->
                        FEATURENONE

                    1 ->
                        FEATUREPROTO3OPTIONAL

                    _ ->
                        FEATURENONE
            )


{-| `Feature` enumeration

-}
type Feature
    = FEATURENONE
    | FEATUREPROTO3OPTIONAL

{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}


module Protoc.Gen.Elm.Test exposing
    ( Value(..), MessageEnum(..), Message, Integers, OneOf, Kind(..)
    , messageDecoder, integersDecoder, oneOfDecoder
    , toMessageEncoder, toIntegersEncoder, toOneOfEncoder
    )

{-| ProtoBuf module: `Protoc.Gen.Elm.Test`

This module was generated automatically using

  - [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 1.0.0-beta-1
  - [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.0.0) 1.0.0
  - `protoc` 3.6.1
  - the following specification file:
      - `tests/test.proto`


# Model

@docs Value, MessageEnum, Message, Integers, OneOf, Kind


# Decoder

@docs messageDecoder, integersDecoder, oneOfDecoder


# Encoder

@docs toMessageEncoder, toIntegersEncoder, toOneOfEncoder

-}

import Bytes
import Dict
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode



-- MODEL


{-| Value
-}
type Value
    = NoValue
    | Value
    | UnrecognizedValue_ Int


{-| MessageEnum
-}
type MessageEnum
    = EnumA
    | EnumB
    | EnumC
    | UnrecognizedMessageEnum_ Int


{-| Message
-}
type alias Message =
    { floatValue : Float
    , doubleValue : Float
    , stringValue : String
    , boolValues : List Bool
    , mapValue : Dict.Dict String MessageEnum
    , bytesValue : Bytes.Bytes
    }


{-| Integers
-}
type alias Integers =
    { int32Value : Int
    , int64Value : Int
    , uint32Value : Int
    , uint64Value : Int
    , sint32Value : Int
    , sint64Value : Int
    , fixed32Value : Int
    , fixed64Value : Int
    , sfixed32Value : Int
    , sfixed64Value : Int
    }


{-| OneOf
-}
type alias OneOf =
    { kind : Maybe Kind
    , value : Value
    }


{-| Kind
-}
type Kind
    = KindFloatValue Float
    | KindDoubleValue Float
    | KindStringValue String
    | KindIntegersValue Integers



-- DECODER


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.int32
        |> Decode.map
            (\value ->
                case value of
                    0 ->
                        NoValue

                    1 ->
                        Value

                    v ->
                        UnrecognizedValue_ v
            )


messageEnumDecoder : Decode.Decoder MessageEnum
messageEnumDecoder =
    Decode.int32
        |> Decode.map
            (\value ->
                case value of
                    0 ->
                        EnumA

                    1 ->
                        EnumB

                    2 ->
                        EnumC

                    v ->
                        UnrecognizedMessageEnum_ v
            )


messageDecoder : Decode.Decoder Message
messageDecoder =
    Decode.message (Message 0 0 "" [] Dict.empty (Encode.encode <| Encode.string ""))
        [ Decode.optional 1 Decode.float setFloatValue
        , Decode.optional 2 Decode.double setDoubleValue
        , Decode.optional 3 Decode.string setStringValue
        , Decode.repeated 4 Decode.bool .boolValues setBoolValues
        , Decode.mapped 5 ( "", EnumA ) Decode.string messageEnumDecoder .mapValue setMapValue
        , Decode.optional 16 Decode.bytes setBytesValue
        ]


integersDecoder : Decode.Decoder Integers
integersDecoder =
    Decode.message (Integers 0 0 0 0 0 0 0 0 0 0)
        [ Decode.optional 1 Decode.int32 setInt32Value
        , Decode.optional 11 Decode.int32 setInt64Value
        , Decode.optional 2 Decode.uint32 setUint32Value
        , Decode.optional 12 Decode.uint32 setUint64Value
        , Decode.optional 3 Decode.sint32 setSint32Value
        , Decode.optional 13 Decode.sint32 setSint64Value
        , Decode.optional 4 Decode.fixed32 setFixed32Value
        , Decode.optional 14 Decode.fixed32 setFixed64Value
        , Decode.optional 5 Decode.sfixed32 setSfixed32Value
        , Decode.optional 15 Decode.sfixed32 setSfixed64Value
        ]


oneOfDecoder : Decode.Decoder OneOf
oneOfDecoder =
    Decode.message (OneOf Nothing NoValue)
        [ Decode.oneOf
            [ ( 1, Decode.map KindFloatValue Decode.float )
            , ( 2, Decode.map KindDoubleValue Decode.double )
            , ( 3, Decode.map KindStringValue Decode.string )
            , ( 99, Decode.map KindIntegersValue integersDecoder )
            ]
            setKind
        , Decode.optional 1000 valueDecoder setValue
        ]



-- ENCODER


toValueEncoder : Value -> Encode.Encoder
toValueEncoder value =
    Encode.int32 <|
        case value of
            NoValue ->
                0

            Value ->
                1

            UnrecognizedValue_ v ->
                v


toMessageEnumEncoder : MessageEnum -> Encode.Encoder
toMessageEnumEncoder value =
    Encode.int32 <|
        case value of
            EnumA ->
                0

            EnumB ->
                1

            EnumC ->
                2

            UnrecognizedMessageEnum_ v ->
                v


toMessageEncoder : Message -> Encode.Encoder
toMessageEncoder model =
    Encode.message
        [ ( 1, Encode.float model.floatValue )
        , ( 2, Encode.double model.doubleValue )
        , ( 3, Encode.string model.stringValue )
        , ( 4, Encode.list Encode.bool model.boolValues )
        , ( 5, Encode.dict Encode.string toMessageEnumEncoder model.mapValue )
        , ( 16, Encode.bytes model.bytesValue )
        ]


toIntegersEncoder : Integers -> Encode.Encoder
toIntegersEncoder model =
    Encode.message
        [ ( 1, Encode.int32 model.int32Value )
        , ( 11, Encode.int32 model.int64Value )
        , ( 2, Encode.uint32 model.uint32Value )
        , ( 12, Encode.uint32 model.uint64Value )
        , ( 3, Encode.sint32 model.sint32Value )
        , ( 13, Encode.sint32 model.sint64Value )
        , ( 4, Encode.fixed32 model.fixed32Value )
        , ( 14, Encode.fixed32 model.fixed64Value )
        , ( 5, Encode.sfixed32 model.sfixed32Value )
        , ( 15, Encode.sfixed32 model.sfixed64Value )
        ]


toOneOfEncoder : OneOf -> Encode.Encoder
toOneOfEncoder model =
    Encode.message
        [ Maybe.withDefault ( 0, Encode.none ) <| Maybe.map toKindEncoder model.kind
        , ( 1000, toValueEncoder model.value )
        ]


toKindEncoder : Kind -> ( Int, Encode.Encoder )
toKindEncoder model =
    case model of
        KindFloatValue value ->
            ( 1, Encode.float value )

        KindDoubleValue value ->
            ( 2, Encode.double value )

        KindStringValue value ->
            ( 3, Encode.string value )

        KindIntegersValue value ->
            ( 99, toIntegersEncoder value )



-- SETTERS


setFloatValue : a -> { b | floatValue : a } -> { b | floatValue : a }
setFloatValue value model =
    { model | floatValue = value }


setDoubleValue : a -> { b | doubleValue : a } -> { b | doubleValue : a }
setDoubleValue value model =
    { model | doubleValue = value }


setStringValue : a -> { b | stringValue : a } -> { b | stringValue : a }
setStringValue value model =
    { model | stringValue = value }


setBoolValues : a -> { b | boolValues : a } -> { b | boolValues : a }
setBoolValues value model =
    { model | boolValues = value }


setMapValue : a -> { b | mapValue : a } -> { b | mapValue : a }
setMapValue value model =
    { model | mapValue = value }


setBytesValue : a -> { b | bytesValue : a } -> { b | bytesValue : a }
setBytesValue value model =
    { model | bytesValue = value }


setInt32Value : a -> { b | int32Value : a } -> { b | int32Value : a }
setInt32Value value model =
    { model | int32Value = value }


setInt64Value : a -> { b | int64Value : a } -> { b | int64Value : a }
setInt64Value value model =
    { model | int64Value = value }


setUint32Value : a -> { b | uint32Value : a } -> { b | uint32Value : a }
setUint32Value value model =
    { model | uint32Value = value }


setUint64Value : a -> { b | uint64Value : a } -> { b | uint64Value : a }
setUint64Value value model =
    { model | uint64Value = value }


setSint32Value : a -> { b | sint32Value : a } -> { b | sint32Value : a }
setSint32Value value model =
    { model | sint32Value = value }


setSint64Value : a -> { b | sint64Value : a } -> { b | sint64Value : a }
setSint64Value value model =
    { model | sint64Value = value }


setFixed32Value : a -> { b | fixed32Value : a } -> { b | fixed32Value : a }
setFixed32Value value model =
    { model | fixed32Value = value }


setFixed64Value : a -> { b | fixed64Value : a } -> { b | fixed64Value : a }
setFixed64Value value model =
    { model | fixed64Value = value }


setSfixed32Value : a -> { b | sfixed32Value : a } -> { b | sfixed32Value : a }
setSfixed32Value value model =
    { model | sfixed32Value = value }


setSfixed64Value : a -> { b | sfixed64Value : a } -> { b | sfixed64Value : a }
setSfixed64Value value model =
    { model | sfixed64Value = value }


setKind : a -> { b | kind : a } -> { b | kind : a }
setKind value model =
    { model | kind = value }


setValue : a -> { b | value : a } -> { b | value : a }
setValue value model =
    { model | value = value }
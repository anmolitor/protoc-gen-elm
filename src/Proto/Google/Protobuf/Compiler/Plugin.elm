{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.Compiler.Plugin exposing (..)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 1.0.0
- `protoc` 3.14.0
- the following specification file: `google/protobuf/compiler/plugin.proto`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.1.0) version 1.1.0 or higher.


-}

import Maybe
import Proto.Google.Protobuf.Descriptor
import Protobuf.Decode
import Protobuf.Encode


{-| Decode a `CodeGeneratorResponse` from Bytes


-}
defaultCodeGeneratorResponse : CodeGeneratorResponse
defaultCodeGeneratorResponse =
    { error = "", supportedFeatures = 0, file = [] }


{-| Decode a `CodeGeneratorResponse` from Bytes


-}
decodeCodeGeneratorResponse : Protobuf.Decode.Decoder CodeGeneratorResponse
decodeCodeGeneratorResponse =
    Protobuf.Decode.message
        defaultCodeGeneratorResponse
        [ Protobuf.Decode.optional 1 Protobuf.Decode.string (\a r -> { r | error = a })
        , Protobuf.Decode.optional 2 Protobuf.Decode.int32 (\a r -> { r | supportedFeatures = a })
        , Protobuf.Decode.repeated 15 decodeCodeGeneratorResponse_File .file (\a r -> { r | file = a })
        ]


{-| Encode a `CodeGeneratorResponse` to Bytes


-}
encodeCodeGeneratorResponse : CodeGeneratorResponse -> Protobuf.Encode.Encoder
encodeCodeGeneratorResponse value =
    Protobuf.Encode.message
        [ ( 1, Protobuf.Encode.string value.error )
        , ( 2, Protobuf.Encode.int32 value.supportedFeatures )
        , ( 15, Protobuf.Encode.list encodeCodeGeneratorResponse_File value.file )
        ]


{-| `CodeGeneratorResponse` message


-}
type alias CodeGeneratorResponse =
    { error : String, supportedFeatures : Int, file : List CodeGeneratorResponse_File }


{-| Decode a `CodeGeneratorResponse_File` from Bytes


-}
defaultCodeGeneratorResponse_File : CodeGeneratorResponse_File
defaultCodeGeneratorResponse_File =
    { name = "", insertionPoint = "", content = "", generatedCodeInfo = Nothing }


{-| Decode a `CodeGeneratorResponse_File` from Bytes


-}
decodeCodeGeneratorResponse_File : Protobuf.Decode.Decoder CodeGeneratorResponse_File
decodeCodeGeneratorResponse_File =
    Protobuf.Decode.message
        defaultCodeGeneratorResponse_File
        [ Protobuf.Decode.optional 1 Protobuf.Decode.string (\a r -> { r | name = a })
        , Protobuf.Decode.optional 2 Protobuf.Decode.string (\a r -> { r | insertionPoint = a })
        , Protobuf.Decode.optional 15 Protobuf.Decode.string (\a r -> { r | content = a })
        , Protobuf.Decode.optional
            16
            (Protobuf.Decode.map Just Proto.Google.Protobuf.Descriptor.decodeGeneratedCodeInfo)
            (\a r -> { r | generatedCodeInfo = a })
        ]


{-| Encode a `CodeGeneratorResponse_File` to Bytes


-}
encodeCodeGeneratorResponse_File : CodeGeneratorResponse_File -> Protobuf.Encode.Encoder
encodeCodeGeneratorResponse_File value =
    Protobuf.Encode.message
        [ ( 1, Protobuf.Encode.string value.name )
        , ( 2, Protobuf.Encode.string value.insertionPoint )
        , ( 15, Protobuf.Encode.string value.content )
        , ( 16
          , (Maybe.map Proto.Google.Protobuf.Descriptor.encodeGeneratedCodeInfo
                >> Maybe.withDefault Protobuf.Encode.none
            )
                value.generatedCodeInfo
          )
        ]


{-| `CodeGeneratorResponse_File` message


-}
type alias CodeGeneratorResponse_File =
    { name : String
    , insertionPoint : String
    , content : String
    , generatedCodeInfo : Maybe Proto.Google.Protobuf.Descriptor.GeneratedCodeInfo
    }


{-| Decode a `CodeGeneratorRequest` from Bytes


-}
defaultCodeGeneratorRequest : CodeGeneratorRequest
defaultCodeGeneratorRequest =
    { fileToGenerate = [], parameter = "", protoFile = [], compilerVersion = Nothing }


{-| Decode a `CodeGeneratorRequest` from Bytes


-}
decodeCodeGeneratorRequest : Protobuf.Decode.Decoder CodeGeneratorRequest
decodeCodeGeneratorRequest =
    Protobuf.Decode.message
        defaultCodeGeneratorRequest
        [ Protobuf.Decode.repeated 1 Protobuf.Decode.string .fileToGenerate (\a r -> { r | fileToGenerate = a })
        , Protobuf.Decode.optional 2 Protobuf.Decode.string (\a r -> { r | parameter = a })
        , Protobuf.Decode.repeated
            15
            Proto.Google.Protobuf.Descriptor.decodeFileDescriptorProto
            .protoFile
            (\a r -> { r | protoFile = a })
        , Protobuf.Decode.optional 3 (Protobuf.Decode.map Just decodeVersion) (\a r -> { r | compilerVersion = a })
        ]


{-| Encode a `CodeGeneratorRequest` to Bytes


-}
encodeCodeGeneratorRequest : CodeGeneratorRequest -> Protobuf.Encode.Encoder
encodeCodeGeneratorRequest value =
    Protobuf.Encode.message
        [ ( 1, Protobuf.Encode.list Protobuf.Encode.string value.fileToGenerate )
        , ( 2, Protobuf.Encode.string value.parameter )
        , ( 15, Protobuf.Encode.list Proto.Google.Protobuf.Descriptor.encodeFileDescriptorProto value.protoFile )
        , ( 3, (Maybe.map encodeVersion >> Maybe.withDefault Protobuf.Encode.none) value.compilerVersion )
        ]


{-| `CodeGeneratorRequest` message


-}
type alias CodeGeneratorRequest =
    { fileToGenerate : List String
    , parameter : String
    , protoFile : List Proto.Google.Protobuf.Descriptor.FileDescriptorProto
    , compilerVersion : Maybe Version
    }


{-| Decode a `Version` from Bytes


-}
defaultVersion : Version
defaultVersion =
    { major = 0, minor = 0, patch = 0, suffix = "" }


{-| Decode a `Version` from Bytes


-}
decodeVersion : Protobuf.Decode.Decoder Version
decodeVersion =
    Protobuf.Decode.message
        defaultVersion
        [ Protobuf.Decode.optional 1 Protobuf.Decode.int32 (\a r -> { r | major = a })
        , Protobuf.Decode.optional 2 Protobuf.Decode.int32 (\a r -> { r | minor = a })
        , Protobuf.Decode.optional 3 Protobuf.Decode.int32 (\a r -> { r | patch = a })
        , Protobuf.Decode.optional 4 Protobuf.Decode.string (\a r -> { r | suffix = a })
        ]


{-| Encode a `Version` to Bytes


-}
encodeVersion : Version -> Protobuf.Encode.Encoder
encodeVersion value =
    Protobuf.Encode.message
        [ ( 1, Protobuf.Encode.int32 value.major )
        , ( 2, Protobuf.Encode.int32 value.minor )
        , ( 3, Protobuf.Encode.int32 value.patch )
        , ( 4, Protobuf.Encode.string value.suffix )
        ]


{-| `Version` message


-}
type alias Version =
    { major : Int, minor : Int, patch : Int, suffix : String }


{-| Encode a `CodeGeneratorResponse_Feature` to Bytes


-}
encodeCodeGeneratorResponse_Feature : CodeGeneratorResponse_Feature -> Protobuf.Encode.Encoder
encodeCodeGeneratorResponse_Feature value =
    Protobuf.Encode.int32 <|
        case value of
            CodeGeneratorResponse_Feature_FEATURENONE ->
                0

            CodeGeneratorResponse_Feature_FEATUREPROTO3OPTIONAL ->
                1


{-| Decode a `CodeGeneratorResponse_Feature` from Bytes


-}
decodeCodeGeneratorResponse_Feature : Protobuf.Decode.Decoder CodeGeneratorResponse_Feature
decodeCodeGeneratorResponse_Feature =
    Protobuf.Decode.int32
        |> Protobuf.Decode.map
            (\i ->
                case i of
                    0 ->
                        CodeGeneratorResponse_Feature_FEATURENONE

                    1 ->
                        CodeGeneratorResponse_Feature_FEATUREPROTO3OPTIONAL

                    _ ->
                        CodeGeneratorResponse_Feature_FEATURENONE
            )


{-| `CodeGeneratorResponse_Feature` enumeration


-}
type CodeGeneratorResponse_Feature
    = CodeGeneratorResponse_Feature_FEATURENONE
    | CodeGeneratorResponse_Feature_FEATUREPROTO3OPTIONAL

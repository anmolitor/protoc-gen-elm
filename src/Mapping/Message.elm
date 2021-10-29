module Mapping.Message exposing (..)

import Elm.CodeGen as C
import Mapping.Common as Common
import Meta.Basics
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), Field(..), FieldName, FieldType(..), Message)
import String.Extra


toAST : Message -> List C.Declaration
toAST msg =
    let
        type_ : C.Declaration
        type_ =
            C.aliasDecl (Just <| messageDocumentation msg.dataType)
                msg.dataType
                []
                (C.recordAnn <| List.map (Tuple.mapSecond fieldToTypeAnnotation) msg.fields)

        setter : FieldName -> C.Declaration
        setter fieldName =
            C.funDecl (Just <| setterDocumentation fieldName)
                (Just <|
                    let
                        fieldType =
                            C.typeVar "a"

                        recordType =
                            C.extRecordAnn "r" [ ( fieldName, fieldType ) ]
                    in
                    C.funAnn fieldType (C.funAnn recordType recordType)
                )
                (setterName fieldName)
                [ C.varPattern "a", C.varPattern "r" ]
                (C.update "r" [ ( fieldName, C.val "a" ) ])

        encoder : C.Declaration
        encoder =
            C.funDecl (Just <| Common.encoderDocumentation msg.dataType)
                (Just <| Meta.Encode.encoder (C.typed msg.dataType []))
                (Common.encoderName msg.dataType)
                [ C.varPattern "value" ]
                (Meta.Encode.message
                    (List.map toEncoder msg.fields)
                )

        decoder : C.Declaration
        decoder =
            C.funDecl (Just <| Common.decoderDocumentation msg.dataType)
                (Just <| Meta.Decode.decoder (C.typed msg.dataType []))
                (Common.decoderName msg.dataType)
                []
                (C.apply
                    [ Meta.Decode.message
                    , C.record <| List.map (Tuple.mapSecond toDefaultValue) msg.fields
                    , C.list <| List.map toDecoder msg.fields
                    ]
                )
    in
    [ type_, encoder, decoder ]


setterName fieldName =
    "set" ++ String.Extra.classify fieldName


toDefaultValue : Field -> C.Expression
toDefaultValue field =
    case field of
        Field _ cardinality fieldType ->
            case ( cardinality, fieldType ) of
                ( Optional, _ ) ->
                    Meta.Basics.nothing

                ( Repeated, _ ) ->
                    C.list []

                ( Required, Primitive _ _ defaultValue ) ->
                    C.val defaultValue

                ( Required, _ ) ->
                    Debug.todo "Unhandled: embedded and enum"

        MapField _ _ ->
            Debug.todo "Map field not supported yet"

        OneOfField _ _ ->
            Debug.todo "Oneof field not supported yet"


toDecoder : ( FieldName, Field ) -> C.Expression
toDecoder ( fieldName, field ) =
    case field of
        Field number cardinality fieldType ->
            case cardinality of
                Optional ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , fieldTypeToDecoder fieldType
                        , C.val <| setterName fieldName
                        ]

                Required ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , fieldTypeToDecoder fieldType
                        , C.val <| setterName fieldName
                        ]

                Repeated ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , fieldTypeToDecoder fieldType
                        , C.accessFun fieldName
                        , C.val <| setterName fieldName
                        ]

        MapField _ _ ->
            Debug.todo "Map field not supported yet"

        OneOfField _ _ ->
            Debug.todo "Oneof field not supported yet"


fieldTypeToDecoder : FieldType -> C.Expression
fieldTypeToDecoder fieldType =
    case fieldType of
        Primitive dataType _ _ ->
            Meta.Decode.forPrimitive dataType

        Embedded dataType ->
            Debug.todo "Embedded not supported yet"

        Enumeration _ dataType ->
            Debug.todo "Enumeration not supported yet"


toEncoder : ( FieldName, Field ) -> C.Expression
toEncoder ( fieldName, field ) =
    -- TODO i need to access fields of "value" variable
    case field of
        Field number cardinality fieldType ->
            C.tuple [ C.int number, fieldTypeToEncoder fieldType ]

        MapField _ _ ->
            Debug.todo "Map field not supported yet"

        OneOfField _ _ ->
            Debug.todo "Oneof field not supported yet"


fieldTypeToEncoder : FieldType -> C.Expression
fieldTypeToEncoder fieldType =
    case fieldType of
        Primitive dataType _ _ ->
            Meta.Encode.forPrimitive dataType

        Embedded dataType ->
            Debug.todo "Embedded not supported yet"

        Enumeration _ dataType ->
            Debug.todo "Enumeration not supported yet"


fieldToTypeAnnotation : Field -> C.TypeAnnotation
fieldToTypeAnnotation field =
    case field of
        Field _ cardinality fieldType ->
            case cardinality of
                Optional ->
                    C.maybeAnn (fieldTypeToTypeAnnotation fieldType)

                Required ->
                    fieldTypeToTypeAnnotation fieldType

                Repeated ->
                    C.listAnn (fieldTypeToTypeAnnotation fieldType)

        MapField _ _ ->
            Debug.todo "Map field not supported yet"

        OneOfField _ _ ->
            Debug.todo "Oneof field not supported yet"


fieldTypeToTypeAnnotation : FieldType -> C.TypeAnnotation
fieldTypeToTypeAnnotation fieldType =
    case fieldType of
        Primitive dataType _ _ ->
            Meta.Type.forPrimitive dataType

        Embedded dataType ->
            Debug.todo "Not supported yet"

        Enumeration _ dataType ->
            Debug.todo "Not supported yet"


messageDocumentation : String -> C.Comment C.DocComment
messageDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` message")


setterDocumentation : String -> C.Comment C.DocComment
setterDocumentation fieldName =
    C.emptyDocComment |> C.markdown ("Updates a field of key `" ++ fieldName ++ "` in any record containing that key")

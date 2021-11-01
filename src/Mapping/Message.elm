module Mapping.Message exposing (..)

import Dict exposing (Dict)
import Elm.CodeGen as C
import Mapping.Common as Common
import Mapping.Dependencies as Dependencies exposing (Dependencies)
import Meta.Basics
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), Field(..), FieldName, FieldType(..), Message)
import String.Extra


toAST : Dependencies -> Message -> List C.Declaration
toAST deps msg =
    let
        type_ : C.Declaration
        type_ =
            C.aliasDecl (Just <| messageDocumentation msg.dataType)
                msg.dataType
                []
                (C.recordAnn <| List.map (Tuple.mapSecond <| fieldToTypeAnnotation deps) msg.fields)

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
                    (List.map (toEncoder deps) msg.fields)
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
                    , C.list <| List.map (toDecoder deps) msg.fields
                    ]
                )
    in
    [ type_, encoder, decoder ] ++ List.map (Tuple.first >> setter) msg.fields


setterName fieldName =
    "set" ++ String.Extra.classify fieldName


toDefaultValue : Field -> C.Expression
toDefaultValue field =
    case field of
        Field _ cardinality fieldType ->
            case ( cardinality, fieldType ) of
                ( Optional, Primitive _ _ defaultValue ) ->
                    C.val defaultValue

                ( Repeated, _ ) ->
                    C.list []

                ( Required, Primitive _ _ defaultValue ) ->
                    C.val defaultValue

                ( Optional, Embedded dataType ) ->
                    Meta.Basics.nothing

                ( _, _ ) ->
                    Debug.todo "Unhandled: embedded and enum"

        MapField _ _ ->
            Debug.todo "Map field not supported yet"

        OneOfField _ _ ->
            Debug.todo "Oneof field not supported yet"


toDecoder : Dependencies -> ( FieldName, Field ) -> C.Expression
toDecoder deps ( fieldName, field ) =
    let
        forEmbedded dataType =
            C.fqFun (Dependencies.resolveType dataType deps |> Maybe.withDefault [])
                (Common.decoderName dataType)
    in
    case field of
        Field number cardinality fieldType ->
            case ( cardinality, fieldType ) of
                ( Optional, Primitive dataType _ _ ) ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , Meta.Decode.forPrimitive dataType
                        , C.val <| setterName fieldName
                        ]

                ( Optional, Embedded dataType ) ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , C.parens (C.apply [ Meta.Decode.map, Meta.Basics.just, forEmbedded dataType ])
                        , C.val <| setterName fieldName
                        ]

                ( Required, Primitive dataType _ _ ) ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , Meta.Decode.forPrimitive dataType
                        , C.val <| setterName fieldName
                        ]

                ( Required, Embedded dataType ) ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , forEmbedded dataType
                        , C.val <| setterName fieldName
                        ]

                ( Repeated, Primitive dataType _ _ ) ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , Meta.Decode.forPrimitive dataType
                        , C.accessFun fieldName
                        , C.val <| setterName fieldName
                        ]

                ( Repeated, Embedded dataType ) ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , C.fqFun (Dependencies.resolveType dataType deps |> Maybe.withDefault [])
                            (Common.decoderName dataType)
                        , C.accessFun fieldName
                        , C.val <| setterName fieldName
                        ]

                _ ->
                    C.string "NOT SUPPORTED"

        MapField _ _ ->
            C.string "Map field not supported yet"

        OneOfField _ _ ->
            C.string "Oneof field not supported yet"


toEncoder : Dependencies -> ( FieldName, Field ) -> C.Expression
toEncoder deps ( fieldName, field ) =
    let
        forEmbedded dataType =
            C.fqFun (Dependencies.resolveType dataType deps |> Maybe.withDefault [])
                (Common.encoderName dataType)

        fieldTypeToEncoder : Cardinality -> FieldType -> C.Expression
        fieldTypeToEncoder cardinality fieldType =
            case ( cardinality, fieldType ) of
                ( Optional, Primitive dataType _ _ ) ->
                    Meta.Encode.forPrimitive dataType

                ( Optional, Embedded dataType ) ->
                    C.parens <|
                        C.applyBinOp
                            (C.apply [ Meta.Basics.mapMaybe, forEmbedded dataType ])
                            C.composer
                            (C.apply [ Meta.Basics.withDefault, Meta.Encode.none ])

                ( Required, Primitive dataType _ _ ) ->
                    Meta.Encode.forPrimitive dataType

                ( Required, Embedded dataType ) ->
                    forEmbedded dataType

                ( Repeated, Primitive dataType _ _ ) ->
                    C.apply [ Meta.Encode.list, Meta.Encode.forPrimitive dataType ]

                ( Repeated, Embedded dataType ) ->
                    C.apply [ Meta.Encode.list, forEmbedded dataType ]

                _ ->
                    C.string "Enumeration not supported yet"
    in
    -- TODO i need to access fields of "value" variable in a better way
    case field of
        Field number cardinality fieldType ->
            C.tuple [ C.int number, C.apply [ fieldTypeToEncoder cardinality fieldType, C.access (C.val "value") fieldName ] ]

        MapField _ _ ->
            C.string "Map field not supported yet"

        OneOfField _ _ ->
            C.string "Oneof field not supported yet"


fieldToTypeAnnotation : Dependencies -> Field -> C.TypeAnnotation
fieldToTypeAnnotation deps field =
    let
        forEmbedded dataType =
            C.fqTyped
                (Dependencies.resolveType dataType deps |> Maybe.withDefault [])
                dataType
                []
    in
    case field of
        Field _ cardinality fieldType ->
            case ( cardinality, fieldType ) of
                ( Optional, Primitive dataType _ _ ) ->
                    Meta.Type.forPrimitive dataType

                ( Required, Primitive dataType _ _ ) ->
                    Meta.Type.forPrimitive dataType

                ( Optional, Embedded dataType ) ->
                    C.maybeAnn <| forEmbedded dataType

                ( Required, Embedded dataType ) ->
                    forEmbedded dataType

                ( Repeated, Primitive dataType _ _ ) ->
                    C.listAnn <| Meta.Type.forPrimitive dataType

                ( Repeated, Embedded dataType ) ->
                    C.listAnn <| forEmbedded dataType

                _ ->
                    C.typeVar "Enumeration not supported yet"

        MapField _ _ ->
            C.typeVar "Map field not supported yet"

        OneOfField _ _ ->
            C.typeVar "Oneof field not supported yet"


messageDocumentation : String -> C.Comment C.DocComment
messageDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` message")


setterDocumentation : String -> C.Comment C.DocComment
setterDocumentation fieldName =
    C.emptyDocComment |> C.markdown ("Updates a field of key `" ++ fieldName ++ "` in any record containing that key")

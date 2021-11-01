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
    [ type_, encoder, decoder ]
        ++ List.concatMap (fieldDeclarations deps) msg.fields


setter : FieldName -> C.Expression
setter fieldName =
    C.parens <|
        C.lambda
            [ C.varPattern "a", C.varPattern "r" ]
            (C.update "r" [ ( fieldName, C.val "a" ) ])


fieldDeclarations : Dependencies -> ( FieldName, Field ) -> List C.Declaration
fieldDeclarations deps ( _, field ) =
    case field of
        Field _ _ _ ->
            []

        MapField _ _ ->
            []

        OneOfField dataType options ->
            let
                typeForFieldType : FieldType -> C.TypeAnnotation
                typeForFieldType ft =
                    case ft of
                        Primitive prim _ _ ->
                            Meta.Type.forPrimitive prim

                        Embedded embedded ->
                            C.fqTyped
                                (Dependencies.resolveType embedded deps |> Maybe.withDefault [])
                                embedded
                                []

                        Enumeration _ _ ->
                            C.typeVar "Enumeration not supported"

                type_ =
                    C.customTypeDecl
                        (Just <| oneofDocumentation dataType)
                        dataType
                        []
                        (List.map (\( _, innerDataType, innerFieldType ) -> ( innerDataType, [ typeForFieldType innerFieldType ] )) options)
            in
            [ type_ ]


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
                    C.string "Unhandled: enum"

        MapField _ _ ->
            C.string "Map field not supported yet"

        OneOfField _ _ ->
            Meta.Basics.nothing


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
                        , setter fieldName
                        ]

                ( Optional, Embedded dataType ) ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , C.parens (C.apply [ Meta.Decode.map, Meta.Basics.just, forEmbedded dataType ])
                        , setter fieldName
                        ]

                ( Required, Primitive dataType _ _ ) ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , Meta.Decode.forPrimitive dataType
                        , setter fieldName
                        ]

                ( Required, Embedded dataType ) ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , forEmbedded dataType
                        , setter fieldName
                        ]

                ( Repeated, Primitive dataType _ _ ) ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , Meta.Decode.forPrimitive dataType
                        , C.accessFun fieldName
                        , setter fieldName
                        ]

                ( Repeated, Embedded dataType ) ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , C.fqFun (Dependencies.resolveType dataType deps |> Maybe.withDefault [])
                            (Common.decoderName dataType)
                        , C.accessFun fieldName
                        , setter fieldName
                        ]

                _ ->
                    C.string "NOT SUPPORTED"

        MapField _ _ ->
            C.string "Map field not supported yet"

        OneOfField _ options ->
            C.apply
                [ Meta.Decode.oneOf
                , C.list
                    (List.map
                        (\( fieldNumber, optionName, fieldType ) ->
                            C.tuple
                                [ C.int fieldNumber
                                , C.apply
                                    [ Meta.Decode.map
                                    , C.val optionName
                                    , case fieldType of
                                        Primitive p _ _ ->
                                            Meta.Decode.forPrimitive p

                                        Embedded e ->
                                            forEmbedded e

                                        Enumeration _ _ ->
                                            C.string "Enumeration not supported"
                                    ]
                                ]
                        )
                        options
                    )
                , setter fieldName
                ]


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

        OneOfField _ options ->
            C.caseExpr (C.access (C.val "value") fieldName)
                (( C.namedPattern "Nothing" [], C.tuple [ C.int 0, Meta.Encode.none ] )
                    :: List.map
                        (\( fieldNumber, optionName, fieldType ) ->
                            ( C.namedPattern "Just" [ C.parensPattern (C.namedPattern optionName [ C.varPattern "innerValue" ]) ]
                            , C.tuple [ C.int fieldNumber, C.apply [ fieldTypeToEncoder Optional fieldType, C.val "innerValue" ] ]
                            )
                        )
                        options
                )


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

        OneOfField dataType _ ->
            C.maybeAnn <| C.typed dataType []


messageDocumentation : String -> C.Comment C.DocComment
messageDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` message")


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")


setterDocumentation : String -> C.Comment C.DocComment
setterDocumentation fieldName =
    C.emptyDocComment |> C.markdown ("Updates a field of key `" ++ fieldName ++ "` in any record containing that key")

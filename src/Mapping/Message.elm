module Mapping.Message exposing (..)

import Elm.CodeGen as C
import Mapping.Common as Common
import Meta.Basics
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), Field(..), FieldName, FieldType(..), Map, Message)


toAST : Message -> List C.Declaration
toAST msg =
    let
        type_ : C.Declaration
        type_ =
            C.aliasDecl (Just <| messageDocumentation msg.dataType)
                msg.dataType
                []
                (C.recordAnn <| List.map (Tuple.mapSecond fieldToTypeAnnotation) msg.fields)

        encoder : C.Declaration
        encoder =
            C.funDecl (Just <| Common.encoderDocumentation msg.dataType)
                (Just <| Meta.Encode.encoder (C.typed msg.dataType []))
                (Common.encoderName msg.dataType)
                [ if msg.fields == [] then
                    C.allPattern

                  else
                    C.varPattern "value"
                ]
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
        ++ List.concatMap fieldDeclarations msg.fields


mapToAST : Map -> List C.Declaration
mapToAST map =
    let
        type_ =
            C.aliasDecl (Just <| mapComment map)
                map.dataType
                []
                (C.fqTyped [ "Dict" ] "Dict" [ fieldTypeToTypeAnnotation map.key, fieldTypeToTypeAnnotation map.value ])
    in
    [ type_ ]


mapComment : Map -> C.Comment C.DocComment
mapComment map =
    C.emptyDocComment
        |> C.markdown ("Dict for " ++ map.dataType)


setter : FieldName -> C.Expression
setter fieldName =
    C.parens <|
        C.lambda
            [ C.varPattern "a", C.varPattern "r" ]
            (C.update "r" [ ( fieldName, C.val "a" ) ])


fieldDeclarations : ( FieldName, Field ) -> List C.Declaration
fieldDeclarations ( _, field ) =
    case field of
        NormalField _ _ _ ->
            []

        MapField _ _ _ ->
            []

        OneOfField dataType options ->
            let
                typeForFieldType : FieldType -> C.TypeAnnotation
                typeForFieldType ft =
                    case ft of
                        Primitive prim _ _ ->
                            Meta.Type.forPrimitive prim

                        Embedded typeRef moduleName ->
                            C.fqTyped moduleName typeRef []

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
        NormalField _ cardinality fieldType ->
            case ( cardinality, fieldType ) of
                ( Optional, Primitive _ _ defaultValue ) ->
                    C.val defaultValue

                ( Repeated, _ ) ->
                    C.list []

                ( Required, Primitive _ _ defaultValue ) ->
                    C.val defaultValue

                ( Optional, Embedded _ _ ) ->
                    Meta.Basics.nothing

                ( _, _ ) ->
                    C.string "Unhandled: enum"

        MapField _ _ _ ->
            C.fqFun [ "Dict" ] "empty"

        OneOfField _ _ ->
            Meta.Basics.nothing


toDecoder : ( FieldName, Field ) -> C.Expression
toDecoder ( fieldName, field ) =
    case field of
        NormalField number cardinality fieldType ->
            case ( cardinality, fieldType ) of
                ( Optional, Primitive dataType _ _ ) ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , Meta.Decode.forPrimitive dataType
                        , setter fieldName
                        ]

                ( Optional, Embedded typeRef moduleName ) ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , C.parens
                            (C.apply
                                [ Meta.Decode.map
                                , Meta.Basics.just
                                , C.fqFun moduleName (Common.decoderName typeRef)
                                ]
                            )
                        , setter fieldName
                        ]

                ( Required, Primitive dataType _ _ ) ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , Meta.Decode.forPrimitive dataType
                        , setter fieldName
                        ]

                ( Required, Embedded typeRef moduleName ) ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , C.fqFun moduleName (Common.decoderName typeRef)
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

                ( Repeated, Embedded typeRef moduleName ) ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , C.fqFun moduleName (Common.decoderName typeRef)
                        , C.accessFun fieldName
                        , setter fieldName
                        ]

                _ ->
                    C.string "NOT SUPPORTED"

        MapField number key value ->
            C.apply
                [ Meta.Decode.mapped
                , C.int number
                , C.tuple []
                , Meta.Decode.forPrimitive key
                , C.accessFun fieldName
                , setter fieldName
                ]

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

                                        Embedded typeRef moduleName ->
                                            C.fqFun moduleName (Common.decoderName typeRef)

                                        Enumeration _ _ ->
                                            C.string "Enumeration not supported"
                                    ]
                                ]
                        )
                        options
                    )
                , setter fieldName
                ]


toEncoder : ( FieldName, Field ) -> C.Expression
toEncoder ( fieldName, field ) =
    let
        fieldTypeToEncoder : Cardinality -> FieldType -> C.Expression
        fieldTypeToEncoder cardinality fieldType =
            case ( cardinality, fieldType ) of
                ( Optional, Primitive dataType _ _ ) ->
                    Meta.Encode.forPrimitive dataType

                ( Optional, Embedded typeRef moduleName ) ->
                    C.parens <|
                        C.applyBinOp
                            (C.apply [ Meta.Basics.mapMaybe, C.fqFun moduleName (Common.encoderName typeRef) ])
                            C.composer
                            (C.apply [ Meta.Basics.withDefault, Meta.Encode.none ])

                ( Required, Primitive dataType _ _ ) ->
                    Meta.Encode.forPrimitive dataType

                ( Required, Embedded typeRef moduleName ) ->
                    C.fqFun moduleName (Common.encoderName typeRef)

                ( Repeated, Primitive dataType _ _ ) ->
                    C.apply [ Meta.Encode.list, Meta.Encode.forPrimitive dataType ]

                ( Repeated, Embedded typeRef moduleName ) ->
                    C.apply [ Meta.Encode.list, C.fqFun moduleName (Common.encoderName typeRef) ]

                _ ->
                    C.string "Enumeration not supported yet"
    in
    -- TODO i need to access fields of "value" variable in a better way
    case field of
        NormalField number cardinality fieldType ->
            C.tuple [ C.int number, C.apply [ fieldTypeToEncoder cardinality fieldType, C.access (C.val "value") fieldName ] ]

        MapField number key value ->
            C.tuple
                [ C.int number
                , C.apply
                    [ Meta.Encode.dict
                    , Meta.Encode.forPrimitive key
                    , fieldTypeToEncoder Optional value
                    , C.access (C.val "value") fieldName
                    ]
                ]

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


fieldTypeToTypeAnnotation : FieldType -> C.TypeAnnotation
fieldTypeToTypeAnnotation fieldType =
    case fieldType of
        Primitive dataType _ _ ->
            Meta.Type.forPrimitive dataType

        Embedded typeRef moduleName ->
            C.fqTyped moduleName typeRef []

        _ ->
            C.typeVar "Enumeration not supported yet"


fieldToTypeAnnotation : Field -> C.TypeAnnotation
fieldToTypeAnnotation field =
    case field of
        NormalField _ cardinality fieldType ->
            (case ( cardinality, fieldType ) of
                ( Optional, Primitive dataType _ _ ) ->
                    identity

                ( Required, _ ) ->
                    identity

                ( Optional, _ ) ->
                    C.maybeAnn

                ( Repeated, _ ) ->
                    C.listAnn
            )
                (fieldTypeToTypeAnnotation fieldType)

        MapField _ key value ->
            Meta.Type.forPrimitive key

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

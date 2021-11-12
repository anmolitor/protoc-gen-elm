module Generator.Message exposing (..)

import Elm.CodeGen as C
import Generator.Common as Common
import Meta.Basics
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), DataType, Field(..), FieldName, FieldType(..), Map, Message, TypeKind(..))


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
            C.valDecl (Just <| Common.decoderDocumentation msg.dataType)
                (Just <| Meta.Decode.decoder (C.typed msg.dataType []))
                (Common.decoderName msg.dataType)
                (C.apply
                    [ Meta.Decode.message
                    , C.val <| Common.defaultName msg.dataType
                    , C.list <| List.map toDecoder msg.fields
                    ]
                )

        default : C.Declaration
        default =
            C.valDecl (Just <| Common.defaultDocumentation msg.dataType)
                (Just <| C.typed msg.dataType [])
                (Common.defaultName msg.dataType)
                (C.record <| List.map (Tuple.mapSecond toDefaultValue) msg.fields)
    in
    [ type_, encoder, decoder, default ]
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


getter : FieldName -> C.Expression
getter fieldName =
    C.accessFun <| "." ++ fieldName


setter : FieldName -> C.Expression
setter fieldName =
    C.parens <|
        C.lambda
            [ C.varPattern "a", C.varPattern "r" ]
            (C.update "r" [ ( fieldName, C.val "a" ) ])


fieldDeclarations : ( FieldName, Field ) -> List C.Declaration
fieldDeclarations ( _, field ) =
    case field of
        NormalField _ _ (Embedded embedded) ->
            case embedded.typeKind of
                Alias ->
                    []

                Type ->
                    let
                        recursiveWrapperName =
                            recursiveDataTypeName embedded.dataType

                        wrappedAnn =
                            C.fqTyped embedded.moduleName embedded.dataType []

                        recursiveTypeWrapper : C.Declaration
                        recursiveTypeWrapper =
                            C.customTypeDecl (Just <| recursiveDataTypeDocumentation embedded.dataType) recursiveWrapperName [] [ ( recursiveWrapperName, [ wrappedAnn ] ) ]

                        unwrapper : C.Declaration
                        unwrapper =
                            C.funDecl (Just <| recursiveUnwrapDocumentation embedded.dataType)
                                (Just <| C.funAnn (C.typed recursiveWrapperName []) wrappedAnn)
                                (recursiveUnwrapName embedded.dataType)
                                [ C.namedPattern recursiveWrapperName [ C.varPattern "wrapped" ] ]
                                (C.val "wrapped")
                    in
                    [ recursiveTypeWrapper, unwrapper ]

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

                        Embedded e ->
                            C.fqTyped e.moduleName e.dataType []

                        Enumeration enum ->
                            C.fqTyped enum.moduleName enum.dataType []

                type_ =
                    C.customTypeDecl
                        (Just <| oneofDocumentation dataType)
                        dataType
                        []
                        (List.map (\( _, innerDataType, innerFieldType ) -> ( innerDataType, [ typeForFieldType innerFieldType ] )) options)
            in
            [ type_ ]


fieldTypeToDefaultValue : FieldType -> C.Expression
fieldTypeToDefaultValue fieldType =
    case fieldType of
        Primitive _ _ defaultValue ->
            C.val defaultValue

        Embedded _ ->
            Meta.Basics.nothing

        Enumeration enum ->
            C.val enum.default


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

                ( Optional, Embedded _ ) ->
                    Meta.Basics.nothing

                ( Required, Embedded e ) ->
                    C.fqVal e.moduleName (Common.defaultName e.dataType)
                        |> (\val ->
                                case e.typeKind of
                                    Alias ->
                                        val

                                    Type ->
                                        C.apply [ C.val (recursiveDataTypeName e.dataType), val ]
                           )

                ( _, Enumeration enum ) ->
                    C.val enum.default

        MapField _ _ _ ->
            C.fqFun [ "Dict" ] "empty"

        OneOfField _ _ ->
            Meta.Basics.nothing


toDecoder : ( FieldName, Field ) -> C.Expression
toDecoder ( fieldName, field ) =
    let
        embeddedDecoder : { dataType : DataType, moduleName : C.ModuleName, typeKind : TypeKind } -> C.Expression
        embeddedDecoder e =
            (case e.typeKind of
                Alias ->
                    identity

                Type ->
                    C.parens
                        << C.applyBinOp (C.apply [ Meta.Decode.map, C.val <| recursiveDataTypeName e.dataType ]) C.pipel
                        << C.applyBinOp Meta.Decode.lazy C.pipel
                        << C.lambda [ C.allPattern ]
            )
                (C.fqFun e.moduleName (Common.decoderName e.dataType))

        fieldTypeToDecoder : FieldType -> Cardinality -> C.Expression
        fieldTypeToDecoder fieldType cardinality =
            case ( cardinality, fieldType ) of
                ( _, Primitive dataType _ _ ) ->
                    Meta.Decode.forPrimitive dataType

                ( Optional, Embedded e ) ->
                    C.parens
                        (C.apply
                            [ Meta.Decode.map
                            , Meta.Basics.just
                            , embeddedDecoder e
                            ]
                        )

                ( Required, Embedded e ) ->
                    embeddedDecoder e

                ( Repeated, Embedded e ) ->
                    embeddedDecoder e

                ( _, Enumeration enum ) ->
                    C.fqFun enum.moduleName (Common.decoderName enum.dataType)
    in
    case field of
        NormalField number cardinality fieldType ->
            case cardinality of
                Optional ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , setter fieldName
                        ]

                Required ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , setter fieldName
                        ]

                Repeated ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , getter fieldName
                        , setter fieldName
                        ]

        MapField number key value ->
            C.apply
                [ Meta.Decode.mapped
                , C.int number
                , C.tuple [ fieldTypeToDefaultValue key, fieldTypeToDefaultValue value ]
                , fieldTypeToDecoder key Optional
                , fieldTypeToDecoder value Optional
                , C.accessFun <| "." ++ fieldName
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

                                        Embedded e ->
                                            C.fqFun e.moduleName (Common.decoderName e.dataType)

                                        Enumeration enum ->
                                            C.fqFun enum.moduleName (Common.decoderName enum.dataType)
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
        embeddedEncoder : { dataType : DataType, moduleName : C.ModuleName, typeKind : TypeKind } -> C.Expression
        embeddedEncoder e =
            (case e.typeKind of
                Alias ->
                    identity

                Type ->
                    C.parens << C.applyBinOp (C.fun <| recursiveUnwrapName e.dataType) C.composer
            )
                (C.fqFun e.moduleName (Common.encoderName e.dataType))

        fieldTypeToEncoder : Cardinality -> FieldType -> C.Expression
        fieldTypeToEncoder cardinality fieldType =
            case ( cardinality, fieldType ) of
                ( Optional, Primitive dataType _ _ ) ->
                    Meta.Encode.forPrimitive dataType

                ( Optional, Embedded e ) ->
                    C.parens <|
                        C.applyBinOp
                            (C.apply [ Meta.Basics.mapMaybe, embeddedEncoder e ])
                            C.composer
                            (C.apply [ Meta.Basics.withDefault, Meta.Encode.none ])

                ( Optional, Enumeration enum ) ->
                    C.fqFun enum.moduleName (Common.encoderName enum.dataType)

                ( Required, Primitive dataType _ _ ) ->
                    Meta.Encode.forPrimitive dataType

                ( Required, Embedded e ) ->
                    embeddedEncoder e

                ( Required, Enumeration enum ) ->
                    C.fqFun enum.moduleName (Common.encoderName enum.dataType)

                ( Repeated, Primitive dataType _ _ ) ->
                    C.apply [ Meta.Encode.list, Meta.Encode.forPrimitive dataType ]

                ( Repeated, Embedded e ) ->
                    C.apply [ Meta.Encode.list, embeddedEncoder e ]

                ( Repeated, Enumeration enum ) ->
                    C.apply [ Meta.Encode.list, C.fqFun enum.moduleName (Common.encoderName enum.dataType) ]
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
                    , fieldTypeToEncoder Optional key
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
                            , C.tuple [ C.int fieldNumber, C.apply [ fieldTypeToEncoder Required fieldType, C.val "innerValue" ] ]
                            )
                        )
                        options
                )


fieldTypeToTypeAnnotation : FieldType -> C.TypeAnnotation
fieldTypeToTypeAnnotation fieldType =
    case fieldType of
        Primitive dataType _ _ ->
            Meta.Type.forPrimitive dataType

        Embedded e ->
            C.fqTyped e.moduleName
                (case e.typeKind of
                    Alias ->
                        e.dataType

                    Type ->
                        recursiveDataTypeName e.dataType
                )
                []

        Enumeration enum ->
            C.fqTyped enum.moduleName enum.dataType []


fieldToTypeAnnotation : Field -> C.TypeAnnotation
fieldToTypeAnnotation field =
    let
        cardinalityModifier cardinality fieldType =
            case ( cardinality, fieldType ) of
                ( Optional, Primitive _ _ _ ) ->
                    identity

                ( Optional, Enumeration _ ) ->
                    identity

                ( Required, _ ) ->
                    identity

                ( Optional, _ ) ->
                    C.maybeAnn

                ( Repeated, _ ) ->
                    C.listAnn
    in
    case field of
        NormalField _ cardinality fieldType ->
            cardinalityModifier cardinality
                fieldType
                (fieldTypeToTypeAnnotation fieldType)

        MapField _ key value ->
            Meta.Type.dict (fieldTypeToTypeAnnotation key) (cardinalityModifier Optional value <| fieldTypeToTypeAnnotation value)

        OneOfField dataType _ ->
            C.maybeAnn <| C.typed dataType []


recursiveDataTypeName : String -> String
recursiveDataTypeName wrappedDataType =
    wrappedDataType ++ "_"


recursiveUnwrapName : String -> String
recursiveUnwrapName wrappedDataType =
    "unwrap" ++ recursiveDataTypeName wrappedDataType


messageDocumentation : String -> C.Comment C.DocComment
messageDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` message")


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")


setterDocumentation : String -> C.Comment C.DocComment
setterDocumentation fieldName =
    C.emptyDocComment |> C.markdown ("Updates a field of key `" ++ fieldName ++ "` in any record containing that key")


recursiveDataTypeDocumentation : String -> C.Comment C.DocComment
recursiveDataTypeDocumentation wrappedDataType =
    C.emptyDocComment |> C.markdown ("Type wrapper for alias type `" ++ wrappedDataType ++ "` to avoid unlimited recursion.")


recursiveUnwrapDocumentation : String -> C.Comment C.DocComment
recursiveUnwrapDocumentation wrappedDataType =
    C.emptyDocComment |> C.markdown ("Unwrap a `" ++ wrappedDataType ++ "` from its wrapper `" ++ recursiveDataTypeName wrappedDataType ++ "`")

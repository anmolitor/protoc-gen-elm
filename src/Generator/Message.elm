module Generator.Message exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import Mapper.Name
import Meta.Basics
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), DataType, Field(..), FieldName, FieldType(..), Map, Message, TypeKind(..))


reexportAST : ModuleName -> Message -> List C.Declaration
reexportAST moduleName msg =
    let
        type_ =
            C.aliasDecl (Just <| messageDocumentation msg.dataType) msg.dataType [] <|
                C.fqTyped Common.internalsModule (Mapper.Name.internalize ( moduleName, msg.dataType )) []

        encoder =
            C.valDecl (Just <| Common.encoderDocumentation msg.dataType)
                (Just <| Meta.Encode.encoder (C.typed msg.dataType []))
                (Common.encoderName msg.dataType)
                (C.fqVal Common.internalsModule <| Common.encoderName <| Mapper.Name.internalize ( moduleName, msg.dataType ))

        decoder =
            C.valDecl (Just <| Common.decoderDocumentation msg.dataType)
                (Just <| Meta.Decode.decoder (C.typed msg.dataType []))
                (Common.decoderName msg.dataType)
                (C.fqVal Common.internalsModule <| Common.decoderName <| Mapper.Name.internalize ( moduleName, msg.dataType ))

        default =
            C.valDecl (Just <| Common.defaultDocumentation msg.dataType)
                (Just <| C.typed msg.dataType [])
                (Common.defaultName msg.dataType)
                (C.fqVal Common.internalsModule <| Common.defaultName <| Mapper.Name.internalize ( moduleName, msg.dataType ))
    in
    [ type_, encoder, decoder, default ] ++ List.concatMap fieldDeclarationsReexport msg.fields


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


mapComment : Map -> C.Comment C.DocComment
mapComment map =
    C.emptyDocComment
        |> C.markdown ("Dict for " ++ map.dataType)


getter : FieldName -> C.Expression
getter fieldName =
    C.accessFun <| "." ++ fieldName


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
                            recursiveDataTypeName (Mapper.Name.internalize ( embedded.moduleName, embedded.dataType ))

                        wrappedAnn =
                            C.typed (Mapper.Name.internalize ( embedded.moduleName, embedded.dataType )) []

                        recursiveTypeWrapper : C.Declaration
                        recursiveTypeWrapper =
                            C.customTypeDecl (Just <| recursiveDataTypeDocumentation embedded.dataType) recursiveWrapperName [] [ ( recursiveWrapperName, [ wrappedAnn ] ) ]

                        unwrapper : C.Declaration
                        unwrapper =
                            C.funDecl (Just <| recursiveUnwrapDocumentation embedded.dataType)
                                (Just <| C.funAnn (C.typed recursiveWrapperName []) wrappedAnn)
                                (recursiveUnwrapName <| Mapper.Name.internalize ( embedded.moduleName, embedded.dataType ))
                                [ C.namedPattern recursiveWrapperName [ C.varPattern "wrapped" ] ]
                                (C.val "wrapped")
                    in
                    [ recursiveTypeWrapper, unwrapper ]

        NormalField _ _ _ ->
            []

        MapField _ _ _ ->
            []

        OneOfField _ _ ->
            []


fieldDeclarationsReexport : ( FieldName, Field ) -> List C.Declaration
fieldDeclarationsReexport ( _, field ) =
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
                            C.typed embedded.dataType []

                        recursiveTypeWrapper : C.Declaration
                        recursiveTypeWrapper =
                            C.aliasDecl (Just <| recursiveDataTypeDocumentation embedded.dataType)
                                recursiveWrapperName
                                []
                                (C.fqTyped Common.internalsModule
                                    (Mapper.Name.internalize
                                        ( embedded.moduleName, recursiveDataTypeName embedded.dataType )
                                    )
                                    []
                                )

                        wrapper : C.Declaration
                        wrapper =
                            C.valDecl (Just <| recursiveWrapDocumentation embedded.dataType)
                                (Just <| C.funAnn wrappedAnn (C.typed recursiveWrapperName []))
                                (recursiveWrapName embedded.dataType)
                                (C.fqVal Common.internalsModule
                                    (Mapper.Name.internalize
                                        ( embedded.moduleName, recursiveDataTypeName embedded.dataType )
                                    )
                                )

                        unwrapper : C.Declaration
                        unwrapper =
                            C.valDecl (Just <| recursiveUnwrapDocumentation embedded.dataType)
                                (Just <| C.funAnn (C.typed recursiveWrapperName []) wrappedAnn)
                                (recursiveUnwrapName embedded.dataType)
                                (C.fqVal Common.internalsModule <| recursiveUnwrapName <| Mapper.Name.internalize ( embedded.moduleName, embedded.dataType ))
                    in
                    [ recursiveTypeWrapper, wrapper, unwrapper ]

        NormalField _ _ _ ->
            []

        MapField _ _ _ ->
            []

        OneOfField _ _ ->
            []


fieldTypeToDefaultValue : FieldType -> C.Expression
fieldTypeToDefaultValue fieldType =
    case fieldType of
        Primitive _ defaultValue ->
            defaultValue

        Embedded _ ->
            Meta.Basics.nothing

        Enumeration enum ->
            C.val (Common.defaultName <| Mapper.Name.internalize ( enum.moduleName, enum.dataType ))


toDefaultValue : Field -> C.Expression
toDefaultValue field =
    case field of
        NormalField _ cardinality fieldType ->
            case ( cardinality, fieldType ) of
                ( Proto3Optional, _ ) ->
                    Meta.Basics.nothing

                ( Optional, Primitive _ defaultValue ) ->
                    defaultValue

                ( Repeated, _ ) ->
                    C.list []

                ( Required, Primitive _ defaultValue ) ->
                    defaultValue

                ( Optional, Embedded _ ) ->
                    Meta.Basics.nothing

                ( Required, Embedded e ) ->
                    C.val (Common.defaultName <| Mapper.Name.internalize ( e.moduleName, e.dataType ))
                        |> (\val ->
                                case e.typeKind of
                                    Alias ->
                                        val

                                    Type ->
                                        C.apply [ C.val (recursiveDataTypeName <| Mapper.Name.internalize ( e.moduleName, e.dataType )), val ]
                           )

                ( _, Enumeration enum ) ->
                    fieldTypeToDefaultValue (Enumeration enum)

        MapField _ _ _ ->
            C.fqFun [ "Dict" ] "empty"

        OneOfField _ _ ->
            Meta.Basics.nothing


toDecoder : ( FieldName, Field ) -> C.Expression
toDecoder ( fieldName, field ) =
    case field of
        NormalField number cardinality fieldType ->
            case cardinality of
                Optional ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , Common.setter fieldName
                        ]

                Proto3Optional ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , Common.setter fieldName
                        ]

                Required ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , Common.setter fieldName
                        ]

                Repeated ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , getter fieldName
                        , Common.setter fieldName
                        ]

        MapField number key value ->
            C.apply
                [ Meta.Decode.mapped
                , C.int number
                , C.tuple [ fieldTypeToDefaultValue key, fieldTypeToDefaultValue value ]
                , fieldTypeToDecoder key Optional
                , fieldTypeToDecoder value Optional
                , C.accessFun <| "." ++ fieldName
                , Common.setter fieldName
                ]

        OneOfField dataType oneOfModuleName ->
            C.apply
                [ C.fun (Common.decoderName <| Mapper.Name.internalize ( oneOfModuleName, dataType ))
                , Common.setter fieldName
                ]


embeddedDecoder : { dataType : DataType, moduleName : C.ModuleName, typeKind : TypeKind } -> C.Expression
embeddedDecoder e =
    (case e.typeKind of
        Alias ->
            identity

        Type ->
            C.parens
                << C.applyBinOp (C.apply [ Meta.Decode.map, C.val <| recursiveDataTypeName <| Mapper.Name.internalize ( e.moduleName, e.dataType ) ]) C.pipel
                << C.applyBinOp Meta.Decode.lazy C.pipel
                << C.lambda [ C.allPattern ]
    )
        (C.fun (Common.decoderName <| Mapper.Name.internalize ( e.moduleName, e.dataType )))


fieldTypeToDecoder : FieldType -> Cardinality -> C.Expression
fieldTypeToDecoder fieldType cardinality =
    case ( cardinality, fieldType ) of
        ( Proto3Optional, Primitive dataType _ ) ->
            C.parens
                (C.apply
                    [ Meta.Decode.map
                    , Meta.Basics.just
                    , Meta.Decode.forPrimitive dataType
                    ]
                )

        ( _, Primitive dataType _ ) ->
            Meta.Decode.forPrimitive dataType

        ( Required, Embedded e ) ->
            embeddedDecoder e

        ( Repeated, Embedded e ) ->
            embeddedDecoder e

        ( _, Embedded e ) ->
            C.parens
                (C.apply
                    [ Meta.Decode.map
                    , Meta.Basics.just
                    , embeddedDecoder e
                    ]
                )

        ( _, Enumeration enum ) ->
            C.fun (Common.decoderName <| Mapper.Name.internalize ( enum.moduleName, enum.dataType ))


toEncoder : ( FieldName, Field ) -> C.Expression
toEncoder ( fieldName, field ) =
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

        OneOfField dataType oneOfModuleName ->
            C.apply [ C.fun (Common.encoderName <| Mapper.Name.internalize ( oneOfModuleName, dataType )), C.access (C.val "value") fieldName ]


embeddedEncoder : { dataType : DataType, moduleName : C.ModuleName, typeKind : TypeKind } -> C.Expression
embeddedEncoder e =
    (case e.typeKind of
        Alias ->
            identity

        Type ->
            C.parens << C.applyBinOp (C.fun <| recursiveUnwrapName <| Mapper.Name.internalize ( e.moduleName, e.dataType )) C.composer
    )
        (C.fun (Common.encoderName <| Mapper.Name.internalize ( e.moduleName, e.dataType )))


fieldTypeToEncoder : Cardinality -> FieldType -> C.Expression
fieldTypeToEncoder cardinality fieldType =
    case ( cardinality, fieldType ) of
        ( Proto3Optional, Primitive dataType _ ) ->
            C.parens <|
                C.applyBinOp
                    (C.apply [ Meta.Basics.mapMaybe, Meta.Encode.forPrimitive dataType ])
                    C.composer
                    (C.apply [ Meta.Basics.withDefault, Meta.Encode.none ])

        ( Optional, Primitive dataType _ ) ->
            Meta.Encode.forPrimitive dataType

        ( Required, Primitive dataType _ ) ->
            Meta.Encode.forPrimitive dataType

        ( Required, Embedded e ) ->
            embeddedEncoder e

        ( Required, Enumeration enum ) ->
            C.fun (Common.encoderName <| Mapper.Name.internalize ( enum.moduleName, enum.dataType ))

        ( Repeated, Primitive dataType _ ) ->
            C.apply [ Meta.Encode.list, Meta.Encode.forPrimitive dataType ]

        ( Repeated, Embedded e ) ->
            C.apply [ Meta.Encode.list, embeddedEncoder e ]

        ( Repeated, Enumeration enum ) ->
            C.apply [ Meta.Encode.list, C.fun (Common.encoderName <| Mapper.Name.internalize ( enum.moduleName, enum.dataType )) ]

        ( _, Embedded e ) ->
            C.parens <|
                C.applyBinOp
                    (C.apply [ Meta.Basics.mapMaybe, embeddedEncoder e ])
                    C.composer
                    (C.apply [ Meta.Basics.withDefault, Meta.Encode.none ])

        ( _, Enumeration enum ) ->
            C.fun (Common.encoderName <| Mapper.Name.internalize ( enum.moduleName, enum.dataType ))


fieldTypeToTypeAnnotation : FieldType -> C.TypeAnnotation
fieldTypeToTypeAnnotation fieldType =
    case fieldType of
        Primitive dataType _ ->
            Meta.Type.forPrimitive dataType

        Embedded e ->
            C.typed
                (Mapper.Name.internalize
                    ( e.moduleName
                    , case e.typeKind of
                        Alias ->
                            e.dataType

                        Type ->
                            recursiveDataTypeName e.dataType
                    )
                )
                []

        Enumeration enum ->
            C.typed (Mapper.Name.internalize ( enum.moduleName, enum.dataType )) []


fieldToTypeAnnotation : Field -> C.TypeAnnotation
fieldToTypeAnnotation field =
    let
        cardinalityModifier cardinality fieldType =
            case ( cardinality, fieldType ) of
                ( Optional, Primitive _ _ ) ->
                    identity

                ( Optional, Enumeration _ ) ->
                    identity

                ( Required, _ ) ->
                    identity

                ( Optional, _ ) ->
                    C.maybeAnn

                ( Repeated, _ ) ->
                    C.listAnn

                ( Proto3Optional, _ ) ->
                    C.maybeAnn
    in
    case field of
        NormalField _ cardinality fieldType ->
            cardinalityModifier cardinality
                fieldType
                (fieldTypeToTypeAnnotation fieldType)

        MapField _ key value ->
            Meta.Type.dict (fieldTypeToTypeAnnotation key)
                (cardinalityModifier Optional value <| fieldTypeToTypeAnnotation value)

        OneOfField dataType moduleName ->
            C.maybeAnn <| C.typed (Mapper.Name.internalize ( moduleName, dataType )) []


recursiveDataTypeName : String -> String
recursiveDataTypeName wrappedDataType =
    wrappedDataType ++ "_"


recursiveUnwrapName : String -> String
recursiveUnwrapName wrappedDataType =
    "unwrap" ++ wrappedDataType


recursiveWrapName : String -> String
recursiveWrapName wrappedDataType =
    "wrap" ++ wrappedDataType


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
    C.emptyDocComment |> C.markdown ("Unwrap a `" ++ wrappedDataType ++ "` from its wrapper `" ++ recursiveDataTypeName wrappedDataType ++ ".`")


recursiveWrapDocumentation : String -> C.Comment C.DocComment
recursiveWrapDocumentation wrappedDataType =
    C.emptyDocComment |> C.markdown ("Wrap a `" ++ wrappedDataType ++ "` into its wrapper `" ++ recursiveDataTypeName wrappedDataType ++ ".`")

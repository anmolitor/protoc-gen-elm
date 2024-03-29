module Generator.OneOf exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import Generator.Message exposing (fieldTypeToEncoder, fieldTypeToJsonEncoder, fieldTypeToTypeAnnotation, oneofDocumentation)
import Json.Decode
import Mapper.Name
import Meta.Basics
import Meta.Decode
import Meta.Encode
import Meta.JsonDecode
import Meta.JsonEncode
import Model exposing (Cardinality(..), FieldType(..), OneOf)
import Options exposing (Options)


reexportDataType : ModuleName -> ModuleName -> { oneOfName : String, options : OneOf, docs : List String } -> List C.Declaration
reexportDataType internalsModule moduleName { oneOfName, docs } =
    let
        oneOfDocs =
            if List.isEmpty docs then
                oneofDocumentation oneOfName

            else
                Common.renderDocs docs

        typeAliasDecl =
            C.aliasDecl (Just oneOfDocs) oneOfName [] (C.fqTyped internalsModule (Mapper.Name.internalize ( moduleName ++ [ oneOfName ], oneOfName )) [])
    in
    [ typeAliasDecl ]


reexportAST : { oneOfName : String, options : OneOf, docs : List String } -> List C.Declaration
reexportAST { oneOfName, options, docs } =
    let
        dataType =
            oneOfName

        documentation =
            if List.isEmpty docs then
                oneofDocumentation dataType

            else
                Common.renderDocs docs

        optionsWithTypeParam =
            List.indexedMap (\i option -> ( "a" ++ String.fromInt i, option )) options

        type_ =
            C.customTypeDecl (Just documentation)
                dataType
                (List.map Tuple.first optionsWithTypeParam)
                (List.map (\( t, o ) -> ( o.dataType, [ C.typeVar t ] )) optionsWithTypeParam)
    in
    [ type_ ]


toAST : Options -> { a | oneOfName : String, options : OneOf } -> List C.Declaration
toAST opts { oneOfName, options } =
    let
        dataType =
            oneOfName

        ( moduleName, externalDataType ) =
            Mapper.Name.externalize oneOfName

        type_ =
            C.aliasDecl
                (Just <| oneofDocumentation dataType)
                dataType
                []
                (C.fqTyped moduleName externalDataType <|
                    List.map
                        (\o -> fieldTypeToTypeAnnotation o.fieldType)
                        options
                )

        encoder =
            case dataType of
                "Proto__Google__Protobuf__Value__Kind__Kind" ->
                    C.funDecl (Just <| Common.encoderDocumentation dataType)
                        (Just <| C.funAnn (C.typed dataType []) (C.tupleAnn [ C.intAnn, C.fqTyped Meta.Encode.moduleName "Encoder" [] ]))
                        (Common.encoderName dataType)
                        [ C.varPattern "value" ]
                    <|
                        C.caseExpr
                            (C.val "value")
                            (List.map
                                (\o ->
                                    let
                                        ( optModName, optDataType ) =
                                            Mapper.Name.externalize o.dataType
                                    in
                                    ( C.fqNamedPattern optModName optDataType [ C.varPattern "innerValue" ]
                                    , C.tuple [ C.int o.fieldNumber, C.apply [ fieldTypeToEncoder Required o.fieldType, C.val "innerValue" ] ]
                                    )
                                )
                                options
                            )

                _ ->
                    C.funDecl (Just <| Common.encoderDocumentation dataType)
                        (Just <| C.funAnn (C.maybeAnn <| C.typed dataType []) (C.tupleAnn [ C.intAnn, C.fqTyped Meta.Encode.moduleName "Encoder" [] ]))
                        (Common.encoderName dataType)
                        [ C.varPattern "value" ]
                    <|
                        C.caseExpr
                            (C.val "value")
                            (( C.namedPattern "Nothing" [], C.tuple [ C.int 0, Meta.Encode.none ] )
                                :: List.map
                                    (\o ->
                                        let
                                            ( optModName, optDataType ) =
                                                Mapper.Name.externalize o.dataType
                                        in
                                        ( C.namedPattern "Just" [ C.parensPattern (C.fqNamedPattern optModName optDataType [ C.varPattern "innerValue" ]) ]
                                        , C.tuple [ C.int o.fieldNumber, C.apply [ fieldTypeToEncoder Required o.fieldType, C.val "innerValue" ] ]
                                        )
                                    )
                                    options
                            )

        jsonEncoder =
            case dataType of
                "Proto__Google__Protobuf__Value__Kind__Kind" ->
                    C.funDecl (Just <| Common.jsonEncoderDocumentation dataType)
                        (Just <| C.funAnn (C.typed dataType []) Meta.JsonEncode.value)
                        (Common.jsonEncoderName dataType)
                        [ C.varPattern "value" ]
                    <|
                        C.caseExpr
                            (C.val "value")
                            (List.map
                                (\o ->
                                    let
                                        ( optModName, optDataType ) =
                                            Mapper.Name.externalize o.dataType
                                    in
                                    ( C.fqNamedPattern optModName optDataType [ C.varPattern "innerValue" ]
                                    , C.apply
                                        [ fieldTypeToJsonEncoder Required o.fieldType
                                        , C.val "innerValue"
                                        ]
                                    )
                                )
                                options
                            )

                _ ->
                    C.funDecl (Just <| Common.jsonEncoderDocumentation dataType)
                        (Just <| C.funAnn (C.maybeAnn <| C.typed dataType []) <| C.listAnn (C.tupleAnn [ C.stringAnn, Meta.JsonEncode.value ]))
                        (Common.jsonEncoderName dataType)
                        [ C.varPattern "value" ]
                    <|
                        C.caseExpr
                            (C.val "value")
                            (( C.namedPattern "Nothing" [], C.list [] )
                                :: List.map
                                    (\o ->
                                        let
                                            ( optModName, optDataType ) =
                                                Mapper.Name.externalize o.dataType
                                        in
                                        ( C.namedPattern "Just" [ C.parensPattern (C.fqNamedPattern optModName optDataType [ C.varPattern "innerValue" ]) ]
                                        , C.list
                                            [ C.tuple
                                                [ C.string o.fieldName.jsonName
                                                , C.apply
                                                    [ fieldTypeToJsonEncoder Required o.fieldType
                                                    , C.val "innerValue"
                                                    ]
                                                ]
                                            ]
                                        )
                                    )
                                    options
                            )

        setterAnn x =
            C.funAnn x <| C.funAnn (C.typeVar "a") (C.typeVar "a")

        decoder =
            case dataType of
                "Proto__Google__Protobuf__Value__Kind__Kind" ->
                    C.valDecl (Just <| Common.decoderDocumentation dataType)
                        (Just <|
                            C.funAnn (setterAnn <| C.maybeAnn <| C.typed dataType []) <|
                                C.fqTyped Meta.Decode.moduleName "FieldDecoder" [ C.typeVar "a" ]
                        )
                        (Common.decoderName dataType)
                    <|
                        C.apply
                            [ Meta.Decode.oneOf
                            , C.list
                                (List.map
                                    (\o ->
                                        let
                                            wrapEmbeddedWithLazy =
                                                case o.fieldType of
                                                    Embedded _ ->
                                                        C.applyBinOp Meta.Decode.lazy C.pipel << C.lambda [ C.allPattern ]

                                                    _ ->
                                                        identity

                                            ( optModName, optDataType ) =
                                                Mapper.Name.externalize o.dataType
                                        in
                                        C.tuple
                                            [ C.int o.fieldNumber
                                            , wrapEmbeddedWithLazy <|
                                                C.apply
                                                    [ Meta.Decode.map
                                                    , C.fqVal optModName optDataType
                                                    , case o.fieldType of
                                                        Primitive p _ ->
                                                            Meta.Decode.forPrimitive p

                                                        Embedded e ->
                                                            Generator.Message.embeddedDecoder e

                                                        Enumeration e ->
                                                            C.fqFun (e.package ++ [ e.name ]) <|
                                                                Common.decoderName e.name
                                                    ]
                                            ]
                                    )
                                    options
                                )
                            ]

                _ ->
                    C.valDecl (Just <| Common.decoderDocumentation dataType)
                        (Just <|
                            C.funAnn (setterAnn <| C.maybeAnn <| C.typed dataType []) <|
                                C.fqTyped Meta.Decode.moduleName "FieldDecoder" [ C.typeVar "a" ]
                        )
                        (Common.decoderName dataType)
                    <|
                        C.apply
                            [ Meta.Decode.oneOf
                            , C.list
                                (List.map
                                    (\o ->
                                        let
                                            wrapEmbeddedWithLazy =
                                                case o.fieldType of
                                                    Embedded _ ->
                                                        C.applyBinOp Meta.Decode.lazy C.pipel << C.lambda [ C.allPattern ]

                                                    _ ->
                                                        identity

                                            ( optModName, optDataType ) =
                                                Mapper.Name.externalize o.dataType
                                        in
                                        C.tuple
                                            [ C.int o.fieldNumber
                                            , wrapEmbeddedWithLazy <|
                                                C.apply
                                                    [ Meta.Decode.map
                                                    , C.fqVal optModName optDataType
                                                    , case o.fieldType of
                                                        Primitive p _ ->
                                                            Meta.Decode.forPrimitive p

                                                        Embedded e ->
                                                            Generator.Message.embeddedDecoder e

                                                        Enumeration e ->
                                                            C.fqFun (e.package ++ [ e.name ]) <|
                                                                Common.decoderName e.name
                                                    ]
                                            ]
                                    )
                                    options
                                )
                            ]

        jsonDecoder =
            let
                wrapEmbeddedWithLazy o =
                    case o.fieldType of
                        Embedded _ ->
                            C.applyBinOp Meta.JsonDecode.lazy C.pipel << C.lambda [ C.allPattern ]

                        _ ->
                            identity

                optionFieldDecoder o =
                    case o.fieldType of
                        Primitive p _ ->
                            Meta.JsonDecode.forPrimitive p

                        Embedded e ->
                            Generator.Message.embeddedJsonDecoder e

                        Enumeration e ->
                            C.fqFun (e.package ++ [ e.name ]) <|
                                Common.jsonDecoderName e.name
            in
            case dataType of
                "Proto__Google__Protobuf__Value__Kind__Kind" ->
                    C.valDecl (Just <| Common.jsonDecoderDocumentation dataType)
                        (Just <| Meta.JsonDecode.decoder (C.typed dataType []))
                        (Common.jsonDecoderName dataType)
                    <|
                        Meta.JsonDecode.oneOf
                            (List.map
                                (\o ->
                                    let
                                        ( optModName, optDataType ) =
                                            Mapper.Name.externalize o.dataType
                                    in
                                    wrapEmbeddedWithLazy o <|
                                        C.apply
                                            [ Meta.JsonDecode.map
                                            , C.fqVal optModName optDataType
                                            , optionFieldDecoder o
                                            ]
                                )
                                options
                            )

                _ ->
                    C.valDecl (Just <| Common.jsonDecoderDocumentation dataType)
                        (Just <| Meta.JsonDecode.decoder (C.maybeAnn <| C.typed dataType []))
                        (Common.jsonDecoderName dataType)
                    <|
                        Meta.JsonDecode.oneOf
                            (List.map
                                (\o ->
                                    let
                                        ( optModName, optDataType ) =
                                            Mapper.Name.externalize o.dataType
                                    in
                                    wrapEmbeddedWithLazy o <|
                                        C.apply
                                            [ Meta.JsonDecode.map
                                            , C.parens (C.applyBinOp (C.fqVal optModName optDataType) C.composer Meta.Basics.just)
                                            , C.apply
                                                [ Meta.JsonDecode.field
                                                , C.string o.fieldName.jsonName
                                                , optionFieldDecoder o
                                                ]
                                            ]
                                )
                                options
                                ++ [ C.apply [ C.fqVal Meta.JsonDecode.moduleName "succeed", Meta.Basics.nothing ] ]
                            )

        fieldNumbersTypeDecl : C.Declaration
        fieldNumbersTypeDecl =
            C.aliasDecl (Just <| Common.fieldNumbersDocumentation dataType) (Common.fieldNumbersTypeName dataType) [] <|
                C.recordAnn <|
                    List.map (\o -> ( o.fieldName.protoName, C.intAnn )) options

        fieldNumbersDecl : C.Declaration
        fieldNumbersDecl =
            C.valDecl (Just <| Common.fieldNumbersDocumentation dataType)
                (Just <| C.typed (Common.fieldNumbersTypeName dataType) [])
                (Common.fieldNumbersName dataType)
                (C.record <| List.map (\o -> ( o.fieldName.protoName, C.int o.fieldNumber )) options)

        recursiveFieldDecls =
            List.concatMap (\o -> Generator.Message.fieldTypeDeclarations o.fieldType) options
    in
    [ type_, encoder, decoder, fieldNumbersTypeDecl, fieldNumbersDecl ]
        ++ recursiveFieldDecls
        ++ (if opts.json == Options.All || opts.json == Options.Encode || opts.grpcDevTools then
                [ jsonEncoder ]

            else
                []
           )
        ++ (if opts.json == Options.All || opts.json == Options.Decode then
                [ jsonDecoder ]

            else
                []
           )


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")

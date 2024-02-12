module Generator.OneOf exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import Generator.Message exposing (fieldTypeToEncoder, fieldTypeToJsonEncoder, fieldTypeToTypeAnnotation, oneofDocumentation)
import Mapper.Name
import Meta.Decode
import Meta.Encode
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
                                        [ C.string o.fieldName
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

                                                -- C.fqFun (Common.internalsModule e.rootModuleName) <|
                                                --     Common.decoderName <|
                                                --         Mapper.Name.internalize ( e.moduleName, e.dataType )
                                                Enumeration e ->
                                                    C.fqFun (Common.internalsModule e.rootPackage) <|
                                                        Common.decoderName <|
                                                            Mapper.Name.internalize ( e.package, e.name )
                                            ]
                                    ]
                            )
                            options
                        )
                    ]

        fieldNumbersTypeDecl : C.Declaration
        fieldNumbersTypeDecl =
            C.aliasDecl (Just <| Common.fieldNumbersDocumentation dataType) (Common.fieldNumbersTypeName dataType) [] <|
                C.recordAnn <|
                    List.map (\o -> ( o.fieldName, C.intAnn )) options

        fieldNumbersDecl : C.Declaration
        fieldNumbersDecl =
            C.valDecl (Just <| Common.fieldNumbersDocumentation dataType)
                (Just <| C.typed (Common.fieldNumbersTypeName dataType) [])
                (Common.fieldNumbersName dataType)
                (C.record <| List.map (\o -> ( o.fieldName, C.int o.fieldNumber )) options)

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


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")

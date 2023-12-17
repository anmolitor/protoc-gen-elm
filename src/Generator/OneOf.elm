module Generator.OneOf exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import Generator.Message exposing (fieldTypeToEncoder, fieldTypeToJsonEncoder, fieldTypeToTypeAnnotation, oneofDocumentation)
import Mapper.Name
import Meta.Decode
import Meta.Encode
import Meta.JsonEncode
import Meta.Type
import Model exposing (Cardinality(..), FieldType(..), OneOf)
import Options exposing (Options)


reexportAST : ModuleName -> ModuleName -> { oneOfName : String, options : OneOf, docs : List String } -> List C.Declaration
reexportAST internalsModule moduleName { oneOfName, options, docs } =
    let
        dataType =
            oneOfName

        documentation =
            if List.isEmpty docs then
                oneofDocumentation dataType

            else
                Common.renderDocs docs

        type_ =
            C.customTypeDecl (Just documentation)
                dataType
                []
                (List.map (\o -> ( o.dataType, [ fieldTypeToTypeAnnotationReexport o.fieldType ] )) options)

        internalName =
            Mapper.Name.internalize ( moduleName, dataType )

        fromInternal =
            C.funDecl (Just <| Common.fromInternalDocumentation dataType internalName)
                (Just <|
                    C.funAnn
                        (C.fqTyped internalsModule
                            internalName
                            []
                        )
                        (C.typed dataType [])
                )
                ("fromInternal" ++ dataType)
                [ C.varPattern "data_" ]
                (C.caseExpr (C.val "data_")
                    (List.map
                        (\o ->
                            ( C.fqNamedPattern internalsModule
                                (Mapper.Name.internalize ( moduleName, o.dataType ))
                                [ C.varPattern "n_" ]
                            , C.apply [ C.val o.dataType, C.val "n_" ]
                            )
                        )
                        options
                    )
                )

        toInternalFuncName =
            "toInternal" ++ dataType

        toInternal =
            C.funDecl (Just <| Common.toInternalDocumentation dataType internalName)
                (Just <|
                    C.funAnn
                        (C.typed dataType [])
                        (C.fqTyped internalsModule
                            internalName
                            []
                        )
                )
                toInternalFuncName
                [ C.varPattern "data_" ]
                (C.caseExpr (C.val "data_")
                    (List.map
                        (\o ->
                            ( C.namedPattern o.dataType [ C.varPattern "n_" ]
                            , C.apply
                                [ C.fqVal internalsModule
                                    (Mapper.Name.internalize ( moduleName, o.dataType ))
                                , C.val "n_"
                                ]
                            )
                        )
                        options
                    )
                )

        constructors =
            List.map
                (\option ->
                    C.funDecl (C.emptyDocComment |> C.markdown ("Construct a " ++ option.dataType ++ """ and immediately turn it into an Internals_ data type.
This is just (""" ++ option.dataType ++ " >> " ++ toInternalFuncName) |> Just)
                        (Just <|
                            C.funAnn
                                (fieldTypeToTypeAnnotationReexport option.fieldType)
                                (C.fqTyped internalsModule internalName [])
                        )
                        option.fieldName
                        []
                        (C.applyBinOp (C.val option.dataType) C.composer (C.fun toInternalFuncName))
                )
                options

        fieldTypeToTypeAnnotationReexport : FieldType -> C.TypeAnnotation
        fieldTypeToTypeAnnotationReexport fieldType =
            case fieldType of
                Primitive dType _ ->
                    Meta.Type.forPrimitive dType

                Embedded e ->
                    C.fqTyped (Common.internalsModule e.rootModuleName)
                        (Mapper.Name.internalize
                            ( e.moduleName
                            , e.dataType
                            )
                        )
                        []

                Enumeration enum ->
                    C.fqTyped (Common.internalsModule enum.rootPackage) (Mapper.Name.internalize ( enum.package, enum.name )) []
    in
    [ type_, fromInternal, toInternal ] ++ constructors


toAST : Options -> { a | oneOfName : String, options : OneOf } -> List C.Declaration
toAST opts { oneOfName, options } =
    let
        dataType =
            oneOfName

        type_ =
            C.customTypeDecl
                (Just <| oneofDocumentation dataType)
                dataType
                []
                (List.map (\o -> ( o.dataType, [ fieldTypeToTypeAnnotation <| Model.setTypeKind Model.Alias o.fieldType ] )) options)

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
                                ( C.namedPattern "Just" [ C.parensPattern (C.namedPattern o.dataType [ C.varPattern "innerValue" ]) ]
                                , C.tuple [ C.int o.fieldNumber, C.apply [ fieldTypeToEncoder Required <| Model.setTypeKind Model.Alias o.fieldType, C.val "innerValue" ] ]
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
                                ( C.namedPattern "Just" [ C.parensPattern (C.namedPattern o.dataType [ C.varPattern "innerValue" ]) ]
                                , C.list
                                    [ C.tuple
                                        [ C.string o.fieldName
                                        , C.apply
                                            [ fieldTypeToJsonEncoder Required <| Model.setTypeKind Model.Alias o.fieldType
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
                                in
                                C.tuple
                                    [ C.int o.fieldNumber
                                    , wrapEmbeddedWithLazy <|
                                        C.apply
                                            [ Meta.Decode.map
                                            , C.val o.dataType
                                            , case o.fieldType of
                                                Primitive p _ ->
                                                    Meta.Decode.forPrimitive p

                                                Embedded e ->
                                                    C.fqFun (Common.internalsModule e.rootModuleName) <|
                                                        Common.decoderName <|
                                                            Mapper.Name.internalize ( e.moduleName, e.dataType )

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
    in
    [ type_, encoder, decoder, fieldNumbersTypeDecl, fieldNumbersDecl ]
        ++ (if opts.json == Options.All || opts.json == Options.Encode || opts.grpcDevTools then
                [ jsonEncoder ]

            else
                []
           )


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")

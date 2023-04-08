module Generator.OneOf exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import Generator.Message exposing (fieldTypeToEncoder, fieldTypeToTypeAnnotation, oneofDocumentation)
import Mapper.Name
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), FieldType(..), OneOf)


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
                ("toInternal" ++ dataType)
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
    [ type_, fromInternal, toInternal ]


toAST : { a | oneOfName : String, options : OneOf } -> List C.Declaration
toAST { oneOfName, options } =
    let
        dataType =
            oneOfName

        type_ =
            C.customTypeDecl
                (Just <| oneofDocumentation dataType)
                dataType
                []
                (List.map (\o -> ( o.dataType, [ fieldTypeToTypeAnnotation o.fieldType ] )) options)

        encoder =
            C.funDecl Nothing
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
                                , C.tuple [ C.int o.fieldNumber, C.apply [ fieldTypeToEncoder Required o.fieldType, C.val "innerValue" ] ]
                                )
                            )
                            options
                    )

        setterAnn x =
            C.funAnn x <| C.funAnn (C.typeVar "a") (C.typeVar "a")

        decoder =
            C.valDecl Nothing
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
                                C.tuple
                                    [ C.int o.fieldNumber
                                    , C.apply
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
            C.aliasDecl Nothing (Common.fieldNumbersTypeName dataType) [] <|
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


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")

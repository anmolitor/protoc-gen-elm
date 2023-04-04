module Generator.OneOf exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import Generator.Message exposing (fieldTypeToEncoder, fieldTypeToTypeAnnotation)
import Mapper.Name
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), FieldType(..), OneOf)


reexportAST : ModuleName -> ModuleName -> ( String, OneOf ) -> List C.Declaration
reexportAST internalsModule moduleName ( dataType, opts ) =
    let
        type_ =
            C.customTypeDecl (Just <| oneofDocumentation dataType)
                dataType
                []
                (List.map (\( _, optionName, optionType ) -> ( optionName, [ fieldTypeToTypeAnnotationReexport optionType ] )) opts)

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
                        (\( _, optionName, _ ) ->
                            ( C.fqNamedPattern internalsModule
                                (Mapper.Name.internalize ( moduleName, optionName ))
                                [ C.varPattern "n_" ]
                            , C.apply [ C.val optionName, C.val "n_" ]
                            )
                        )
                        opts
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
                        (\( _, optionName, _ ) ->
                            ( C.namedPattern optionName [ C.varPattern "n_" ]
                            , C.apply
                                [ C.fqVal internalsModule
                                    (Mapper.Name.internalize ( moduleName, optionName ))
                                , C.val "n_"
                                ]
                            )
                        )
                        opts
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


toAST : ( String, OneOf ) -> List C.Declaration
toAST ( dataType, opts ) =
    let
        type_ =
            C.customTypeDecl
                (Just <| oneofDocumentation dataType)
                dataType
                []
                (List.map (\( _, optionName, innerFieldType ) -> ( optionName, [ fieldTypeToTypeAnnotation innerFieldType ] )) opts)

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
                            (\( fieldNumber, optionName, fieldType ) ->
                                ( C.namedPattern "Just" [ C.parensPattern (C.namedPattern optionName [ C.varPattern "innerValue" ]) ]
                                , C.tuple [ C.int fieldNumber, C.apply [ fieldTypeToEncoder Required fieldType, C.val "innerValue" ] ]
                                )
                            )
                            opts
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
                            (\( fieldNumber, optionName, fieldType ) ->
                                C.tuple
                                    [ C.int fieldNumber
                                    , C.apply
                                        [ Meta.Decode.map
                                        , C.val optionName
                                        , case fieldType of
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
                            opts
                        )
                    ]
    in
    [ type_, encoder, decoder ]


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")

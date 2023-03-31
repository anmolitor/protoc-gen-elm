module Generator.OneOf exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import Generator.Message exposing (fieldTypeToEncoder, fieldTypeToTypeAnnotation)
import Mapper.Name
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), FieldType(..), OneOf)


reexportAST : ModuleName -> ( String, OneOf ) -> List C.Declaration
reexportAST moduleName ( dataType, opts ) =
    let
        type_ =
            C.customTypeDecl (Just <| oneofDocumentation dataType)
                dataType
                []
                (List.map (\( _, optionName, optionType ) -> ( optionName, [ fieldTypeToTypeAnnotationReexport optionType ] )) opts)

        fromInternal =
            C.funDecl (Just <| C.emptyDocComment)
                (Just <|
                    C.funAnn
                        (C.fqTyped Common.internalsModule
                            (Mapper.Name.internalize ( moduleName, dataType ))
                            []
                        )
                        (C.typed dataType [])
                )
                ("fromInternal" ++ dataType)
                [ C.varPattern "data_" ]
                (C.caseExpr (C.val "data_")
                    (List.map
                        (\( _, optionName, _ ) ->
                            ( C.fqNamedPattern Common.internalsModule
                                (Mapper.Name.internalize ( moduleName, optionName ))
                                [ C.varPattern "n_" ]
                            , C.apply [ C.val optionName, C.val "n_" ]
                            )
                        )
                        opts
                    )
                )

        toInternal =
            C.funDecl (Just <| C.emptyDocComment)
                (Just <|
                    C.funAnn
                        (C.typed dataType [])
                        (C.fqTyped Common.internalsModule
                            (Mapper.Name.internalize ( moduleName, dataType ))
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
                                [ C.fqVal Common.internalsModule
                                    (Mapper.Name.internalize ( moduleName, optionName ))
                                , C.val "n_"
                                ]
                            )
                        )
                        opts
                    )
                )
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
                                                C.fun <| Common.decoderName <| Mapper.Name.internalize ( e.moduleName, e.dataType )

                                            Enumeration e ->
                                                C.fun <| Common.decoderName <| Mapper.Name.internalize ( e.moduleName, e.dataType )
                                        ]
                                    ]
                            )
                            opts
                        )
                    ]
    in
    [ type_, encoder, decoder ]


fieldTypeToTypeAnnotationReexport : FieldType -> C.TypeAnnotation
fieldTypeToTypeAnnotationReexport fieldType =
    case fieldType of
        Primitive dataType _ ->
            Meta.Type.forPrimitive dataType

        Embedded e ->
            C.fqTyped Common.internalsModule
                (Mapper.Name.internalize
                    ( e.moduleName
                    , e.dataType
                    )
                )
                []

        Enumeration enum ->
            C.fqTyped Common.internalsModule (Mapper.Name.internalize ( enum.moduleName, enum.dataType )) []


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")

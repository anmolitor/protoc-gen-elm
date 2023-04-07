module Generator.Enum exposing (reexportAST, toAST)

import Elm.CodeGen as C exposing (ModuleName)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Pattern exposing (Pattern)
import Generator.Common as Common
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Mapper.Name
import Meta.Decode
import Meta.Encode
import Model exposing (Enum)


reexportAST : ModuleName -> ModuleName -> Enum -> List C.Declaration
reexportAST internalsModule moduleName enum =
    let
        fields =
            NonEmpty.toList enum.fields
                |> List.map Tuple.second

        withUnrecognized f =
            if enum.withUnrecognized then
                f (enum.dataType ++ "Unrecognized_")

            else
                identity

        type_ =
            C.customTypeDecl (Just <| enumDocumentation enum.dataType)
                enum.dataType
                []
                (List.map (\optionName -> ( optionName, [] )) fields
                    |> withUnrecognized (\unrecognized constructors -> constructors ++ [ ( unrecognized, [ C.intAnn ] ) ])
                )

        internalName =
            Mapper.Name.internalize ( moduleName, enum.dataType )

        fromInternal =
            C.funDecl (Just <| Common.fromInternalDocumentation enum.dataType internalName)
                (Just <|
                    C.funAnn
                        (C.fqTyped internalsModule
                            internalName
                            []
                        )
                        (C.typed enum.dataType [])
                )
                ("fromInternal" ++ enum.dataType)
                [ C.varPattern "data_" ]
                (C.caseExpr (C.val "data_")
                    (List.map
                        (\optionName ->
                            ( C.fqNamedPattern internalsModule
                                (Mapper.Name.internalize ( moduleName, optionName ))
                                []
                            , C.val optionName
                            )
                        )
                        fields
                        |> withUnrecognized
                            (\unrecognized cases ->
                                cases
                                    ++ [ ( C.fqNamedPattern internalsModule
                                            (Mapper.Name.internalize ( moduleName, unrecognized ))
                                            [ C.varPattern "n_" ]
                                         , C.apply [ C.val unrecognized, C.val "n_" ]
                                         )
                                       ]
                            )
                    )
                )

        toInternal =
            C.funDecl (Just <| Common.toInternalDocumentation enum.dataType internalName)
                (Just <|
                    C.funAnn
                        (C.typed enum.dataType [])
                        (C.fqTyped internalsModule
                            internalName
                            []
                        )
                )
                ("toInternal" ++ enum.dataType)
                [ C.varPattern "data_" ]
                (C.caseExpr (C.val "data_")
                    (List.map
                        (\optionName ->
                            ( C.namedPattern optionName []
                            , C.fqVal internalsModule
                                (Mapper.Name.internalize ( moduleName, optionName ))
                            )
                        )
                        fields
                        |> withUnrecognized
                            (\unrecognized cases ->
                                cases
                                    ++ [ ( C.namedPattern unrecognized [ C.varPattern "n_" ]
                                         , C.apply
                                            [ C.fqVal internalsModule
                                                (Mapper.Name.internalize ( moduleName, unrecognized ))
                                            , C.val "n_"
                                            ]
                                         )
                                       ]
                            )
                    )
                )

        encoder =
            C.valDecl (Just <| Common.encoderDocumentation enum.dataType)
                (Just <| Meta.Encode.encoder (C.typed enum.dataType []))
                (Common.encoderName enum.dataType)
                (C.applyBinOp (C.val <| "toInternal" ++ enum.dataType)
                    C.composer
                    (C.fqVal internalsModule <| Common.encoderName <| Mapper.Name.internalize ( moduleName, enum.dataType ))
                )

        decoder =
            C.valDecl (Just <| Common.decoderDocumentation enum.dataType)
                (Just <| Meta.Decode.decoder (C.typed enum.dataType []))
                (Common.decoderName enum.dataType)
                (C.apply
                    [ Meta.Decode.map
                    , C.val <| "fromInternal" ++ enum.dataType
                    , C.fqVal internalsModule <| Common.decoderName <| Mapper.Name.internalize ( moduleName, enum.dataType )
                    ]
                )

        defaultEnum =
            NonEmpty.find (\( index, _ ) -> index == 0) enum.fields
                |> Maybe.withDefault (NonEmpty.head enum.fields)
                |> Tuple.second

        default : C.Declaration
        default =
            C.valDecl (Just <| Common.defaultDocumentation enum.dataType)
                (Just <| C.typed enum.dataType [])
                (Common.defaultName enum.dataType)
                (C.val defaultEnum)

        fieldNumbersDecl : C.Declaration
        fieldNumbersDecl =
            C.funDecl (Just <| Common.fieldNumbersDocumentation enum.dataType)
                (Just <| C.funAnn (C.typed enum.dataType []) C.intAnn)
                (Common.fieldNumbersName enum.dataType)
                [ C.varPattern "n_" ]
                (C.caseExpr (C.val "n_") <|
                    withUnrecognized (\unrecognized cases -> cases ++ [ ( C.namedPattern unrecognized [ C.varPattern "m_" ], C.val "m_" ) ]) <|
                        List.map
                            (\( n, optName ) -> ( C.namedPattern optName [], C.int n ))
                        <|
                            NonEmpty.toList enum.fields
                )
    in
    [ type_, decoder, encoder, fromInternal, toInternal, default, fieldNumbersDecl ]


toAST : Enum -> List C.Declaration
toAST enum =
    let
        enumName =
            enum.dataType

        unrecognizedOption =
            enumName ++ "Unrecognized_"

        decodeCases : String -> NonEmpty ( Pattern, Expression )
        decodeCases varName =
            NonEmpty.append
                (NonEmpty.map
                    (\( number, name ) ->
                        ( C.intPattern number
                        , C.val name
                        )
                    )
                    enum.fields
                )
                (NonEmpty.singleton
                    ( C.allPattern
                    , if enum.withUnrecognized then
                        C.apply [ C.val unrecognizedOption, C.val varName ]

                      else
                        NonEmpty.head enum.fields |> Tuple.second |> C.val
                    )
                )

        encodeCases : NonEmpty ( Pattern, Expression )
        encodeCases =
            let
                definedCases =
                    NonEmpty.map (\( number, name ) -> ( C.varPattern name, C.int number )) enum.fields
            in
            if enum.withUnrecognized then
                NonEmpty.append definedCases <|
                    NonEmpty.singleton
                        ( C.namedPattern unrecognizedOption [ C.varPattern "i" ], C.val "i" )

            else
                definedCases

        constructors : NonEmpty ( String, List C.TypeAnnotation )
        constructors =
            let
                knownConstructors =
                    NonEmpty.map (\( _, name ) -> ( name, [] )) enum.fields
            in
            if enum.withUnrecognized then
                NonEmpty.append knownConstructors (NonEmpty.singleton ( unrecognizedOption, [ C.intAnn ] ))

            else
                knownConstructors

        type_ : C.Declaration
        type_ =
            C.customTypeDecl (Just <| enumDocumentation enumName)
                enumName
                []
                (NonEmpty.toList constructors)

        decoder : C.Declaration
        decoder =
            C.funDecl (Just <| Common.decoderDocumentation enumName)
                (Just <| Meta.Decode.decoder (C.typed enumName []))
                (Common.decoderName enumName)
                []
                (C.applyBinOp Meta.Decode.int32
                    C.piper
                    (C.apply
                        [ Meta.Decode.map
                        , C.parens <|
                            C.lambda
                                [ C.varPattern "i" ]
                                (C.caseExpr (C.val "i") (NonEmpty.toList <| decodeCases "i"))
                        ]
                    )
                )

        encoder : C.Declaration
        encoder =
            C.funDecl (Just <| Common.encoderDocumentation enumName)
                (Just <| Meta.Encode.encoder (C.typed enumName []))
                (Common.encoderName enumName)
                [ C.varPattern "value" ]
                (C.applyBinOp
                    Meta.Encode.int32
                    C.pipel
                    (C.caseExpr (C.val "value") (NonEmpty.toList encodeCases))
                )

        defaultEnum =
            NonEmpty.find (\( index, _ ) -> index == 0) enum.fields
                |> Maybe.withDefault (NonEmpty.head enum.fields)
                |> Tuple.second

        default : C.Declaration
        default =
            C.valDecl (Just <| Common.defaultDocumentation enumName)
                (Just <| C.typed enumName [])
                (Common.defaultName enumName)
                (C.val defaultEnum)
    in
    [ type_, decoder, encoder, default ]


enumDocumentation : String -> C.Comment C.DocComment
enumDocumentation typeName =
    C.emptyDocComment |> C.markdown ("`" ++ typeName ++ "` enumeration")

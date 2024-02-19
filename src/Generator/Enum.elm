module Generator.Enum exposing (toAST)

import Elm.CodeGen as C
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Pattern exposing (Pattern)
import Generator.Common as Common
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Mapper.Name
import Meta.Decode
import Meta.Encode
import Meta.JsonDecode
import Meta.JsonEncode
import Model exposing (Enum)
import Options exposing (Options)


toAST : Options -> Enum -> List C.Declaration
toAST options enum =
    let
        enumName =
            enum.dataType

        unrecognizedOption =
            enumName ++ "Unrecognized_"

        withUnrecognized f =
            if enum.withUnrecognized then
                f (enum.dataType ++ "Unrecognized_")

            else
                identity

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

        decodeJsonCases : String -> NonEmpty ( Pattern, Expression )
        decodeJsonCases _ =
            NonEmpty.append
                (NonEmpty.map
                    (\( _, name ) ->
                        ( C.stringPattern name
                        , C.val name
                        )
                    )
                    enum.fields
                )
                (NonEmpty.singleton
                    ( C.allPattern
                    , NonEmpty.head enum.fields |> Tuple.second |> C.val
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

        encodeJsonCases : NonEmpty ( Pattern, Expression )
        encodeJsonCases =
            let
                definedCases =
                    NonEmpty.map
                        (\( _, name ) ->
                            ( C.varPattern name
                            , Mapper.Name.externalize name
                                |> Tuple.second
                                |> C.string
                            )
                        )
                        enum.fields
            in
            if enum.withUnrecognized then
                NonEmpty.append definedCases <|
                    NonEmpty.singleton
                        ( C.namedPattern unrecognizedOption [ C.varPattern "i" ]
                        , C.applyBinOp (C.string "_UNRECOGNIZED_")
                            C.append
                            (C.apply [ C.fqFun [ "String" ] "fromInt", C.val "i" ])
                        )

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

        jsonEncoder : C.Declaration
        jsonEncoder =
            C.funDecl (Just <| Common.jsonEncoderDocumentation enumName)
                (Just <| Meta.JsonEncode.encoder (C.typed enumName []))
                (Common.jsonEncoderName enumName)
                [ C.varPattern "value" ]
                (C.applyBinOp
                    Meta.JsonEncode.string
                    C.pipel
                    (C.caseExpr (C.val "value") (NonEmpty.toList encodeJsonCases))
                )

        jsonDecoder : C.Declaration
        jsonDecoder =
            C.funDecl (Just <| Common.jsonDecoderDocumentation enumName)
                (Just <| Meta.JsonDecode.decoder (C.typed enumName []))
                (Common.jsonDecoderName enumName)
                []
                (C.applyBinOp Meta.JsonDecode.string
                    C.piper
                    (C.apply
                        [ Meta.JsonDecode.map
                        , C.parens <|
                            C.lambda
                                [ C.varPattern "i" ]
                                (C.caseExpr (C.val "i") (NonEmpty.toList <| decodeJsonCases "i"))
                        ]
                    )
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
    [ type_, decoder, encoder, default, fieldNumbersDecl ]
        ++ (if options.json == Options.All || options.json == Options.Encode || options.grpcDevTools then
                [ jsonEncoder ]

            else
                []
           )
        ++ (if options.json == Options.All || options.json == Options.Decode then
                [ jsonDecoder ]

            else
                []
           )


enumDocumentation : String -> C.Comment C.DocComment
enumDocumentation typeName =
    C.emptyDocComment |> C.markdown ("`" ++ typeName ++ "` enumeration")

module Generator.Enum exposing (toAST)

import Elm.CodeGen as C
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Pattern exposing (Pattern)
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Generator.Common as Common
import Meta.Decode
import Meta.Encode
import Model exposing (Enum)


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
    in
    [ type_, decoder, encoder ]


enumDocumentation : String -> C.Comment C.DocComment
enumDocumentation typeName =
    C.emptyDocComment |> C.markdown ("`" ++ typeName ++ "` enumeration")

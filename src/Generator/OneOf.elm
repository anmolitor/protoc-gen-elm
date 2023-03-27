module Generator.OneOf exposing (..)

import Elm.CodeGen as C
import Generator.Common as Common
import Generator.Message exposing (fieldTypeToEncoder)
import Meta.Basics
import Meta.Decode
import Meta.Encode
import Meta.Type
import Model exposing (Cardinality(..), FieldType(..), OneOf)


toAST : ( String, OneOf ) -> List C.Declaration
toAST ( dataType, opts ) =
    let
        typeForFieldType : FieldType -> C.TypeAnnotation
        typeForFieldType ft =
            case ft of
                Primitive prim _ ->
                    Meta.Type.forPrimitive prim

                Embedded e ->
                    C.fqTyped e.moduleName e.dataType []

                Enumeration enum ->
                    C.fqTyped enum.moduleName enum.dataType []

        type_ =
            C.customTypeDecl
                (Just <| oneofDocumentation dataType)
                dataType
                []
                (List.map (\( _, innerDataType, innerFieldType ) -> ( innerDataType, [ typeForFieldType innerFieldType ] )) opts)

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
                                                C.fqFun e.moduleName (Common.decoderName e.dataType)

                                            Enumeration enum ->
                                                C.fqFun enum.moduleName (Common.decoderName enum.dataType)
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

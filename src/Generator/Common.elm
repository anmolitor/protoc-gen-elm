module Generator.Common exposing (..)

import Elm.CodeGen as C
import Mapper.Name exposing (Ref)
import Model exposing (FieldName)


decoderName : String -> String
decoderName typeName =
    "decode" ++ typeName


encoderName : String -> String
encoderName typeName =
    "encode" ++ typeName


defaultName : String -> String
defaultName typeName =
    "default" ++ typeName


decoderDocumentation : String -> C.Comment C.DocComment
decoderDocumentation typeName =
    C.emptyDocComment |> C.markdown ("Declares how to decode a `" ++ typeName ++ "` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from " ++ elmProtocolBuffersName ++ ".")


encoderDocumentation : String -> C.Comment C.DocComment
encoderDocumentation typeName =
    C.emptyDocComment |> C.markdown ("Declares how to encode a `" ++ typeName ++ "` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from " ++ elmProtocolBuffersName ++ ".")


elmProtocolBuffersName : String
elmProtocolBuffersName =
    "eriktim/elm-protocol-buffers"


defaultDocumentation : String -> C.Comment C.DocComment
defaultDocumentation typeName =
    C.emptyDocComment |> C.markdown ("Default for " ++ typeName ++ ". Should only be used for 'required' decoders as an initial value.")


fromInternalDocumentation : String -> String -> C.Comment C.DocComment
fromInternalDocumentation typeName internalName =
    C.emptyDocComment
        |> C.markdown ("Convert the internal type `" ++ internalName ++ "` into a `" ++ typeName ++ "`.")
        |> C.markdown "Using two different types is necessary to avoid recursive module references while having readable constructor names."


toInternalDocumentation : String -> String -> C.Comment C.DocComment
toInternalDocumentation typeName internalName =
    C.emptyDocComment
        |> C.markdown ("Convert a `" ++ typeName ++ "` into its internal representation `" ++ internalName ++ "`.")
        |> C.markdown "Using two different types is necessary to avoid recursive module references while having readable constructor names."


renderDocs : List String -> C.Comment C.DocComment
renderDocs docs =
    addDocs docs C.emptyDocComment


addDocs : List String -> C.Comment c -> C.Comment c
addDocs docs comment =
    List.foldl C.markdown comment docs


fieldNumbersName : String -> String
fieldNumbersName typeName =
    "fieldNumbers" ++ typeName


fieldNumbersTypeName : String -> String
fieldNumbersTypeName typeName =
    "FieldNumbers" ++ typeName


fieldNumbersDocumentation : String -> C.Comment C.DocComment
fieldNumbersDocumentation typeName =
    C.emptyDocComment
        |> C.markdown ("The field numbers for the fields of `" ++ typeName ++ "`. This is mostly useful for internals, like documentation generation.")


setter : FieldName -> C.Expression
setter fieldName =
    C.parens <|
        C.lambda
            [ C.varPattern "a", C.varPattern "r" ]
            (C.update "r" [ ( fieldName, C.val "a" ) ])


internalsModule : C.ModuleName -> C.ModuleName
internalsModule mod =
    if List.isEmpty mod then
        []

    else
        mod ++ [ "Internals_" ]

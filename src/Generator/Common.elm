module Generator.Common exposing (..)

import Elm.CodeGen as C
import Mapper.Name
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
    C.emptyDocComment |> C.markdown ("Decode a `" ++ typeName ++ "` from Bytes")


encoderDocumentation : String -> C.Comment C.DocComment
encoderDocumentation typeName =
    C.emptyDocComment |> C.markdown ("Encode a `" ++ typeName ++ "` to Bytes")


defaultDocumentation : String -> C.Comment C.DocComment
defaultDocumentation typeName =
    C.emptyDocComment |> C.markdown ("Default for " ++ typeName ++ ". Should only be used for 'required' decoders as an initial value.")


setter : FieldName -> C.Expression
setter fieldName =
    C.parens <|
        C.lambda
            [ C.varPattern "a", C.varPattern "r" ]
            (C.update "r" [ ( fieldName, C.val "a" ) ])


internalsModule : C.ModuleName
internalsModule =
    [ "Proto", "Internals_" ]

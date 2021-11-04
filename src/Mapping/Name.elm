module Mapping.Name exposing (field, module_, type_)

import Elm.Syntax.ModuleName exposing (ModuleName)
import List.Extra
import String.Extra


module_ : String -> ModuleName
module_ descriptorName =
    String.split "/" descriptorName
        |> List.Extra.unconsLast
        |> Maybe.map (\( name, segments ) -> segments ++ [ removeExtension name ])
        |> Maybe.withDefault []
        |> List.map String.Extra.classify
        |> (::) "Proto"


removeExtension : String -> String
removeExtension =
    String.split "."
        >> List.Extra.init
        >> Maybe.withDefault []
        >> String.join "."


type_ : String -> String
type_ =
    String.split "."
        >> List.map String.Extra.classify
        >> String.join "_"
        >> escapeType


field : String -> String
field =
    escape << String.Extra.camelize


escape : String -> String
escape word =
    if List.member word reservedWords then
        word ++ "_"

    else
        word


reservedWords : List String
reservedWords =
    [ "if"
    , "then"
    , "else"
    , "case"
    , "of"
    , "let"
    , "in"
    , "type"
    , "module"
    , "where"
    , "import"
    , "exposing"
    , "as"
    , "port"
    ]


escapeType : String -> String
escapeType t =
    if List.member t reservedTypes then
        t ++ "_"

    else
        t


reservedTypes : List String
reservedTypes =
    {- https://package.elm-lang.org/packages/elm/core/1.0.2/ -}
    [ "List"
    , "Maybe"
    , "Result"
    , "String"
    , "Char"
    , "Program"
    , "Cmd"
    , "Sub"
    , "Int"
    , "Float"
    , "Order"
    , "Bool"
    , "Never"
    ]

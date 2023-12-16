module Mapper.Name exposing (ModuleRef, Ref, absoluteRef, field, internalize, moduleRef_, module_, type_, externalize)

import Elm.Syntax.ModuleName exposing (ModuleName)
import List.Extra
import String.Extra


internalize : ( ModuleName, String ) -> String
internalize ( moduleName, name ) =
    String.join "__" <| moduleName ++ [ name ]


externalize : String -> ( ModuleName, String )
externalize =
    String.split "__"
        >> List.Extra.unconsLast
        >> Maybe.map
            (\( last, rest ) -> ( rest, last ))
        >> Maybe.withDefault ( [], "" )


module_ : String -> ModuleName
module_ =
    String.split "."
        >> packageFromSegments


moduleRef_ : String -> ModuleRef
moduleRef_ =
    String.split "."
        >> (\segments -> { rootPackage = determineRootPackage segments, package = packageFromSegments segments })


packageFromSegments : List String -> ModuleName
packageFromSegments =
    List.map String.Extra.classify
        >> List.filter (not << String.isEmpty)
        >> (::) "Proto"


type alias Ref =
    { rootPackage : ModuleName, package : ModuleName, name : String }


type alias ModuleRef =
    { rootPackage : ModuleName, package : ModuleName }


absoluteRef : String -> Ref
absoluteRef =
    String.split "."
        >> List.Extra.unconsLast
        -- never happens since split returns a non-empty list
        >> Maybe.withDefault ( "", [] )
        >> (\( name, segments ) -> { rootPackage = determineRootPackage segments, package = packageFromSegments segments, name = type_ name })


determineRootPackage : List String -> ModuleName
determineRootPackage =
    let
        helper segments =
            case segments of
                [] ->
                    []

                segment :: rest ->
                    case String.uncons segment of
                        Just ( char, _ ) ->
                            if Char.isLower char then
                                segment :: helper rest

                            else
                                []

                        Nothing ->
                            helper rest
    in
    helper >> packageFromSegments


type_ : String -> String
type_ =
    String.split "."
        >> List.map String.Extra.classify
        >> String.join "_"
        >> escapeType


field : String -> String
field =
    escape << String.Extra.decapitalize << String.Extra.camelize


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

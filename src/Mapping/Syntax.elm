module Mapping.Syntax exposing (..)


type Syntax
    = Proto2
    | Proto3


parseSyntax : String -> Syntax
parseSyntax str =
    if str == "proto3" then
        Proto3

    else
        Proto2

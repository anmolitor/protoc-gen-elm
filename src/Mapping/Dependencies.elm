module Mapping.Dependencies exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Model exposing (DataType)


type alias Dependencies =
    Dict ModuleName (List DataType)


empty : Dependencies
empty =
    Dict.empty


addModule : ModuleName -> List DataType -> Dependencies -> Dependencies
addModule =
    Dict.insert


{-| Resolve a type name given a given dictionary of dependencies.
-}
resolveType : DataType -> Dependencies -> Maybe ModuleName
resolveType dataType =
    Dict.filter (\_ -> List.member dataType)
        >> Dict.toList
        >> List.head
        >> Maybe.map Tuple.first

module Mapping.Dependencies exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import List.Extra
import Model exposing (DataType)


type alias Dependencies =
    Dict ( ModuleName, PackageName ) (List DataType)


type alias PackageName =
    String


empty : Dependencies
empty =
    Dict.empty


addModule : ( ModuleName, PackageName ) -> List DataType -> Dependencies -> Dependencies
addModule =
    Dict.insert


{-| Resolve a type name given a given dictionary of dependencies.
The data type is either of type [package].[datatype] or just [datatype].
In the former case, we can filter the dependencies to those of the given package.
In the latter case, we can filter the dependencies to those without any given package.

The method returns the module name and the type that should be written out.

-}
resolveType : DataType -> Dependencies -> Maybe ( ModuleName, String )
resolveType dataType =
    case List.Extra.unconsLast <| String.split "." dataType of
        Just ( actualDataType, package ) ->
            Dict.filter (\( _, packageName ) dataTypes -> packageName == String.join "." package && List.member actualDataType dataTypes)
                >> Dict.toList
                >> List.head
                >> Maybe.map (\((moduleName,_),_) -> (moduleName, actualDataType))

        Nothing ->
            always Nothing


restrictToModuleNames : List ModuleName -> Dependencies -> Dependencies
restrictToModuleNames moduleNames =
    Dict.filter (\( moduleName, _ ) _ -> List.member moduleName moduleNames)

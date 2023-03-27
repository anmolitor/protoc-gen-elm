module Mapper.Package exposing (Packages, addPackage, append, concat, empty)

import Dict exposing (Dict)
import Mapper.Struct as Struct exposing (Struct)


type alias Packages =
    Dict (List String) Struct


empty : Packages
empty =
    Dict.empty


addPackage : List String -> Struct -> Packages -> Packages
addPackage name defs =
    Dict.update name
        (\val ->
            case val of
                Just existingDefs ->
                    Just <| Struct.append existingDefs defs

                Nothing ->
                    Just defs
        )


append : Packages -> Packages -> Packages
append p1 p2 =
    Dict.merge Dict.insert (\k v1 v2 -> Dict.insert k (Struct.append v1 v2)) Dict.insert p1 p2 Dict.empty


concat : List Packages -> Packages
concat =
    List.foldl append empty

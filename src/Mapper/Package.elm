module Mapper.Package exposing (Packages, addPackage, append, concat, empty, unify)

import Dict exposing (Dict)
import List.NonEmpty as NonEmpty
import Mapper.Name as Name
import Mapper.Struct as Struct exposing (Struct)


type alias Packages =
    Dict (List String) Struct


unify : Packages -> Struct
unify packages =
    Dict.toList packages
        |> Struct.concatMap
            (\( moduleName, struct ) ->
                { messages = List.map (\message -> { message | dataType = Name.internalize ( moduleName, message.dataType ) }) struct.messages
                , enums =
                    List.map
                        (\enum ->
                            { enum
                                | dataType = Name.internalize ( moduleName, enum.dataType )
                                , fields = NonEmpty.map (Tuple.mapSecond <| \optionName -> Name.internalize ( moduleName, optionName )) enum.fields
                            }
                        )
                        struct.enums
                , services = struct.services
                , oneOfs =
                    List.map
                        (\( name, oneOf ) ->
                            ( Name.internalize ( moduleName, name )
                            , List.map (\( index, optName, optType ) -> ( index, Name.internalize ( moduleName, optName ), optType )) oneOf
                            )
                        )
                        struct.oneOfs
                }
            )


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

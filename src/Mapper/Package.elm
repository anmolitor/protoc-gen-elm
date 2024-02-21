module Mapper.Package exposing (Packages, addPackage, append, concat, empty, unify)

import Dict exposing (Dict)
import Elm.CodeGen exposing (ModuleName)
import List.NonEmpty as NonEmpty
import Mapper.Name as Name
import Mapper.Struct as Struct exposing (Struct)
import Model exposing (unqualifySelectedMessage)


type alias Packages =
    Dict (List String) Struct


unify : ModuleName -> Packages -> Struct
unify rootModName packages =
    Dict.toList packages
        |> Struct.concatMap
            (\( moduleName, struct ) ->
                { messages =
                    List.map (\message -> { message | dataType = Name.internalize ( moduleName, message.dataType ) }) struct.messages
                        |> List.map (unqualifySelectedMessage rootModName)
                , enums =
                    List.map
                        (\enum ->
                            { enum
                                | dataType = Name.internalize ( moduleName, enum.dataType )
                                , fields =
                                    NonEmpty.map
                                        (Tuple.mapSecond <|
                                            \optionName ->
                                                { protoName = Name.internalize ( moduleName, optionName.protoName ), jsonName = optionName.jsonName }
                                        )
                                        enum.fields
                            }
                        )
                        struct.enums
                , services = []
                , docs = []
                , originFiles = struct.originFiles
                , oneOfs =
                    List.map
                        (\{ oneOfName, options, docs } ->
                            { oneOfName = Name.internalize ( moduleName, oneOfName )
                            , options =
                                List.map
                                    (\o ->
                                        { o
                                            | dataType = Name.internalize ( moduleName, o.dataType )
                                            , fieldType = Model.unqualifySelectedFieldType rootModName o.fieldType
                                        }
                                    )
                                    options
                            , docs = docs
                            }
                        )
                        struct.oneOfs
                , oneOfReexports = []
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

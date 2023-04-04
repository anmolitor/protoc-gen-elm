module Mapper.Struct exposing (..)

import Model exposing (Enum, Message, OneOf, Service)
import Set exposing (Set)


type alias Struct =
    { messages : List Message
    , enums : List Enum
    , services : List Service
    , oneOfs : List ( String, OneOf )
    , originFiles : Set String
    }


empty : Struct
empty =
    { messages = []
    , enums = []
    , services = []
    , oneOfs = []
    , originFiles = Set.empty
    }


append : Struct -> Struct -> Struct
append { messages, enums, services, oneOfs, originFiles } struct =
    { messages = struct.messages ++ messages
    , enums = struct.enums ++ enums
    , services = struct.services ++ services
    , oneOfs = struct.oneOfs ++ oneOfs
    , originFiles = Set.union struct.originFiles originFiles
    }


concat : List Struct -> Struct
concat =
    List.foldl append empty


concatMap : (a -> Struct) -> List a -> Struct
concatMap fn =
    List.map fn >> concat

module Mapper.Struct exposing (..)

import Model exposing (Enum, Message, OneOf, Service)
import Set exposing (Set)


type alias Struct =
    { messages : List Message
    , enums : List Enum
    , services : List Service
    , oneOfs : List { oneOfName : String, options : OneOf, docs : List String }
    , docs : List String
    , originFiles : Set String
    }


empty : Struct
empty =
    { messages = []
    , enums = []
    , services = []
    , oneOfs = []
    , docs = []
    , originFiles = Set.empty
    }


append : Struct -> Struct -> Struct
append { messages, enums, services, oneOfs, originFiles, docs } struct =
    { messages = struct.messages ++ messages
    , enums = struct.enums ++ enums
    , services = struct.services ++ services
    , oneOfs = struct.oneOfs ++ oneOfs
    , docs = struct.docs ++ docs
    , originFiles = Set.union struct.originFiles originFiles
    }


concat : List Struct -> Struct
concat =
    List.foldl append empty


concatMap : (a -> Struct) -> List a -> Struct
concatMap fn =
    List.map fn >> concat

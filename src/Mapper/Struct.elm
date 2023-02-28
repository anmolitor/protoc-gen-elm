module Mapper.Struct exposing (..)

import Model exposing (Enum, Message, Service)


type alias Struct =
    { messages : List Message
    , enums : List Enum
    , services : List Service
    }


empty : Struct
empty =
    { messages = []
    , enums = []
    , services = []
    }


append : Struct -> Struct -> Struct
append { messages, enums, services } struct =
    { messages = struct.messages ++ messages
    , enums = struct.enums ++ enums
    , services = struct.services ++ services
    }


concat : List Struct -> Struct
concat =
    List.foldl append empty


concatMap : (a -> Struct) -> List a -> Struct
concatMap fn =
    List.map fn >> concat

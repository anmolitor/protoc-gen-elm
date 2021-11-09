module Mapper.Struct exposing (..)

import Model exposing (Enum, Message)


type alias Struct =
    { messages : List Message
    , enums : List Enum
    }


empty : Struct
empty =
    { messages = []
    , enums = []
    }


append : Struct -> Struct -> Struct
append { messages, enums } struct =
    { messages = struct.messages ++ messages
    , enums = struct.enums ++ enums
    }


concat : List Struct -> Struct
concat =
    List.foldl append empty


concatMap : (a -> Struct) -> List a -> Struct
concatMap fn =
    List.map fn >> concat

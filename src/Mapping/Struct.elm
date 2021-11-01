module Mapping.Struct exposing (..)

import Model exposing (Enum, Map, Message)


type alias Struct =
    { messages : List Message
    , enums : List Enum
    , maps : List Map
    }


empty : Struct
empty =
    { messages = []
    , enums = []
    , maps = []
    }


append : Struct -> Struct -> Struct
append { messages, enums, maps } struct =
    { messages = struct.messages ++ messages
    , enums = struct.enums ++ enums
    , maps = struct.maps ++ maps
    }


concatMap : (a -> Struct) -> List a -> Struct
concatMap fn xs =
    List.foldl (append << fn) empty xs

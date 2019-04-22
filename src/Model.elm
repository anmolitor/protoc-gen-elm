module Model exposing (Cardinality(..), DataType, Enum, Field(..), FieldName, FieldNumber, FieldType(..), Map, Message, OneOf, Package)

import Set


type alias Package =
    { name : String
    , files : List String
    , messages : List Message
    , enums : List Enum
    , imports : Set.Set String
    }


type alias Message =
    { dataType : DataType
    , isTopLevel : Bool
    , fields : List ( FieldName, Field )
    }


type alias Enum =
    { dataType : DataType
    , isTopLevel : Bool
    , withUnrecognized : Bool
    , fields : ( ( Int, String ), List ( Int, String ) )
    }


type alias Map =
    { dataType : DataType
    , key : FieldType
    , value : FieldType
    }


type alias OneOf =
    List ( FieldNumber, DataType, FieldType )


type alias FieldName =
    String


type alias FieldNumber =
    Int


type alias DataType =
    String


type alias Default =
    String


type FieldType
    = Primitive DataType String Default
    | Embedded DataType
    | Enumeration (Maybe Default) DataType


type Field
    = Field FieldNumber Cardinality FieldType
    | MapField FieldNumber Map
    | OneOfField DataType OneOf


type Cardinality
    = Optional
    | Required
    | Repeated

module Model exposing (Cardinality(..), DataType, Enum, Field(..), FieldName, FieldNumber, FieldType(..), IntFlavor(..), Map, Message, OneOf, Package, Primitive(..), TypeKind(..))

import Elm.CodeGen as C
import Elm.Syntax.ModuleName exposing (ModuleName)
import List.NonEmpty exposing (NonEmpty)
import Set exposing (Set)


type alias Package =
    { name : String
    , files : List String
    , messages : List Message
    , enums : List Enum
    , imports : Set String
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
    , fields : NonEmpty ( Int, String )
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
    C.Expression


type TypeKind
    = Alias
    | Type


type FieldType
    = -- Primitive Type, Method in protobuf/[en/de]code to use, default value
      Primitive Primitive Default
    | Embedded { dataType : DataType, moduleName : ModuleName, typeKind : TypeKind }
    | Enumeration { dataType : DataType, moduleName : ModuleName, values : List DataType, default : DataType }


type Primitive
    = Prim_String
    | Prim_Int32 IntFlavor
    | Prim_Int64 IntFlavor
    | Prim_Float
    | Prim_Bool
    | Prim_Bytes
    | Prim_Double


type IntFlavor
    = Int_
    | SInt
    | UInt
    | Fixed
    | SFixed


type Field
    = NormalField FieldNumber Cardinality FieldType
    | MapField FieldNumber FieldType FieldType
    | OneOfField DataType OneOf


type Cardinality
    = Optional
    | Required
    | Repeated

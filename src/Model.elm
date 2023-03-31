module Model exposing
    ( Cardinality(..)
    , DataType
    , Enum
    , Field(..)
    , FieldName
    , FieldNumber
    , FieldType(..)
    , IntFlavor(..)
    , Map
    , Message
    , Method
    , OneOf
    , Primitive(..)
    , Service
    , TypeKind(..)
    )

import Elm.CodeGen as C exposing (ModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import List.NonEmpty exposing (NonEmpty)


type alias Service =
    { name : String
    , methods : List Method
    , package : String
    }


type alias Method =
    { name : String
    , reqType : ( C.ModuleName, String )
    , resType : ( C.ModuleName, String )
    }


type alias Message =
    { dataType : DataType
    , fields : List ( FieldName, Field )
    }


type alias Enum =
    { dataType : DataType
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
    | Enumeration { dataType : DataType, moduleName : ModuleName }


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
    | OneOfField DataType ModuleName


type Cardinality
    = Optional
    | Required
    | Repeated

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
    , unqualifySelectedFieldType
    , unqualifySelectedMessage, setTypeKind
    )

import Elm.CodeGen as C exposing (ModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import List.NonEmpty exposing (NonEmpty)
import Mapper.Name exposing (Ref)


type alias Service =
    { name : String
    , methods : List Method
    , package : String
    , docs : List String
    }


type alias Method =
    { name : String
    , reqType : Ref
    , resType : Ref
    , docs : List String
    }


type alias Message =
    { dataType : DataType
    , fields : List ( FieldName, Field )
    , docs : List String
    }


unqualifySelectedMessage : ModuleName -> Message -> Message
unqualifySelectedMessage modName message =
    { message | fields = List.map (Tuple.mapSecond <| unqualifySelectedField modName) message.fields }


unqualifySelectedField : ModuleName -> Field -> Field
unqualifySelectedField modName field =
    case field of
        NormalField n c t ->
            NormalField n c <| unqualifySelectedFieldType modName t

        MapField n k v ->
            MapField n (unqualifySelectedFieldType modName k) (unqualifySelectedFieldType modName v)

        OneOfField ref ->
            if ref.rootPackage == modName then
                OneOfField { ref | rootPackage = [] }

            else
                field


unqualifySelectedFieldType : ModuleName -> FieldType -> FieldType
unqualifySelectedFieldType modName fieldType =
    case fieldType of
        Primitive _ _ ->
            fieldType

        Enumeration ref ->
            if ref.rootPackage == modName then
                Enumeration { ref | rootPackage = [] }

            else
                fieldType

        Embedded ref ->
            if ref.rootModuleName == modName then
                Embedded { ref | rootModuleName = [] }

            else
                fieldType


type alias Enum =
    { dataType : DataType
    , withUnrecognized : Bool
    , fields : NonEmpty ( Int, String )
    , docs : List String
    }


type alias Map =
    { dataType : DataType
    , key : FieldType
    , value : FieldType
    }


type alias OneOf =
    List { fieldNumber : Int, dataType : String, fieldName : String, fieldType : FieldType }


type alias FieldName =
    String


type alias FieldNumber =
    Int


type alias DataType =
    String


type alias Default =
    C.Expression


setTypeKind : TypeKind -> FieldType -> FieldType
setTypeKind kind fieldType =
    case fieldType of
        Embedded e ->
            Embedded { e | typeKind = kind }

        _ ->
            fieldType


type TypeKind
    = Alias
    | Type


type FieldType
    = -- Primitive Type, default value
      Primitive Primitive Default
    | Embedded { dataType : DataType, moduleName : ModuleName, rootModuleName : ModuleName, typeKind : TypeKind }
    | Enumeration Ref


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
    | OneOfField Ref


type Cardinality
    = Optional
    | Proto3Optional
    | Required
    | Repeated

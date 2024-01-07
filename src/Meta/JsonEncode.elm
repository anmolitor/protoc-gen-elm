module Meta.JsonEncode exposing (..)

import Elm.CodeGen as C exposing (Expression, ModuleName, TypeAnnotation)
import Model exposing (IntFlavor(..), Primitive(..))


encoder : TypeAnnotation -> TypeAnnotation
encoder encodedType =
    C.funAnn encodedType value


value : TypeAnnotation
value =
    C.fqTyped moduleName "Value" []


string : Expression
string =
    C.fqFun moduleName "string"


moduleName : ModuleName
moduleName =
    [ "Json", "Encode" ]


null : Expression
null =
    C.fqVal moduleName "null"


list : Expression
list =
    C.fqFun moduleName "list"


dict : Expression
dict =
    C.fqFun moduleName "dict"


object : Expression
object =
    C.fqFun moduleName "object"


forPrimitive : Primitive -> Expression
forPrimitive prim =
    case prim of
        Prim_Int32 Int_ ->
            int32

        Prim_Int32 SInt ->
            int32

        Prim_Int32 UInt ->
            int32

        Prim_Int32 Fixed ->
            int32

        Prim_Int32 SFixed ->
            int32

        Prim_Int64 Int_ ->
            int64

        Prim_Int64 SInt ->
            int64

        Prim_Int64 UInt ->
            uint64

        Prim_Int64 Fixed ->
            int64

        Prim_Int64 SFixed ->
            int64

        Prim_Bool ->
            bool

        Prim_Float ->
            float

        Prim_String ->
            string

        Prim_Bytes ->
            bytes

        Prim_Double ->
            float


primitiveToMapKey : Primitive -> Expression
primitiveToMapKey prim =
    case prim of
        Prim_Int32 Int_ ->
            int32_toMapKey

        Prim_Int32 SInt ->
            int32_toMapKey

        Prim_Int32 UInt ->
            int32_toMapKey

        Prim_Int32 Fixed ->
            int32_toMapKey

        Prim_Int32 SFixed ->
            int32_toMapKey

        Prim_Int64 Int_ ->
            int64_toMapKey

        Prim_Int64 SInt ->
            int64_toMapKey

        Prim_Int64 UInt ->
            uint64_toMapKey

        Prim_Int64 Fixed ->
            int64_toMapKey

        Prim_Int64 SFixed ->
            int64_toMapKey

        Prim_String ->
            C.fun "identity"

        Prim_Bool ->
            C.string "ERROR: Bool is not a valid key for a protobuf map!"

        Prim_Float ->
            C.string "ERROR: Float is not a valid key for a protobuf map!"

        Prim_Bytes ->
            C.string "ERROR: Bytes is not a valid key for a protobuf map!"

        Prim_Double ->
            C.string "ERROR: Double is not a valid key for a protobuf map!"


int32 : Expression
int32 =
    C.fqFun moduleName "int"


bool : Expression
bool =
    C.fqFun moduleName "bool"


float : Expression
float =
    C.fqFun moduleName "float"


bytes : Expression
bytes =
    C.fqFun [ "Protobuf", "Utils", "Bytes" ] "jsonEncoder"


int64 : Expression
int64 =
    C.parens <| C.applyBinOp int64_toString C.composer string


int32_toMapKey : Expression
int32_toMapKey =
    C.fqFun [ "String" ] "fromInt"


int64_toMapKey : Expression
int64_toMapKey =
    C.parens
        (C.lambda [ C.tuplePattern [ C.varPattern "upper", C.varPattern "lower" ] ] <|
            C.pipe (C.apply [ C.fqFun [ "Protobuf", "Types", "Int64" ] "fromInts", C.val "upper", C.val "lower" ]) [ int64_toString ]
        )


uint64_toMapKey : Expression
uint64_toMapKey =
    C.parens
        (C.lambda [ C.tuplePattern [ C.varPattern "upper", C.varPattern "lower" ] ] <|
            C.pipe (C.apply [ C.fqFun [ "Protobuf", "Types", "Int64" ] "fromInts", C.val "upper", C.val "lower" ]) [ uint64_toString ]
        )


int64_toString : Expression
int64_toString =
    C.fqFun [ "Protobuf", "Utils", "Int64" ] "toSignedString"


uint64_toString : Expression
uint64_toString =
    C.fqFun [ "Protobuf", "Utils", "Int64" ] "toUnsignedString"


uint64 : Expression
uint64 =
    C.parens <| C.applyBinOp uint64_toString C.composer string

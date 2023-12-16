module Meta.JsonEncode exposing (..)

import Elm.CodeGen as C exposing (Expression, ModuleName, TypeAnnotation)
import Json.Encode
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
            int64

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
    C.parens <| C.applyBinOp (C.fqFun [ "Hex", "Convert" ] "toString") C.composer string


int64 : Expression
int64 =
    -- TODO!
    C.lambda [ C.allPattern ] (C.apply [ string, C.string "Int64 not supported yet" ])

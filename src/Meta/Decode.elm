module Meta.Decode exposing (..)

import Elm.CodeGen as C
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Model exposing (Primitive(..))


decoder : TypeAnnotation -> TypeAnnotation
decoder =
    List.singleton >> C.fqTyped [ "Protobuf", "Decode" ] "Decoder"


map : Expression
map =
    C.fqFun moduleName "map"


oneOf : Expression
oneOf =
    C.fqFun moduleName "oneOf"


mapped : Expression
mapped =
    C.fqFun moduleName "mapped"


int32 : Expression
int32 =
    C.fqFun moduleName "int32"


string : Expression
string =
    C.fqFun moduleName "string"


float : Expression
float =
    C.fqFun moduleName "float"


bool : Expression
bool =
    C.fqFun moduleName "bool"


bytes : Expression
bytes =
    C.fqFun moduleName "bytes"


message : Expression
message =
    C.fqFun moduleName "message"


required : Expression
required =
    C.fqFun moduleName "required"


optional : Expression
optional =
    C.fqFun moduleName "optional"


repeated : Expression
repeated =
    C.fqFun moduleName "repeated"


forPrimitive : Primitive -> Expression
forPrimitive prim =
    case prim of
        Prim_String ->
            string

        Prim_Bool ->
            bool

        Prim_Bytes ->
            bytes

        Prim_Float ->
            float

        Prim_Int ->
            int32


moduleName : ModuleName
moduleName =
    [ "Protobuf", "Decode" ]

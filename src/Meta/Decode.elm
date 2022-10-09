module Meta.Decode exposing (..)

import Elm.CodeGen as C
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Model exposing (IntFlavor(..), Primitive(..))


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


sint32 : Expression
sint32 =
    C.fqFun moduleName "sint32"


uint32 : Expression
uint32 =
    C.fqFun moduleName "uint32"


fixed32 : Expression
fixed32 =
    C.fqFun moduleName "fixed32"


sfixed32 : Expression
sfixed32 =
    C.fqFun moduleName "sfixed32"


string : Expression
string =
    C.fqFun moduleName "string"


float : Expression
float =
    C.fqFun moduleName "float"


double : Expression
double =
    C.fqFun moduleName "double"


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


lazy : Expression
lazy =
    C.fqFun moduleName "lazy"


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

        Prim_Int Int32 ->
            int32

        Prim_Int SInt32 ->
            sint32

        Prim_Int UInt32 ->
            uint32

        Prim_Int Fixed32 ->
            fixed32

        Prim_Int SFixed32 ->
            sfixed32

        Prim_Double ->
            double


moduleName : ModuleName
moduleName =
    [ "Protobuf", "Decode" ]

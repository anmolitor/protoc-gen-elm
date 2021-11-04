module Meta.Encode exposing (..)

import Elm.CodeGen as C
import Elm.Syntax.Expression exposing (Expression, Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Model exposing (Primitive(..))


encoder : TypeAnnotation -> TypeAnnotation
encoder encodedType =
    C.funAnn encodedType (C.fqTyped moduleName "Encoder" [])


int32 : Expression
int32 =
    C.fqFun moduleName "int32"


dict : Expression
dict =
    C.fqFun moduleName "dict"


bool : Expression
bool =
    C.fqFun moduleName "bool"


float : Expression
float =
    C.fqFun moduleName "float"


string : Expression
string =
    C.fqFun moduleName "string"


bytes : Expression
bytes =
    C.fqFun moduleName "bytes"


none : Expression
none =
    C.fqFun moduleName "none"


list : Expression
list =
    C.fqFun moduleName "list"


forPrimitive : Primitive -> Expression
forPrimitive prim =
    case prim of
        Prim_Int ->
            int32

        Prim_Bool ->
            bool

        Prim_Float ->
            float

        Prim_String ->
            string

        Prim_Bytes ->
            bytes


message : List Expression -> Expression
message fieldEncoderTuples =
    C.apply [ C.fqFun moduleName "message", C.list fieldEncoderTuples ]


moduleName : ModuleName
moduleName =
    [ "Protobuf", "Encode" ]

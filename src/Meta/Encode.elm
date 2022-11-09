module Meta.Encode exposing (..)

import Elm.CodeGen as C
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Model exposing (IntFlavor(..), Primitive(..))


encoder : TypeAnnotation -> TypeAnnotation
encoder encodedType =
    C.funAnn encodedType (C.fqTyped moduleName "Encoder" [])


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


int64 : Expression
int64 =
    C.fqFun moduleName "int64"


sint64 : Expression
sint64 =
    C.fqFun moduleName "sint64"


uint64 : Expression
uint64 =
    C.fqFun moduleName "uint64"


fixed64 : Expression
fixed64 =
    C.fqFun moduleName "fixed64"


sfixed64 : Expression
sfixed64 =
    C.fqFun moduleName "sfixed64"


dict : Expression
dict =
    C.fqFun moduleName "dict"


bool : Expression
bool =
    C.fqFun moduleName "bool"


float : Expression
float =
    C.fqFun moduleName "float"


double : Expression
double =
    C.fqFun moduleName "double"


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
        Prim_Int32 Int_ ->
            int32

        Prim_Int32 SInt ->
            sint32

        Prim_Int32 UInt ->
            uint32

        Prim_Int32 Fixed ->
            fixed32

        Prim_Int32 SFixed ->
            sfixed32

        Prim_Int64 Int_ ->
            int64

        Prim_Int64 SInt ->
            sint64

        Prim_Int64 UInt ->
            uint64

        Prim_Int64 Fixed ->
            fixed64

        Prim_Int64 SFixed ->
            sfixed64

        Prim_Bool ->
            bool

        Prim_Float ->
            float

        Prim_String ->
            string

        Prim_Bytes ->
            bytes

        Prim_Double ->
            double


message : List Expression -> Expression
message fieldEncoderTuples =
    C.apply [ C.fqFun moduleName "message", C.list fieldEncoderTuples ]


moduleName : ModuleName
moduleName =
    [ "Protobuf", "Encode" ]

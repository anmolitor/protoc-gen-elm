module Meta.Type exposing (..)

import Elm.CodeGen as C
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Model exposing (Primitive(..))


dict : TypeAnnotation -> TypeAnnotation -> TypeAnnotation
dict k v =
    C.fqTyped [ "Dict" ] "Dict" [ k, v ]


result : TypeAnnotation -> TypeAnnotation -> TypeAnnotation
result e a =
    C.fqTyped [ "Result" ] "Result" [ e, a ]


cmd : TypeAnnotation -> TypeAnnotation
cmd v =
    C.fqTyped [ "Platform", "Cmd" ] "Cmd" [ v ]


forPrimitive : Primitive -> TypeAnnotation
forPrimitive prim =
    case prim of
        Prim_String ->
            C.stringAnn

        Prim_Bool ->
            C.boolAnn

        Prim_Int32 _ ->
            C.intAnn

        Prim_Int64 _ ->
            C.fqTyped [ "Protobuf", "Types", "Int64" ] "Int64" []

        Prim_Float ->
            C.floatAnn

        Prim_Bytes ->
            C.fqTyped [ "Bytes" ] "Bytes" []

        Prim_Double ->
            C.floatAnn

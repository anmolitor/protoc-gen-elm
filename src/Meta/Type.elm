module Meta.Type exposing (..)

import Elm.CodeGen as C
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Model exposing (Primitive(..))


dict : TypeAnnotation -> TypeAnnotation -> TypeAnnotation
dict k v =
    C.fqTyped [ "Dict" ] "Dict" [ k, v ]


forPrimitive : Primitive -> TypeAnnotation
forPrimitive prim =
    case prim of
        Prim_String ->
            C.stringAnn

        Prim_Bool ->
            C.boolAnn

        Prim_Int ->
            C.intAnn

        Prim_Float ->
            C.floatAnn

        Prim_Bytes ->
            C.fqTyped [ "Bytes" ] "Bytes" []

        Prim_Double ->
            C.floatAnn

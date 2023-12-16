module Meta.Basics exposing (..)

import Elm.CodeGen as C
import Elm.Syntax.Expression exposing (Expression)


nothing : Expression
nothing =
    C.fun "Nothing"


identity : Expression
identity =
    C.fun "identity"


just : Expression
just =
    C.fun "Just"


mapMaybe : Expression
mapMaybe =
    C.fqFun [ "Maybe" ] "map"


withDefault : Expression
withDefault =
    C.fqFun [ "Maybe" ] "withDefault"

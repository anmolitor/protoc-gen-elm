module Meta.Basics exposing (..)

import Elm.CodeGen as C
import Elm.Syntax.Expression exposing (Expression)


nothing : Expression
nothing =
    C.fun "Nothing"

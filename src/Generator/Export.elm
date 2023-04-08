module Generator.Export exposing (fromDeclarations)

import Elm.CodeGen as C
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node


fromDeclarations : List C.Declaration -> List C.TopLevelExpose
fromDeclarations =
    List.filterMap exportDecl


exportDecl : C.Declaration -> Maybe C.TopLevelExpose
exportDecl decl =
    let
        handleInnerDecl : Declaration.Declaration -> Maybe C.TopLevelExpose
        handleInnerDecl d =
            case d of
                Declaration.AliasDeclaration { name } ->
                    Just <| C.typeOrAliasExpose <| Node.value name

                Declaration.CustomTypeDeclaration { name } ->
                    Just <| C.openTypeExpose <| Node.value name

                Declaration.FunctionDeclaration fun ->
                    let
                        { name } =
                            Node.value fun.declaration
                    in
                    Just <| C.funExpose <| Node.value name

                _ ->
                    Nothing
    in
    handleInnerDecl <|
        case decl of
            C.DeclWithComment _ d ->
                d ""

            C.DeclNoComment d ->
                d

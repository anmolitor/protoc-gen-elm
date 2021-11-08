module Mapping.Declarations exposing (removeDuplicateDeclarations)

import Elm.CodeGen as C
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node
import Set exposing (Set)


removeDuplicateDeclarations : List C.Declaration -> List C.Declaration
removeDuplicateDeclarations =
    let
        removeDuplicates : C.Declaration -> ( List C.Declaration, Set String ) -> ( List C.Declaration, Set String )
        removeDuplicates decl ( previousDecls, usedNames ) =
            let
                declName =
                    retrieveName decl
            in
            if Set.member declName usedNames then
                ( previousDecls, usedNames )

            else
                ( decl :: previousDecls, Set.insert declName usedNames )
    in
    List.foldl removeDuplicates ( [], Set.empty ) >> Tuple.first


retrieveName : C.Declaration -> String
retrieveName decl =
    let
        unwrappedDecl : Declaration
        unwrappedDecl =
            case decl of
                C.DeclWithComment _ f ->
                    f ""

                C.DeclNoComment d ->
                    d
    in
    case unwrappedDecl of
        FunctionDeclaration fun ->
            fun.declaration |> Node.value |> .name |> Node.value

        AliasDeclaration al ->
            al.name |> Node.value

        CustomTypeDeclaration customType ->
            customType.name |> Node.value

        PortDeclaration prt ->
            prt.name |> Node.value

        InfixDeclaration infx ->
            infx.function |> Node.value

        Destructuring pattern _ ->
            -- cannot occur top level
            ""

module Generator.Internals exposing (..)

{- This module exists because of a recursive import limitation for the Elm language
   The idea is that we instead generate "Internals_.elm" files containing all the code for a package
   and then the actual module tree imports from there.

   So if there is a proto file like this:

   message Outer {}

   message InnerScope {
      message Inner {
        Outer outer = 1;
      }
      Inner inner = 1;
   }

   we don't get the Proto.elm <-> Proto.InnerScope.elm circular dependency, but instead
   generate

   type alias Outer = Proto.Internals_.Proto_Outer

   in Proto.elm and

   type alias Inner = Proto.Internals_.Proto_InnerScope_Inner

   in Proto.InnerScope.elm
-}

import Mapper.Package exposing (Packages)


toInternals : Packages -> Packages
toInternals packages =
    Debug.todo ""



-- Mapper.Package.unify
--   -- |>

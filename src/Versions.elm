module Versions exposing (..)

import Proto.Google.Protobuf.Compiler exposing (Version)


type alias PluginAndLibVersions =
    { plugin : String
    , library : String
    }


type alias AllVersions =
    { plugin : String
    , library : String
    , compiler : String
    }


addCompilerVersion : Maybe Version -> PluginAndLibVersions -> AllVersions
addCompilerVersion compilerVersion { plugin, library } =
    { plugin = plugin, library = library, compiler = Maybe.withDefault "unknown version" (Maybe.map versionToString compilerVersion) }


versionToString : Version -> String
versionToString v =
    String.join "." [ String.fromInt v.major, String.fromInt v.minor, String.fromInt v.patch ] ++ v.suffix

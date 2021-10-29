module GeneratorNew exposing (..)

import Elm.CodeGen exposing (File)
import Elm.Pretty
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node
import Elm.Writer as Writer
import Internal.Google.Protobuf.Compiler exposing (CodeGeneratorResponseFile)


generate : File -> CodeGeneratorResponseFile
generate file =
    { name = (Node.value file.moduleDefinition |> Module.moduleName |> String.join "/") ++ ".elm"
    , content = Elm.Pretty.pretty 120 file
    , insertionPoint = ""
    , generatedCodeInfo = Nothing
    }

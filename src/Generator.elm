module Generator exposing (requestToResponse)

import Dict exposing (Dict)
import Elm.CodeGen as C exposing (ModuleName)
import Elm.Pretty
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node
import Errors exposing (Res)
import Generator.Common as Common
import Generator.Declarations exposing (removeDuplicateDeclarations)
import Generator.DevToolsWorker exposing (generateDevToolsWorker)
import Generator.Enum as Enum
import Generator.Export as Export
import Generator.Import as Import
import Generator.Message as Message
import Generator.OneOf as OneOf
import Generator.Service as Service
import List.Extra
import Mapper
import Mapper.Package as Package exposing (Packages)
import Mapper.Struct exposing (Struct)
import Model exposing (Field(..))
import Options exposing (Options)
import Proto.Google.Protobuf exposing (FileDescriptorProto)
import Proto.Google.Protobuf.Compiler exposing (CodeGeneratorRequest, CodeGeneratorResponse)
import Proto.Google.Protobuf.Compiler.CodeGeneratorResponse as CodeGeneratorResponse
import Protobuf.Types.Int64
import Set exposing (Set)
import Versions exposing (AllVersions)


requestToResponse :
    AllVersions
    -> Options
    -> CodeGeneratorRequest
    -> CodeGeneratorResponse
requestToResponse versions options req =
    let
        filesToResponse : Res (List CodeGeneratorResponse.File) -> CodeGeneratorResponse
        filesToResponse fileResults =
            case fileResults of
                Err error ->
                    { error = Errors.format error, supportedFeatures = Protobuf.Types.Int64.fromInts 0 3, file = [] }

                Ok file ->
                    { error = "", supportedFeatures = Protobuf.Types.Int64.fromInts 0 3, file = file }

        files =
            convert versions options req.fileToGenerate req.protoFile

        devToolsFiles =
            if options.grpcDevTools then
                [ Generator.DevToolsWorker.devToolsJsFile versions, Generator.DevToolsWorker.devToolsDTsFile ]

            else
                []
    in
    files |> Result.map (List.map generate >> (++) devToolsFiles) |> filesToResponse


generate : C.File -> CodeGeneratorResponse.File
generate file =
    { name = (Node.value file.moduleDefinition |> Module.moduleName |> String.join "/") ++ ".elm"
    , content = "{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}\n\n" ++ Elm.Pretty.pretty 120 file
    , insertionPoint = ""
    , generatedCodeInfo = Nothing
    }


convert : AllVersions -> Options -> List String -> List FileDescriptorProto -> Res (List C.File)
convert versions options fileNames descriptors =
    let
        files : Res (Dict ModuleName Packages)
        files =
            descriptors
                |> List.filter (.name >> (\name -> List.member name fileNames))
                |> Mapper.mapMain options
                |> Errors.combineMap (\( mod, res ) -> Result.map (Tuple.pair mod) res)
                |> Result.map
                    (List.foldl
                        (\( mod, pkg ) ->
                            Dict.update mod
                                (Maybe.map (Package.append pkg)
                                    >> Maybe.withDefault pkg
                                    >> Just
                                )
                        )
                        Dict.empty
                    )

        mkInternalsFile : ModuleName -> Packages -> C.File
        mkInternalsFile moduleName =
            packageToFile moduleName << Package.unify moduleName

        packageToFile : ModuleName -> Struct -> C.File
        packageToFile moduleName struct =
            let
                declarations =
                    removeDuplicateDeclarations
                        (List.concatMap (Enum.toAST options) struct.enums
                            ++ List.concatMap (Message.toAST options) struct.messages
                            ++ List.concatMap (OneOf.toAST options) struct.oneOfs
                        )

                packageName =
                    moduleName ++ [ "Internals_" ]

                exports =
                    Export.fromDeclarations declarations
            in
            C.file
                (C.normalModule packageName exports)
                (List.map (\importedModule -> C.importStmt importedModule Nothing Nothing) (Set.toList <| Import.extractImports declarations))
                declarations
                (C.emptyFileComment
                    |> fileComment versions struct.originFiles
                    |> C.docTagsFromExposings exports
                    |> Just
                )

        packageToReexportFile : ModuleName -> ModuleName -> Struct -> C.File
        packageToReexportFile rootModName packageName struct =
            let
                internalsModule =
                    rootModName ++ [ "Internals_" ]

                declarations =
                    List.concatMap (Enum.reexportAST options internalsModule packageName) struct.enums
                        ++ List.concatMap (Message.reexportAST options internalsModule packageName) struct.messages
                        ++ List.concatMap (OneOf.reexportAST internalsModule packageName) struct.oneOfs
                        ++ List.concatMap Service.toAST struct.services

                exports =
                    Export.fromDeclarations declarations

                fileDocs =
                    List.Extra.unique struct.docs
                        ++ (struct.services |> List.concatMap .docs)
            in
            C.file
                (C.normalModule packageName exports)
                (List.map (\importedModule -> C.importStmt importedModule Nothing Nothing) (Set.toList <| Import.extractImports declarations))
                (removeDuplicateDeclarations declarations)
                (C.emptyFileComment
                    |> fileComment versions struct.originFiles
                    |> Common.addDocs fileDocs
                    |> C.docTagsFromExposings exports
                    |> Just
                )
    in
    Result.map
        (\modDict ->
            let
                mods =
                    Dict.toList modDict

                services =
                    List.concatMap (\( _, package ) -> Dict.values package |> List.concatMap .services) mods

                devToolsWorkerFile =
                    if options.grpcDevTools && options.grpc then
                        [ generateDevToolsWorker services ]

                    else
                        []
            in
            devToolsWorkerFile
                ++ List.concatMap (\( mod, package ) -> Dict.map (packageToReexportFile mod) package |> Dict.values |> (::) (mkInternalsFile mod package)) mods
        )
        files


fileComment : AllVersions -> Set String -> C.Comment C.FileComment -> C.Comment C.FileComment
fileComment versions originFiles =
    C.markdown <| """
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) """ ++ versions.plugin ++ """
- `protoc` """ ++ versions.compiler ++ """
- the following specification files: `""" ++ (Set.toList originFiles |> String.join ", ") ++ """`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version """ ++ versions.library ++ """ or higher."""

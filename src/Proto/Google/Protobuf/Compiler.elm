{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.Compiler exposing (CodeGeneratorRequest, CodeGeneratorResponse, Version, decodeCodeGeneratorRequest, decodeCodeGeneratorResponse, decodeVersion, defaultCodeGeneratorRequest, defaultCodeGeneratorResponse, defaultVersion, encodeCodeGeneratorRequest, encodeCodeGeneratorResponse, encodeVersion, fieldNumbersCodeGeneratorRequest, fieldNumbersCodeGeneratorResponse, fieldNumbersVersion)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 3.4.4
- `protoc` 3.19.4
- the following specification files: `google/protobuf/compiler/plugin.proto`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version 1.2.0 or higher.

 Protocol Buffers - Google's data interchange format
 Copyright 2008 Google Inc.  All rights reserved.
 https://developers.google.com/protocol-buffers/

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are
 met:

     * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above
 copyright notice, this list of conditions and the following disclaimer
 in the documentation and/or other materials provided with the
 distribution.
     * Neither the name of Google Inc. nor the names of its
 contributors may be used to endorse or promote products derived from
 this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


 Author: kenton@google.com (Kenton Varda)

 WARNING:  The plugin interface is currently EXPERIMENTAL and is subject to
   change.

 protoc (aka the Protocol Compiler) can be extended via plugins.  A plugin is
 just a program that reads a CodeGeneratorRequest from stdin and writes a
 CodeGeneratorResponse to stdout.

 Plugins written using C++ can use google/protobuf/compiler/plugin.h instead
 of dealing with the raw protocol defined here.

 A plugin executable needs only to be placed somewhere in the path.  The
 plugin should be named "protoc-gen-$NAME", and will then be used when the
 flag "--${NAME}_out" is passed to protoc.


@docs CodeGeneratorRequest, CodeGeneratorResponse, Version, decodeCodeGeneratorRequest, decodeCodeGeneratorResponse, decodeVersion

@docs defaultCodeGeneratorRequest, defaultCodeGeneratorResponse, defaultVersion, encodeCodeGeneratorRequest

@docs encodeCodeGeneratorResponse, encodeVersion, fieldNumbersCodeGeneratorRequest, fieldNumbersCodeGeneratorResponse

@docs fieldNumbersVersion

-}

import Proto.Google.Protobuf.Compiler.Internals_
import Protobuf.Decode
import Protobuf.Encode


{-| The field numbers for the fields of `CodeGeneratorResponse`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersCodeGeneratorResponse : { error : Int, supportedFeatures : Int, file : Int }
fieldNumbersCodeGeneratorResponse =
    Proto.Google.Protobuf.Compiler.Internals_.fieldNumbersProto__Google__Protobuf__Compiler__CodeGeneratorResponse


{-| Default for CodeGeneratorResponse. Should only be used for 'required' decoders as an initial value.

-}
defaultCodeGeneratorResponse : CodeGeneratorResponse
defaultCodeGeneratorResponse =
    Proto.Google.Protobuf.Compiler.Internals_.defaultProto__Google__Protobuf__Compiler__CodeGeneratorResponse


{-| Declares how to decode a `CodeGeneratorResponse` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeCodeGeneratorResponse : Protobuf.Decode.Decoder CodeGeneratorResponse
decodeCodeGeneratorResponse =
    Proto.Google.Protobuf.Compiler.Internals_.decodeProto__Google__Protobuf__Compiler__CodeGeneratorResponse


{-| Declares how to encode a `CodeGeneratorResponse` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeCodeGeneratorResponse : CodeGeneratorResponse -> Protobuf.Encode.Encoder
encodeCodeGeneratorResponse =
    Proto.Google.Protobuf.Compiler.Internals_.encodeProto__Google__Protobuf__Compiler__CodeGeneratorResponse


{-|  The plugin writes an encoded CodeGeneratorResponse to stdout.


## Fields

### error

 Error message.  If non-empty, code generation failed.  The plugin process
 should exit with status code zero even if it reports an error in this way.

 This should be used to indicate errors in .proto files which prevent the
 code generator from generating correct code.  Errors which indicate a
 problem in protoc itself -- such as the input CodeGeneratorRequest being
 unparseable -- should be reported by writing a message to stderr and
 exiting with a non-zero status code.


### supportedFeatures

 A bitmask of supported features that the code generator supports.
 This is a bitwise "or" of values from the Feature enum.


-}
type alias CodeGeneratorResponse =
    Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorResponse


{-| The field numbers for the fields of `CodeGeneratorRequest`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersCodeGeneratorRequest : { fileToGenerate : Int, parameter : Int, protoFile : Int, compilerVersion : Int }
fieldNumbersCodeGeneratorRequest =
    Proto.Google.Protobuf.Compiler.Internals_.fieldNumbersProto__Google__Protobuf__Compiler__CodeGeneratorRequest


{-| Default for CodeGeneratorRequest. Should only be used for 'required' decoders as an initial value.

-}
defaultCodeGeneratorRequest : CodeGeneratorRequest
defaultCodeGeneratorRequest =
    Proto.Google.Protobuf.Compiler.Internals_.defaultProto__Google__Protobuf__Compiler__CodeGeneratorRequest


{-| Declares how to decode a `CodeGeneratorRequest` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeCodeGeneratorRequest : Protobuf.Decode.Decoder CodeGeneratorRequest
decodeCodeGeneratorRequest =
    Proto.Google.Protobuf.Compiler.Internals_.decodeProto__Google__Protobuf__Compiler__CodeGeneratorRequest


{-| Declares how to encode a `CodeGeneratorRequest` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeCodeGeneratorRequest : CodeGeneratorRequest -> Protobuf.Encode.Encoder
encodeCodeGeneratorRequest =
    Proto.Google.Protobuf.Compiler.Internals_.encodeProto__Google__Protobuf__Compiler__CodeGeneratorRequest


{-|  An encoded CodeGeneratorRequest is written to the plugin's stdin.


## Fields

### fileToGenerate

 The .proto files that were explicitly listed on the command-line.  The
 code generator should generate code only for these files.  Each file's
 descriptor will be included in proto_file, below.


### parameter

 The generator parameter passed on the command-line.


### protoFile

 FileDescriptorProtos for all files in files_to_generate and everything
 they import.  The files will appear in topological order, so each file
 appears before any file that imports it.

 protoc guarantees that all proto_files will be written after
 the fields above, even though this is not technically guaranteed by the
 protobuf wire format.  This theoretically could allow a plugin to stream
 in the FileDescriptorProtos and handle them one by one rather than read
 the entire set into memory at once.  However, as of this writing, this
 is not similarly optimized on protoc's end -- it will store all fields in
 memory at once before sending them to the plugin.

 Type names of fields and extensions in the FileDescriptorProto are always
 fully qualified.


### compilerVersion

 The version number of protocol compiler.


-}
type alias CodeGeneratorRequest =
    Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorRequest


{-| The field numbers for the fields of `Version`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersVersion : { major : Int, minor : Int, patch : Int, suffix : Int }
fieldNumbersVersion =
    Proto.Google.Protobuf.Compiler.Internals_.fieldNumbersProto__Google__Protobuf__Compiler__Version


{-| Default for Version. Should only be used for 'required' decoders as an initial value.

-}
defaultVersion : Version
defaultVersion =
    Proto.Google.Protobuf.Compiler.Internals_.defaultProto__Google__Protobuf__Compiler__Version


{-| Declares how to decode a `Version` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeVersion : Protobuf.Decode.Decoder Version
decodeVersion =
    Proto.Google.Protobuf.Compiler.Internals_.decodeProto__Google__Protobuf__Compiler__Version


{-| Declares how to encode a `Version` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeVersion : Version -> Protobuf.Encode.Encoder
encodeVersion =
    Proto.Google.Protobuf.Compiler.Internals_.encodeProto__Google__Protobuf__Compiler__Version


{-|  The version number of protocol compiler.


## Fields

### suffix

 A suffix for alpha, beta or rc release, e.g., "alpha-1", "rc2". It should
 be empty for mainline stable releases.


-}
type alias Version =
    Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__Version

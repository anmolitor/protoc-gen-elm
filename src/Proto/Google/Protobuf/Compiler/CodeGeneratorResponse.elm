{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.Compiler.CodeGeneratorResponse exposing (Feature(..), File, decodeFeature, decodeFile, defaultFeature, defaultFile, encodeFeature, encodeFile, fieldNumbersFeature, fieldNumbersFile, fromInternalFeature, toInternalFeature)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 3.4.1
- `protoc` 3.19.4
- the following specification files: `google/protobuf/compiler/plugin.proto`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version 1.2.0 or higher.

@docs Feature, File, decodeFeature, decodeFile, defaultFeature, defaultFile, encodeFeature, encodeFile, fieldNumbersFeature, fieldNumbersFile

@docs fromInternalFeature, toInternalFeature

-}

import Proto.Google.Protobuf.Compiler.Internals_
import Protobuf.Decode
import Protobuf.Encode


{-| The field numbers for the fields of `File`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersFile : { name : Int, insertionPoint : Int, content : Int, generatedCodeInfo : Int }
fieldNumbersFile =
    Proto.Google.Protobuf.Compiler.Internals_.fieldNumbersProto__Google__Protobuf__Compiler__CodeGeneratorResponse__File


{-| Default for File. Should only be used for 'required' decoders as an initial value.

-}
defaultFile : File
defaultFile =
    Proto.Google.Protobuf.Compiler.Internals_.defaultProto__Google__Protobuf__Compiler__CodeGeneratorResponse__File


{-| Declares how to decode a `File` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeFile : Protobuf.Decode.Decoder File
decodeFile =
    Proto.Google.Protobuf.Compiler.Internals_.decodeProto__Google__Protobuf__Compiler__CodeGeneratorResponse__File


{-| Declares how to encode a `File` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeFile : File -> Protobuf.Encode.Encoder
encodeFile =
    Proto.Google.Protobuf.Compiler.Internals_.encodeProto__Google__Protobuf__Compiler__CodeGeneratorResponse__File


{-|  Represents a single generated file.


## Fields

### name

 The file name, relative to the output directory.  The name must not
 contain "." or ".." components and must be relative, not be absolute (so,
 the file cannot lie outside the output directory).  "/" must be used as
 the path separator, not "\".

 If the name is omitted, the content will be appended to the previous
 file.  This allows the generator to break large files into small chunks,
 and allows the generated text to be streamed back to protoc so that large
 files need not reside completely in memory at one time.  Note that as of
 this writing protoc does not optimize for this -- it will read the entire
 CodeGeneratorResponse before writing files to disk.


### insertionPoint

 If non-empty, indicates that the named file should already exist, and the
 content here is to be inserted into that file at a defined insertion
 point.  This feature allows a code generator to extend the output
 produced by another code generator.  The original generator may provide
 insertion points by placing special annotations in the file that look
 like:
   @@protoc_insertion_point(NAME)
 The annotation can have arbitrary text before and after it on the line,
 which allows it to be placed in a comment.  NAME should be replaced with
 an identifier naming the point -- this is what other generators will use
 as the insertion_point.  Code inserted at this point will be placed
 immediately above the line containing the insertion point (thus multiple
 insertions to the same point will come out in the order they were added).
 The double-@ is intended to make it unlikely that the generated code
 could contain things that look like insertion points by accident.

 For example, the C++ code generator places the following line in the
 .pb.h files that it generates:
   // @@protoc_insertion_point(namespace_scope)
 This line appears within the scope of the file's package namespace, but
 outside of any particular class.  Another plugin can then specify the
 insertion_point "namespace_scope" to generate additional classes or
 other declarations that should be placed in this scope.

 Note that if the line containing the insertion point begins with
 whitespace, the same whitespace will be added to every line of the
 inserted text.  This is useful for languages like Python, where
 indentation matters.  In these languages, the insertion point comment
 should be indented the same amount as any inserted code will need to be
 in order to work correctly in that context.

 The code generator that generates the initial file and the one which
 inserts into it must both run as part of a single invocation of protoc.
 Code generators are executed in the order in which they appear on the
 command line.

 If |insertion_point| is present, |name| must also be present.


### content

 The file contents.


### generatedCodeInfo

 Information describing the file content being inserted. If an insertion
 point is used, this information will be appropriately offset and inserted
 into the code generation metadata for the generated files.


-}
type alias File =
    Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__File


{-| The field numbers for the fields of `Feature`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersFeature : Feature -> Int
fieldNumbersFeature n_ =
    case n_ of
        FEATURENONE ->
            0

        FEATUREPROTO3OPTIONAL ->
            1


{-| Default for Feature. Should only be used for 'required' decoders as an initial value.

-}
defaultFeature : Feature
defaultFeature =
    FEATURENONE


{-| Convert a `Feature` into its internal representation `Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__Feature`.

Using two different types is necessary to avoid recursive module references while having readable constructor names.

-}
toInternalFeature :
    Feature
    -> Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__Feature
toInternalFeature data_ =
    case data_ of
        FEATURENONE ->
            Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__FEATURENONE

        FEATUREPROTO3OPTIONAL ->
            Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__FEATUREPROTO3OPTIONAL


{-| Convert the internal type `Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__Feature` into a `Feature`.

Using two different types is necessary to avoid recursive module references while having readable constructor names.

-}
fromInternalFeature :
    Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__Feature
    -> Feature
fromInternalFeature data_ =
    case data_ of
        Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__FEATURENONE ->
            FEATURENONE

        Proto.Google.Protobuf.Compiler.Internals_.Proto__Google__Protobuf__Compiler__CodeGeneratorResponse__FEATUREPROTO3OPTIONAL ->
            FEATUREPROTO3OPTIONAL


{-| Declares how to encode a `Feature` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeFeature : Feature -> Protobuf.Encode.Encoder
encodeFeature =
    toInternalFeature
        >> Proto.Google.Protobuf.Compiler.Internals_.encodeProto__Google__Protobuf__Compiler__CodeGeneratorResponse__Feature


{-| Declares how to decode a `Feature` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeFeature : Protobuf.Decode.Decoder Feature
decodeFeature =
    Protobuf.Decode.map
        fromInternalFeature
        Proto.Google.Protobuf.Compiler.Internals_.decodeProto__Google__Protobuf__Compiler__CodeGeneratorResponse__Feature


{-|  Sync with code_generator.h.


-}
type Feature
    = FEATURENONE
    | FEATUREPROTO3OPTIONAL

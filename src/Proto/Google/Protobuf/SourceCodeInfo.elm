{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.SourceCodeInfo exposing (Location, decodeLocation, defaultLocation, encodeLocation, fieldNumbersLocation)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 3.4.4
- `protoc` 3.19.4
- the following specification files: `google/protobuf/descriptor.proto`

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version 1.2.0 or higher.

@docs Location, decodeLocation, defaultLocation, encodeLocation, fieldNumbersLocation

-}

import Proto.Google.Protobuf.Internals_
import Protobuf.Decode
import Protobuf.Encode


{-| The field numbers for the fields of `Location`. This is mostly useful for internals, like documentation generation.

-}
fieldNumbersLocation :
    { path : Int, span : Int, leadingComments : Int, trailingComments : Int, leadingDetachedComments : Int }
fieldNumbersLocation =
    Proto.Google.Protobuf.Internals_.fieldNumbersProto__Google__Protobuf__SourceCodeInfo__Location


{-| Default for Location. Should only be used for 'required' decoders as an initial value.

-}
defaultLocation : Location
defaultLocation =
    Proto.Google.Protobuf.Internals_.defaultProto__Google__Protobuf__SourceCodeInfo__Location


{-| Declares how to decode a `Location` from Bytes. To actually perform the conversion from Bytes, you need to use Protobuf.Decode.decode from eriktim/elm-protocol-buffers.

-}
decodeLocation : Protobuf.Decode.Decoder Location
decodeLocation =
    Proto.Google.Protobuf.Internals_.decodeProto__Google__Protobuf__SourceCodeInfo__Location


{-| Declares how to encode a `Location` to Bytes. To actually perform the conversion to Bytes, you need to use Protobuf.Encode.encode from eriktim/elm-protocol-buffers.

-}
encodeLocation : Location -> Protobuf.Encode.Encoder
encodeLocation =
    Proto.Google.Protobuf.Internals_.encodeProto__Google__Protobuf__SourceCodeInfo__Location


{-| ## Fields

### path

 Identifies which part of the FileDescriptorProto was defined at this
 location.

 Each element is a field number or an index.  They form a path from
 the root FileDescriptorProto to the place where the definition.  For
 example, this path:
   [ 4, 3, 2, 7, 1 ]
 refers to:
   file.message_type(3)  // 4, 3
       .field(7)         // 2, 7
       .name()           // 1
 This is because FileDescriptorProto.message_type has field number 4:
   repeated DescriptorProto message_type = 4;
 and DescriptorProto.field has field number 2:
   repeated FieldDescriptorProto field = 2;
 and FieldDescriptorProto.name has field number 1:
   optional string name = 1;

 Thus, the above path gives the location of a field name.  If we removed
 the last element:
   [ 4, 3, 2, 7 ]
 this path refers to the whole field declaration (from the beginning
 of the label to the terminating semicolon).


### span

 Always has exactly three or four elements: start line, start column,
 end line (optional, otherwise assumed same as start line), end column.
 These are packed into a single field for efficiency.  Note that line
 and column numbers are zero-based -- typically you will want to add
 1 to each before displaying to a user.


### leadingComments

 If this SourceCodeInfo represents a complete declaration, these are any
 comments appearing before and after the declaration which appear to be
 attached to the declaration.

 A series of line comments appearing on consecutive lines, with no other
 tokens appearing on those lines, will be treated as a single comment.

 leading_detached_comments will keep paragraphs of comments that appear
 before (but not connected to) the current element. Each paragraph,
 separated by empty lines, will be one comment element in the repeated
 field.

 Only the comment content is provided; comment markers (e.g. //) are
 stripped out.  For block comments, leading whitespace and an asterisk
 will be stripped from the beginning of each line other than the first.
 Newlines are included in the output.

 Examples:

   optional int32 foo = 1;  // Comment attached to foo.
   // Comment attached to bar.
   optional int32 bar = 2;

   optional string baz = 3;
   // Comment attached to baz.
   // Another line attached to baz.

   // Comment attached to qux.
   //
   // Another line attached to qux.
   optional double qux = 4;

   // Detached comment for corge. This is not leading or trailing comments
   // to qux or corge because there are blank lines separating it from
   // both.

   // Detached comment for corge paragraph 2.

   optional string corge = 5;
   /* Block comment attached
    * to corge.  Leading asterisks
    * will be removed. */
   /* Block comment attached to
    * grault. */
   optional int32 grault = 6;

   // ignored detached comments.


-}
type alias Location =
    Proto.Google.Protobuf.Internals_.Proto__Google__Protobuf__SourceCodeInfo__Location

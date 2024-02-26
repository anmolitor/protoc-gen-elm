## 4.0.0

- Improved grpc dev tools integration to handle both http and grpc errors and concurrent requests
- Added all of the custom requirements for JSON encoding/decoding in the canonical encoding documentation
- Added JSON Decoder code generation
- Removed fromInternal/toInternal functions in generated code
  - Generate a module for each enum/oneof so that this is possible
  - Enum/OneOf constructors can now be imported from the specific module for that enum
  - OneOf Types should be imported from the parent module (the type declared in the specific module has type parameters - otherwise we couldn't have removed the internal conversion functions)
- Moved some generated code into elm-protoc-utils to generate less duplicated code
- Fixed elm-ls warning on generated code for structs without oneof fields
- Changed generated Dict type for `map<string, SomeMsg>` to be `Dict String SomeMsg` instead of `Dict String (Maybe SomeMsg)`   

## 3.4.4

- Fixed JSON encoder code that did not compile for map<int64, string>, since Protobuf.Types.Int64 is not comparable. Uses the (Int, Int) representation now instead.
- Made tests generate JSON code so errors in generated code behind options are caught sooner

## 3.4.3

- Fixed code that did not compile for map<int64, string>, since Protobuf.Types.Int64 is not comparable. Uses the (Int, Int) representation now instead.

## 3.4.0

- Added support for JSON encoder generation
- Added support for grpc dev tools helper code generation
- Remove the NO_GRPC environment variable in favor of elm_opt=grpc=false

## 3.1.1

- Fixed wrong casing of field headlines for documentation. Now the headlines should match the Elm field names and not the proto field names.

## 3.1.0

- Introduced generation of `fieldNumbers...` functions. This is primarily useful for parsing index- and fieldnumber-based paths into protobuf messages.
- Added a generated doc comment `@docs Msg, encodeMsg, ...` to the file comment for public files. This should make it possible to publish generated code as an elm package.
- Added parsing of protoc's `SourceCodeInfo` based on field numbers to add comments to declarations based on the comments in the `.proto` file.
- Fixed an issue with not compiling source code for a specific recursive message case.

## 3.0.0

- Completely revamped the generated module structure. 
  - Previously, the generated files were based on the `.proto` file names, i.e. `Name.proto` would always generate a `Name.elm` file.
  - This was problematic because it led to name collisions - Protoc uses packages and messages as namespaces and not files.
  - Now, one file is generated for each package, with a nested module for declarations nested in a `message` and `oneof` statements
  - The new reliance on the protobuf module system means that we can no longer ignore weirdly named messages/packages:
    - packages need to be lower case
    - messages need to be upper case

- Added gRPC support.
  - Run `elm install anmolitor/elm-grpc` to install the necessary dependencies if you want to use it.
  - Pass `NO_GRPC=true` as an environment variable if you have some other solution and don't want gRPC code to be generated. 

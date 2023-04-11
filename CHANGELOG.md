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

- Added gRPC support.
  - Run `elm install andreasewering/elm-grpc` to install the necessary dependencies if you want to use it.
  - Pass `NO_GRPC=true` as an environment variable if you have some other solution and don't want gRPC code to be generated. 
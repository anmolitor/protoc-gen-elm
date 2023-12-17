# Elm Plugin for Protocol Buffers [![build status](https://github.com/andreasewering/protoc-gen-elm/actions/workflows/build_and_test.yml/badge.svg)](https://github.com/andreasewering/protoc-gen-elm/actions)

This [`protoc`](https://developers.google.com/protocol-buffers/) plug-in generates [Elm](https://elm-lang.org/) modules from `.proto` specification files. The generated modules make use of the [elm-protocol-buffers](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/latest/) library to handle the (de)serialization. They can be used to transmit bytes over HTTP(S) or via web-sockets. 

Remote Procedure Call (RPC) generation is supported but experimental. If you encounter issues, please open a issue. In the meantime, you can disable gRPC generation with the option `--elm_opt=grpc=false`. If used, you need to add a dependency on the [elm-grpc](https://package.elm-lang.org/packages/andreasewering/elm-grpc/latest/) library.

**Take a look [here](https://developers.google.com/protocol-buffers/) for a general introduction on Protocol Buffers.**

## Installation

This package is a plug-in for `protoc`, make sure you have [installed](https://developers.google.com/protocol-buffers/docs/downloads) it and `protoc` is available on your path. After installing `protoc-gen-elm` globally from NPM, `protoc` will automatically find the binary when you add the `--elm_out` flag to your command.

```
npm install --global protoc-gen-elm
```

Alternatively, you can add protoc as a dev-dependency to your project. This should be the preferred way if you want to build your project in CI. If you wrap the call to `protoc` in some npm script, it should still work as expected.

```
npm install --save-dev protoc-gen-elm
```

**You can now turn any `.proto` file into an Elm module**. A similar approach can be used to generate code for C++, Dart, Go, Java, Python, Ruby, C#, Objective C, JavaScript, PHP or [another language](https://github.com/protocolbuffers/protobuf/blob/master/docs/third_party.md) to build a compliant back-end server!

```
protoc --elm_out=. api.proto
```

## Overview

The following table gives an overview of how `.proto` types correspond to Elm types and what their default values are.

| `.proto` type | Elm type                            | Default value\*\*                                                        |
| ------------- | ----------------------------------- | ------------------------------------------------------------------------ |
| `package`     | The name of the module              | `Proto` |
| `double`      | `Float`                             | `0`                                                                      |
| `float`       | `Float`                             | `0`                                                                      |
| `int32`       | `Int`                               | `0`                                                                      |
| `int64`       | `Int64`\*                           | `0`                                                                      |
| `uint32`      | `Int`                               | `0`                                                                      |
| `uint64`      | `Int64`\*                           | `0`                                                                      |
| `sint32`      | `Int`                               | `0`                                                                      |
| `sint64`      | `Int64`\*                           | `0`                                                                      |
| `fixed32`     | `Int`                               | `0`                                                                      |
| `fixed64`     | `Int64`\*                           | `0`                                                                      |
| `bool`        | `Bool`                              | `False`                                                                  |
| `string`      | `String`                            | `""`                                                                     |
| `bytes`       | `Bytes.Bytes`                       | Empty bytes sequence                                                     |
| `required a`  | `a`                                 | No default                                                               |
| `optional a`  | `a`                                 | Default of `a`                                                           |
| `repeated a`  | `List a`                            | `[]`                                                                     |
| `enum`        | Custom type                         | First element                                                            |
| `message`     | Record                              | All fields take their default value                                      |
| `a`           | `Maybe` Record                      | `Nothing`                                                                |
| `oneof`       | Custom type with an associated data | `Nothing`                                                                |
| `map<k, v>`   | `Dict.Dict k v`                     | `Dict.empty`                                                             |
| `service`     | `Grpc.Rpc req res`***                              | No default                                                                         |
| `reserved`    | N/A                                 |                                                                          |
| `extensions`  | N/A                                 |                                                                          |

\*) 64-bit integers are defined in [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers) in `Protobuf.Types.Int64`.

\*\*) Some default values can be overridden in `proto2` specifications. This is currently not supported.

\*\*\*) Rpc is implemented via the [`elm-grpc`](https://package.elm-lang.org/packages/andreasewering/elm-grpc/latest/) library.

## Json Encoding

Protocol Buffers specify a [canonical json encoding](https://protobuf.dev/programming-guides/proto3/#json).
When you pass the additional option `--elm_opt=json` to the protoc invocation, you will get JSON Encoders and Decoders generated.
If you want to be more granular `--elm_opt=json=encode` or `--elm_opt=json=decode` will only generate one or the other.

**NOTE**: Json Decoding is not yet implemented. If you are interested, feel free to open a PR. Int64 decoding will be tricky and the main work
will be done in this repository: https://github.com/andreasewering/elm-protoc-utils

## Explanations about the generated code

In general, the generated code tries to be close to what the code looks like in other languages while still being ideomatic Elm code.
Elm's concept of "Only one solution to solve" a problem has several consequences here.

### General

- Protobufs `message`s are product types, `enum`s and `oneof`s are union types. 
- Each `message` and `enum` generates `encode[name]` and `decode[name]` functions, which integrate seamlessly with elm-protocol-buffers
- Each `message` and `enum` generates a `default[name]` function, which sets the defaults as seen in the table above
- `enum`s and `oneof`s generate `fromInternal[name]` and `toInternal[name]` functions. These are needed for use inside of other messages (you will see why in the section "Module Nesting")
- For serialization, using `toInternal[name]` can be skipped by using lowercase enum constructor functions. I.e. for an ADT constructor `MyCons`
  of a data type `MyDataType` a function `myCons` is generated, which is equivalent to `MyCons >> toInternalMyDataType`.

### Module Nesting

Protobufs have their own module system, which is different from Elms. Here are some interesting points about it:

- Modules are defined by packages and not by files
- Protoc disallows circular imports of packages
- Declarations can be nested inside of messages, which pretty much makes an inline module
- There are no visibility modifiers. You can access all declarations inside of a message from outside and the other way around

Elm disallows circular imports as well, luckily protoc helps us out here on the package front. However, Elm does not have supported for nested modules, which is a problem.

For an illustration why, see the following example

```
// file: test.proto
package test;

enum Outer { A = 0 }

message Scope {
  enum Inner { B = 0 }
  message InnerMsg {
    Outer outer = 1;
    Inner inner = 2;
  }

  InnerMsg msg = 1;
}
```

Obviously there are two modules here: `test` and `test.Scope`.
We generate two Elm files:

```
// file: Test.elm
import Test.Scope

type Outer = A

type alias Scope = {
  inner : Test.Scope.InnerMsg
}
```

```
// file: Test/Scope.elm
import Test

type Inner = B

type alias InnerMsg = {
  outer : Test.Outer,
  inner : Inner
}
```

This might look fine on first glance, but if we try to compile this we get a compile error. Why? Because the two modules are mutually recursive.

The only solution to this problem is making a large module for each package, so this is exactly what we do. But if we want to keep the nice, short names, we will get name conflicts. Protoc has no problems with identical names as long as they are in different scopes.

Therefore, we hide the large modules as `.Internals_.elm` modules, which you should not need to use and re-export from other modules with nicer names from there. The only downside: We lose the ability to pattern match on types, since we can not alias constructors.
So that's the complete explanation why the `fromInternal` and `toInternal` functions exist.

### Recursive Data Types

For ease of construction, `protoc-gen-elm` prefers to generate type aliases instead of nominal types. Type aliases have one downside though: they cannot be recursive. Otherwise, the Elm compiler would have to do infinite work to expand the type.
So if you have a recursive type like this:

```
message Rec {
  repeated Rec rec = 1;
}
```

we generate

```
type alias Rec = { rec : List Rec_ }

type Rec_ = Rec_ Rec
```

and corresponding `wrapRec` and `unwrapRec` functions.

### gRPC

If your .proto file includes a `service` declaration, an Elm module will be generated based on `package` and the services name.

This file:
```
package some_package

service SomeService {}
```
will generate a `Proto/SomePackage/SomeService.elm` module.

The code that needs to be generated inside is actually rather small.
A gRPC call just needs
- the package name
- the method name
- the service name
- references to the en/decoder functions

The rest of the work is done by the `elm-grpc` package.
It provides functions to convert the generated `Grpc.Rpc` instances into `Cmd`s and `Task`s, as well as setting the usual Http Request fields (headers, timeout, tracker etc.)


## Live Example

To run a minimal live example in your browser, follow the instructions in `/example/grpc/README.md`.
For a more advanced/realistic example, look at `/example/tonic_vite/README.md`.

## Well-known types

If you want to use protobufs [well-known-types](https://developers.google.com/protocol-buffers/docs/reference/google.protobuf), 
you need to install the pre-built package [elm-protoc-types](https://package.elm-lang.org/packages/andreasewering/elm-protoc-types/1.1.0/) or include the paths to the proto files in the compilation.

Example: If this is your proto file `test.proto` which uses the well-known type `Timestamp`,
```
import "google/protobuf/timestamp.proto";

message TestMessage {
  google.protobuf.Timestamp timestamp = 1;
}
```

the `protoc` invocation will need to include the path to the well-known types .proto file.
```
protoc --elm_out=. test.proto /usr/local/include/google/protobuf/timestamp.proto
```

## Limitations

- All limitations of [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/latest#known-limitations) apply;
- This is still a **beta** release. Please report any issues you have generating your Elm modules;

## Development

Note: Currently, this project won't run on Windows (WSL works) because of shell scripts/executable js files.

Execute `npm install`, `npm run build` and `npm test` and you should be good to go.
You will need `protoc` installed and on your PATH.

- The plugin logic is written in Elm itself. To be executable via node, there is a index.js wrapper. It converts the incoming bytes to base64, because there currently is no way to directly send the type `Bytes` through a port.
- Main.elm essentially wires up the binding to JS: A request is received through a port, gets decoded, processed and then sent through another port.
- For decoding the protoc request, it uses "itself", meaning that upgrading protoc versions should be done by running the plugin against the new `include` files from protoc to generate the new encoders/decoders (use the `upgrade.sh` script).
- A `Mapper` converts the request into a convenient internal structure
- A `Generator` then uses this internal structure to build an Elm AST
  which is then pretty-printed to a file.

Run `build.sh` to build the elm code into `index.min.js` (which is imported by the entrypoint `index.js`).

To analyse the protoc requests, there are `debug.js`, `DebugMain` and `build_debug.sh` files. Run `build_debug.sh`, then use `debug.js` in place of `index.js` when running `protoc`. This should dump the deserialized request into `debug.log`. You can then put this into the Elm repl for example or use it as input for tests.

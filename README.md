# Elm Plugin for Protocol Buffers [![Build Status](https://travis-ci.org/eriktim/protoc-gen-elm.svg?branch=master)](https://travis-ci.org/eriktim/protoc-gen-elm)

This [`protoc`](https://developers.google.com/protocol-buffers/) plug-in generates [Elm](https://elm-lang.org/) modules from `.proto` specification files. The generated modules make use of the [elm-protocol-buffers](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/latest/) library to handle the (de)serialization. They can be used to transmit bytes over HTTP(S) or via web-sockets. However, this plug-in itself does **not** implement or generate any Remote Procedure Call (RPC) logic.

**Take a look [here](https://developers.google.com/protocol-buffers/) for a general introduction on Protocol Buffers.**

## Installation

This package is a plug-in for `protoc`, make sure you have [installed](https://developers.google.com/protocol-buffers/docs/downloads) it and `protoc` is available on your path. After installing `protoc-gen-elm` globally from NPM, `protoc` will automatically find the binary when you add the `--elm_out` flag to your command.

```
npm install --global protoc-gen-elm
```
**You can now turn any `.proto` file into an Elm module**. A similar approach can be used to generate code for C++, Dart, Go, Java, Python, Ruby, C#, Objective C, JavaScript, PHP or [another language](https://github.com/protocolbuffers/protobuf/blob/master/docs/third_party.md) to build a compliant back-end server!

```
protoc --elm_out=. api.proto
```

## Overview

The following table gives an overview of how `.proto` types correspond to Elm types and what their default values are.

| `.proto` type | Elm type                            | Default value**                                              |
| ------------- | ----------------------------------- | ------------------------------------------------------------ |
| `package`     | The name of the module              | The `.proto` filename, e.g. `proto/api.proto` becomes `module Proto.Api` |
| `double`      | `Float`                             | `0`                                                          |
| `float`       | `Float`                             | `0`                                                          |
| `int32`       | `Int`                               | `0`                                                          |
| `int64`       | `Int`\*                             | `0`                                                          |
| `uint32`      | `Int`                               | `0`                                                          |
| `uint64`      | `Int`\*                             | `0`                                                          |
| `sint32`      | `Int`                               | `0`                                                          |
| `sint64`      | `Int`\*                             | `0`                                                          |
| `fixed32`     | `Int`                               | `0`                                                          |
| `fixed64`     | `Int`\*                             | `0`                                                          |
| `bool`        | `Bool`                              | `False`                                                      |
| `string`      | `String`                            | `""`                                                         |
| `bytes`       | `Bytes.Bytes`                       | Empty bytes sequence                                         |
| `required a`  | `a`                                 | No default                                                   |
| `optional a`  | `a`                                 | Default of `a`                                               |
| `repeated a`  | `List a`                            | `[]`                                                         |
| `enum`        | Custom type                         | First element                                                |
| `message`     | Record                              | All fields take their default value                          |
| `a`           | `Maybe` Record                      | `Nothing`                                                    |
| `oneof`       | Custom type with an associated data | `Nothing`                                                    |
| `map<k, v>`   | `Dict.Dict k v`                     | `Dict.empty`                                                 |
| `service`     | N/A                                 |                                                              |
| `reserved`     | N/A                                 |                                                              |
| `extensions`     | N/A                                 |                                                              |

*) 64-bit integers are processed as 32-bit integers, see [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/latest#known-limitations)

**) Some default values can be overridden in `proto2` specifications

## Live Example

To run a live example in your browser, first start the example back-end server:

```bash
npm install
node example/server.js
```

The server implements a (basic) back-end for `example/greeter.proto`. You can now generate an Elm module from the same specification. The example code will use the generated `example/src/Greeter.elm` to communicate with the server.  Start the reactor and give it a try on http://localhost:8000/src/Main.elm:

```bash
cd example
protoc --elm_out=src greeter.proto
elm reactor
```

## Limitations
* All limitations of  [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/latest#known-limitations) apply;
* This is still a **beta** release. Please report any issues you have generating your Elm modules;
* Currently, only self-referencing recursive fields are being detected, e.g. `message Foo { repeated Foo foo = 1; }`. Support for all cyclic dependent types is still a work-in-progress. 

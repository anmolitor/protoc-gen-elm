{- !!! DO NOT EDIT THIS FILE MANUALLY !!! -}

module Proto.Google.Protobuf.Value exposing (Kind)

{-| 
This file was automatically generated by
- [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) 4.0.0
- `protoc` 3.19.4
- the following specification files: ``

To run it, add a dependency via `elm install` on [`elm-protocol-buffers`](https://package.elm-lang.org/packages/eriktim/elm-protocol-buffers/1.2.0) version 1.2.0 or higher.

@docs Kind

-}

import Proto.Google.Protobuf.Internals_


{-|  The kind of value.


## Options

### NullValue

 Represents a null value.


### NumberValue

 Represents a double value.


### StringValue

 Represents a string value.


### BoolValue

 Represents a boolean value.


### StructValue

 Represents a structured value.


### ListValue

 Represents a repeated `Value`.


-}
type alias Kind =
    Proto.Google.Protobuf.Internals_.Proto__Google__Protobuf__Value__Kind__Kind
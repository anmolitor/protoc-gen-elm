module Meta.JsonDecode exposing (..)

import Dict exposing (Dict)
import Elm.CodeGen as C exposing (Expression, ModuleName, TypeAnnotation)
import Json.Decode
import List.Extra
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Meta.Basics
import Model exposing (IntFlavor(..), Primitive(..))


decoder : TypeAnnotation -> TypeAnnotation
decoder encodedType =
    C.fqTyped moduleName "Decoder" [ encodedType ]


string : Expression
string =
    C.fqFun moduleName "string"


moduleName : ModuleName
moduleName =
    [ "Json", "Decode" ]


null : Expression
null =
    C.fqVal moduleName "null"


succeed : Expression
succeed =
    C.fqVal moduleName "succeed"


list : Expression
list =
    C.fqFun moduleName "list"


stringKeyDict : Expression -> Expression
stringKeyDict valueDecoder =
    C.apply [ C.fqFun moduleName "dict", valueDecoder ]


dict : Expression -> Expression -> Expression
dict keyDecoder valueDecoder =
    C.apply [ C.fqFun [ "Protobuf", "Utils", "Dict" ] "dictDecoder", keyDecoder, valueDecoder ]


field : Expression
field =
    C.fqFun moduleName "field"


maybe : Expression
maybe =
    C.fqFun moduleName "maybe"


map : Expression
map =
    C.fqFun moduleName "map"


lazy : Expression
lazy =
    C.fqFun moduleName "lazy"


oneOf : List Expression -> Expression
oneOf decoders =
    C.apply [ C.fqFun moduleName "oneOf", C.list decoders ]


{-| Generates a decoder from a fixed size list of decoders and a function combining the results.
For example,
-> mapN (C.val "myfunc") [int, string, bool]
will generate
-> map3 myfunc int string bool
-}
mapN : Expression -> NonEmpty Expression -> Expression
mapN funcWithNArgs decoders =
    let
        mapUpTo8 f decoderGroup =
            let
                n =
                    List.length decoderGroup

                mapFuncName =
                    if n == 1 then
                        "map"

                    else
                        "map" ++ String.fromInt n
            in
            C.apply ([ C.fqVal moduleName mapFuncName, f ] ++ decoderGroup)
    in
    case List.Extra.greedyGroupsOf 8 (NonEmpty.toList decoders) of
        [] ->
            C.val "This cannot happen because the list is non-empty!"

        firstDecoderGroup :: otherDecoderGroups ->
            C.pipe (mapUpTo8 funcWithNArgs firstDecoderGroup)
                (List.map
                    (\decoderGroup ->
                        C.apply
                            [ andThen, C.lambda [ C.varPattern "f" ] (mapUpTo8 (C.val "f") decoderGroup) ]
                    )
                    otherDecoderGroups
                )


andThen : Expression
andThen =
    C.fqFun moduleName "andThen"


forPrimitive : Primitive -> Expression
forPrimitive prim =
    case prim of
        Prim_Int32 Int_ ->
            int32

        Prim_Int32 SInt ->
            int32

        Prim_Int32 UInt ->
            uint32

        Prim_Int32 Fixed ->
            uint32

        Prim_Int32 SFixed ->
            int32

        Prim_Int64 Int_ ->
            int64

        Prim_Int64 SInt ->
            int64

        Prim_Int64 UInt ->
            uint64

        Prim_Int64 Fixed ->
            uint64

        Prim_Int64 SFixed ->
            int64

        Prim_Bool ->
            bool

        Prim_Float ->
            float

        Prim_String ->
            string

        Prim_Bytes ->
            bytes

        Prim_Double ->
            float


primitiveFromMapKey : Primitive -> Expression
primitiveFromMapKey prim =
    case prim of
        Prim_Int32 Int_ ->
            int32_fromMapKey

        Prim_Int32 SInt ->
            int32_fromMapKey

        Prim_Int32 UInt ->
            int32_fromMapKey

        Prim_Int32 Fixed ->
            int32_fromMapKey

        Prim_Int32 SFixed ->
            int32_fromMapKey

        Prim_Int64 Int_ ->
            int64_fromMapKey

        Prim_Int64 SInt ->
            int64_fromMapKey

        Prim_Int64 UInt ->
            uint64_fromMapKey

        Prim_Int64 Fixed ->
            uint64_fromMapKey

        Prim_Int64 SFixed ->
            int64_fromMapKey

        Prim_String ->
            C.fun "identity"

        Prim_Bool ->
            C.string "ERROR: Bool is not a valid key for a protobuf map!"

        Prim_Float ->
            C.string "ERROR: Float is not a valid key for a protobuf map!"

        Prim_Bytes ->
            C.string "ERROR: Bytes is not a valid key for a protobuf map!"

        Prim_Double ->
            C.string "ERROR: Double is not a valid key for a protobuf map!"


int32 : Expression
int32 =
    C.fqFun [ "Protobuf", "Utils", "Int32" ] "int32JsonDecoder"


uint32 : Expression
uint32 =
    C.fqFun [ "Protobuf", "Utils", "Int32" ] "uint32JsonDecoder"


bool : Expression
bool =
    C.fqFun moduleName "bool"


float : Expression
float =
    C.fqFun [ "Protobuf", "Utils", "Float" ] "stringOrFloatJsonDecoder"


bytes : Expression
bytes =
    C.fqFun [ "Protobuf", "Utils", "Bytes" ] "jsonDecoder"


int64 : Expression
int64 =
    C.fqFun [ "Protobuf", "Utils", "Int64" ] "int64JsonDecoder"


int32_fromMapKey : Expression
int32_fromMapKey =
    C.applyBinOp (C.fqFun [ "String" ] "toInt") C.composer (C.apply [ C.fqFun [ "Result" ] "fromMaybe", C.string "Expected int32" ])


int64_fromMapKey : Expression
int64_fromMapKey =
    C.chain int64_fromString
        [ C.apply [ Meta.Basics.mapMaybe, C.fqFun [ "Protobuf", "Types", "Int64" ] "toInts" ]
        , C.apply [ C.fqFun [ "Result" ] "fromMaybe", C.string "Expected int64" ]
        ]


uint64_fromMapKey : Expression
uint64_fromMapKey =
    C.chain uint64_fromString
        [ C.apply [ Meta.Basics.mapMaybe, C.fqFun [ "Protobuf", "Types", "Int64" ] "toInts" ]
        , C.apply [ C.fqFun [ "Result" ] "fromMaybe", C.string "Expected uint64" ]
        ]


int64_fromString : Expression
int64_fromString =
    C.fqFun [ "Protobuf", "Utils", "Int64" ] "fromSignedString"


uint64_fromString : Expression
uint64_fromString =
    C.fqFun [ "Protobuf", "Utils", "Int64" ] "fromUnsignedString"


uint64 : Expression
uint64 =
    C.fqFun [ "Protobuf", "Utils", "Int64" ] "uint64JsonDecoder"

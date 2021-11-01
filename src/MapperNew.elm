module MapperNew exposing (enum, message)

import Internal.Google.Protobuf exposing (DescriptorProto, DescriptorProtoNestedType(..), EnumDescriptorProto, FieldDescriptorProto, FieldDescriptorProtoLabel(..), FieldDescriptorProtoType(..), FileDescriptorProto)
import List.Extra
import Mapping.Name as Name
import Mapping.Struct as Struct exposing (Struct)
import Mapping.Syntax exposing (Syntax(..))
import Maybe.Extra
import Model exposing (Cardinality(..), Enum, Field(..), FieldName, FieldType(..), Map, Primitive(..))



-- MODEL
{--
    Transform an `EnumDescriptorProto` into our internal `Enum` representation.
    Prefix is used for enums nested inside of message declarations,
    since they could potentially overlap otherwise.
-}


enum : Syntax -> Maybe String -> EnumDescriptorProto -> Enum
enum syntax prefix descriptor =
    let
        name =
            Maybe.map (\pre -> pre ++ "_" ++ descriptor.name) prefix
                |> Maybe.withDefault descriptor.name
                |> Name.type_

        fields =
            descriptor.value
                |> List.map (\value -> ( value.number, Name.type_ value.name ))
                |> List.Extra.uncons
                |> Maybe.withDefault ( ( 0, name ), [] )
    in
    { dataType = Name.type_ name
    , isTopLevel = Maybe.Extra.isNothing prefix
    , withUnrecognized = syntax == Proto3
    , fields = fields
    }



{--
    Transform a `DescriptorProto` into our internal `Struct` representation.
    Prefix is used for nested messages inside of message declarations,
    since they could potentially overlap otherwise.
-}


message : Syntax -> Maybe String -> DescriptorProto -> Struct
message syntax prefix descriptor =
    let
        name =
            Maybe.map (\pre -> pre ++ "_" ++ descriptor.name) prefix
                |> Maybe.withDefault descriptor.name

        fieldsMeta =
            List.map (messageFieldMeta name) descriptor.field

        nested =
            case descriptor.nestedType of
                DescriptorProtoNestedType nestedType ->
                    Struct.concatMap (message syntax <| Just descriptor.name) nestedType

        isMap =
            Maybe.withDefault False <| Maybe.map .mapEntry descriptor.options

        struct =
            if isMap then
                mapField name descriptor

            else
                { messages =
                    [ { dataType = name
                      , isTopLevel = Maybe.Extra.isNothing prefix
                      , fields = messageFields oneOfFieldNames nested.maps fieldsMeta descriptor
                      }
                    ]
                , enums = []
                , maps = []
                }

        oneOfFieldNames : List String
        oneOfFieldNames =
            List.map .name descriptor.oneofDecl
    in
    { messages = struct.messages
    , enums = List.map (enum syntax <| Just descriptor.name) descriptor.enumType
    , maps = struct.maps
    }
        |> Struct.append nested



-- FIELD


messageFields : List String -> List Map -> List { field : ( FieldName, Field ), oneOfIndex : Int } -> DescriptorProto -> List ( FieldName, Field )
messageFields oneOfFieldNames maps fieldsMeta parentDescriptor =
    let
        oneOfFields =
            List.indexedMap (oneOfField fieldsMeta parentDescriptor.name) oneOfFieldNames

        maybeMapField field =
            case field of
                Field fieldNumber _ type_ ->
                    case type_ of
                        Embedded d ->
                            List.filter ((==) d << .dataType) maps
                                |> List.head
                                |> Maybe.map (MapField fieldNumber)

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    fieldsMeta
        |> List.filterMap
            (\{ field, oneOfIndex } ->
                if oneOfIndex > 0 then
                    List.Extra.getAt (oneOfIndex - 1) oneOfFields

                else
                    maybeMapField (Tuple.second field)
                        |> Maybe.map (Tuple.pair <| Tuple.first field)
                        |> Maybe.withDefault field
                        |> Just
            )
        |> List.Extra.uniqueBy Tuple.first


messageFieldMeta : String -> FieldDescriptorProto -> { field : ( FieldName, Field ), oneOfIndex : Int }
messageFieldMeta name descriptor =
    let
        type_ =
            fieldType descriptor

        field =
            Field descriptor.number (cardinality descriptor.label) type_
    in
    { field = ( Name.field descriptor.name, field )
    , oneOfIndex = descriptor.oneofIndex
    }


oneOfField : List { field : ( FieldName, Field ), oneOfIndex : Int } -> String -> Int -> String -> ( FieldName, Field )
oneOfField fields prefix index name =
    List.filter ((==) index << (-) 1 << .oneOfIndex) fields
        |> List.map .field
        |> List.filterMap
            (\( fieldName, field ) ->
                case field of
                    Field fieldNumber _ type_ ->
                        Just ( fieldNumber, Name.type_ <| prefix ++ "_" ++ name ++ "_" ++ fieldName, type_ )

                    _ ->
                        Nothing
            )
        |> OneOfField (prefix ++ Name.type_ name)
        |> Tuple.pair (Name.field name)


mapField : String -> DescriptorProto -> Struct
mapField name descriptor =
    let
        field1 =
            List.filter ((==) 1 << .number) descriptor.field
                |> List.head
                |> Maybe.map fieldType

        field2 =
            List.filter ((==) 2 << .number) descriptor.field
                |> List.head
                |> Maybe.map fieldType
    in
    Maybe.withDefault Struct.empty <|
        Maybe.map2
            (\keyType valueType ->
                { messages = []
                , enums = []
                , maps =
                    [ { dataType = name
                      , key = keyType
                      , value = valueType
                      }
                    ]
                }
            )
            field1
            field2


fieldType : FieldDescriptorProto -> FieldType
fieldType descriptor =
    case descriptor.type_ of
        TypeDouble ->
            Primitive Prim_Float "double" <| defaultNumber descriptor

        TypeFloat ->
            Primitive Prim_Float "float" <| defaultNumber descriptor

        TypeInt64 ->
            Primitive Prim_Int "int32" <| defaultNumber descriptor

        TypeUint64 ->
            Primitive Prim_Int "uint32" <| defaultNumber descriptor

        TypeInt32 ->
            Primitive Prim_Int "int32" <| defaultNumber descriptor

        TypeFixed64 ->
            Primitive Prim_Int "fixed32" <| defaultNumber descriptor

        TypeFixed32 ->
            Primitive Prim_Int "fixed32" <| defaultNumber descriptor

        TypeBool ->
            Primitive Prim_Bool "bool" <| defaultBool descriptor

        TypeString ->
            Primitive Prim_String "string" <| defaultString descriptor

        TypeGroup ->
            Embedded descriptor.typeName

        TypeMessage ->
            -- for some reason the type always starts with a "."
            Embedded <| String.dropLeft 1 descriptor.typeName

        TypeBytes ->
            Primitive Prim_Bytes "bytes" <| defaultBytes descriptor

        TypeUint32 ->
            Primitive Prim_Int "uint32" <| defaultNumber descriptor

        TypeEnum ->
            Enumeration (defaultEnum descriptor) descriptor.typeName

        TypeSfixed32 ->
            Primitive Prim_Int "sfixed32" <| defaultNumber descriptor

        TypeSfixed64 ->
            Primitive Prim_Int "sfixed32" <| defaultNumber descriptor

        TypeSint32 ->
            Primitive Prim_Int "sint32" <| defaultNumber descriptor

        TypeSint64 ->
            Primitive Prim_Int "sint32" <| defaultNumber descriptor



-- DEFAULTS


defaultBool : FieldDescriptorProto -> String
defaultBool descriptor =
    if descriptor.defaultValue == "true" then
        "True"

    else
        "False"


defaultBytes : FieldDescriptorProto -> String
defaultBytes descriptor =
    -- TODO c escaped bytes, see https://github.com/golang/protobuf/pull/427/files
    "(Encode.encode <| Encode.string \"" ++ descriptor.defaultValue ++ "\")"


defaultEnum : FieldDescriptorProto -> Maybe String
defaultEnum descriptor =
    if descriptor.defaultValue == "" then
        Nothing

    else
        Just (Name.type_ descriptor.defaultValue)


defaultNumber : FieldDescriptorProto -> String
defaultNumber descriptor =
    if descriptor.defaultValue == "" then
        "0"

    else
        descriptor.defaultValue


defaultString : FieldDescriptorProto -> String
defaultString descriptor =
    "\"" ++ descriptor.defaultValue ++ "\""


cardinality : FieldDescriptorProtoLabel -> Cardinality
cardinality value =
    case value of
        LabelOptional ->
            Optional

        LabelRequired ->
            Required

        LabelRepeated ->
            Repeated

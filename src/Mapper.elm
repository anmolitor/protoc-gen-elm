module Mapper exposing (map)

import Dict exposing (Dict)
import Google.Protobuf exposing (DescriptorProto, DescriptorProtoNestedType(..), EnumDescriptorProto, FieldDescriptorProto, FieldDescriptorProtoLabel(..), FieldDescriptorProtoType(..), FileDescriptorProto)
import List.Extra
import Model exposing (..)
import Set exposing (Set)
import String.Extra


map : List String -> List FileDescriptorProto -> List Model.Package
map files descriptors =
    let
        fileDict =
            descriptors
                |> List.filter ((\name -> List.member name files) << .name)
                |> List.map (\descriptor -> ( descriptor.name, packageName descriptor ))
                |> Dict.fromList
    in
    Dict.values fileDict
        |> List.Extra.unique
        |> List.map
            (\pkg ->
                let
                    packageDescriptors =
                        List.filter ((==) pkg << packageName) descriptors

                    struct =
                        List.foldl
                            (\descriptor acc ->
                                let
                                    syntax =
                                        if descriptor.syntax == "proto3" then
                                            Proto3

                                        else
                                            Proto2
                                in
                                acc
                                    |> append
                                        { messages = []
                                        , enums = List.map (enum syntax Nothing) descriptor.enumType
                                        , maps = []
                                        , imports = List.filter ((/=) pkg) <| dependencies fileDict descriptor.dependency
                                        }
                                    |> append (concatMap (message syntax pkg Nothing) descriptor.messageType)
                            )
                            empty
                            packageDescriptors
                in
                { name = pkg
                , files = List.sort <| List.map .name packageDescriptors
                , messages = struct.messages
                , enums = struct.enums
                , imports = Set.fromList struct.imports
                }
            )



-- MODEL


type Syntax
    = Proto2
    | Proto3


type alias Struct =
    { messages : List Message
    , enums : List Enum
    , maps : List Map
    , imports : List String
    }


empty : Struct
empty =
    { messages = []
    , enums = []
    , maps = []
    , imports = []
    }


append : Struct -> Struct -> Struct
append { messages, enums, maps, imports } struct =
    { messages = struct.messages ++ messages
    , enums = struct.enums ++ enums
    , maps = struct.maps ++ maps
    , imports = struct.imports ++ imports
    }


concatMap : (a -> Struct) -> List a -> Struct
concatMap fn xs =
    List.foldl (append << fn) empty xs



-- ENUM


enum : Syntax -> Maybe String -> EnumDescriptorProto -> Enum
enum syntax prefix descriptor =
    let
        name =
            typeName <| maybe descriptor.name (\pre -> pre ++ "_" ++ descriptor.name) prefix

        fields =
            descriptor.value
                |> List.map (\value -> ( value.number, enumTypeName value.name ))
                |> List.Extra.uncons
                |> Maybe.withDefault ( ( 0, name ), [] )
    in
    { dataType = typeName name
    , isTopLevel = maybe True (always False) prefix
    , withUnrecognized = syntax == Proto3
    , fields = fields
    }



-- MESSAGE


message : Syntax -> String -> Maybe String -> DescriptorProto -> Struct
message syntax pkg prefix descriptor =
    let
        name =
            typeName (maybe descriptor.name (\pre -> pre ++ "_" ++ descriptor.name) prefix)

        fieldsMeta =
            List.map (messageFieldMeta pkg name) descriptor.field

        nested =
            case descriptor.nestedType of
                DescriptorProtoNestedType nestedType ->
                    concatMap (message syntax pkg <| Just descriptor.name) nestedType

        isMap =
            Maybe.withDefault False <| Maybe.map .mapEntry descriptor.options

        struct =
            if isMap then
                mapField pkg name descriptor

            else
                { messages =
                    [ { dataType = name
                      , isTopLevel = maybe True (always False) prefix
                      , fields = messageFields oneOfFieldNames nested.maps fieldsMeta
                      }
                    ]
                , enums = []
                , maps = []
                , imports = []
                }

        oneOfFieldNames =
            List.map .name descriptor.oneofDecl
    in
    { messages = struct.messages
    , enums = List.map (enum syntax <| Just descriptor.name) descriptor.enumType
    , maps = struct.maps
    , imports = List.filterMap .importedPackage fieldsMeta ++ struct.imports
    }
        |> append nested



-- FIELD


messageFields : List String -> List Map -> List { field : ( FieldName, Field ), oneOfIndex : Int, importedPackage : Maybe String } -> List ( FieldName, Field )
messageFields oneOfFieldNames maps fieldsMeta =
    let
        oneOfFields =
            List.indexedMap (oneOfField fieldsMeta) oneOfFieldNames

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


messageFieldMeta : String -> String -> FieldDescriptorProto -> { field : ( FieldName, Field ), oneOfIndex : Int, importedPackage : Maybe String }
messageFieldMeta pkg name descriptor =
    let
        ( type_, importedPackage ) =
            fieldType pkg descriptor

        field =
            Field descriptor.number (cardinality descriptor.label) type_
    in
    { field = ( valueName descriptor.name, field )
    , oneOfIndex = descriptor.oneofIndex
    , importedPackage = importedPackage
    }


mapField : String -> String -> DescriptorProto -> Struct
mapField pkg name descriptor =
    let
        field1 =
            List.filter ((==) 1 << .number) descriptor.field
                |> List.head
                |> Maybe.map (fieldType pkg)

        field2 =
            List.filter ((==) 2 << .number) descriptor.field
                |> List.head
                |> Maybe.map (fieldType pkg)
    in
    Maybe.withDefault empty <|
        Maybe.map2
            (\( keyType, keyImport ) ( valueType, valueImport ) ->
                { messages = []
                , enums = []
                , maps =
                    [ { dataType = name
                      , key = keyType
                      , value = valueType
                      }
                    ]
                , imports = "Dict" :: List.filterMap identity [ keyImport, valueImport ]
                }
            )
            field1
            field2


oneOfField : List { field : ( FieldName, Field ), oneOfIndex : Int, importedPackage : Maybe String } -> Int -> String -> ( FieldName, Field )
oneOfField fields index name =
    List.filter ((==) index << (-) 1 << .oneOfIndex) fields
        |> List.map .field
        |> List.filterMap
            (\( fieldName, field ) ->
                case field of
                    Field fieldNumber _ type_ ->
                        Just ( fieldNumber, typeName <| name ++ "_" ++ fieldName, type_ )

                    _ ->
                        Nothing
            )
        |> OneOfField (typeName name)
        |> Tuple.pair (valueName name)


fieldType : String -> FieldDescriptorProto -> ( FieldType, Maybe String )
fieldType pkg descriptor =
    case descriptor.type_ of
        TypeDouble ->
            ( Primitive "Float" "double" <| defaultNumber descriptor, Nothing )

        TypeFloat ->
            ( Primitive "Float" "float" <| defaultNumber descriptor, Nothing )

        TypeInt64 ->
            ( Primitive "Int" "int32" <| defaultNumber descriptor, Nothing )

        TypeUint64 ->
            ( Primitive "Int" "uint32" <| defaultNumber descriptor, Nothing )

        TypeInt32 ->
            ( Primitive "Int" "int32" <| defaultNumber descriptor, Nothing )

        TypeFixed64 ->
            ( Primitive "Int" "fixed32" <| defaultNumber descriptor, Nothing )

        TypeFixed32 ->
            ( Primitive "Int" "fixed32" <| defaultNumber descriptor, Nothing )

        TypeBool ->
            ( Primitive "Bool" "bool" <| defaultBool descriptor, Nothing )

        TypeString ->
            ( Primitive "String" "string" <| defaultString descriptor, Nothing )

        TypeGroup ->
            complexFieldType Embedded pkg descriptor

        TypeMessage ->
            complexFieldType Embedded pkg descriptor

        TypeBytes ->
            ( Primitive "Bytes.Bytes" "bytes" <| defaultBytes descriptor, Just "Bytes" )

        TypeUint32 ->
            ( Primitive "Int" "uint32" <| defaultNumber descriptor, Nothing )

        TypeEnum ->
            complexFieldType (Enumeration <| defaultEnum descriptor) pkg descriptor

        TypeSfixed32 ->
            ( Primitive "Int" "sfixed32" <| defaultNumber descriptor, Nothing )

        TypeSfixed64 ->
            ( Primitive "Int" "sfixed32" <| defaultNumber descriptor, Nothing )

        TypeSint32 ->
            ( Primitive "Int" "sint32" <| defaultNumber descriptor, Nothing )

        TypeSint64 ->
            ( Primitive "Int" "sint32" <| defaultNumber descriptor, Nothing )


complexFieldType : (DataType -> FieldType) -> String -> FieldDescriptorProto -> ( FieldType, Maybe String )
complexFieldType makeType pkg descriptor =
    let
        fieldPackage_ =
            fieldPackage descriptor

        dataType =
            packageType descriptor
    in
    if fieldPackage_ == "" || pkg == fieldPackage_ then
        ( makeType dataType, Nothing )

    else
        ( makeType <| fieldPackage_ ++ "." ++ dataType, Just fieldPackage_ )



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
        Just (enumTypeName descriptor.defaultValue)


defaultNumber : FieldDescriptorProto -> String
defaultNumber descriptor =
    if descriptor.defaultValue == "" then
        "0"

    else
        descriptor.defaultValue


defaultString : FieldDescriptorProto -> String
defaultString descriptor =
    "\"" ++ descriptor.defaultValue ++ "\""



-- HELPERS


cardinality : FieldDescriptorProtoLabel -> Cardinality
cardinality value =
    case value of
        LabelOptional ->
            Optional

        LabelRequired ->
            Required

        LabelRepeated ->
            Repeated


dependencies : Dict String String -> List String -> List String
dependencies fileDict =
    List.filterMap (\file -> Dict.get file fileDict)


packageName : FileDescriptorProto -> String
packageName descriptor =
    classify <|
        if descriptor.package == "" then
            descriptor.name
                |> String.Extra.leftOfBack ".proto"
                |> String.replace "/" "."

        else
            descriptor.package


classify : String -> String
classify =
    String.join "." << List.map String.Extra.classify << String.split "."


typeName : String -> String
typeName =
    escapeType << String.Extra.classify


enumTypeName : String -> String
enumTypeName =
    typeName << String.toLower


valueName : String -> String
valueName =
    escape << String.Extra.camelize


fieldPackage : FieldDescriptorProto -> String
fieldPackage descriptor =
    String.split "." descriptor.typeName
        |> List.drop 1
        |> List.Extra.takeWhile
            (\part ->
                case String.uncons part of
                    Just ( char, _ ) ->
                        Char.isLower char

                    Nothing ->
                        False
            )
        |> String.join "."
        |> classify


packageType : FieldDescriptorProto -> String
packageType descriptor =
    String.split "." descriptor.typeName
        |> List.drop 1
        |> List.Extra.dropWhile
            (\part ->
                case String.uncons part of
                    Just ( char, _ ) ->
                        Char.isLower char

                    Nothing ->
                        False
            )
        |> String.join "."
        |> typeName


escape : String -> String
escape word =
    if List.member word reservedWords then
        word ++ "_"

    else
        word


reservedWords : List String
reservedWords =
    [ "if"
    , "then"
    , "else"
    , "case"
    , "of"
    , "let"
    , "in"
    , "type"
    , "module"
    , "where"
    , "import"
    , "exposing"
    , "as"
    , "port"
    ]


escapeType : String -> String
escapeType type_ =
    if List.member type_ reservedWords then
        type_ ++ "_"

    else
        type_


reservedTypes : List String
reservedTypes =
    {- https://package.elm-lang.org/packages/elm/core/1.0.2/ -}
    [ "List"
    , "Maybe"
    , "Result"
    , "String"
    , "Char"
    , "Program"
    , "Cmd"
    , "Sub"
    , "Int"
    , "Float"
    , "Order"
    , "Bool"
    , "Never"
    ]


maybe : b -> (a -> b) -> Maybe a -> b
maybe default fn val =
    Maybe.withDefault default <| Maybe.map fn val

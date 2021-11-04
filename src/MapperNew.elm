module MapperNew exposing (definedTypesInFileDescriptor, definedTypesInMessageDescriptor, mapMain, splitNonEmpty)

import Dict exposing (Dict)
import Elm.CodeGen exposing (ModuleName)
import Errors exposing (Error(..), Res)
import Internal.Google.Protobuf exposing (DescriptorProto, DescriptorProtoNestedType(..), EnumDescriptorProto, FieldDescriptorProto, FieldDescriptorProtoLabel(..), FieldDescriptorProtoType(..), FileDescriptorProto)
import List.Extra
import Mapping.Name as Name
import Mapping.Struct as Struct exposing (Struct)
import Mapping.Syntax exposing (Syntax(..), parseSyntax)
import Maybe.Extra
import Model exposing (Cardinality(..), Enum, Field(..), FieldName, FieldType(..), Primitive(..))
import Result.Extra
import String.Extra


type alias DefinedTypes =
    List String


type alias PackageName =
    String


type alias TypeRefs =
    ( -- Own Types
      DefinedTypes
    , -- Types defined in package dependencies
      Dict PackageName ( ModuleName, DefinedTypes )
    )


definedTypesInMessageDescriptor : DescriptorProto -> DefinedTypes
definedTypesInMessageDescriptor descriptor =
    case descriptor.nestedType of
        DescriptorProtoNestedType nested ->
            descriptor.name
                :: (List.map ((++) (descriptor.name ++ ".")) <|
                        List.concatMap definedTypesInMessageDescriptor nested
                            ++ List.map .name descriptor.enumType
                   )


definedTypesInFileDescriptor : FileDescriptorProto -> DefinedTypes
definedTypesInFileDescriptor descriptor =
    List.map .name descriptor.enumType
        ++ List.concatMap definedTypesInMessageDescriptor descriptor.messageType


mapMain : List FileDescriptorProto -> List ( String, Res Struct )
mapMain descriptors =
    let
        dict : Dict String ( PackageName, DefinedTypes )
        dict =
            descriptors
                |> List.map (\descriptor -> ( descriptor.name, ( descriptor.package, definedTypesInFileDescriptor descriptor ) ))
                |> Dict.fromList
    in
    descriptors
        |> List.map
            (\descriptor ->
                let
                    typeRefs =
                        ( definedTypesInFileDescriptor descriptor
                        , dict
                            |> Dict.filter (\name _ -> List.member name descriptor.dependency)
                            |> Dict.toList
                            |> List.map (\( name, ( packageName, types ) ) -> ( packageName, ( Name.module_ name, types ) ))
                            |> Dict.fromList
                        )

                    syntax =
                        parseSyntax descriptor.syntax
                in
                ( descriptor.name
                , Result.map
                    (Struct.append
                        { enums = List.map (enum syntax Nothing) descriptor.enumType
                        , messages = []
                        }
                        << Struct.concat
                    )
                    (Errors.combineMap (message syntax typeRefs Nothing) descriptor.messageType)
                )
            )


lookForTypeRef : String -> TypeRefs -> Res FieldType
lookForTypeRef name ( ownModuleTypes, dependencies ) =
    let
        nameWithoutPrefixDot =
            name |> String.dropLeft 1
    in
    case List.filter (\definedType -> definedType == nameWithoutPrefixDot) ownModuleTypes of
        [ referencedType ] ->
            Ok (Embedded (Name.type_ referencedType) [])

        [] ->
            let
                dependencySearchResult =
                    nameWithoutPrefixDot
                        |> String.split "."
                        |> (\segments -> ( [], segments ) :: splitNonEmpty segments)
                        -- example: [("", "Test.Inner.Msg"), ("Test.Inner", "Msg"), ("Test", "Inner.Msg")]
                        |> List.map (Tuple.mapBoth (String.join ".") (String.join "."))
                        |> List.filterMap
                            (\( packageName, typeName ) ->
                                Dict.get packageName dependencies
                                    |> Maybe.andThen
                                        (\( modName, types ) ->
                                            List.filter ((==) typeName) types
                                                |> List.head
                                                |> Maybe.map (Tuple.pair modName)
                                        )
                            )
            in
            case dependencySearchResult of
                [ ( moduleName, referencedType ) ] ->
                    Ok <| Embedded (Name.type_ referencedType) moduleName

                [] ->
                    Err <| NoTypeReferenceFound name

                _ ->
                    Err <| AmbiguousTypeReference name

        _ ->
            Err <| AmbiguousTypeReference name


{-| Split a list at all possible points that result in two non-empty lists.

    splitNonEmpty [ 1, 2, 3, 4 ] == [ ( [ 1 ], [ 2, 3, 4 ] ), ( [ 1, 2 ], [ 3, 4 ] ), ( [ 1, 2, 3 ], [ 4 ] ) ]

-}
splitNonEmpty : List a -> List ( List a, List a )
splitNonEmpty l =
    case l of
        head :: ((_ :: _) as rest) ->
            ( [ head ], rest ) :: (List.map << Tuple.mapFirst) ((::) head) (splitNonEmpty rest)

        _ ->
            []



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
            Maybe.map (\pre -> pre ++ "." ++ descriptor.name) prefix
                |> Maybe.withDefault descriptor.name
                |> Name.type_

        fields =
            descriptor.value
                |> List.map (\value -> ( value.number, Name.type_ value.name ))
                |> List.Extra.uncons
                |> Maybe.withDefault ( ( 0, name ), [] )
    in
    { dataType = name
    , isTopLevel = Maybe.Extra.isNothing prefix
    , withUnrecognized = syntax == Proto3
    , fields = fields
    }



{--
    Transform a `DescriptorProto` into our internal `Struct` representation.
    Prefix is used for nested messages inside of message declarations,
    since they could potentially overlap otherwise.
-}


message : Syntax -> TypeRefs -> Maybe String -> DescriptorProto -> Res Struct
message syntax typeRefs prefix descriptor =
    let
        name =
            Maybe.map (\pre -> pre ++ "." ++ descriptor.name) prefix
                |> Maybe.withDefault descriptor.name
                |> Name.type_

        messageFieldMeta : FieldDescriptorProto -> Res { field : ( FieldName, Field ), oneOfIndex : Int }
        messageFieldMeta fieldDescriptor =
            fieldType fieldDescriptor typeRefs
                |> Result.map (NormalField fieldDescriptor.number (cardinality fieldDescriptor.label))
                |> Result.map
                    (\field ->
                        { field = ( Name.field fieldDescriptor.name, field )
                        , oneOfIndex = fieldDescriptor.oneofIndex
                        }
                    )

        fieldsMetaResult : Res (List { field : ( FieldName, Field ), oneOfIndex : Int })
        fieldsMetaResult =
            Errors.combineMap messageFieldMeta descriptor.field

        nested : Res Struct
        nested =
            case descriptor.nestedType of
                DescriptorProtoNestedType nestedType ->
                    Errors.combineMap (message syntax typeRefs <| Just descriptor.name) nestedType
                        |> Result.map Struct.concat

        mainStruct : Res Struct
        mainStruct =
            Result.map
                (\fieldsMeta ->
                    { messages =
                        [ { dataType = name
                          , isTopLevel = Maybe.Extra.isNothing prefix
                          , fields = messageFields oneOfFieldNames fieldsMeta descriptor
                          }
                        ]
                    , enums = List.map (enum syntax <| Just descriptor.name) descriptor.enumType
                    }
                )
                fieldsMetaResult

        oneOfFieldNames : List String
        oneOfFieldNames =
            List.map .name descriptor.oneofDecl
    in
    Errors.map2 Struct.append mainStruct nested



-- FIELD


messageFields : List String -> List { field : ( FieldName, Field ), oneOfIndex : Int } -> DescriptorProto -> List ( FieldName, Field )
messageFields oneOfFieldNames fieldsMeta parentDescriptor =
    let
        oneOfFields =
            List.indexedMap (oneOfField fieldsMeta parentDescriptor.name) oneOfFieldNames
    in
    fieldsMeta
        |> List.filterMap
            (\{ field, oneOfIndex } ->
                if oneOfIndex > 0 then
                    List.Extra.getAt (oneOfIndex - 1) oneOfFields

                else
                    Just field
            )
        |> List.Extra.uniqueBy Tuple.first


oneOfField : List { field : ( FieldName, Field ), oneOfIndex : Int } -> String -> Int -> String -> ( FieldName, Field )
oneOfField fields prefix index name =
    List.filter ((==) index << (-) 1 << .oneOfIndex) fields
        |> List.map .field
        |> List.filterMap
            (\( fieldName, field ) ->
                case field of
                    NormalField fieldNumber _ type_ ->
                        Just ( fieldNumber, Name.type_ <| prefix ++ "_" ++ name ++ "_" ++ fieldName, type_ )

                    _ ->
                        Nothing
            )
        |> OneOfField (prefix ++ Name.type_ name)
        |> Tuple.pair (Name.field name)


fieldType : FieldDescriptorProto -> TypeRefs -> Res FieldType
fieldType descriptor typeRefs =
    case descriptor.type_ of
        TypeDouble ->
            Ok <| Primitive Prim_Float "double" <| defaultNumber descriptor

        TypeFloat ->
            Ok <| Primitive Prim_Float "float" <| defaultNumber descriptor

        TypeInt64 ->
            Ok <| Primitive Prim_Int "int32" <| defaultNumber descriptor

        TypeUint64 ->
            Ok <| Primitive Prim_Int "uint32" <| defaultNumber descriptor

        TypeInt32 ->
            Ok <| Primitive Prim_Int "int32" <| defaultNumber descriptor

        TypeFixed64 ->
            Ok <| Primitive Prim_Int "fixed32" <| defaultNumber descriptor

        TypeFixed32 ->
            Ok <| Primitive Prim_Int "fixed32" <| defaultNumber descriptor

        TypeBool ->
            Ok <| Primitive Prim_Bool "bool" <| defaultBool descriptor

        TypeString ->
            Ok <| Primitive Prim_String "string" <| defaultString descriptor

        TypeGroup ->
            -- read about this feature. it is deprecated but this package should probably still support it somehow
            Err <| UnsupportedFeature "Groups (Deprecated)"

        TypeMessage ->
            lookForTypeRef descriptor.typeName typeRefs

        TypeBytes ->
            Ok <| Primitive Prim_Bytes "bytes" <| defaultBytes descriptor

        TypeUint32 ->
            Ok <| Primitive Prim_Int "uint32" <| defaultNumber descriptor

        TypeEnum ->
            -- Difference: Enum has configurable default value!
            --Enumeration (defaultEnum descriptor) lookForTypeRef descriptor.typeName typeRefs
            lookForTypeRef descriptor.typeName typeRefs

        TypeSfixed32 ->
            Ok <| Primitive Prim_Int "sfixed32" <| defaultNumber descriptor

        TypeSfixed64 ->
            Ok <| Primitive Prim_Int "sfixed32" <| defaultNumber descriptor

        TypeSint32 ->
            Ok <| Primitive Prim_Int "sint32" <| defaultNumber descriptor

        TypeSint64 ->
            Ok <| Primitive Prim_Int "sint32" <| defaultNumber descriptor



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

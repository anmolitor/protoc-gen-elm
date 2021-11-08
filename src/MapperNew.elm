module MapperNew exposing (definedTypesInFileDescriptor, definedTypesInMessageDescriptor, mapMain, splitNonEmpty)

import Dict exposing (Dict)
import Elm.CodeGen exposing (ModuleName)
import Errors exposing (Error(..), Res)
import Internal.Google.Protobuf exposing (DescriptorProto, DescriptorProtoNestedType(..), EnumDescriptorProto, FieldDescriptorProto, FieldDescriptorProtoLabel(..), FieldDescriptorProtoType(..), FileDescriptorProto)
import List.Extra
import List.NonEmpty as NonEmpty
import Mapping.Name as Name
import Mapping.Struct as Struct exposing (Struct)
import Mapping.Syntax exposing (Syntax(..), parseSyntax)
import Model exposing (Cardinality(..), Enum, Field(..), FieldName, FieldType(..), Primitive(..))
import Set exposing (Set)


type alias DefinedTypes =
    List DefinedType


type DefinedType
    = -- name, references to further types
      Message String (Set String)
    | Enum String (List String)


type alias PackageName =
    String


type alias TypeRefs =
    ( -- Own PackageName
      PackageName
      -- Own Types
    , DefinedTypes
    , -- Types defined in package dependencies
      Dict PackageName ( ModuleName, DefinedTypes )
    )


nestDefinedType : String -> DefinedType -> DefinedType
nestDefinedType prefix definedType =
    let
        nest name =
            prefix ++ "." ++ name
    in
    case definedType of
        Message name deps ->
            Message (nest name) deps

        Enum name values ->
            Enum (nest name) (List.map nest values)


elmNameDefinedType : DefinedType -> DefinedType
elmNameDefinedType definedType =
    case definedType of
        Message name deps ->
            Message (Name.type_ name) deps

        Enum name values ->
            Enum (Name.type_ name) (List.map Name.type_ values)


definedTypesInEnumDescriptor : EnumDescriptorProto -> DefinedType
definedTypesInEnumDescriptor descriptor =
    Enum descriptor.name (List.map (.name >> (++) (descriptor.name ++ ".")) descriptor.value)


definedTypesInMessageDescriptor : DescriptorProto -> DefinedTypes
definedTypesInMessageDescriptor descriptor =
    case descriptor.nestedType of
        DescriptorProtoNestedType nested ->
            Message descriptor.name (List.map .typeName descriptor.field |> Set.fromList)
                :: (List.map (nestDefinedType descriptor.name) <|
                        List.concatMap definedTypesInMessageDescriptor nested
                            ++ List.map definedTypesInEnumDescriptor descriptor.enumType
                   )


definedTypesInFileDescriptor : FileDescriptorProto -> DefinedTypes
definedTypesInFileDescriptor descriptor =
    List.map definedTypesInEnumDescriptor descriptor.enumType
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
                        ( descriptor.package
                        , definedTypesInFileDescriptor descriptor
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
                        { enums = List.map (enum syntax emptyPrefixer) descriptor.enumType
                        , messages = []
                        }
                        << Struct.concat
                    )
                    (Errors.combineMap (message syntax typeRefs emptyPrefixer) descriptor.messageType)
                )
            )


lookForTypeRef : String -> TypeRefs -> Res ( DefinedType, ModuleName )
lookForTypeRef name ( ownPackageName, ownModuleTypes, dependencies ) =
    let
        nameWithoutPrefixDot =
            name |> String.dropLeft 1

        nameWithoutOwnPackage =
            if String.isEmpty ownPackageName then
                nameWithoutPrefixDot

            else
                String.replace (ownPackageName ++ ".") "" nameWithoutPrefixDot

        matchesString str definedType =
            case definedType of
                Message messageName _ ->
                    str == messageName

                Enum enumName _ ->
                    str == enumName
    in
    case List.filter (matchesString nameWithoutOwnPackage) ownModuleTypes of
        [ referencedType ] ->
            Ok ( elmNameDefinedType referencedType, [] )

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
                                            List.filter (matchesString typeName) types
                                                |> List.head
                                                |> Maybe.map (Tuple.pair modName)
                                        )
                            )
            in
            case dependencySearchResult of
                [ ( moduleName, referencedType ) ] ->
                    Ok ( elmNameDefinedType referencedType, moduleName )

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


type alias Prefixer =
    String -> String


isEmptyPrefixer : Prefixer -> Bool
isEmptyPrefixer p =
    p "" == ""


emptyPrefixer : Prefixer
emptyPrefixer =
    identity


addPrefix : String -> Prefixer -> Prefixer
addPrefix prefix prefixer str =
    [ prefixer "", prefix, str ]
        |> List.filter (not << String.isEmpty)
        |> String.join "."


enum : Syntax -> Prefixer -> EnumDescriptorProto -> Enum
enum syntax prefixer descriptor =
    let
        name =
            prefixer descriptor.name
                |> Name.type_

        fields =
            descriptor.value
                |> List.map (\value -> ( value.number, addPrefix descriptor.name prefixer value.name |> Name.type_ ))
                |> List.Extra.uncons
                |> Maybe.withDefault ( ( 0, name ), [] )
    in
    { dataType = name
    , isTopLevel = prefixer "" == ""
    , withUnrecognized = syntax == Proto3
    , fields = fields
    }


isAMap : DescriptorProto -> Bool
isAMap =
    .options >> Maybe.map .mapEntry >> Maybe.withDefault False



{--
    Transform a `DescriptorProto` into our internal `Struct` representation.
    Prefix is used for nested messages inside of message declarations,
    since they could potentially overlap otherwise.
-}


message : Syntax -> TypeRefs -> Prefixer -> DescriptorProto -> Res Struct
message syntax typeRefs prefixer descriptor =
    let
        name =
            prefixer descriptor.name
                |> Name.type_

        getFromMaps : FieldDescriptorProto -> Maybe { key : FieldType, value : FieldType }
        getFromMaps fieldDescriptor =
            case fieldDescriptor.type_ of
                TypeMessage ->
                    Dict.get (String.dropLeft 1 fieldDescriptor.typeName) maps

                _ ->
                    Nothing

        messageFieldMeta : FieldDescriptorProto -> Res { field : ( FieldName, Field ), oneOfIndex : Int }
        messageFieldMeta fieldDescriptor =
            (case getFromMaps fieldDescriptor of
                Just { key, value } ->
                    case key of
                        Primitive _ _ _ ->
                            Ok <| MapField fieldDescriptor.number key value

                        _ ->
                            Err <| NonPrimitiveMapKey fieldDescriptor.typeName

                Nothing ->
                    fieldType fieldDescriptor typeRefs
                        |> Result.map (NormalField fieldDescriptor.number (cardinality fieldDescriptor.label))
            )
                |> Result.map
                    (\field ->
                        { field = ( Name.field fieldDescriptor.name, field )
                        , oneOfIndex = fieldDescriptor.oneofIndex
                        }
                    )

        fieldsMetaResult : Res (List { field : ( FieldName, Field ), oneOfIndex : Int })
        fieldsMetaResult =
            Errors.combineMap messageFieldMeta descriptor.field

        (DescriptorProtoNestedType nestedTypes) =
            descriptor.nestedType

        maps : Dict String { key : FieldType, value : FieldType }
        maps =
            nestedTypes
                |> List.filter isAMap
                |> List.filterMap
                    (\d ->
                        case ( List.filter (.number >> (==) 1) d.field, List.filter (.number >> (==) 2) d.field ) of
                            ( [ field1 ], [ field2 ] ) ->
                                case ( fieldType field1 typeRefs, fieldType field2 typeRefs ) of
                                    ( Ok t1, Ok t2 ) ->
                                        Just ( addPrefix descriptor.name prefixer d.name, { key = t1, value = t2 } )

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> Dict.fromList

        nested : Res Struct
        nested =
            List.filter (not << isAMap) nestedTypes
                |> Errors.combineMap (message syntax typeRefs (addPrefix descriptor.name prefixer))
                |> Result.map Struct.concat

        mainStruct : Res Struct
        mainStruct =
            Result.map
                (\fieldsMeta ->
                    { messages =
                        [ { dataType = name
                          , isTopLevel = isEmptyPrefixer prefixer
                          , fields = messageFields oneOfFieldNames fieldsMeta descriptor
                          }
                        ]
                    , enums = List.map (enum syntax (addPrefix descriptor.name prefixer)) descriptor.enumType
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


handleMessage : String -> TypeRefs -> Res FieldType
handleMessage messageName typeRefs =
    let
        searchForRecursion : Set String -> String -> Bool
        searchForRecursion encounteredTypes currentName =
            case lookForTypeRef currentName typeRefs of
                Ok ( Message name fieldDeps, _ ) ->
                    Set.member name encounteredTypes
                        || (Set.toList fieldDeps
                                |> List.any (searchForRecursion <| Set.insert name encounteredTypes)
                           )

                _ ->
                    False
    in
    case lookForTypeRef messageName typeRefs of
        Ok ( Message name _, moduleName ) ->
            Ok <|
                Embedded
                    { dataType = name
                    , moduleName = moduleName
                    , typeKind =
                        if searchForRecursion Set.empty messageName then
                            Model.Type

                        else
                            Model.Alias
                    }

        Ok ( Enum enumName _, _ ) ->
            Err <| EnumReferenceInsteadOfMessage enumName

        Err err ->
            Err err


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
            handleMessage descriptor.typeName typeRefs

        TypeBytes ->
            Ok <| Primitive Prim_Bytes "bytes" <| defaultBytes descriptor

        TypeUint32 ->
            Ok <| Primitive Prim_Int "uint32" <| defaultNumber descriptor

        TypeEnum ->
            lookForTypeRef descriptor.typeName typeRefs
                |> Result.andThen
                    (\( definedType, moduleName ) ->
                        case definedType of
                            Message messageName _ ->
                                Err <| MessageReferenceInsteadOfEnum messageName

                            Enum enumName values ->
                                case NonEmpty.fromList values of
                                    Just nonEmptyValues ->
                                        Ok <|
                                            Enumeration
                                                { dataType = enumName
                                                , values = values
                                                , moduleName = moduleName
                                                , default =
                                                    if String.isEmpty descriptor.defaultValue then
                                                        NonEmpty.head nonEmptyValues

                                                    else
                                                        Name.type_ (enumName ++ "." ++ descriptor.defaultValue)
                                                }

                                    Nothing ->
                                        Err <| NoEnumValues enumName
                    )

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

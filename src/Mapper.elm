module Mapper exposing (definedTypesInFileDescriptor, definedTypesInMessageDescriptor, mapMain, splitNonEmpty)

import Dict exposing (Dict)
import Dict.Extra
import Elm.CodeGen as C exposing (ModuleName)
import Errors exposing (Error(..), Res)
import List.Extra
import List.NonEmpty as NonEmpty
import Mapper.Name as Name
import Mapper.Package as Package exposing (Packages)
import Mapper.Struct as Struct exposing (Struct)
import Mapper.Syntax exposing (Syntax(..), parseSyntax)
import Meta.Encode
import Model exposing (Cardinality(..), DataType, Enum, Field(..), FieldName, FieldType(..), IntFlavor(..), Method, OneOf, Primitive(..), Service)
import Proto.Google.Protobuf.Descriptor exposing (DescriptorProto, DescriptorProto_(..), EnumDescriptorProto, FieldDescriptorProto, FieldDescriptorProto_Label(..), FieldDescriptorProto_Type(..), FileDescriptorProto, MethodDescriptorProto, ServiceDescriptorProto, unwrapDescriptorProto_)
import Set exposing (Set)


type alias DefinedTypes =
    List DefinedType


type DefinedType
    = -- name, references to further types
      Message String (Set String)
    | Enum String (List String)


getDefinedTypeName : DefinedType -> String
getDefinedTypeName definedType =
    case definedType of
        Message name _ ->
            name

        Enum name _ ->
            name


type alias PackageName =
    String


type alias TypeRefs =
    ( -- Own PackageName
      PackageName
      -- Own Types
    , DefinedTypes
    , -- Types defined in package dependencies
      Dict PackageName (List ( ModuleName, DefinedTypes ))
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
    List.map unwrapDescriptorProto_ descriptor.nestedType
        |> (\nested ->
                Message descriptor.name (List.map .typeName descriptor.field |> List.filter (not << String.isEmpty) |> Set.fromList)
                    :: (List.map (nestDefinedType descriptor.name) <|
                            List.concatMap definedTypesInMessageDescriptor nested
                                ++ List.map definedTypesInEnumDescriptor descriptor.enumType
                       )
           )


definedTypesInFileDescriptor : FileDescriptorProto -> DefinedTypes
definedTypesInFileDescriptor descriptor =
    List.map definedTypesInEnumDescriptor descriptor.enumType
        ++ List.concatMap definedTypesInMessageDescriptor descriptor.messageType


mapMain : Bool -> List FileDescriptorProto -> List ( String, Res Packages )
mapMain grpcOn descriptors =
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
                            |> List.map (\( name, ( packageName_, types ) ) -> ( packageName_, [ ( Name.module_ name, types ) ] ))
                            |> Dict.Extra.fromListDedupe (++)
                        )

                    mapMethod : MethodDescriptorProto -> Maybe (Res Method)
                    mapMethod { name, inputType, outputType, serverStreaming, clientStreaming } =
                        if serverStreaming || clientStreaming then
                            Nothing

                        else
                            Just <|
                                Result.map2
                                    (\( definedInputType, inputTypeModuleName ) ( definedOutputType, outputTypeModuleName ) ->
                                        { name = name
                                        , reqType = ( inputTypeModuleName, getDefinedTypeName definedInputType )
                                        , resType = ( outputTypeModuleName, getDefinedTypeName definedOutputType )
                                        }
                                    )
                                    (lookForTypeRef inputType typeRefs)
                                    (lookForTypeRef outputType typeRefs)

                    mapService : ServiceDescriptorProto -> Res Service
                    mapService service =
                        List.filterMap mapMethod service.method
                            |> Errors.combineMap identity
                            |> Result.map
                                (\methods ->
                                    { name = service.name
                                    , package = descriptor.package
                                    , methods = methods
                                    }
                                )

                    syntax =
                        parseSyntax descriptor.syntax

                    packageName =
                        Name.module_ descriptor.package
                in
                ( descriptor.name
                , Result.map2
                    (\messagePackages services ->
                        Package.addPackage packageName
                            { enums = List.map (enum syntax) descriptor.enumType
                            , messages = []
                            , services = services
                            , oneOfs = []
                            }
                        <|
                            Package.concat messagePackages
                    )
                    (Errors.combineMap (message packageName syntax typeRefs) descriptor.messageType)
                    (if grpcOn then
                        Errors.combineMap mapService descriptor.service

                     else
                        Ok []
                    )
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
                                        (List.filterMap
                                            (\( modName, types ) ->
                                                List.filter (matchesString typeName) types
                                                    |> List.head
                                                    |> Maybe.map (Tuple.pair modName)
                                            )
                                            >> List.head
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


enum : Syntax -> EnumDescriptorProto -> Enum
enum syntax descriptor =
    let
        name =
            Name.type_ descriptor.name

        fields =
            descriptor.value
                |> List.map (\value -> ( value.number, name ++ "_" ++ Name.type_ value.name ))
                |> List.Extra.uncons
                |> Maybe.withDefault ( ( 0, name ), [] )
    in
    { dataType = name
    , withUnrecognized = syntax == Proto3
    , fields = fields
    }


isAMap : DescriptorProto -> Bool
isAMap =
    .options >> Maybe.map .mapEntry >> Maybe.withDefault False


{-|

    Transform a `DescriptorProto` into our internal `Struct` representation.
    Prefix is used for nested messages inside of message declarations,
    since they could potentially overlap otherwise.

-}
message : List String -> Syntax -> TypeRefs -> DescriptorProto -> Res Packages
message packageName_ syntax typeRefs descriptor =
    let
        name =
            Name.type_ descriptor.name

        removePackageName : String -> TypeRefs -> String
        removePackageName typeName ( packageName, _, _ ) =
            if String.isEmpty packageName then
                String.dropLeft 1 typeName

            else
                String.replace ("." ++ packageName ++ ".") "" typeName

        getFromMaps : FieldDescriptorProto -> Maybe { key : FieldType, value : FieldType }
        getFromMaps fieldDescriptor =
            case fieldDescriptor.type_ of
                FieldDescriptorProto_Type_TYPEMESSAGE ->
                    Dict.get (removePackageName fieldDescriptor.typeName typeRefs) maps

                _ ->
                    Nothing

        messageFieldMeta : FieldDescriptorProto -> Res { field : ( FieldName, Field ), oneOfIndex : Int }
        messageFieldMeta fieldDescriptor =
            (case getFromMaps fieldDescriptor of
                Just { key, value } ->
                    case key of
                        Primitive _ _ ->
                            Ok <| MapField fieldDescriptor.number key value

                        _ ->
                            Err <| NonPrimitiveMapKey fieldDescriptor.typeName

                Nothing ->
                    fieldType name fieldDescriptor typeRefs
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

        nestedTypes =
            List.map unwrapDescriptorProto_ descriptor.nestedType

        maps : Dict String { key : FieldType, value : FieldType }
        maps =
            nestedTypes
                |> List.filter isAMap
                |> List.filterMap
                    (\d ->
                        case ( List.filter (.number >> (==) 1) d.field, List.filter (.number >> (==) 2) d.field ) of
                            ( [ field1 ], [ field2 ] ) ->
                                case ( fieldType name field1 typeRefs, fieldType name field2 typeRefs ) of
                                    ( Ok t1, Ok t2 ) ->
                                        Just ( descriptor.name ++ "_" ++ d.name, { key = t1, value = t2 } )

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> Dict.fromList

        nestedPackageName =
            packageName_ ++ [ Name.type_ descriptor.name ]

        nested : Res Packages
        nested =
            List.filter (not << isAMap) nestedTypes
                |> Errors.combineMap (message nestedPackageName syntax typeRefs)
                |> Result.map Package.concat

        mainStruct : Res Struct
        mainStruct =
            Result.map
                (\fieldsMeta ->
                    { messages =
                        [ { dataType = name
                          , fields = messageFields oneOfFieldNames fieldsMeta { prefix = name ++ "_" }
                          }
                        ]
                    , enums = List.map (enum syntax) descriptor.enumType
                    , services = []
                    , oneOfs = []
                    }
                )
                fieldsMetaResult

        oneofPackage =
            Result.map
                (\fieldsMeta ->
                    if List.isEmpty oneOfFieldNames then
                        identity

                    else
                        Package.addPackage nestedPackageName (oneofStruct oneOfFieldNames fieldsMeta)
                )
                fieldsMetaResult

        oneOfFieldNames : List String
        oneOfFieldNames =
            List.map .name descriptor.oneofDecl
    in
    Errors.map3 (\mainS addOneOfPackage nestedPackage -> Package.addPackage packageName_ mainS nestedPackage |> addOneOfPackage)
        mainStruct
        oneofPackage
        nested



-- FIELD


oneofStruct : List String -> List { field : ( FieldName, Field ), oneOfIndex : Int } -> Struct
oneofStruct oneOfFieldNames fieldsMeta =
    let
        oneOfFields =
            List.indexedMap (oneOfFieldPackage fieldsMeta) oneOfFieldNames

        base =
            Struct.empty
    in
    { base | oneOfs = oneOfFields }


messageFields : List String -> List { field : ( FieldName, Field ), oneOfIndex : Int } -> { prefix : String } -> List ( FieldName, Field )
messageFields oneOfFieldNames fieldsMeta prefix =
    let
        oneOfFields =
            List.indexedMap (oneOfField fieldsMeta prefix) oneOfFieldNames
    in
    fieldsMeta
        |> List.filterMap
            (\{ field, oneOfIndex } ->
                if oneOfIndex >= 0 then
                    List.Extra.getAt oneOfIndex oneOfFields

                else
                    Just field
            )
        |> List.Extra.uniqueBy Tuple.first


oneOfField : List { field : ( FieldName, Field ), oneOfIndex : Int } -> { prefix : String } -> Int -> String -> ( FieldName, Field )
oneOfField fields { prefix } index name =
    List.filter (\field -> field.oneOfIndex == index) fields
        |> List.map .field
        |> List.filterMap
            (\( fieldName, field ) ->
                case field of
                    NormalField fieldNumber _ type_ ->
                        Just ( fieldNumber, prefix ++ Name.type_ name ++ "_" ++ Name.type_ fieldName, type_ )

                    _ ->
                        Nothing
            )
        |> OneOfField (Name.type_ name)
        |> Tuple.pair (Name.field name)


oneOfFieldPackage : List { field : ( FieldName, Field ), oneOfIndex : Int } -> Int -> String -> ( String, OneOf )
oneOfFieldPackage fields index name =
    List.filter (\field -> field.oneOfIndex == index) fields
        |> List.map .field
        |> List.filterMap
            (\( fieldName, field ) ->
                case field of
                    NormalField fieldNumber _ type_ ->
                        Just ( fieldNumber, Name.type_ fieldName, type_ )

                    _ ->
                        Nothing
            )
        |> Tuple.pair (Name.type_ name)


handleMessage : DataType -> String -> TypeRefs -> Res FieldType
handleMessage parentDataType messageName typeRefs =
    let
        searchForRecursionHelper : Set String -> String -> Bool
        searchForRecursionHelper encounteredTypes currentName =
            case lookForTypeRef currentName typeRefs of
                Ok ( Message name fieldDeps, [] ) ->
                    if Set.member name encounteredTypes then
                        name == parentDataType

                    else
                        Set.toList fieldDeps
                            |> List.any (searchForRecursionHelper <| Set.insert name encounteredTypes)

                _ ->
                    False

        searchForRecursion : String -> Set String -> Bool
        searchForRecursion name fieldDeps =
            Set.toList fieldDeps
                |> List.any (searchForRecursionHelper <| Set.singleton name)
    in
    case lookForTypeRef messageName typeRefs of
        Ok ( Message name fieldDeps, moduleName ) ->
            Ok <|
                Embedded
                    { dataType = name
                    , moduleName = moduleName
                    , typeKind =
                        if searchForRecursion name fieldDeps then
                            Model.Type

                        else
                            Model.Alias
                    }

        Ok ( Enum enumName _, _ ) ->
            Err <| EnumReferenceInsteadOfMessage enumName

        Err err ->
            Err err


fieldType : DataType -> FieldDescriptorProto -> TypeRefs -> Res FieldType
fieldType parentDataType descriptor typeRefs =
    case descriptor.type_ of
        FieldDescriptorProto_Type_TYPEDOUBLE ->
            Ok <| Primitive Prim_Double <| defaultNumber descriptor

        FieldDescriptorProto_Type_TYPEFLOAT ->
            Ok <| Primitive Prim_Float <| defaultNumber descriptor

        FieldDescriptorProto_Type_TYPEINT64 ->
            Ok <| Primitive (Prim_Int64 Int_) <| defaultInt64 descriptor

        FieldDescriptorProto_Type_TYPEINT32 ->
            Ok <| Primitive (Prim_Int32 Int_) <| defaultNumber descriptor

        FieldDescriptorProto_Type_TYPEUINT64 ->
            Ok <| Primitive (Prim_Int64 UInt) <| defaultInt64 descriptor

        FieldDescriptorProto_Type_TYPEUINT32 ->
            Ok <| Primitive (Prim_Int32 UInt) <| defaultNumber descriptor

        FieldDescriptorProto_Type_TYPEFIXED64 ->
            Ok <| Primitive (Prim_Int64 Fixed) <| defaultInt64 descriptor

        FieldDescriptorProto_Type_TYPEFIXED32 ->
            Ok <| Primitive (Prim_Int32 Fixed) <| defaultNumber descriptor

        FieldDescriptorProto_Type_TYPESFIXED64 ->
            Ok <| Primitive (Prim_Int64 SFixed) <| defaultInt64 descriptor

        FieldDescriptorProto_Type_TYPESFIXED32 ->
            Ok <| Primitive (Prim_Int32 SFixed) <| defaultNumber descriptor

        FieldDescriptorProto_Type_TYPESINT64 ->
            Ok <| Primitive (Prim_Int64 SInt) <| defaultInt64 descriptor

        FieldDescriptorProto_Type_TYPESINT32 ->
            Ok <| Primitive (Prim_Int32 SInt) <| defaultNumber descriptor

        FieldDescriptorProto_Type_TYPEBOOL ->
            Ok <| Primitive Prim_Bool <| defaultBool descriptor

        FieldDescriptorProto_Type_TYPESTRING ->
            Ok <| Primitive Prim_String <| defaultString descriptor

        FieldDescriptorProto_Type_TYPEGROUP ->
            handleMessage parentDataType descriptor.typeName typeRefs

        FieldDescriptorProto_Type_TYPEMESSAGE ->
            handleMessage parentDataType descriptor.typeName typeRefs

        FieldDescriptorProto_Type_TYPEBYTES ->
            Ok <| Primitive Prim_Bytes <| defaultBytes descriptor

        FieldDescriptorProto_Type_TYPEENUM ->
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
                                                        enumName ++ "_" ++ Name.type_ descriptor.defaultValue
                                                }

                                    Nothing ->
                                        Err <| NoEnumValues enumName
                    )



-- DEFAULTS


defaultBool : FieldDescriptorProto -> C.Expression
defaultBool descriptor =
    if descriptor.defaultValue == "true" then
        C.val "True"

    else
        C.val "False"


defaultBytes : FieldDescriptorProto -> C.Expression
defaultBytes descriptor =
    -- TODO c escaped bytes, see https://github.com/golang/protobuf/pull/427/files
    C.applyBinOp (C.fqFun [ "Protobuf", "Encode" ] "encode") C.pipel <|
        C.apply [ Meta.Encode.string, C.string descriptor.defaultValue ]


defaultNumber : FieldDescriptorProto -> C.Expression
defaultNumber descriptor =
    if descriptor.defaultValue == "" then
        C.int 0

    else
        C.val descriptor.defaultValue


defaultInt64 : FieldDescriptorProto -> C.Expression
defaultInt64 descriptor =
    C.apply [ C.fqFun [ "Protobuf", "Types", "Int64" ] "fromInts", C.int 0, defaultNumber descriptor ]


defaultString : FieldDescriptorProto -> C.Expression
defaultString descriptor =
    C.string descriptor.defaultValue


cardinality : FieldDescriptorProto_Label -> Cardinality
cardinality value =
    case value of
        FieldDescriptorProto_Label_LABELOPTIONAL ->
            Optional

        FieldDescriptorProto_Label_LABELREQUIRED ->
            Required

        FieldDescriptorProto_Label_LABELREPEATED ->
            Repeated

module Mapper exposing (TypeRefs, definedTypesInFileDescriptor, definedTypesInMessageDescriptor, mapMain, message)

import Dict exposing (Dict)
import Elm.CodeGen as C exposing (ModuleName)
import Errors exposing (Error(..), Res)
import List.Extra
import Mapper.Name as Name
import Mapper.Package as Package exposing (Packages)
import Mapper.Struct as Struct exposing (Struct, empty)
import Mapper.Syntax exposing (Syntax(..), parseSyntax)
import Meta.Encode
import Model exposing (Cardinality(..), Enum, Field(..), FieldName, FieldType(..), IntFlavor(..), Method, OneOf, Primitive(..), Service)
import Proto.Google.Protobuf.Descriptor exposing (DescriptorProto, DescriptorProto_(..), EnumDescriptorProto, FieldDescriptorProto, FieldDescriptorProto_Label(..), FieldDescriptorProto_Type(..), FileDescriptorProto, MethodDescriptorProto, ServiceDescriptorProto, unwrapDescriptorProto_)
import Set exposing (Set)


type alias TypeRefs =
    Dict ( ModuleName, String ) (Set ( ModuleName, String ))


type alias Ctx =
    { typeRefs : TypeRefs
    , originFiles : Set String
    , syntax : Syntax
    }


definedTypesInMessageDescriptor : ModuleName -> DescriptorProto -> TypeRefs
definedTypesInMessageDescriptor ownModuleName descriptor =
    let
        types =
            List.map .typeName descriptor.field
                |> List.filter (not << String.isEmpty)
                |> Set.fromList
                |> Set.map Name.absoluteRef

        nestedTypeRefs =
            List.map unwrapDescriptorProto_ descriptor.nestedType
                |> List.map (definedTypesInMessageDescriptor ownModuleName)
                |> List.foldl Dict.union Dict.empty
    in
    Dict.insert ( ownModuleName, Name.type_ descriptor.name )
        types
        nestedTypeRefs


definedTypesInFileDescriptor : FileDescriptorProto -> TypeRefs
definedTypesInFileDescriptor descriptor =
    let
        moduleName =
            Name.module_ descriptor.package
    in
    List.map (definedTypesInMessageDescriptor moduleName) descriptor.messageType
        |> List.foldl Dict.union Dict.empty


mapMain : Bool -> List FileDescriptorProto -> List ( String, Res Packages )
mapMain grpcOn descriptors =
    let
        typeRefs : TypeRefs
        typeRefs =
            descriptors
                |> List.map definedTypesInFileDescriptor
                |> List.foldl Dict.union Dict.empty
    in
    descriptors
        |> List.map
            (\descriptor ->
                let
                    mapMethod : MethodDescriptorProto -> Maybe (Res Method)
                    mapMethod { name, inputType, outputType, serverStreaming, clientStreaming } =
                        if serverStreaming || clientStreaming then
                            Nothing

                        else
                            Just <|
                                Ok <|
                                    { name = name
                                    , reqType = Name.absoluteRef inputType
                                    , resType = Name.absoluteRef outputType
                                    }

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

                    servicePackages : List Service -> Packages
                    servicePackages =
                        List.foldl
                            (\service ->
                                Package.addPackage (packageName ++ [ Name.type_ service.name ])
                                    { empty | services = [ service ], originFiles = originFiles }
                            )
                            Package.empty

                    originFiles =
                        Set.singleton descriptor.name

                    ctx =
                        { typeRefs = typeRefs, originFiles = originFiles, syntax = syntax }

                    syntax =
                        parseSyntax descriptor.syntax

                    packageName =
                        Name.module_ descriptor.package
                in
                ( descriptor.name
                , Result.map2
                    (\messagePackages services ->
                        Package.concat messagePackages
                            |> Package.addPackage packageName
                                { empty
                                    | enums = List.map (enum syntax) descriptor.enumType
                                    , originFiles = originFiles
                                }
                            |> Package.append (servicePackages services)
                    )
                    (Errors.combineMap (message packageName ctx) descriptor.messageType)
                    (if grpcOn then
                        Errors.combineMap mapService descriptor.service

                     else
                        Ok []
                    )
                )
            )



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
                |> List.map (\value -> ( value.number, Name.type_ value.name ))
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
message : ModuleName -> Ctx -> DescriptorProto -> Res Packages
message packageName_ ctx descriptor =
    let
        name =
            Name.type_ descriptor.name

        getFromMaps : FieldDescriptorProto -> Maybe { key : FieldType, value : FieldType }
        getFromMaps fieldDescriptor =
            let
                typeNameWithoutPackage =
                    fieldDescriptor.typeName
                        |> String.split "."
                        |> List.reverse
                        |> List.take 2
                        |> List.reverse
                        |> String.join "."
            in
            case fieldDescriptor.type_ of
                FieldDescriptorProto_Type_TYPEMESSAGE ->
                    Dict.get typeNameWithoutPackage maps

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
                    fieldType ctx.typeRefs parentRef fieldDescriptor
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

        parentRef =
            ( packageName_, name )

        maps : Dict String { key : FieldType, value : FieldType }
        maps =
            nestedTypes
                |> List.filter isAMap
                |> List.filterMap
                    (\d ->
                        case ( List.filter (.number >> (==) 1) d.field, List.filter (.number >> (==) 2) d.field ) of
                            ( [ field1 ], [ field2 ] ) ->
                                case ( fieldType ctx.typeRefs parentRef field1, fieldType ctx.typeRefs parentRef field2 ) of
                                    ( Ok t1, Ok t2 ) ->
                                        Just ( descriptor.name ++ "." ++ d.name, { key = t1, value = t2 } )

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
                |> Errors.combineMap (message nestedPackageName ctx)
                |> Result.map Package.concat

        mainStruct : Res Struct
        mainStruct =
            Result.map
                (\fieldsMeta ->
                    { empty
                        | messages =
                            [ { dataType = name
                              , fields = messageFields nestedPackageName oneOfFieldNames fieldsMeta
                              }
                            ]
                        , originFiles = ctx.originFiles
                    }
                )
                fieldsMetaResult

        enumPackage =
            if List.isEmpty descriptor.enumType then
                identity

            else
                Package.addPackage nestedPackageName
                    { empty
                        | enums = List.map (enum ctx.syntax) descriptor.enumType
                        , originFiles = ctx.originFiles
                    }

        oneofPackage =
            Result.map
                (\fieldsMeta ->
                    if List.isEmpty oneOfFieldNames then
                        identity

                    else
                        Package.append (oneofStruct ctx.originFiles nestedPackageName oneOfFieldNames fieldsMeta)
                )
                fieldsMetaResult

        oneOfFieldNames : List String
        oneOfFieldNames =
            List.map .name descriptor.oneofDecl
    in
    Errors.map3
        (\mainS addOneOfPackage nestedPackage ->
            Package.addPackage packageName_ mainS nestedPackage
                |> addOneOfPackage
                |> enumPackage
        )
        mainStruct
        oneofPackage
        nested



-- FIELD


oneofStruct : Set String -> C.ModuleName -> List String -> List { field : ( FieldName, Field ), oneOfIndex : Int } -> Package.Packages
oneofStruct originFiles basePackageName oneOfFieldNames fieldsMeta =
    let
        oneOfFields =
            List.indexedMap (oneOfFieldPackage fieldsMeta) oneOfFieldNames
    in
    List.foldl (\oneOf -> Package.addPackage (basePackageName ++ [ Tuple.first oneOf ]) { empty | oneOfs = [ oneOf ], originFiles = originFiles })
        Package.empty
        oneOfFields


messageFields : ModuleName -> List String -> List { field : ( FieldName, Field ), oneOfIndex : Int } -> List ( FieldName, Field )
messageFields nestedModuleName oneOfFieldNames fieldsMeta =
    let
        oneOfFields =
            List.map (oneOfField nestedModuleName) oneOfFieldNames
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


oneOfField : ModuleName -> String -> ( FieldName, Field )
oneOfField nestedModuleName name =
    OneOfField (Name.type_ name) (nestedModuleName ++ [ Name.type_ name ])
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


handleMessage : ( ModuleName, String ) -> TypeRefs -> String -> FieldType
handleMessage parentRef typeRefs messageName =
    let
        ( moduleName, dataType ) =
            Name.absoluteRef messageName
    in
    Embedded
        { dataType = dataType
        , moduleName = moduleName
        , typeKind =
            if isRecursive typeRefs parentRef (Name.absoluteRef messageName) then
                Model.Type

            else
                Model.Alias
        }


isRecursive : TypeRefs -> ( ModuleName, String ) -> ( ModuleName, String ) -> Bool
isRecursive typeRefs parentRef ref =
    let
        isRecursiveHelper encounteredTypes currRef =
            case Dict.get currRef typeRefs of
                Nothing ->
                    False

                Just nextRefs ->
                    Set.toList nextRefs
                        |> List.any
                            (\nextRef ->
                                if Set.member nextRef encounteredTypes then
                                    nextRef == parentRef

                                else
                                    isRecursiveHelper (Set.insert currRef encounteredTypes) nextRef
                            )
    in
    isRecursiveHelper Set.empty ref


fieldType : TypeRefs -> ( ModuleName, String ) -> FieldDescriptorProto -> Res FieldType
fieldType typeRefs parentRef descriptor =
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
            Ok <| handleMessage parentRef typeRefs descriptor.typeName

        FieldDescriptorProto_Type_TYPEMESSAGE ->
            Ok <| handleMessage parentRef typeRefs descriptor.typeName

        FieldDescriptorProto_Type_TYPEBYTES ->
            Ok <| Primitive Prim_Bytes <| defaultBytes descriptor

        FieldDescriptorProto_Type_TYPEENUM ->
            let
                ( moduleName, dataType ) =
                    Name.absoluteRef descriptor.typeName
            in
            Ok <|
                Enumeration
                    { dataType = dataType
                    , moduleName = moduleName
                    }



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

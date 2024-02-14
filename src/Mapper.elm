module Mapper exposing (TypeRefs, calcTypeKind, definedTypesInFileDescriptor, definedTypesInMessageDescriptor, mapMain, message)

import Dict exposing (Dict)
import Elm.CodeGen as C exposing (ModuleName)
import Errors exposing (Error(..), Res)
import List.Extra
import Mapper.Name as Name
import Mapper.Package as Package exposing (Packages)
import Mapper.Struct exposing (Struct, empty)
import Mapper.Syntax exposing (Syntax(..), parseSyntax)
import Meta.Decode exposing (oneOf)
import Meta.Encode
import Model exposing (Cardinality(..), Enum, Field(..), FieldName, FieldType(..), IntFlavor(..), Method, OneOf, Primitive(..), Service)
import Options exposing (Options)
import Proto.Google.Protobuf exposing (DescriptorProto, EnumDescriptorProto, FieldDescriptorProto, FileDescriptorProto, MethodDescriptorProto, ServiceDescriptorProto, fieldNumbersDescriptorProto, fieldNumbersFileDescriptorProto, fieldNumbersServiceDescriptorProto, unwrapDescriptorProto)
import Proto.Google.Protobuf.FieldDescriptorProto.Label as Label exposing (Label)
import Proto.Google.Protobuf.FieldDescriptorProto.Type as Type
import Proto.Google.Protobuf.SourceCodeInfo as SourceCodeInfo
import Set exposing (Set)


type alias TypeRefs =
    Dict ( ModuleName, ModuleName, String ) (Set ( ModuleName, ModuleName, String ))


refToKey : Name.Ref -> ( ModuleName, ModuleName, String )
refToKey ref =
    ( ref.rootPackage, ref.package, ref.name )


type alias Ctx =
    { typeRefs : TypeRefs
    , originFiles : Set String
    , syntax : Syntax
    , sourceCodeInfo : Dict (List Int) SourceCodeInfo.Location
    }


sourceDocumentation : Ctx -> List Int -> List String
sourceDocumentation { sourceCodeInfo } sourceCodePath =
    Dict.get sourceCodePath sourceCodeInfo
        |> Maybe.map (\info -> info.leadingDetachedComments ++ [ info.leadingComments, info.trailingComments ])
        |> Maybe.withDefault []
        |> List.filter (not << String.isEmpty)


definedTypesInMessageDescriptor : Name.ModuleRef -> DescriptorProto -> TypeRefs
definedTypesInMessageDescriptor moduleNames descriptor =
    let
        types =
            List.map .typeName descriptor.field
                |> List.filter (not << String.isEmpty)
                |> Set.fromList
                |> Set.map (Name.absoluteRef >> refToKey)

        nestedTypeRefs =
            List.map unwrapDescriptorProto descriptor.nestedType
                |> List.map (definedTypesInMessageDescriptor moduleNames)
                |> List.foldl Dict.union Dict.empty
    in
    Dict.insert ( moduleNames.rootPackage, moduleNames.package, Name.type_ descriptor.name )
        types
        nestedTypeRefs


definedTypesInFileDescriptor : FileDescriptorProto -> TypeRefs
definedTypesInFileDescriptor descriptor =
    let
        moduleNames =
            Name.moduleRef_ descriptor.package
    in
    List.map (definedTypesInMessageDescriptor moduleNames) descriptor.messageType
        |> List.foldl Dict.union Dict.empty


mapMain : Options -> List FileDescriptorProto -> List ( ModuleName, Res Packages )
mapMain options descriptors =
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
                    sourceCodeInfo : Dict (List Int) SourceCodeInfo.Location
                    sourceCodeInfo =
                        descriptor.sourceCodeInfo
                            |> Maybe.map .location
                            |> Maybe.withDefault []
                            |> List.map (\info -> ( info.path, info ))
                            |> Dict.fromList

                    fileDocs =
                        sourceDocumentation ctx [ fieldNumbersFileDescriptorProto.syntax ]
                            ++ sourceDocumentation ctx [ fieldNumbersFileDescriptorProto.package ]

                    mapMethod : List Int -> MethodDescriptorProto -> Maybe (Res Method)
                    mapMethod sourceCodePath { name, inputType, outputType, serverStreaming, clientStreaming } =
                        if serverStreaming || clientStreaming then
                            Nothing

                        else
                            Just <|
                                Ok <|
                                    { name = name
                                    , reqType = Name.absoluteRef inputType
                                    , resType = Name.absoluteRef outputType
                                    , docs = sourceDocumentation ctx sourceCodePath
                                    }

                    mapService : List Int -> ServiceDescriptorProto -> Res Service
                    mapService sourceCodePath service =
                        List.indexedMap (\index -> mapMethod <| sourceCodePath ++ [ fieldNumbersServiceDescriptorProto.method, index ]) service.method
                            |> List.filterMap identity
                            |> Errors.combineMap identity
                            |> Result.map
                                (\methods ->
                                    { name = service.name
                                    , package = descriptor.package
                                    , methods = methods
                                    , docs = sourceDocumentation ctx sourceCodePath
                                    }
                                )

                    servicePackages : List Service -> Packages
                    servicePackages =
                        List.foldl
                            (\service ->
                                Package.addPackage (moduleRef.package ++ [ Name.type_ service.name ])
                                    { empty | services = [ service ], originFiles = originFiles }
                            )
                            Package.empty

                    originFiles =
                        Set.singleton descriptor.name

                    ctx =
                        { typeRefs = typeRefs, originFiles = originFiles, syntax = syntax, sourceCodeInfo = sourceCodeInfo }

                    syntax =
                        parseSyntax descriptor.syntax

                    moduleRef =
                        Name.moduleRef_ descriptor.package

                    isLowerCase =
                        String.uncons
                            >> Maybe.map (\( char, _ ) -> Char.isLower char)
                            >> Maybe.withDefault True

                    packageIsLowerCase =
                        String.split "." descriptor.package
                            |> List.all isLowerCase
                in
                if not packageIsLowerCase then
                    ( moduleRef.rootPackage, Err <| AmbiguousPackageName descriptor.package )

                else
                    ( moduleRef.rootPackage
                    , Result.map2
                        (\messagePackages services ->
                            Package.concat messagePackages
                                |> Package.append (enumPackages ctx [] moduleRef.package descriptor.enumType)
                                |> Package.addPackage moduleRef.package
                                    { empty
                                        | docs = fileDocs
                                        , originFiles = originFiles
                                    }
                                |> Package.append (servicePackages services)
                        )
                        (Errors.combine <|
                            List.indexedMap
                                (\index ->
                                    message moduleRef
                                        ctx
                                        [ fieldNumbersFileDescriptorProto.messageType, index ]
                                )
                                descriptor.messageType
                        )
                        (if options.grpc then
                            List.indexedMap (\index -> mapService [ fieldNumbersFileDescriptorProto.service, index ]) descriptor.service
                                |> Errors.combine

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


enum : Ctx -> List Int -> EnumDescriptorProto -> Enum
enum ctx sourceCodePath descriptor =
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
    , withUnrecognized = ctx.syntax == Proto3
    , fields = fields
    , docs = sourceDocumentation ctx sourceCodePath
    }


isAMap : DescriptorProto -> Bool
isAMap =
    .options >> Maybe.map .mapEntry >> Maybe.withDefault False


{-|

    Transform a `DescriptorProto` into our internal `Struct` representation.
    Prefix is used for nested messages inside of message declarations,
    since they could potentially overlap otherwise.

-}
message : Name.ModuleRef -> Ctx -> List Int -> DescriptorProto -> Res Packages
message moduleRef ctx sourceCodePath descriptor =
    let
        name =
            Name.type_ descriptor.name

        messageIsLowerCase =
            String.uncons descriptor.name
                |> Maybe.map (\( char, _ ) -> Char.isLower char)
                |> Maybe.withDefault True

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
                Type.TYPEMESSAGE ->
                    Dict.get typeNameWithoutPackage maps

                _ ->
                    Nothing

        messageFieldMeta : Int -> FieldDescriptorProto -> Res { field : ( FieldName, Field ), oneOfIndex : Int, docs : List String }
        messageFieldMeta index fieldDescriptor =
            (case getFromMaps fieldDescriptor of
                Just { key, value } ->
                    case key of
                        Primitive _ _ ->
                            Ok <| MapField fieldDescriptor.number key value

                        _ ->
                            Err <| NonPrimitiveMapKey fieldDescriptor.typeName

                Nothing ->
                    fieldType ctx.typeRefs parentRef fieldDescriptor
                        |> Result.map
                            (NormalField fieldDescriptor.number
                                (if fieldDescriptor.proto3Optional then
                                    Proto3Optional

                                 else
                                    cardinality fieldDescriptor.label
                                )
                            )
            )
                |> Result.map
                    (\field ->
                        { field = ( Name.field fieldDescriptor.name, field )
                        , oneOfIndex =
                            if fieldDescriptor.proto3Optional then
                                -1

                            else
                                fieldDescriptor.oneofIndex
                        , docs = sourceDocumentation ctx (sourceCodePath ++ [ fieldNumbersDescriptorProto.field, index ])
                        }
                    )

        fieldsMetaResult : Res (List { field : ( FieldName, Field ), oneOfIndex : Int, docs : List String })
        fieldsMetaResult =
            List.indexedMap messageFieldMeta descriptor.field
                |> Errors.combine

        nestedTypes =
            List.map unwrapDescriptorProto descriptor.nestedType

        parentRef =
            { rootPackage = moduleRef.rootPackage, package = moduleRef.package, name = name }

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
            moduleRef.package ++ [ Name.type_ descriptor.name ]

        nested : Res Packages
        nested =
            List.indexedMap Tuple.pair nestedTypes
                |> List.filter (\( _, x ) -> not <| isAMap x)
                |> Errors.combineMap
                    (\( index, x ) ->
                        message { moduleRef | package = nestedPackageName }
                            ctx
                            (sourceCodePath ++ [ fieldNumbersDescriptorProto.nestedType, index ])
                            x
                    )
                |> Result.map Package.concat

        docs =
            sourceDocumentation ctx sourceCodePath
                ++ (if List.isEmpty fieldDocs then
                        []

                    else
                        "## Fields" :: fieldDocs
                   )

        fieldDocs =
            List.indexedMap
                (\index field ->
                    if field.oneofIndex >= 0 then
                        []

                    else
                        case sourceDocumentation ctx (sourceCodePath ++ [ fieldNumbersDescriptorProto.field, index ]) of
                            [] ->
                                []

                            nonEmptyLines ->
                                ("### " ++ Name.field field.name) :: nonEmptyLines
                )
                descriptor.field
                |> List.concatMap identity

        mainStruct : Res Struct
        mainStruct =
            Result.map
                (\fieldsMeta ->
                    { empty
                        | messages =
                            [ { dataType = name
                              , fields = messageFields { moduleRef | package = nestedPackageName } oneOfFieldNames fieldsMeta
                              , docs = docs
                              , typeKind = calcTypeKind ctx.typeRefs parentRef parentRef
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
                Package.append (enumPackages ctx sourceCodePath nestedPackageName descriptor.enumType)

        -- Weird: protoc generates oneof fields for proto3 optionals and then refers to those. They are always the fieldname itself prefixed with "_"
        proto3OptionalFields =
            descriptor.field
                |> List.filter (\field -> field.proto3Optional)
                |> List.map (\field -> "_" ++ field.name)
                |> Set.fromList

        oneofPackage =
            Result.map
                (\fieldsMeta ->
                    if List.isEmpty oneOfFieldNames then
                        identity

                    else
                        Package.append (oneofStruct ctx sourceCodePath nestedPackageName oneOfFieldNames fieldsMeta)
                )
                fieldsMetaResult

        oneOfReexportPackage =
            Result.map
                (\fieldsMeta ->
                    let
                        oneOfFields =
                            List.indexedMap (oneOfFieldPackage ctx sourceCodePath fieldsMeta) oneOfFieldNames
                    in
                    if List.isEmpty oneOfFieldNames then
                        identity

                    else
                        Package.addPackage nestedPackageName { empty | oneOfReexports = oneOfFields }
                )
                fieldsMetaResult

        oneOfFieldNames : List String
        oneOfFieldNames =
            List.map .name descriptor.oneofDecl
                |> List.filter (\n -> not <| Set.member n proto3OptionalFields)
    in
    if messageIsLowerCase then
        Err <| Errors.AmbiguousMessageName descriptor.name

    else
        Errors.map4
            (\mainS addOneOfPackage addOneOfReexportPackage nestedPackage ->
                Package.addPackage moduleRef.package mainS nestedPackage
                    |> addOneOfPackage
                    |> addOneOfReexportPackage
                    |> enumPackage
            )
            mainStruct
            oneofPackage
            oneOfReexportPackage
            nested



-- FIELD


oneofStruct :
    Ctx
    -> List Int
    -> C.ModuleName
    -> List String
    -> List { field : ( FieldName, Field ), oneOfIndex : Int, docs : List String }
    -> Package.Packages
oneofStruct ctx messageSourceCodePath basePackageName oneOfFieldNames fieldsMeta =
    let
        oneOfFields =
            List.indexedMap (oneOfFieldPackage ctx messageSourceCodePath fieldsMeta) oneOfFieldNames
    in
    List.foldl (\oneOf -> Package.addPackage (basePackageName ++ [ oneOf.oneOfName ]) { empty | oneOfs = [ oneOf ], originFiles = ctx.originFiles })
        Package.empty
        oneOfFields


enumPackages : Ctx -> List Int -> C.ModuleName -> List EnumDescriptorProto -> Package.Packages
enumPackages ctx messageSourceCodePath basePackageName enumDescriptor =
    let
        enums =
            List.indexedMap
                (\index ->
                    enum ctx
                        (messageSourceCodePath ++ [ fieldNumbersDescriptorProto.enumType, index ])
                )
                enumDescriptor
    in
    List.foldl (\e -> Package.addPackage (basePackageName ++ [ e.dataType ]) { empty | enums = [ e ], originFiles = ctx.originFiles })
        Package.empty
        enums


messageFields : Name.ModuleRef -> List String -> List { a | field : ( FieldName, Field ), oneOfIndex : Int } -> List ( FieldName, Field )
messageFields nestedModuleRef oneOfFieldNames fieldsMeta =
    let
        oneOfFields =
            List.map (oneOfField nestedModuleRef) oneOfFieldNames
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


oneOfField : Name.ModuleRef -> String -> ( FieldName, Field )
oneOfField moduleRef name =
    OneOfField { name = Name.type_ name, package = moduleRef.package ++ [ Name.type_ name ], rootPackage = moduleRef.rootPackage }
        |> Tuple.pair (Name.field name)


oneOfFieldPackage :
    Ctx
    -> List Int
    -> List { field : ( FieldName, Field ), oneOfIndex : Int, docs : List String }
    -> Int
    -> String
    -> { oneOfName : String, options : OneOf, docs : List String }
oneOfFieldPackage ctx messageSourceCodePath fields index name =
    List.filter (\field -> field.oneOfIndex == index) fields
        |> List.filterMap
            (\o ->
                let
                    ( fieldName, field ) =
                        o.field
                in
                case field of
                    NormalField fieldNumber _ type_ ->
                        Just
                            ( { fieldNumber = fieldNumber
                              , fieldType = type_
                              , fieldName = fieldName
                              , dataType = Name.type_ fieldName
                              }
                            , o.docs
                            )

                    _ ->
                        Nothing
            )
        |> (\optionsAndDocs ->
                { oneOfName = Name.type_ name
                , options = List.map Tuple.first optionsAndDocs
                , docs =
                    sourceDocumentation ctx (messageSourceCodePath ++ [ fieldNumbersDescriptorProto.oneofDecl, index ])
                        ++ ("## Options"
                                :: List.concatMap
                                    (\( option, docs ) ->
                                        ("### " ++ option.dataType) :: docs
                                    )
                                    optionsAndDocs
                           )
                }
           )


handleMessage : Name.Ref -> TypeRefs -> String -> FieldType
handleMessage parentRef typeRefs messageName =
    let
        ref =
            Name.absoluteRef messageName
    in
    Embedded
        { dataType = ref.name
        , moduleName = ref.package
        , rootModuleName = ref.rootPackage
        , typeKind = calcTypeKind typeRefs parentRef ref
        }


calcTypeKind : TypeRefs -> Name.Ref -> Name.Ref -> Model.TypeKind
calcTypeKind typeRefs parentRef ref =
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
                                    nextRef == refToKey parentRef

                                else
                                    isRecursiveHelper (Set.insert currRef encounteredTypes) nextRef
                            )
    in
    if isRecursiveHelper (Set.singleton <| refToKey parentRef) (refToKey ref) then
        Model.Type

    else
        Model.Alias


fieldType : TypeRefs -> Name.Ref -> FieldDescriptorProto -> Res FieldType
fieldType typeRefs parentRef descriptor =
    case descriptor.type_ of
        Type.TYPEDOUBLE ->
            Ok <| Primitive Prim_Double <| defaultNumber descriptor

        Type.TYPEFLOAT ->
            Ok <| Primitive Prim_Float <| defaultNumber descriptor

        Type.TYPEINT64 ->
            Ok <| Primitive (Prim_Int64 Int_) <| defaultInt64 descriptor

        Type.TYPEINT32 ->
            Ok <| Primitive (Prim_Int32 Int_) <| defaultNumber descriptor

        Type.TYPEUINT64 ->
            Ok <| Primitive (Prim_Int64 UInt) <| defaultInt64 descriptor

        Type.TYPEUINT32 ->
            Ok <| Primitive (Prim_Int32 UInt) <| defaultNumber descriptor

        Type.TYPEFIXED64 ->
            Ok <| Primitive (Prim_Int64 Fixed) <| defaultInt64 descriptor

        Type.TYPEFIXED32 ->
            Ok <| Primitive (Prim_Int32 Fixed) <| defaultNumber descriptor

        Type.TYPESFIXED64 ->
            Ok <| Primitive (Prim_Int64 SFixed) <| defaultInt64 descriptor

        Type.TYPESFIXED32 ->
            Ok <| Primitive (Prim_Int32 SFixed) <| defaultNumber descriptor

        Type.TYPESINT64 ->
            Ok <| Primitive (Prim_Int64 SInt) <| defaultInt64 descriptor

        Type.TYPESINT32 ->
            Ok <| Primitive (Prim_Int32 SInt) <| defaultNumber descriptor

        Type.TYPEBOOL ->
            Ok <| Primitive Prim_Bool <| defaultBool descriptor

        Type.TYPESTRING ->
            Ok <| Primitive Prim_String <| defaultString descriptor

        Type.TYPEGROUP ->
            Ok <| handleMessage parentRef typeRefs descriptor.typeName

        Type.TYPEMESSAGE ->
            Ok <| handleMessage parentRef typeRefs descriptor.typeName

        Type.TYPEBYTES ->
            Ok <| Primitive Prim_Bytes <| defaultBytes descriptor

        Type.TYPEENUM ->
            let
                ref =
                    Name.absoluteRef descriptor.typeName
            in
            Ok <| Enumeration ref



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


cardinality : Label -> Cardinality
cardinality value =
    case value of
        Label.LABELOPTIONAL ->
            Optional

        Label.LABELREQUIRED ->
            Required

        Label.LABELREPEATED ->
            Repeated

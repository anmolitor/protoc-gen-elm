module Generator.Message exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import List.NonEmpty as NonEmpty
import Mapper.Name
import Meta.Basics
import Meta.Decode
import Meta.Encode
import Meta.JsonDecode
import Meta.JsonEncode
import Meta.Type
import Model exposing (Cardinality(..), DataType, Field(..), FieldName, FieldType(..), IntFlavor(..), Map, Message, Primitive(..), TypeKind(..))
import Options exposing (Options)


reexportAST : Options -> ModuleName -> ModuleName -> Message -> List C.Declaration
reexportAST options internalsModule moduleName msg =
    let
        documentation =
            if List.isEmpty msg.docs then
                messageDocumentation msg.dataType

            else
                Common.renderDocs msg.docs

        messageAsFieldType =
            Embedded { typeKind = msg.typeKind, dataType = msg.dataType, moduleName = moduleName, rootModuleName = moduleName }

        type_ =
            C.aliasDecl (Just documentation) msg.dataType [] <|
                C.fqTyped internalsModule (Mapper.Name.internalize ( moduleName, msg.dataType )) []

        encoder =
            C.valDecl (Just <| Common.encoderDocumentation msg.dataType)
                (Just <| Meta.Encode.encoder (C.typed msg.dataType []))
                (Common.encoderName msg.dataType)
                (C.fqVal internalsModule <| Common.encoderName <| Mapper.Name.internalize ( moduleName, msg.dataType ))

        jsonEncoder =
            C.valDecl (Just <| Common.jsonEncoderDocumentation msg.dataType)
                (Just <| Meta.JsonEncode.encoder (C.typed msg.dataType []))
                (Common.jsonEncoderName msg.dataType)
                (C.fqVal internalsModule <| Common.jsonEncoderName <| Mapper.Name.internalize ( moduleName, msg.dataType ))

        decoder =
            C.valDecl (Just <| Common.decoderDocumentation msg.dataType)
                (Just <| Meta.Decode.decoder (C.typed msg.dataType []))
                (Common.decoderName msg.dataType)
                (C.fqVal internalsModule <| Common.decoderName <| Mapper.Name.internalize ( moduleName, msg.dataType ))

        jsonDecoder =
            C.valDecl (Just <| Common.decoderDocumentation msg.dataType)
                (Just <| Meta.JsonDecode.decoder (C.typed msg.dataType []))
                (Common.jsonDecoderName msg.dataType)
                (C.fqVal internalsModule <| Common.jsonDecoderName <| Mapper.Name.internalize ( moduleName, msg.dataType ))

        default =
            C.valDecl (Just <| Common.defaultDocumentation msg.dataType)
                (Just <| C.typed msg.dataType [])
                (Common.defaultName msg.dataType)
                (C.fqVal internalsModule <| Common.defaultName <| Mapper.Name.internalize ( moduleName, msg.dataType ))

        fieldNumbersDecl : C.Declaration
        fieldNumbersDecl =
            C.valDecl (Just <| Common.fieldNumbersDocumentation msg.dataType)
                (Just <| C.recordAnn <| List.map (Tuple.mapBoth .protoName fieldNumberTypeForField) msg.fields)
                (Common.fieldNumbersName msg.dataType)
                (C.fqVal internalsModule <| Common.fieldNumbersName <| Mapper.Name.internalize ( moduleName, msg.dataType ))
    in
    [ type_, encoder, decoder, default, fieldNumbersDecl ]
        ++ fieldTypeDeclarationsReexport internalsModule messageAsFieldType
        ++ List.concatMap (fieldDeclarationsReexport internalsModule) msg.fields
        ++ (if options.json == Options.All || options.json == Options.Encode || options.grpcDevTools then
                [ jsonEncoder ]

            else
                []
           )
        ++ (if options.json == Options.All || options.json == Options.Decode then
                [ jsonDecoder ]

            else
                []
           )


toAST : Options -> Message -> List C.Declaration
toAST options msg =
    let
        type_ : C.Declaration
        type_ =
            C.aliasDecl (Just <| messageDocumentation msg.dataType)
                msg.dataType
                []
                (C.recordAnn <| List.map (Tuple.mapBoth .protoName fieldToTypeAnnotation) msg.fields)

        encoder : C.Declaration
        encoder =
            C.funDecl (Just <| Common.encoderDocumentation msg.dataType)
                (Just <| Meta.Encode.encoder (C.typed msg.dataType []))
                (Common.encoderName msg.dataType)
                [ if msg.fields == [] then
                    C.allPattern

                  else
                    C.varPattern "value"
                ]
                (Meta.Encode.message
                    (List.map toEncoder msg.fields)
                )

        isAnyFieldAOneOfField =
            List.any (Tuple.second >> isOneOfField) msg.fields

        jsonEncoder : C.Declaration
        jsonEncoder =
            C.funDecl (Just <| Common.jsonEncoderDocumentation msg.dataType)
                (Just <| Meta.JsonEncode.encoder (C.typed msg.dataType []))
                (Common.jsonEncoderName msg.dataType)
                [ if msg.fields == [] then
                    C.allPattern

                  else
                    C.varPattern "value"
                ]
                -- Custom JSON encoders for some well-known types
                (case msg.dataType of
                    "Proto__Google__Protobuf__Timestamp" ->
                        C.apply [ C.fqFun [ "Protobuf", "Utils", "Timestamp" ] "timestampJsonEncoder", C.val "value" ]

                    "Proto__Google__Protobuf__BoolValue" ->
                        C.apply [ Meta.JsonEncode.forPrimitive Prim_Bool, C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__BytesValue" ->
                        C.apply [ Meta.JsonEncode.forPrimitive Prim_Bytes, C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__DoubleValue" ->
                        C.apply [ Meta.JsonEncode.forPrimitive Prim_Double, C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__FloatValue" ->
                        C.apply [ Meta.JsonEncode.forPrimitive Prim_Float, C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__Int32Value" ->
                        C.apply [ Meta.JsonEncode.forPrimitive (Prim_Int32 Int_), C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__Int64Value" ->
                        C.apply [ Meta.JsonEncode.forPrimitive (Prim_Int64 Int_), C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__StringValue" ->
                        C.apply [ Meta.JsonEncode.forPrimitive Prim_String, C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__UInt32Value" ->
                        C.apply [ Meta.JsonEncode.forPrimitive (Prim_Int32 UInt), C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__UInt64Value" ->
                        C.apply [ Meta.JsonEncode.forPrimitive (Prim_Int64 UInt), C.access (C.val "value") "value" ]

                    "Proto__Google__Protobuf__ListValue" ->
                        C.apply (List.map (Tuple.second >> fieldToJsonEncoder) msg.fields ++ [ C.access (C.val "value") "values" ])

                    "Proto__Google__Protobuf__Struct" ->
                        C.apply (List.map (Tuple.second >> fieldToJsonEncoder) msg.fields ++ [ C.access (C.val "value") "fields" ])

                    _ ->
                        C.applyBinOp Meta.JsonEncode.object
                            C.pipel
                            (if isAnyFieldAOneOfField then
                                C.apply [ C.fqFun [ "List" ] "concat", C.list (List.map (toJsonEncoder isAnyFieldAOneOfField) msg.fields) ]

                             else
                                C.list (List.map (toJsonEncoder isAnyFieldAOneOfField) msg.fields)
                            )
                )

        decoder : C.Declaration
        decoder =
            C.valDecl (Just <| Common.decoderDocumentation msg.dataType)
                (Just <| Meta.Decode.decoder (C.typed msg.dataType []))
                (Common.decoderName msg.dataType)
                (C.apply
                    [ Meta.Decode.message
                    , C.val <| Common.defaultName msg.dataType
                    , C.list <| List.map toDecoder msg.fields
                    ]
                )

        jsonDecoder : C.Declaration
        jsonDecoder =
            C.valDecl (Just <| Common.jsonDecoderDocumentation msg.dataType)
                (Just <| Meta.JsonDecode.decoder (C.typed msg.dataType []))
                (Common.jsonDecoderName msg.dataType)
                (case msg.dataType of
                    "Proto__Google__Protobuf__Timestamp" ->
                        C.fqFun [ "Protobuf", "Utils", "Timestamp" ] "timestampJsonDecoder"

                    "Proto__Google__Protobuf__BoolValue" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive Prim_Bool ]

                    "Proto__Google__Protobuf__BytesValue" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive Prim_Bytes ]

                    "Proto__Google__Protobuf__DoubleValue" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive Prim_Double ]

                    "Proto__Google__Protobuf__FloatValue" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive Prim_Float ]

                    "Proto__Google__Protobuf__Int32Value" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive (Prim_Int32 Int_) ]

                    "Proto__Google__Protobuf__Int64Value" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive (Prim_Int64 Int_) ]

                    "Proto__Google__Protobuf__StringValue" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive Prim_String ]

                    "Proto__Google__Protobuf__UInt32Value" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive (Prim_Int32 UInt) ]

                    "Proto__Google__Protobuf__UInt64Value" ->
                        C.apply [ Meta.JsonDecode.map, C.val msg.dataType, Meta.JsonDecode.forPrimitive (Prim_Int64 UInt) ]

                    "Proto__Google__Protobuf__ListValue" ->
                        case msg.fields of
                            [ ( _, NormalField _ Repeated fieldType ) ] ->
                                C.apply
                                    [ Meta.JsonDecode.map
                                    , C.val msg.dataType
                                    , C.apply [ Meta.JsonDecode.list, fieldTypeToJsonDecoder fieldType Repeated ]
                                    ]

                            _ ->
                                C.val "Expected well-known type ListValue to have a single repeated field."

                    "Proto__Google__Protobuf__Struct" ->
                        case msg.fields of
                            [ ( _, MapField _ key value ) ] ->
                                C.apply [ Meta.JsonDecode.map, C.val msg.dataType, mapFieldJsonDecoder key value ]

                            _ ->
                                C.val "Expected well-known type Struct to have a single map type field."

                    _ ->
                        case NonEmpty.fromList msg.fields of
                            Just nonEmptyFields ->
                                Meta.JsonDecode.mapN (C.val msg.dataType) <|
                                    NonEmpty.map toJsonDecoder nonEmptyFields

                            Nothing ->
                                C.apply [ Meta.JsonDecode.succeed, C.record [] ]
                )

        default : C.Declaration
        default =
            C.valDecl (Just <| Common.defaultDocumentation msg.dataType)
                (Just <| C.typed msg.dataType [])
                (Common.defaultName msg.dataType)
                (C.record <| List.map (Tuple.mapBoth .protoName toDefaultValue) msg.fields)

        fieldNumbersDecl : C.Declaration
        fieldNumbersDecl =
            C.valDecl (Just <| Common.fieldNumbersDocumentation msg.dataType)
                (Just <| C.recordAnn <| List.map (Tuple.mapBoth .protoName fieldNumberTypeForField) msg.fields)
                (Common.fieldNumbersName msg.dataType)
                (C.record <| List.map (Tuple.mapBoth .protoName fieldNumberForField) msg.fields)
    in
    [ type_, encoder, decoder, default, fieldNumbersDecl ]
        ++ List.concatMap fieldDeclarations msg.fields
        ++ (if options.json == Options.All || options.json == Options.Encode || options.grpcDevTools then
                [ jsonEncoder ]

            else
                []
           )
        ++ (if options.json == Options.All || options.json == Options.Decode then
                [ jsonDecoder ]

            else
                []
           )


fieldDeclarationsReexport : ModuleName -> ( FieldName, Field ) -> List C.Declaration
fieldDeclarationsReexport internalsModule ( _, field ) =
    case field of
        NormalField _ _ fieldType ->
            fieldTypeDeclarationsReexport internalsModule fieldType

        MapField _ _ _ ->
            []

        OneOfField _ ->
            []


fieldTypeDeclarationsReexport : ModuleName -> FieldType -> List C.Declaration
fieldTypeDeclarationsReexport internalsModule fieldType =
    case fieldType of
        Embedded embedded ->
            case embedded.typeKind of
                Alias ->
                    []

                Type ->
                    let
                        recursiveWrapperName =
                            recursiveDataTypeName embedded.dataType

                        wrappedAnn =
                            C.typed embedded.dataType []

                        recursiveTypeWrapper : C.Declaration
                        recursiveTypeWrapper =
                            C.aliasDecl (Just <| recursiveDataTypeDocumentation embedded.dataType)
                                recursiveWrapperName
                                []
                                (C.fqTyped internalsModule
                                    (Mapper.Name.internalize
                                        ( embedded.moduleName, recursiveDataTypeName embedded.dataType )
                                    )
                                    []
                                )

                        wrapper : C.Declaration
                        wrapper =
                            C.valDecl (Just <| recursiveWrapDocumentation embedded.dataType)
                                (Just <| C.funAnn wrappedAnn (C.typed recursiveWrapperName []))
                                (recursiveWrapName embedded.dataType)
                                (C.fqVal internalsModule
                                    (Mapper.Name.internalize
                                        ( embedded.moduleName, recursiveDataTypeName embedded.dataType )
                                    )
                                )

                        unwrapper : C.Declaration
                        unwrapper =
                            C.valDecl (Just <| recursiveUnwrapDocumentation embedded.dataType)
                                (Just <| C.funAnn (C.typed recursiveWrapperName []) wrappedAnn)
                                (recursiveUnwrapName embedded.dataType)
                                (C.fqVal internalsModule <| recursiveUnwrapName <| Mapper.Name.internalize ( embedded.moduleName, embedded.dataType ))
                    in
                    [ recursiveTypeWrapper, wrapper, unwrapper ]

        _ ->
            []


mapComment : Map -> C.Comment C.DocComment
mapComment map =
    C.emptyDocComment
        |> C.markdown ("Dict for " ++ map.dataType)


getter : FieldName -> C.Expression
getter fieldName =
    C.accessFun <| "." ++ fieldName.protoName


fieldDeclarations : ( FieldName, Field ) -> List C.Declaration
fieldDeclarations ( _, field ) =
    case field of
        NormalField _ _ fieldType ->
            fieldTypeDeclarations fieldType

        MapField _ _ _ ->
            []

        OneOfField _ ->
            []


fieldTypeDeclarations : FieldType -> List C.Declaration
fieldTypeDeclarations fieldType =
    case fieldType of
        Embedded embedded ->
            case embedded.typeKind of
                Alias ->
                    []

                Type ->
                    let
                        recursiveWrapperName =
                            recursiveDataTypeName (Mapper.Name.internalize ( embedded.moduleName, embedded.dataType ))

                        wrappedAnn =
                            C.typed (Mapper.Name.internalize ( embedded.moduleName, embedded.dataType )) []

                        recursiveTypeWrapper : C.Declaration
                        recursiveTypeWrapper =
                            C.customTypeDecl (Just <| recursiveDataTypeDocumentation embedded.dataType) recursiveWrapperName [] [ ( recursiveWrapperName, [ wrappedAnn ] ) ]

                        unwrapper : C.Declaration
                        unwrapper =
                            C.funDecl (Just <| recursiveUnwrapDocumentation embedded.dataType)
                                (Just <| C.funAnn (C.typed recursiveWrapperName []) wrappedAnn)
                                (recursiveUnwrapName <| Mapper.Name.internalize ( embedded.moduleName, embedded.dataType ))
                                [ C.namedPattern recursiveWrapperName [ C.varPattern "wrapped" ] ]
                                (C.val "wrapped")
                    in
                    [ recursiveTypeWrapper, unwrapper ]

        _ ->
            []


fieldTypeToDefaultValueRequired : FieldType -> C.Expression
fieldTypeToDefaultValueRequired fieldType =
    case fieldType of
        Primitive _ defaultValue ->
            defaultValue

        Embedded e ->
            let
                name =
                    Mapper.Name.internalize ( e.moduleName, e.dataType )

                default =
                    C.fqVal (Common.internalsModule e.rootModuleName) (Common.defaultName name)
            in
            case e.typeKind of
                Alias ->
                    default

                Type ->
                    C.apply
                        [ C.fqVal
                            (Common.internalsModule e.rootModuleName)
                            (recursiveDataTypeName name)
                        , default
                        ]

        Enumeration enum ->
            C.fqVal (enum.package ++ [ enum.name ]) (Common.defaultName enum.name)


fieldTypeToDefaultValue : FieldType -> C.Expression
fieldTypeToDefaultValue fieldType =
    case fieldType of
        Primitive _ defaultValue ->
            defaultValue

        Embedded _ ->
            Meta.Basics.nothing

        Enumeration enum ->
            C.fqVal (enum.package ++ [ enum.name ]) (Common.defaultName enum.name)


toDefaultValue : Field -> C.Expression
toDefaultValue field =
    case field of
        NormalField _ cardinality fieldType ->
            case ( cardinality, fieldType ) of
                ( Proto3Optional, _ ) ->
                    Meta.Basics.nothing

                ( Optional, Primitive _ defaultValue ) ->
                    defaultValue

                ( Repeated, _ ) ->
                    C.list []

                ( Required, Primitive _ defaultValue ) ->
                    defaultValue

                ( Optional, Embedded _ ) ->
                    Meta.Basics.nothing

                ( Required, Embedded e ) ->
                    C.fqVal (Common.internalsModule e.rootModuleName) (Common.defaultName <| Mapper.Name.internalize ( e.moduleName, e.dataType ))
                        |> (\val ->
                                case e.typeKind of
                                    Alias ->
                                        val

                                    Type ->
                                        C.apply
                                            [ C.fqVal
                                                (Common.internalsModule e.rootModuleName)
                                                (recursiveDataTypeName <| Mapper.Name.internalize ( e.moduleName, e.dataType ))
                                            , val
                                            ]
                           )

                ( _, Enumeration enum ) ->
                    fieldTypeToDefaultValue (Enumeration enum)

        MapField _ _ _ ->
            C.fqFun [ "Dict" ] "empty"

        OneOfField _ ->
            Meta.Basics.nothing


toDecoder : ( FieldName, Field ) -> C.Expression
toDecoder ( fieldName, field ) =
    case field of
        NormalField number cardinality fieldType ->
            case cardinality of
                Optional ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , Common.setter fieldName
                        ]

                Proto3Optional ->
                    C.apply
                        [ Meta.Decode.optional
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , Common.setter fieldName
                        ]

                Required ->
                    C.apply
                        [ Meta.Decode.required
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , Common.setter fieldName
                        ]

                Repeated ->
                    C.apply
                        [ Meta.Decode.repeated
                        , C.int number
                        , fieldTypeToDecoder fieldType cardinality
                        , getter fieldName
                        , Common.setter fieldName
                        ]

        MapField number (Primitive ((Prim_Int64 _) as prim) defaultValue) value ->
            -- special case for int64 types since they are not comparable -> we use the unwrapped (int, int) representation instead
            C.apply
                [ Meta.Decode.mapped
                , C.int number
                , C.tuple [ C.apply [ C.fqFun [ "Protobuf", "Types", "Int64" ] "toInts", defaultValue ], fieldTypeToDefaultValue value ]
                , C.parens <| C.apply [ Meta.Decode.map, C.fqFun [ "Protobuf", "Types", "Int64" ] "toInts", Meta.Decode.forPrimitive prim ]
                , fieldTypeToDecoder value Required
                , C.accessFun <| "." ++ fieldName.protoName
                , Common.setter fieldName
                ]

        MapField number key value ->
            C.apply
                [ Meta.Decode.mapped
                , C.int number
                , C.tuple [ fieldTypeToDefaultValue key, fieldTypeToDefaultValueRequired value ]
                , fieldTypeToDecoder key Optional
                , fieldTypeToDecoder value Required
                , C.accessFun <| "." ++ fieldName.protoName
                , Common.setter fieldName
                ]

        OneOfField ref ->
            C.apply
                [ C.fqFun (Common.internalsModule ref.rootPackage) (Common.decoderName <| Mapper.Name.internalize ( ref.package, ref.name ))
                , Common.setter fieldName
                ]


toJsonDecoder : ( FieldName, Field ) -> C.Expression
toJsonDecoder ( fieldName, field ) =
    case field of
        NormalField _ cardinality fieldType ->
            case cardinality of
                Optional ->
                    C.pipe
                        (C.apply
                            [ Meta.JsonDecode.maybe
                            , C.parens <|
                                C.apply
                                    [ Meta.JsonDecode.field
                                    , C.string fieldName.jsonName
                                    , fieldTypeToJsonDecoder fieldType cardinality
                                    ]
                            ]
                        )
                        [ C.apply [ Meta.JsonDecode.map, C.parens <| C.apply [ Meta.Basics.withDefault, fieldTypeToDefaultValue fieldType ] ] ]

                Proto3Optional ->
                    C.apply
                        [ Meta.JsonDecode.maybe
                        , C.parens <|
                            C.apply
                                [ Meta.JsonDecode.field
                                , C.string fieldName.jsonName
                                , fieldTypeToJsonDecoder fieldType cardinality
                                ]
                        ]

                Required ->
                    C.apply
                        [ Meta.JsonDecode.field
                        , C.string fieldName.jsonName
                        , fieldTypeToJsonDecoder fieldType cardinality
                        ]

                Repeated ->
                    C.apply
                        [ Meta.JsonDecode.field
                        , C.string fieldName.jsonName
                        , C.apply [ Meta.JsonDecode.list, fieldTypeToJsonDecoder fieldType cardinality ]
                        ]

        MapField _ key value ->
            C.pipe
                (C.apply
                    [ Meta.JsonDecode.field
                    , C.string fieldName.jsonName
                    , mapFieldJsonDecoder key value
                    ]
                )
                [ C.apply [ Meta.JsonDecode.maybe ]
                , C.apply
                    [ Meta.JsonDecode.map
                    , C.parens <|
                        C.apply [ Meta.Basics.withDefault, C.fqFun [ "Dict" ] "empty" ]
                    ]
                ]

        OneOfField ref ->
            C.apply
                [ Meta.JsonDecode.field
                , C.string fieldName.jsonName
                , C.fqFun (Common.internalsModule ref.rootPackage) (Common.jsonDecoderName <| Mapper.Name.internalize ( ref.package, ref.name ))
                ]


mapFieldJsonDecoder : FieldType -> FieldType -> C.Expression
mapFieldJsonDecoder key value =
    case key of
        Primitive Prim_String _ ->
            -- special case for String since no additional mapping step is required
            Meta.JsonDecode.stringKeyDict (fieldTypeToJsonDecoder value Required)

        Primitive primKey _ ->
            Meta.JsonDecode.dict (Meta.JsonDecode.primitiveFromMapKey primKey) (fieldTypeToJsonDecoder value Required)

        _ ->
            C.val "Non Primitive Keys are not supported."


embeddedDecoder : { dataType : DataType, moduleName : C.ModuleName, rootModuleName : C.ModuleName, typeKind : TypeKind } -> C.Expression
embeddedDecoder e =
    (case e.typeKind of
        Alias ->
            identity

        Type ->
            C.parens
                << C.applyBinOp
                    (C.apply
                        [ Meta.Decode.map
                        , C.fqVal (Common.internalsModule e.rootModuleName) <|
                            recursiveDataTypeName <|
                                Mapper.Name.internalize ( e.moduleName, e.dataType )
                        ]
                    )
                    C.pipel
                << C.applyBinOp Meta.Decode.lazy C.pipel
                << C.lambda [ C.allPattern ]
    )
        (C.fqFun (Common.internalsModule e.rootModuleName) (Common.decoderName <| Mapper.Name.internalize ( e.moduleName, e.dataType )))


embeddedJsonDecoder : { dataType : DataType, moduleName : C.ModuleName, rootModuleName : C.ModuleName, typeKind : TypeKind } -> C.Expression
embeddedJsonDecoder e =
    (case e.typeKind of
        Alias ->
            identity

        Type ->
            C.parens
                << C.applyBinOp
                    (C.apply
                        [ Meta.JsonDecode.map
                        , C.fqVal (Common.internalsModule e.rootModuleName) <|
                            recursiveDataTypeName <|
                                Mapper.Name.internalize ( e.moduleName, e.dataType )
                        ]
                    )
                    C.pipel
                << C.applyBinOp Meta.JsonDecode.lazy C.pipel
                << C.lambda [ C.allPattern ]
    )
        (C.fqFun (Common.internalsModule e.rootModuleName) (Common.jsonDecoderName <| Mapper.Name.internalize ( e.moduleName, e.dataType )))


fieldTypeToDecoder : FieldType -> Cardinality -> C.Expression
fieldTypeToDecoder fieldType cardinality =
    case ( cardinality, fieldType ) of
        ( Proto3Optional, Primitive dataType _ ) ->
            C.parens
                (C.apply
                    [ Meta.Decode.map
                    , Meta.Basics.just
                    , Meta.Decode.forPrimitive dataType
                    ]
                )

        ( _, Primitive dataType _ ) ->
            Meta.Decode.forPrimitive dataType

        ( Required, Embedded e ) ->
            embeddedDecoder e

        ( Repeated, Embedded e ) ->
            embeddedDecoder e

        ( _, Embedded e ) ->
            C.parens
                (C.apply
                    [ Meta.Decode.map
                    , Meta.Basics.just
                    , embeddedDecoder e
                    ]
                )

        ( Proto3Optional, Enumeration enum ) ->
            C.parens
                (C.apply
                    [ Meta.Decode.map
                    , Meta.Basics.just
                    , C.fqFun
                        (enum.package ++ [ enum.name ])
                        (Common.decoderName enum.name)
                    ]
                )

        ( _, Enumeration enum ) ->
            C.fqFun (enum.package ++ [ enum.name ])
                (Common.decoderName enum.name)


fieldTypeToJsonDecoder : FieldType -> Cardinality -> C.Expression
fieldTypeToJsonDecoder fieldType cardinality =
    case ( cardinality, fieldType ) of
        ( _, Primitive dataType _ ) ->
            Meta.JsonDecode.forPrimitive dataType

        ( Required, Embedded e ) ->
            embeddedJsonDecoder e

        ( Repeated, Embedded e ) ->
            embeddedJsonDecoder e

        ( _, Embedded e ) ->
            C.parens
                (C.apply
                    [ Meta.JsonDecode.map
                    , Meta.Basics.just
                    , embeddedJsonDecoder e
                    ]
                )

        ( _, Enumeration enum ) ->
            C.fqFun (enum.package ++ [ enum.name ])
                (Common.jsonDecoderName enum.name)


toEncoder : ( FieldName, Field ) -> C.Expression
toEncoder ( fieldName, field ) =
    case field of
        NormalField number cardinality fieldType ->
            C.tuple [ C.int number, C.apply [ fieldTypeToEncoder cardinality fieldType, C.access (C.val "value") fieldName.protoName ] ]

        MapField number (Primitive ((Prim_Int64 _) as prim) _) value ->
            -- special case for int64 types since they are not comparable -> we use the unwrapped (int, int) representation instead
            C.tuple
                [ C.int number
                , C.apply
                    [ Meta.Encode.dict
                    , C.lambda [ C.tuplePattern [ C.varPattern "upper", C.varPattern "lower" ] ]
                        (C.apply
                            [ Meta.Encode.forPrimitive prim
                            , C.parens <| C.apply [ C.fqFun [ "Protobuf", "Types", "Int64" ] "fromInts", C.val "upper", C.val "lower" ]
                            ]
                        )
                    , fieldTypeToEncoder Required value
                    , C.access (C.val "value") fieldName.protoName
                    ]
                ]

        MapField number key value ->
            C.tuple
                [ C.int number
                , C.apply
                    [ Meta.Encode.dict
                    , fieldTypeToEncoder Optional key
                    , fieldTypeToEncoder Required value
                    , C.access (C.val "value") fieldName.protoName
                    ]
                ]

        OneOfField ref ->
            C.apply
                [ C.fqFun (Common.internalsModule ref.rootPackage)
                    (Common.encoderName <|
                        Mapper.Name.internalize ( ref.package, ref.name )
                    )
                , C.access (C.val "value") fieldName.protoName
                ]


toJsonEncoder : Bool -> ( FieldName, Field ) -> C.Expression
toJsonEncoder isAnyFieldAOneOfField ( fieldName, field ) =
    case ( field, isAnyFieldAOneOfField ) of
        ( OneOfField _, _ ) ->
            C.apply [ fieldToJsonEncoder field, C.access (C.val "value") fieldName.protoName ]

        ( _, True ) ->
            C.list [ C.tuple [ C.string fieldName.jsonName, C.apply [ fieldToJsonEncoder field, C.access (C.val "value") fieldName.protoName ] ] ]

        ( _, False ) ->
            C.tuple [ C.string fieldName.jsonName, C.apply [ fieldToJsonEncoder field, C.access (C.val "value") fieldName.protoName ] ]


isOneOfField : Field -> Bool
isOneOfField field =
    case field of
        OneOfField _ ->
            True

        _ ->
            False


fieldToJsonEncoder : Field -> C.Expression
fieldToJsonEncoder field =
    case field of
        NormalField _ cardinality fieldType ->
            fieldTypeToJsonEncoder cardinality fieldType

        MapField _ key value ->
            C.apply
                [ Meta.JsonEncode.dict
                , fieldTypeToJsonMapKey key
                , fieldTypeToJsonEncoder Required value
                ]

        OneOfField ref ->
            C.fqFun (Common.internalsModule ref.rootPackage)
                (Common.jsonEncoderName <|
                    Mapper.Name.internalize ( ref.package, ref.name )
                )


embeddedEncoder : { dataType : DataType, moduleName : C.ModuleName, rootModuleName : C.ModuleName, typeKind : TypeKind } -> C.Expression
embeddedEncoder e =
    (case e.typeKind of
        Alias ->
            identity

        Type ->
            C.parens
                << C.applyBinOp
                    (C.fqFun (Common.internalsModule e.rootModuleName) <|
                        recursiveUnwrapName <|
                            Mapper.Name.internalize ( e.moduleName, e.dataType )
                    )
                    C.composer
    )
        (C.fqFun (Common.internalsModule e.rootModuleName) (Common.encoderName <| Mapper.Name.internalize ( e.moduleName, e.dataType )))


embeddedJsonEncoder : { dataType : DataType, moduleName : C.ModuleName, rootModuleName : C.ModuleName, typeKind : TypeKind } -> C.Expression
embeddedJsonEncoder e =
    (case e.typeKind of
        Alias ->
            identity

        Type ->
            C.parens
                << C.applyBinOp
                    (C.fqFun (Common.internalsModule e.rootModuleName) <|
                        recursiveUnwrapName <|
                            Mapper.Name.internalize ( e.moduleName, e.dataType )
                    )
                    C.composer
    )
        (C.fqFun (Common.internalsModule e.rootModuleName) (Common.jsonEncoderName <| Mapper.Name.internalize ( e.moduleName, e.dataType )))


fieldTypeToEncoder : Cardinality -> FieldType -> C.Expression
fieldTypeToEncoder cardinality fieldType =
    case ( cardinality, fieldType ) of
        ( Proto3Optional, Primitive dataType _ ) ->
            C.parens <|
                C.applyBinOp
                    (C.apply [ Meta.Basics.mapMaybe, Meta.Encode.forPrimitive dataType ])
                    C.composer
                    (C.apply [ Meta.Basics.withDefault, Meta.Encode.none ])

        ( Optional, Primitive dataType _ ) ->
            Meta.Encode.forPrimitive dataType

        ( Required, Primitive dataType _ ) ->
            Meta.Encode.forPrimitive dataType

        ( Required, Embedded e ) ->
            embeddedEncoder e

        ( Required, Enumeration enum ) ->
            C.fqFun (enum.package ++ [ enum.name ]) (Common.encoderName enum.name)

        ( Repeated, Primitive dataType _ ) ->
            C.apply [ Meta.Encode.list, Meta.Encode.forPrimitive dataType ]

        ( Repeated, Embedded e ) ->
            C.apply [ Meta.Encode.list, embeddedEncoder e ]

        ( Repeated, Enumeration enum ) ->
            C.apply
                [ Meta.Encode.list
                , C.fqFun (enum.package ++ [ enum.name ])
                    (Common.encoderName enum.name)
                ]

        ( _, Embedded e ) ->
            C.parens <|
                C.applyBinOp
                    (C.apply [ Meta.Basics.mapMaybe, embeddedEncoder e ])
                    C.composer
                    (C.apply [ Meta.Basics.withDefault, Meta.Encode.none ])

        ( Proto3Optional, Enumeration enum ) ->
            C.parens <|
                C.applyBinOp
                    (C.apply
                        [ Meta.Basics.mapMaybe
                        , C.fqFun (enum.package ++ [ enum.name ]) (Common.encoderName enum.name)
                        ]
                    )
                    C.composer
                    (C.apply [ Meta.Basics.withDefault, Meta.Encode.none ])

        ( Optional, Enumeration enum ) ->
            C.fqFun (enum.package ++ [ enum.name ]) (Common.encoderName enum.name)


fieldTypeToJsonMapKey : FieldType -> C.Expression
fieldTypeToJsonMapKey fieldType =
    case fieldType of
        Primitive primitive _ ->
            Meta.JsonEncode.primitiveToMapKey primitive

        _ ->
            C.string "ERROR: This should not happen. Map keys are supposed to be primitive only."


fieldTypeToJsonEncoder : Cardinality -> FieldType -> C.Expression
fieldTypeToJsonEncoder cardinality fieldType =
    case ( cardinality, fieldType ) of
        ( Proto3Optional, Primitive dataType _ ) ->
            C.parens <|
                C.applyBinOp
                    (C.apply [ Meta.Basics.mapMaybe, Meta.JsonEncode.forPrimitive dataType ])
                    C.composer
                    (C.apply [ Meta.Basics.withDefault, Meta.JsonEncode.null ])

        ( Optional, Primitive dataType _ ) ->
            Meta.JsonEncode.forPrimitive dataType

        ( Required, Primitive dataType _ ) ->
            Meta.JsonEncode.forPrimitive dataType

        ( Required, Embedded e ) ->
            embeddedJsonEncoder e

        ( Required, Enumeration enum ) ->
            C.fqFun (enum.package ++ [ enum.name ]) (Common.jsonEncoderName enum.name)

        ( Repeated, Primitive dataType _ ) ->
            C.apply [ Meta.JsonEncode.list, Meta.JsonEncode.forPrimitive dataType ]

        ( Repeated, Embedded e ) ->
            C.apply [ Meta.JsonEncode.list, embeddedJsonEncoder e ]

        ( Repeated, Enumeration enum ) ->
            C.apply
                [ Meta.JsonEncode.list
                , C.fqFun (enum.package ++ [ enum.name ])
                    (Common.jsonEncoderName enum.name)
                ]

        ( _, Embedded e ) ->
            C.parens <|
                C.applyBinOp
                    (C.apply [ Meta.Basics.mapMaybe, embeddedJsonEncoder e ])
                    C.composer
                    (C.apply [ Meta.Basics.withDefault, Meta.JsonEncode.null ])

        ( Proto3Optional, Enumeration enum ) ->
            C.parens <|
                C.applyBinOp
                    (C.apply
                        [ Meta.Basics.mapMaybe
                        , C.fqFun (enum.package ++ [ enum.name ]) (Common.jsonEncoderName enum.name)
                        ]
                    )
                    C.composer
                    (C.apply [ Meta.Basics.withDefault, Meta.JsonEncode.null ])

        ( Optional, Enumeration enum ) ->
            C.fqFun (enum.package ++ [ enum.name ]) (Common.jsonEncoderName enum.name)


fieldTypeToTypeAnnotation : FieldType -> C.TypeAnnotation
fieldTypeToTypeAnnotation fieldType =
    case fieldType of
        Primitive dataType _ ->
            Meta.Type.forPrimitive dataType

        Embedded e ->
            C.fqTyped (Common.internalsModule e.rootModuleName)
                (Mapper.Name.internalize
                    ( e.moduleName
                    , case e.typeKind of
                        Alias ->
                            e.dataType

                        Type ->
                            recursiveDataTypeName e.dataType
                    )
                )
                []

        Enumeration enum ->
            C.fqTyped (enum.package ++ [ enum.name ]) enum.name []


fieldToTypeAnnotation : Field -> C.TypeAnnotation
fieldToTypeAnnotation field =
    let
        cardinalityModifier cardinality fieldType =
            case ( cardinality, fieldType ) of
                ( Optional, Primitive _ _ ) ->
                    identity

                ( Optional, Enumeration _ ) ->
                    identity

                ( Required, _ ) ->
                    identity

                ( Optional, _ ) ->
                    C.maybeAnn

                ( Repeated, _ ) ->
                    C.listAnn

                ( Proto3Optional, _ ) ->
                    C.maybeAnn
    in
    case field of
        NormalField _ cardinality fieldType ->
            cardinalityModifier cardinality
                fieldType
                (fieldTypeToTypeAnnotation fieldType)

        MapField _ (Primitive (Prim_Int64 _) _) value ->
            -- special case for int64 types since they are not comparable -> we use the unwrapped (int, int) representation instead
            Meta.Type.dict (C.tupleAnn [ C.intAnn, C.intAnn ])
                (fieldTypeToTypeAnnotation value)

        MapField _ key value ->
            Meta.Type.dict (fieldTypeToTypeAnnotation key)
                (fieldTypeToTypeAnnotation value)

        OneOfField ref ->
            C.maybeAnn <| C.fqTyped (Common.internalsModule ref.rootPackage) (Mapper.Name.internalize ( ref.package, ref.name )) []


fieldNumberTypeForField : Field -> C.TypeAnnotation
fieldNumberTypeForField field =
    case field of
        NormalField _ _ _ ->
            C.intAnn

        MapField _ _ _ ->
            C.intAnn

        OneOfField ref ->
            C.fqTyped
                (Common.internalsModule ref.rootPackage)
                (Common.fieldNumbersTypeName <| Mapper.Name.internalize ( ref.package, ref.name ))
                []


fieldNumberForField : Field -> C.Expression
fieldNumberForField field =
    case field of
        NormalField n _ _ ->
            C.int n

        MapField n _ _ ->
            C.int n

        OneOfField ref ->
            C.fqVal (Common.internalsModule ref.rootPackage) (Common.fieldNumbersName <| Mapper.Name.internalize ( ref.package, ref.name ))


recursiveDataTypeName : String -> String
recursiveDataTypeName wrappedDataType =
    wrappedDataType ++ "_"


recursiveUnwrapName : String -> String
recursiveUnwrapName wrappedDataType =
    "unwrap" ++ wrappedDataType


recursiveWrapName : String -> String
recursiveWrapName wrappedDataType =
    "wrap" ++ wrappedDataType


messageDocumentation : String -> C.Comment C.DocComment
messageDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` message")


oneofDocumentation : String -> C.Comment C.DocComment
oneofDocumentation msgName =
    C.emptyDocComment |> C.markdown ("`" ++ msgName ++ "` options")


recursiveDataTypeDocumentation : String -> C.Comment C.DocComment
recursiveDataTypeDocumentation wrappedDataType =
    C.emptyDocComment
        |> C.markdown
            ("Type wrapper for alias type `"
                ++ wrappedDataType
                ++ "` to avoid unlimited recursion."
            )
        |> C.markdown
            ("For a more in-depth explanation why we need this, read this: " ++ recursiveExplanationLink ++ ".")


recursiveExplanationLink : String
recursiveExplanationLink =
    "https://github.com/elm/compiler/blob/master/hints/recursive-alias.md"


recursiveUnwrapDocumentation : String -> C.Comment C.DocComment
recursiveUnwrapDocumentation wrappedDataType =
    C.emptyDocComment |> C.markdown ("Unwrap a `" ++ wrappedDataType ++ "` from its wrapper `" ++ recursiveDataTypeName wrappedDataType ++ ".`")


recursiveWrapDocumentation : String -> C.Comment C.DocComment
recursiveWrapDocumentation wrappedDataType =
    C.emptyDocComment |> C.markdown ("Wrap a `" ++ wrappedDataType ++ "` into its wrapper `" ++ recursiveDataTypeName wrappedDataType ++ ".`")

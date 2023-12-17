module MapperTest exposing (..)

import Dict
import Elm.CodeGen as C
import Errors exposing (Res)
import Expect
import Mapper
import Mapper.Package as Package exposing (Packages)
import Mapper.Struct exposing (Struct, empty)
import Mapper.Syntax exposing (Syntax(..))
import Model exposing (Cardinality(..), Field(..), FieldType(..), Primitive(..), TypeKind(..))
import Options
import Proto.Google.Protobuf exposing (DescriptorProto, DescriptorProto_, EnumDescriptorProto, EnumValueDescriptorProto, FieldDescriptorProto, FileDescriptorProto, MessageOptions, MethodDescriptorProto, wrapDescriptorProto)
import Proto.Google.Protobuf.FieldDescriptorProto as FieldDescriptorProto
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Mapper"
        [ test "converts a single file descriptor correctly" <|
            \_ ->
                let
                    expected : List ( C.ModuleName, Res Packages )
                    expected =
                        [ ( [ "Proto", "Testpackage" ]
                          , Package.empty
                                |> Package.addPackage
                                    [ "Proto", "Testpackage" ]
                                    { empty
                                        | messages =
                                            [ { dataType = "OtherMsg", fields = [], docs = [] }
                                            , { dataType = "Msg"
                                              , fields =
                                                    [ ( "test"
                                                      , NormalField 0 Optional (Primitive Prim_Bool <| C.val "False")
                                                      )
                                                    , ( "test2"
                                                      , NormalField 0 Optional (Embedded { dataType = "OtherMsg", moduleName = [ "Proto", "Testpackage" ], rootModuleName = [ "Proto", "Testpackage" ], typeKind = Alias })
                                                      )
                                                    , ( "test3"
                                                      , NormalField 0 Optional (Enumeration { name = "AnEnum", package = [ "Proto", "Testpackage", "OtherMsg" ], rootPackage = [ "Proto", "Testpackage" ] })
                                                      )
                                                    , ( "test4"
                                                      , NormalField 0 Optional (Embedded { dataType = "Msg", moduleName = [ "Proto", "Testpackage" ], rootModuleName = [ "Proto", "Testpackage" ], typeKind = Type })
                                                      )
                                                    ]
                                              , docs = []
                                              }
                                            ]
                                        , originFiles = Set.singleton "test.proto"
                                    }
                                |> Package.addPackage [ "Proto", "Testpackage", "OtherMsg" ]
                                    { empty
                                        | enums = [ { dataType = "AnEnum", fields = ( ( 0, "Bla" ), [ ( 2, "Blub" ) ] ), withUnrecognized = True, docs = [] } ]
                                        , originFiles = Set.singleton "test.proto"
                                    }
                                |> Package.addPackage [ "Proto", "Testpackage", "SomeService" ]
                                    { empty
                                        | services =
                                            [ { name = "SomeService"
                                              , package = "testpackage"
                                              , methods =
                                                    [ { name = "SomeMethod"
                                                      , reqType = { name = "OtherMsg", package = [ "Proto", "Testpackage" ], rootPackage = [ "Proto", "Testpackage" ] }
                                                      , resType = { name = "AnEnum", package = [ "Proto", "Testpackage", "OtherMsg" ], rootPackage = [ "Proto", "Testpackage" ] }
                                                      , docs = []
                                                      }
                                                    ]
                                              , docs = []
                                              }
                                            ]
                                        , originFiles = Set.singleton "test.proto"
                                    }
                                |> Ok
                          )
                        ]
                in
                Mapper.mapMain Options.default
                    [ { defaultFileDescriptorProto
                        | name = "test.proto"
                        , package = "testpackage"
                        , messageType =
                            [ { defaultDescriptorProto
                                | name = "OtherMsg"
                                , enumType =
                                    [ { defaultEnumDescriptorProto
                                        | name = "AnEnum"
                                        , value =
                                            [ enumValue "Bla" 0
                                            , enumValue "Blub" 2
                                            ]
                                      }
                                    ]
                              }
                            , { defaultDescriptorProto
                                | name = "Msg"
                                , field =
                                    [ { defaultFieldDescriptorProto | name = "test" }
                                    , { defaultFieldDescriptorProto | name = "test2", type_ = FieldDescriptorProto.toInternalType FieldDescriptorProto.TYPEMESSAGE, typeName = ".testpackage.OtherMsg" }
                                    , { defaultFieldDescriptorProto | name = "test3", type_ = FieldDescriptorProto.toInternalType FieldDescriptorProto.TYPEENUM, typeName = ".testpackage.OtherMsg.AnEnum" }
                                    , { defaultFieldDescriptorProto | name = "test4", type_ = FieldDescriptorProto.toInternalType FieldDescriptorProto.TYPEMESSAGE, typeName = ".testpackage.Msg" }
                                    ]
                              }
                            ]
                        , service =
                            [ { name = "SomeService"
                              , options = Nothing
                              , method =
                                    [ { defaultMethodDescriptorProto
                                        | inputType = ".testpackage.OtherMsg"
                                        , outputType = ".testpackage.OtherMsg.AnEnum"
                                        , name = "SomeMethod"
                                      }
                                    ]
                              }
                            ]
                      }
                    ]
                    |> Expect.equal
                        expected
        , test "converts dependant file descriptors correctly" <|
            \_ ->
                let
                    file1 =
                        { defaultFileDescriptorProto
                            | name = "test.proto"
                            , package = "some.pkg.name"
                            , messageType =
                                [ { defaultDescriptorProto
                                    | name = "Msg"
                                    , nestedType =
                                        [ wrapDescriptorProto { defaultDescriptorProto | name = "OtherMsg" }
                                        ]
                                  }
                                ]
                        }

                    file2 =
                        { defaultFileDescriptorProto
                            | name = "no_package.proto"
                            , messageType =
                                [ { defaultDescriptorProto
                                    | name = "Abc"
                                  }
                                ]
                        }

                    file3 =
                        { defaultFileDescriptorProto
                            | name = "importing.proto"
                            , dependency = [ "test.proto", "no_package.proto" ]
                            , messageType =
                                [ { defaultDescriptorProto
                                    | name = "Msg"
                                    , field =
                                        [ { defaultFieldDescriptorProto | name = "field1", type_ = FieldDescriptorProto.toInternalType FieldDescriptorProto.TYPEMESSAGE, typeName = ".some.pkg.name.Msg.OtherMsg" }
                                        , { defaultFieldDescriptorProto | name = "field2", type_ = FieldDescriptorProto.toInternalType FieldDescriptorProto.TYPEMESSAGE, typeName = ".Abc" }
                                        ]
                                  }
                                ]
                        }
                in
                Mapper.mapMain Options.default [ file1, file2, file3 ]
                    |> List.map (Tuple.second >> Result.withDefault Dict.empty)
                    |> Package.concat
                    |> Expect.equal
                        (Dict.fromList
                            [ ( [ "Proto", "Some", "Pkg", "Name" ]
                              , { empty | messages = [ { dataType = "Msg", fields = [], docs = [] } ], originFiles = Set.singleton "test.proto" }
                              )
                            , ( [ "Proto", "Some", "Pkg", "Name", "Msg" ]
                              , { empty | messages = [ { dataType = "OtherMsg", fields = [], docs = [] } ], originFiles = Set.singleton "test.proto" }
                              )
                            , ( [ "Proto" ]
                              , { empty
                                    | messages =
                                        [ { dataType = "Abc", fields = [], docs = [] }
                                        , { dataType = "Msg"
                                          , fields =
                                                [ ( "field1"
                                                  , NormalField 0
                                                        Optional
                                                        (Embedded
                                                            { dataType = "OtherMsg"
                                                            , moduleName = [ "Proto", "Some", "Pkg", "Name", "Msg" ]
                                                            , rootModuleName = [ "Proto", "Some", "Pkg", "Name" ]
                                                            , typeKind = Alias
                                                            }
                                                        )
                                                  )
                                                , ( "field2"
                                                  , NormalField 0
                                                        Optional
                                                        (Embedded
                                                            { dataType = "Abc"
                                                            , moduleName = [ "Proto" ]
                                                            , rootModuleName = [ "Proto" ]
                                                            , typeKind = Alias
                                                            }
                                                        )
                                                  )
                                                ]
                                          , docs = []
                                          }
                                        ]
                                    , originFiles = Set.fromList [ "importing.proto", "no_package.proto" ]
                                }
                              )
                            ]
                        )
        ]


defaultFileDescriptorProto : FileDescriptorProto
defaultFileDescriptorProto =
    { name = ""
    , package = ""
    , dependency = []
    , publicDependency = []
    , weakDependency = []
    , messageType = []
    , enumType = []
    , service = []
    , extension = []
    , options = Nothing
    , sourceCodeInfo = Nothing
    , syntax = "proto3"
    }


defaultDescriptorProto : DescriptorProto
defaultDescriptorProto =
    { enumType = []
    , extension = []
    , extensionRange = []
    , field = []
    , name = ""
    , nestedType = []
    , oneofDecl = []
    , options = Nothing
    , reservedName = []
    , reservedRange = []
    }


defaultFieldDescriptorProto : FieldDescriptorProto
defaultFieldDescriptorProto =
    { name = ""
    , number = 0
    , label = FieldDescriptorProto.toInternalLabel FieldDescriptorProto.LABELOPTIONAL
    , type_ = FieldDescriptorProto.toInternalType FieldDescriptorProto.TYPEBOOL
    , typeName = ""
    , extendee = ""
    , defaultValue = ""
    , oneofIndex = -1
    , jsonName = ""
    , options = Nothing
    , proto3Optional = False
    }


defaultMethodDescriptorProto : MethodDescriptorProto
defaultMethodDescriptorProto =
    { name = ""
    , inputType = ""
    , outputType = ""
    , options = Nothing
    , clientStreaming = False
    , serverStreaming = False
    }


defaultEnumDescriptorProto : EnumDescriptorProto
defaultEnumDescriptorProto =
    { name = "", value = [], options = Nothing, reservedName = [], reservedRange = [] }


enumValue : String -> Int -> EnumValueDescriptorProto
enumValue name number =
    { name = name, number = number, options = Nothing }


defaultEnumValueDescriptorProto : EnumValueDescriptorProto
defaultEnumValueDescriptorProto =
    { name = "", number = 0, options = Nothing }


defaultMessageOptions : MessageOptions
defaultMessageOptions =
    { deprecated = False
    , mapEntry = False
    , messageSetWireFormat = False
    , noStandardDescriptorAccessor = False
    , uninterpretedOption = []
    }

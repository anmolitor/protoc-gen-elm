module MapperTest exposing (..)

import Elm.CodeGen as C
import Expect
import Mapper
import Mapper.Syntax exposing (Syntax(..))
import Model exposing (Cardinality(..), Field(..), FieldType(..), Primitive(..), TypeKind(..))
import Proto.Google.Protobuf.Descriptor exposing (DescriptorProto, DescriptorProto_(..), EnumDescriptorProto, EnumValueDescriptorProto, FieldDescriptorProto, FieldDescriptorProto_Label(..), FieldDescriptorProto_Type(..), FileDescriptorProto, MessageOptions)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Mapper"
        [ test "converts a single file descriptor correctly" <|
            \_ ->
                Mapper.mapMain
                    [ { defaultFileDescriptorProto
                        | name = "test.proto"
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
                                    , { defaultFieldDescriptorProto | name = "test2", type_ = FieldDescriptorProto_Type_TYPEMESSAGE, typeName = ".OtherMsg" }
                                    , { defaultFieldDescriptorProto | name = "test3", type_ = FieldDescriptorProto_Type_TYPEENUM, typeName = ".OtherMsg.AnEnum" }
                                    , { defaultFieldDescriptorProto | name = "test4", type_ = FieldDescriptorProto_Type_TYPEMESSAGE, typeName = ".Msg" }
                                    ]
                              }
                            ]
                      }
                    ]
                    |> Expect.equal
                        [ ( "test.proto"
                          , Ok
                                { messages =
                                    [ { dataType = "OtherMsg", isTopLevel = True, fields = [] }
                                    , { dataType = "Msg"
                                      , isTopLevel = True
                                      , fields =
                                            [ ( "test"
                                              , NormalField 0 Optional (Primitive Prim_Bool <| C.val "False")
                                              )
                                            , ( "test2"
                                              , NormalField 0 Optional (Embedded { dataType = "OtherMsg", moduleName = [], typeKind = Alias })
                                              )
                                            , ( "test3"
                                              , NormalField 0 Optional (Enumeration { dataType = "OtherMsg_AnEnum", moduleName = [], default = "OtherMsg_AnEnum_Bla", values = [ "OtherMsg_AnEnum_Bla", "OtherMsg_AnEnum_Blub" ] })
                                              )
                                            , ( "test4"
                                              , NormalField 0 Optional (Embedded { dataType = "Msg", moduleName = [], typeKind = Type })
                                              )
                                            ]
                                      }
                                    ]
                                , enums = [ { dataType = "OtherMsg_AnEnum", fields = ( ( 0, "OtherMsg_AnEnum_Bla" ), [ ( 2, "OtherMsg_AnEnum_Blub" ) ] ), isTopLevel = False, withUnrecognized = True } ]
                                }
                          )
                        ]
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
                                        [ DescriptorProto_ { defaultDescriptorProto | name = "OtherMsg" }
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
                                        [ { defaultFieldDescriptorProto | name = "field1", type_ = FieldDescriptorProto_Type_TYPEMESSAGE, typeName = ".some.pkg.name.Msg.OtherMsg" }
                                        , { defaultFieldDescriptorProto | name = "field2", type_ = FieldDescriptorProto_Type_TYPEMESSAGE, typeName = ".Abc" }
                                        ]
                                  }
                                ]
                        }
                in
                Mapper.mapMain [ file1, file2, file3 ]
                    |> Expect.equal
                        [ ( "test.proto"
                          , Ok <|
                                { messages =
                                    [ { dataType = "Msg_OtherMsg", isTopLevel = False, fields = [] }
                                    , { dataType = "Msg", isTopLevel = True, fields = [] }
                                    ]
                                , enums = []
                                }
                          )
                        , ( "no_package.proto"
                          , Ok <|
                                { messages =
                                    [ { dataType = "Abc", isTopLevel = True, fields = [] }
                                    ]
                                , enums = []
                                }
                          )
                        , ( "importing.proto"
                          , Ok <|
                                { messages =
                                    [ { dataType = "Msg"
                                      , isTopLevel = True
                                      , fields =
                                            [ ( "field1", NormalField 0 Optional (Embedded { dataType = "Msg_OtherMsg", moduleName = [ "Proto", "Test" ], typeKind = Alias }) )
                                            , ( "field2", NormalField 0 Optional (Embedded { dataType = "Abc", moduleName = [ "Proto", "NoPackage" ], typeKind = Alias }) )
                                            ]
                                      }
                                    ]
                                , enums = []
                                }
                          )
                        ]
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
    , label = FieldDescriptorProto_Label_LABELOPTIONAL
    , type_ = FieldDescriptorProto_Type_TYPEBOOL
    , typeName = ""
    , extendee = ""
    , defaultValue = ""
    , oneofIndex = -1
    , jsonName = ""
    , options = Nothing
    , proto3Optional = False
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

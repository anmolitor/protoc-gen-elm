module Generator.DevToolsWorker exposing (..)

import Bytes.Encode
import DebugMain exposing (subscriptions)
import Elm.CodeGen as C
import Generator.Common as Common
import Generator.Declarations exposing (removeDuplicateDeclarations)
import Generator.Export as Export
import Generator.Import as Import
import Json.Encode
import Meta.Basics
import Meta.JsonEncode
import Model exposing (Service)
import Set


generateDevToolsWorker : List Service -> C.File
generateDevToolsWorker services =
    let
        msgType : C.Declaration
        msgType =
            C.aliasDecl Nothing "Msg" [] <| C.recordAnn [ ( "bytes", C.listAnn C.intAnn ), ( "url", C.stringAnn ), ( "isRequest", C.boolAnn ) ]

        outMsgType : C.Declaration
        outMsgType =
            C.aliasDecl Nothing "OutMsg" [] <| C.recordAnn [ ( "json", Meta.JsonEncode.value ), ( "state", C.stringAnn ) ]

        portOutDecl : C.Declaration
        portOutDecl =
            C.portDecl portOutFunctionName (C.funAnn (C.typed "OutMsg" []) (C.fqTyped [ "Platform", "Cmd" ] "Cmd" [ C.typeVar "msg" ]))

        portInDecl : C.Declaration
        portInDecl =
            C.portDecl portInFunctionName (C.funAnn (C.funAnn (C.typed "Msg" []) (C.typeVar "msg")) (C.fqTyped [ "Platform", "Sub" ] "Sub" [ C.typeVar "msg" ]))

        main : C.Declaration
        main =
            C.valDecl Nothing
                (Just <| C.fqTyped [ "Platform" ] "Program" [ C.unitAnn, C.unitAnn, C.typed "Msg" [] ])
                "main"
            <|
                C.apply
                    [ C.fqFun [ "Platform" ] "worker"
                    , C.record
                        [ ( "init"
                          , C.lambda [ C.allPattern ] <| C.tuple [ C.unit, C.fqFun [ "Platform", "Cmd" ] "none" ]
                          )
                        , ( "update"
                          , C.lambda [ C.varPattern "msg", C.varPattern "model" ] <|
                                C.tuple
                                    [ C.val "model"
                                    , C.pipe (C.access (C.val "msg") "bytes")
                                        [ C.apply [ C.fqFun [ "List" ] "map", C.fqFun [ "Bytes", "Encode" ] "unsignedInt8" ]
                                        , C.fqFun [ "Bytes", "Encode" ] "sequence"
                                        , C.fqFun [ "Bytes", "Encode" ] "encode"
                                        , C.apply [ C.val toJsonFunctionName, C.access (C.val "msg") "url", C.access (C.val "msg") "isRequest" ]
                                        , C.val portOutFunctionName
                                        ]
                                    ]
                          )
                        , ( "subscriptions ", C.lambda [ C.allPattern ] <| C.apply [ C.fun "inc", Meta.Basics.identity ] )
                        ]
                    ]

        serviceMatch : Service -> List ( C.Pattern, C.Expression )
        serviceMatch service =
            List.concatMap
                (\method ->
                    [ ( "True", method.reqType ), ( "False", method.resType ) ]
                        |> List.map
                            (\( boolPattern, type_ ) ->
                                ( C.tuplePattern [ C.listPattern [ C.stringPattern <| service.package ++ "." ++ service.name, C.stringPattern method.name ], C.namedPattern boolPattern [] ]
                                , C.apply [ C.fun "convert", C.fqFun type_.package <| Common.decoderName type_.name, C.fqFun type_.package <| Common.jsonEncoderName type_.name ]
                                )
                            )
                )
                service.methods

        toJsonDecl : C.Declaration
        toJsonDecl =
            C.funDecl Nothing
                (Just <| C.funAnn C.stringAnn <| C.funAnn C.boolAnn <| C.funAnn (C.fqTyped [ "Bytes" ] "Bytes" []) (C.typed "OutMsg" []))
                toJsonFunctionName
                [ C.varPattern "url", C.varPattern "isRequest", C.varPattern "bytes" ]
            <|
                C.letExpr
                    [ C.letVal "urlSegments" <|
                        C.pipe (C.val "url")
                            [ C.apply [ C.fqFun [ "String" ] "split", C.string "/" ]
                            , C.apply [ C.fqFun [ "List" ] "filter", C.parens <| C.applyBinOp (C.fun "not") C.composel (C.fqFun [ "String" ] "isEmpty") ]
                            ]
                    , C.letVal "responseDecoder" <|
                        C.apply
                            [ C.fqFun [ "Bytes", "Decode" ] "map2"
                            , C.parens (C.lambda [ C.allPattern, C.varPattern "b" ] (C.val "b"))
                            , C.parens (C.apply [ C.fqFun [ "Bytes", "Decode" ] "bytes", C.int 1 ])
                            , C.parens
                                (C.pipe (C.apply [ C.fqFun [ "Bytes", "Decode" ] "unsignedInt32", C.fqVal [ "Bytes" ] "BE" ])
                                    [ C.apply [ C.fqFun [ "Bytes", "Decode" ] "andThen", C.fqFun [ "Bytes", "Decode" ] "bytes" ]
                                    ]
                                )
                            ]
                    , C.letFunction "convert" [ C.varPattern "decode", C.varPattern "encode" ] <|
                        C.pipe (C.val "bytes")
                            [ C.apply [ C.fqFun [ "Bytes", "Decode" ] "decode", C.val "responseDecoder" ]
                            , C.apply [ C.fqFun [ "Protobuf", "Decode" ] "decode", C.val "decode" ]
                            , C.apply
                                [ Meta.Basics.mapMaybe
                                , C.parens <|
                                    C.lambda [ C.varPattern "message" ]
                                        (C.record
                                            [ ( "json", C.apply [ C.val "encode", C.val "message" ] )
                                            , ( "state", C.string "success" )
                                            ]
                                        )
                                ]
                            , C.apply
                                [ Meta.Basics.withDefault
                                , C.record
                                    [ ( "json", Meta.JsonEncode.null )
                                    , ( "state", C.string "failure" )
                                    ]
                                ]
                            ]
                    ]
                <|
                    C.caseExpr (C.tuple [ C.val "urlSegments", C.val "isRequest" ])
                        (List.concatMap serviceMatch services
                            ++ [ ( C.allPattern, C.record [ ( "json", Meta.JsonEncode.null ), ( "state", C.string "no_match" ) ] )
                               ]
                        )

        toJsonFunctionName =
            "toJson"

        portOutFunctionName =
            "out"

        portInFunctionName =
            "inc"

        declarations =
            [ msgType, outMsgType, main, toJsonDecl, portInDecl, portOutDecl ]

        exports =
            Export.fromDeclarations declarations

        packageName =
            [ "DevToolsWorker" ]
    in
    C.file (C.portModule packageName exports)
        (List.map (\importedModule -> C.importStmt importedModule Nothing Nothing) (Set.toList <| Import.extractImports declarations))
        (removeDuplicateDeclarations declarations)
        (C.emptyFileComment
            --|> fileComment versions struct.originFiles
            |> Just
        )

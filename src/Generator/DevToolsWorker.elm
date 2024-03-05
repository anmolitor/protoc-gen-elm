module Generator.DevToolsWorker exposing (devToolsDTsFile, devToolsJsFile, generateDevToolsWorker)

import Elm.CodeGen as C
import Generator.Common as Common
import Generator.Declarations exposing (removeDuplicateDeclarations)
import Generator.Export as Export
import Generator.Import as Import
import Meta.Basics
import Meta.JsonEncode
import Model exposing (Service)
import Proto.Google.Protobuf.Compiler.CodeGeneratorResponse as CodeGeneratorResponse
import Set
import Versions exposing (AllVersions)


generateDevToolsWorker : List Service -> C.File
generateDevToolsWorker services =
    let
        msgType : C.Declaration
        msgType =
            C.aliasDecl Nothing "Msg" [] <| C.recordAnn [ ( "bytes", C.listAnn C.intAnn ), ( "url", C.stringAnn ), ( "isRequest", C.boolAnn ), ( "reqId", C.intAnn ) ]

        outMsgType : C.Declaration
        outMsgType =
            C.aliasDecl Nothing "OutMsg" [] <| C.recordAnn [ ( "json", Meta.JsonEncode.value ), ( "state", C.stringAnn ), ( "reqId", C.intAnn ) ]

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
                                        , C.apply
                                            [ C.val toJsonFunctionName
                                            , C.access (C.val "msg") "reqId"
                                            , C.access (C.val "msg") "url"
                                            , C.access (C.val "msg") "isRequest"
                                            ]
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
                (Just <| C.funAnn C.intAnn <| C.funAnn C.stringAnn <| C.funAnn C.boolAnn <| C.funAnn (C.fqTyped [ "Bytes" ] "Bytes" []) (C.typed "OutMsg" []))
                toJsonFunctionName
                [ C.varPattern "reqId", C.varPattern "url", C.varPattern "isRequest", C.varPattern "bytes" ]
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
                            , C.apply [ C.fqFun [ "Maybe" ] "andThen", C.parens <| C.apply [ C.fqFun [ "Protobuf", "Decode" ] "decode", C.val "decode" ] ]
                            , C.apply
                                [ Meta.Basics.mapMaybe
                                , C.parens <|
                                    C.lambda [ C.varPattern "message" ]
                                        (C.record
                                            [ ( "json", C.apply [ C.val "encode", C.val "message" ] )
                                            , ( "state", C.string "success" )
                                            , ( "reqId", C.val "reqId" )
                                            ]
                                        )
                                ]
                            , C.apply
                                [ Meta.Basics.withDefault
                                , C.record
                                    [ ( "json", Meta.JsonEncode.null )
                                    , ( "state", C.string "failure" )
                                    , ( "reqId", C.val "reqId" )
                                    ]
                                ]
                            ]
                    ]
                <|
                    C.caseExpr (C.tuple [ C.val "urlSegments", C.val "isRequest" ])
                        (List.concatMap serviceMatch services
                            ++ [ ( C.allPattern, C.record [ ( "json", Meta.JsonEncode.null ), ( "state", C.string "no_match" ), ( "reqId", C.val "reqId" ) ] )
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
            [ "Proto", "DevToolsWorker" ]
    in
    C.file (C.portModule packageName exports)
        (List.map (\importedModule -> C.importStmt importedModule Nothing Nothing) (Set.toList <| Import.extractImports declarations))
        (removeDuplicateDeclarations declarations)
        (C.emptyFileComment
            --|> fileComment versions struct.originFiles
            |> Just
        )


devToolsJsFile : AllVersions -> CodeGeneratorResponse.File
devToolsJsFile versions =
    { name = "Proto/dev-tools.mjs", content = devToolsJsFileContent versions, insertionPoint = "", generatedCodeInfo = Nothing }


devToolsDTsFile : CodeGeneratorResponse.File
devToolsDTsFile =
    { name = "Proto/dev-tools.d.mts", content = "export {}", insertionPoint = "", generatedCodeInfo = Nothing }


devToolsJsFileContent : AllVersions -> String
devToolsJsFileContent versions =
    """/**
 * !!! DO NOT EDIT THIS FILE MANUALLY !!!
 *
 * This file was automatically generated by
 *  - [`protoc-gen-elm`](https://www.npmjs.com/package/protoc-gen-elm) """ ++ versions.plugin ++ """
 *
 * This is a XMLHttpRequest interceptor that collects url, request body and response body.
 * The technique used here wraps the lifecycle methods 'open', 'send' and 'loadend'
 * to call the original code but also save some data for later use.
 *
 * The way this then interacts with the dev tools is the following:
 * We generate an additional elm worker which accepts bytes (as int[]) over a port and
 * uses the generated decoders to convert the data into elm data types.
 * We decide which decoder to use based on the url of the request.
 *
 * The elm worker then uses the generated JSON encoders to convert the data type into JSON format
 * and returns that as Json.Encode.Value back to Javascript.
 *
 * In JS we then convert our data into something the [grpc-dev-tools](https://github.com/SafetyCulture/grpc-web-devtools) extension can work with:
 * 1. We create a client
 * 2. We pass the client to enableDevTools([client])
 * 3. The client has a client_ property which has a rpcCall function which we call by passing the
 *    collected data from the interceptor and the decoded data from the elm worker.
 *    The signature is rpcCall(method, requestObject, metadata(?), info(?), responseCallback).
 *    The responseCallback can just be an empty function: () => {}
 */

import { Elm } from './DevToolsWorker.elm';

const worker = Elm.Proto.DevToolsWorker.init();

const reqIdGen = function* () {
  let id = 0;
  while (true) {
    yield id++;
  }
};

const reqIdGenInstance = reqIdGen();

const workerDecoder = (workerRequest) =>
  new Promise((resolve, reject) => {
    function subscription(workerResponse) {
      if (workerResponse.reqId !== workerRequest.reqId) {
        return;
      }
      worker.ports.out.unsubscribe(subscription);
      if (workerResponse.state === 'success') {
        resolve(workerResponse.json);
      }
      reject(workerResponse.state);
    }
    worker.ports.out.subscribe(subscription);
    worker.ports.inc.send(workerRequest);
  });

let responses = new Map();
const client = {
  client_: {
    rpcCall: (method, req, metadata, info, callback) => {
      const { body, isError } = responses.get(method);
      if (isError) {
        // first argument is error
        callback(body, null);
      } else {
        callback(null, body);
      }
      responses.delete(method);
    },
  },
};
const enableDevTools = window.__GRPCWEB_DEVTOOLS__ || (() => {});
enableDevTools([client]);

function wrapToObject(jsonObj) {
  return {
    toObject() {
      return jsonObj;
    },
  };
}

const bufferToArr = (buffer) => {
  const bytes = new Uint8Array(buffer);
  const arr = new Array(bytes.length);
  bytes.forEach((value, index) => (arr[index] = value));
  return arr;
};

const grpcStati = {
  1: 'Cancelled',
  2: 'Unknown',
  3: 'InvalidArgument',
  4: 'DeadlineExceeded',
  5: 'NotFound',
  6: 'AlreadyExists',
  7: 'PermissionDenied',
  8: 'ResourceExhausted',
  9: 'FailedPrecondition',
  10: 'Aborted',
  11: 'OutOfRange',
  12: 'Unimplemented',
  13: 'Internal',
  14: 'Unavailable',
  15: 'DataLoss',
  16: 'Unauthenticated',
};

function applyMonkeypatch(decoder) {
  const ORIGINAL = XMLHttpRequest;
  window.XMLHttpRequest = function () {
    let serviceAndMethod;
    let reqBody;
    let isGrpcRequest = false;
    let reqId;

    const original = new ORIGINAL();

    function sendToGrpcDevTools(req, res, isError) {
      const resAsJson = isError ? res : wrapToObject(res);
      responses.set(serviceAndMethod, { body: resAsJson, isError });
      client.client_.rpcCall(
        serviceAndMethod,
        wrapToObject(req),
        null,
        null,
        () => {}
      );
    }

    original.addEventListener('loadend', async () => {
      if (!isGrpcRequest) {
        return;
      }

      const reqAsJson = await decoder({
        url: serviceAndMethod,
        isRequest: true,
        bytes: bufferToArr(reqBody.buffer),
        reqId,
      });
      const responseStatus = original.getResponseHeader('grpc-status');

      if (
        original.getResponseHeader('content-type') !==
        'application/grpc-web+proto'
      ) {
        sendToGrpcDevTools(
          reqAsJson,
          { status: original.status, statusText: original.statusText },
          true
        );
        return;
      }
      if (responseStatus && responseStatus !== '0') {
        const errorMessage = original.getResponseHeader('grpc-message');
        sendToGrpcDevTools(
          reqAsJson,
          {
            status: grpcStati[responseStatus],
            message: decodeURIComponent(errorMessage),
          },
          true
        );
        return;
      }
      const resAsJson = await decoder({
        url: serviceAndMethod,
        isRequest: false,
        bytes: bufferToArr(original.response),
        reqId,
      });
      sendToGrpcDevTools(reqAsJson, resAsJson, false);
    });

    const originalOpen = original.open.bind(original);
    original.open = function (httpMethod, url, async, username, password) {
      serviceAndMethod = url;

      return originalOpen(httpMethod, url, async, username, password);
    };

    const originalSend = original.send.bind(original);
    original.send = function (body) {
      reqBody = body;
      originalSend(body);
    };

    const originalSetHeader = original.setRequestHeader.bind(original);
    original.setRequestHeader = function (name, value) {
      if (
        ['accept', 'content-type'].includes(name.toLowerCase()) &&
        value === 'application/grpc-web+proto'
      ) {
        isGrpcRequest = true;
        reqId = reqIdGenInstance.next().value;
      }
      originalSetHeader(name, value);
    };

    return original;
  };
}

applyMonkeypatch(workerDecoder);
"""

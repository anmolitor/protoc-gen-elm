module Generator.Service exposing (..)

import Elm.CodeGen as C
import Generator.Common as Common
import Mapper.Name as Name
import Model exposing (Method, Service)


toAST : Service -> List C.Declaration
toAST service =
    List.map (methodToAST { name = service.name, package = service.package }) service.methods


methodToAST : { name : String, package : String } -> Method -> C.Declaration
methodToAST service method =
    let
        reqModule =
            method.reqType.package

        reqType =
            method.reqType.name

        resModule =
            method.resType.package

        resType =
            method.resType.name

        comment =
            C.emptyDocComment
                |> C.markdown
                    (String.concat
                        [ "A template for a gRPC call to the method '"
                        , method.name
                        , "' sending a `"
                        , reqType
                        , "` to get back a `"
                        , resType
                        , "`."
                        ]
                    )
                |> Common.addDocs method.docs

        typeAnn =
            C.fqTyped [ "Grpc", "Internal" ]
                "Rpc"
                [ C.fqTyped reqModule reqType []
                , C.fqTyped resModule resType []
                ]

        expr =
            C.apply
                [ C.fqFun [ "Grpc", "Internal" ] "Rpc"
                , C.record
                    [ ( "service", C.string service.name )
                    , ( "package", C.string service.package )
                    , ( "rpcName", C.string method.name )
                    , ( "encoder", C.fqVal reqModule <| Common.encoderName reqType )
                    , ( "decoder", C.fqVal resModule <| Common.decoderName resType )
                    ]
                ]
    in
    C.valDecl (Just comment)
        (Just typeAnn)
        (Name.field method.name)
        expr

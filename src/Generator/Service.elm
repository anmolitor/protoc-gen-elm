module Generator.Service exposing (..)

import Elm.CodeGen as C exposing (ModuleName)
import Generator.Common as Common
import Mapper.Name
import Model exposing (Method, Service)


toAST : ModuleName -> Service -> List C.Declaration
toAST currentModuleName service =
    List.map (methodToAST currentModuleName { name = service.name, package = service.package }) service.methods


methodToAST : ModuleName -> { name : String, package : String } -> Method -> C.Declaration
methodToAST currentModuleName service method =
    let
        ( reqModule, reqType ) =
            method.reqType

        ( resModule, resType ) =
            method.resType

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

        typeAnn =
            C.fqTyped [ "Grpc", "Internal" ]
                "Rpc"
                [ Common.mayFq currentModuleName C.fqTyped reqModule reqType []
                , Common.mayFq currentModuleName C.fqTyped resModule resType []
                ]

        expr =
            C.apply
                [ C.fqFun [ "Grpc", "Internal" ] "Rpc"
                , C.record
                    [ ( "service", C.string service.name )
                    , ( "package", C.string service.package )
                    , ( "rpcName", C.string method.name )
                    , ( "encoder", Common.mayFq currentModuleName C.fqVal reqModule <| Common.encoderName reqType )
                    , ( "decoder", Common.mayFq currentModuleName C.fqVal resModule <| Common.decoderName resType )
                    ]
                ]
    in
    C.funDecl (Just comment)
        (Just typeAnn)
        (Mapper.Name.field method.name)
        []
        expr

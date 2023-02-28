module Generator.Service exposing (..)

import Elm.CodeGen as C
import Generator.Common
import Mapper.Name
import Meta.Type
import Model exposing (Method, Service)


toAST : Service -> List C.Declaration
toAST service =
    List.map (methodToAST { name = service.name, package = service.package }) service.methods


methodToAST : { name : String, package : String } -> Method -> C.Declaration
methodToAST service method =
    let
        ( reqModule, reqType ) =
            method.reqType

        ( resModule, resType ) =
            method.resType

        comment =
            C.emptyDocComment
                |> C.markdown
                    (String.concat
                        [ "Calls the GRPC method '"
                        , method.name
                        , "' with the given `"
                        , reqType
                        , "`."
                        ]
                    )

        callbackTypeAnn =
            C.funAnn
                (Meta.Type.result (C.fqTyped [ "Grpc" ] "Error" [])
                    (C.fqTyped resModule resType [])
                )
                (C.typeVar "msg")

        typeAnn =
            C.funAnn (C.fqTyped reqModule reqType [])
                (C.funAnn callbackTypeAnn (Meta.Type.cmd <| C.typeVar "msg"))

        expr =
            C.apply
                [ C.fqFun [ "Grpc" ] "send"
                , C.record
                    [ ( "service", C.string service.name )
                    , ( "package", C.string service.package )
                    , ( "rpcName", C.string method.name )
                    ]
                , C.fqVal reqModule <| Generator.Common.encoderName reqType
                , C.fqVal resModule <| Generator.Common.decoderName resType
                ]
    in
    C.funDecl (Just comment)
        (Just typeAnn)
        (Mapper.Name.field method.name)
        []
        expr

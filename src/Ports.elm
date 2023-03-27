port module Ports exposing (debug, request, response)


port request : (String -> msg) -> Sub msg


port response : String -> Cmd msg


port debug : String -> Cmd msg

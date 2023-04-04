port module Ports exposing (request, response)


port request : (String -> msg) -> Sub msg


port response : String -> Cmd msg

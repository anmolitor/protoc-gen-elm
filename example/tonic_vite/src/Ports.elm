port module Ports exposing (deleteUserId, setUserId)


setUserId : String -> Cmd msg
setUserId =
    Just >> setOrDeleteUserId


deleteUserId : Cmd msg
deleteUserId =
    setOrDeleteUserId Nothing


port setOrDeleteUserId : Maybe String -> Cmd msg

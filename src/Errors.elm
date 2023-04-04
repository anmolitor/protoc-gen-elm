module Errors exposing (..)


type Error
    = AmbiguousPackageName String
    | UnsupportedFeature String
    | NonPrimitiveMapKey String
    | NoEnumValues String
    | MultipleErrors (List Error)
    | AddContext String Error


type alias Res a =
    Result Error a


format : Error -> String
format =
    let
        formatInternal depth err =
            case err of
                AmbiguousPackageName packageName ->
                    String.repeat depth " " ++ "Package Name '" ++ packageName ++ "' is not in lower case. This is problematic since we need to differentiate between packages and inner modules (definitions inside of messages)."

                UnsupportedFeature feature ->
                    String.repeat depth " " ++ "This plugin does not support the feature: '" ++ feature ++ "' yet."

                NonPrimitiveMapKey context ->
                    String.repeat depth " " ++ "Cannot use a non-primitive as a map key. Context: '" ++ context ++ "'."

                NoEnumValues enumName ->
                    String.repeat depth " " ++ "Found enum type without any values '" ++ enumName ++ "'. Enums are required to have at least one constructor."

                MultipleErrors errors ->
                    List.map (formatInternal depth) errors |> String.join "\n"

                AddContext context error ->
                    String.repeat depth " " ++ "Error in " ++ context ++ ":" ++ formatInternal (depth + 1) error
    in
    formatInternal 0


map2 : (a -> b -> c) -> Res a -> Res b -> Res c
map2 f res1 res2 =
    case ( res1, res2 ) of
        ( Ok ok1, Ok ok2 ) ->
            Ok (f ok1 ok2)

        ( Err err1, Ok _ ) ->
            Err err1

        ( Ok _, Err err2 ) ->
            Err err2

        ( Err err1, Err err2 ) ->
            Err (MultipleErrors [ err1, err2 ])


map3 : (a -> b -> c -> d) -> Res a -> Res b -> Res c -> Res d
map3 f res1 res2 res3 =
    map2 f res1 res2
        |> map2 (|>) res3


combine : List (Res a) -> Res (List a)
combine =
    List.foldr (map2 (::)) (Ok [])


combineMap : (a -> Res b) -> List a -> Res (List b)
combineMap f =
    List.map f >> combine

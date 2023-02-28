module ByteUtil exposing (RoundTrip, makeRoundtrip)

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Protobuf.Decode
import Protobuf.Encode


type alias RoundTrip =
    List Int -> Maybe (List Int)


makeRoundtrip : Protobuf.Decode.Decoder a -> (a -> Protobuf.Encode.Encoder) -> RoundTrip
makeRoundtrip decoder encoder =
    fromInts
        >> Protobuf.Decode.decode decoder
        >> Maybe.map (encoder >> Protobuf.Encode.encode >> toInts)


fromInts : List Int -> Bytes
fromInts =
    List.map Bytes.Encode.unsignedInt8 >> Bytes.Encode.sequence >> Bytes.Encode.encode


toInts : Bytes -> List Int
toInts bytes =
    Bytes.Decode.decode (Bytes.Decode.loop ( Bytes.width bytes, Array.empty ) listStep) bytes
        |> Maybe.withDefault Array.empty
        |> Array.toList


listStep : ( Int, Array Int ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, Array Int ) (Array Int))
listStep ( n, xs ) =
    if n <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done xs)

    else
        Bytes.Decode.map (\x -> Bytes.Decode.Loop ( n - 1, Array.push x xs )) Bytes.Decode.unsignedInt8

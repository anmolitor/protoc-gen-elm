module Main exposing (main)

import Browser
import Greeter
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Response
    = NotAsked
    | Loading
    | Success Greeter.HelloResponse
    | Failure


type alias Model =
    { name : String
    , response : Response
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" NotAsked, Cmd.none )


type Msg
    = SetName String
    | Submit
    | SendRequest (Result Http.Error Greeter.HelloResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName name ->
            ( { model | name = name, response = NotAsked }, Cmd.none )

        Submit ->
            ( { model | name = "", response = Loading }, sayHello SendRequest model )

        SendRequest (Ok response) ->
            ( { model | response = Success response }, Cmd.none )

        SendRequest (Err _) ->
            ( { model | response = Failure }, Cmd.none )


sayHello : (Result Http.Error Greeter.HelloResponse -> msg) -> Model -> Cmd msg
sayHello toMsg model =
    Http.post
        { url = "http://localhost:8001"
        , body =
            Http.bytesBody "application/octet-stream" <|
                Encode.encode (Greeter.toHelloRequestEncoder (Greeter.HelloRequest model.name))
        , expect = Decode.expectBytes toMsg Greeter.helloResponseDecoder
        }


view : Model -> Html.Html Msg
view model =
    let
        ( color, message ) =
            case model.response of
                NotAsked ->
                    ( "auto", "" )

                Loading ->
                    ( "gray", "Loading..." )

                Success res ->
                    ( "green", res.message )

                Failure ->
                    ( "red", "Request failed" )
    in
    Html.form [ Events.onSubmit Submit ]
        [ Html.input
            [ Attr.placeholder "Enter your name"
            , Attr.value model.name
            , Events.onInput SetName
            ]
            []
        , Html.button [] [ Html.text "Send" ]
        , Html.span
            [ Attr.style "margin-left" "1em"
            , Attr.style "color" color
            ]
            [ Html.text message ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

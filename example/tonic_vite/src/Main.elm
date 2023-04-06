module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, button, form, input, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput, onSubmit)
import Ports
import Proto.Todos exposing (Todo, TodoWithId)
import Protobuf.Types.Int64 exposing (Int64)
import Random
import UUID


type alias UserId =
    String


type Msg
    = EditLoginFormField String
    | SubmitLogin String
    | CreateRandomUser
    | CreatedRandomUser UUID.UUID
    | Logout
    | AddTodo Todo
    | DeleteTodo Int64
    | GetTodos UserId
    | GotTodos (List TodoWithId)


type alias Model =
    { userId : Maybe String, loginFormFieldValue : String }


type alias Flags =
    { userId : Maybe String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { userId = flags.userId, loginFormFieldValue = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditLoginFormField newValue ->
            ( { model | loginFormFieldValue = newValue }, Cmd.none )

        SubmitLogin userId ->
            ( { model | userId = Just userId, loginFormFieldValue = "" }, Ports.setUserId userId )

        CreateRandomUser ->
            ( model, Random.generate CreatedRandomUser UUID.generator )

        CreatedRandomUser uuid ->
            let
                userId =
                    UUID.toString uuid
            in
            ( { model | userId = Just userId, loginFormFieldValue = "" }, Ports.setUserId userId )

        Logout ->
            ( model, Cmd.batch [ Ports.deleteUserId, Browser.Navigation.reload ] )

        GetTodos ->
            ( model, Proto.Todos.)    

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    case model.userId of
        Just userId ->
            { title = "Todo App"
            , body =
                [ button [ onClick Logout ] [ text "Logout" ]
                ]
            }

        Nothing ->
            { title = "Login"
            , body = loginForm model
            }


loginForm : Model -> List (Html Msg)
loginForm model =
    [ text "Login using any uuid"
    , form [ onSubmit <| SubmitLogin model.loginFormFieldValue ]
        [ input
            [ Html.Attributes.value model.loginFormFieldValue
            , onInput EditLoginFormField
            ]
            []
        , button [] [ text "Submit" ]
        , button [ onClick CreateRandomUser, Html.Attributes.type_ "button" ] [ text "Login as random user" ]
        ]
    ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

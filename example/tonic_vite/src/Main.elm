module Main exposing (main)

import Browser
import Browser.Navigation
import Grpc
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput, onSubmit)
import Ports
import Proto.Google.Protobuf
import Proto.Todos exposing (Todo, TodoWithId)
import Proto.Todos.TodoService
import Protobuf.Types.Int64 exposing (Int64)
import Random
import UUID


type alias UserId =
    String


type alias TodoId =
    Int64


type Msg
    = EditLoginFormField String
    | SubmitLogin String
    | CreateRandomUser
    | CreatedRandomUser UUID.UUID
    | Logout
    | AddTodo UserId Todo
    | AddedTodo (Result Grpc.Error TodoId)
    | DeleteTodo TodoId
    | DeletedTodo (Result Grpc.Error TodoId)
    | GetTodos UserId
    | GotTodos (Result Grpc.Error (List TodoWithId))


type alias Model =
    { userId : Maybe String, loginFormFieldValue : String, todos : List TodoWithId }


type alias Flags =
    { userId : Maybe String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { userId = Nothing, loginFormFieldValue = "", todos = [] }
    in
    case flags.userId of
        Just userId ->
            onLogin userId model

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditLoginFormField newValue ->
            ( { model | loginFormFieldValue = newValue }, Cmd.none )

        SubmitLogin userId ->
            onLogin userId model

        CreateRandomUser ->
            ( model, Random.generate CreatedRandomUser UUID.generator )

        CreatedRandomUser uuid ->
            let
                userId =
                    UUID.toString uuid
            in
            onLogin userId model

        Logout ->
            ( model, Cmd.batch [ Ports.deleteUserId, Browser.Navigation.reload ] )

        GetTodos userId ->
            ( model
            , getTodos userId
            )

        GotTodos (Ok todos) ->
            ( { model | todos = todos }, Cmd.none )

        AddTodo userId todo ->
            ( model
            , { userId = userId, todo = Just todo }
                |> Grpc.new Proto.Todos.TodoService.addTodo
                |> Grpc.toCmd (Result.map .todoId >> AddedTodo)
            )

        DeleteTodo todoId ->
            ( model
            , { todoId = todoId }
                |> Grpc.new Proto.Todos.TodoService.deleteTodo
                |> Grpc.toCmd (Result.map (always todoId) >> DeletedTodo)
            )

        _ ->
            ( model, Cmd.none )


onLogin : UserId -> Model -> ( Model, Cmd Msg )
onLogin userId model =
    ( { model | userId = Just userId, loginFormFieldValue = "" }
    , Cmd.batch [ Ports.setUserId userId, getTodos userId ]
    )


getTodos : UserId -> Cmd Msg
getTodos userId =
    { userId = userId }
        |> Grpc.new Proto.Todos.TodoService.getTodos
        |> Grpc.toCmd (Result.map .todos >> GotTodos)


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
                , button
                    [ onClick <|
                        AddTodo userId
                            { title = "First todo", description = "", dueAt = Just Proto.Google.Protobuf.defaultTimestamp }
                    ]
                    [ text "Add todo" ]
                , div [] <| List.map viewTodo model.todos
                ]
            }

        Nothing ->
            { title = "Login"
            , body = loginForm model
            }


viewTodo : TodoWithId -> Html Msg
viewTodo todo =
    text (todo.todo |> Maybe.map .title |> Maybe.withDefault "")


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

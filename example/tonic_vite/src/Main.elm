module Main exposing (main)

import Browser
import Browser.Navigation
import Grpc
import Html exposing (Html, button, div, form, h2, input, label, p, text, textarea)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput, onSubmit)
import Iso8601
import Ports
import Proto.Google.Protobuf
import Proto.Todos exposing (Todo, TodoWithId)
import Proto.Todos.TodoService
import Protobuf.Types.Int64 exposing (Int64)
import Random
import Time
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
    | GetTodos UserId
    | GotTodos (Result Grpc.Error (List TodoWithId))
    | SetNewTodo TodoForm
    | AddTodo UserId Todo
    | AddedTodo (Result Grpc.Error TodoWithId)
    | DeleteTodo TodoId
    | DeletedTodo (Result Grpc.Error TodoId)
    | FormValidationError String


type alias Model =
    { userId : Maybe String
    , loginFormFieldValue : String
    , todos : List TodoWithId
    , todoForm : TodoForm
    }


type alias TodoForm =
    { title : String
    , description : String
    , dueAt : String
    }


initTodoForm : Time.Posix -> TodoForm
initTodoForm dueAt =
    { title = "", description = "", dueAt = Iso8601.fromTime dueAt }


type alias Flags =
    { userId : Maybe String, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { userId = Nothing, loginFormFieldValue = "", todos = [], todoForm = initTodoForm <| Time.millisToPosix flags.now }
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
                |> Grpc.toCmd (Result.map (\{ todoId } -> { id = todoId, todo = Just todo }) >> AddedTodo)
            )

        AddedTodo (Ok todoWithId) ->
            ( { model | todos = todoWithId :: model.todos }, Cmd.none )

        DeleteTodo todoId ->
            ( model
            , { todoId = todoId }
                |> Grpc.new Proto.Todos.TodoService.deleteTodo
                |> Grpc.toCmd (Result.map (always todoId) >> DeletedTodo)
            )

        DeletedTodo (Ok todoId) ->
            ( { model | todos = List.filter (\todo -> todo.id /= todoId) model.todos }, Cmd.none )

        SetNewTodo todoForm ->
            ( { model | todoForm = todoForm }, Cmd.none )

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


posixToTimestamp : Time.Posix -> Proto.Google.Protobuf.Timestamp
posixToTimestamp posix =
    let
        millis =
            Time.posixToMillis posix

        seconds =
            millis // 1000

        nanos =
            modBy 1000 millis
    in
    { seconds = Protobuf.Types.Int64.fromInts 0 seconds, nanos = nanos }


submitForm : UserId -> TodoForm -> Msg
submitForm userId todoForm =
    case Iso8601.toTime todoForm.dueAt of
        Ok dueAt ->
            AddTodo userId
                { title = todoForm.title, description = todoForm.description, dueAt = Just <| posixToTimestamp dueAt }

        Err _ ->
            FormValidationError "Invalid date"


view : Model -> Browser.Document Msg
view ({ todoForm } as model) =
    case model.userId of
        Just userId ->
            { title = "Todo App"
            , body =
                [ button [ onClick Logout ] [ text "Logout" ]
                , form
                    [ class "todo-form"
                    , onSubmit <| submitForm userId todoForm
                    ]
                    [ label []
                        [ text "Title"
                        , input
                            [ Html.Attributes.value model.todoForm.title
                            , onInput <| \newTitle -> SetNewTodo { todoForm | title = newTitle }
                            ]
                            []
                        ]
                    , label []
                        [ text "Description"
                        , textarea
                            [ Html.Attributes.value model.todoForm.description
                            , onInput <| \newDescription -> SetNewTodo { todoForm | description = newDescription }
                            ]
                            []
                        ]
                    , label []
                        [ text "Due at"
                        , input
                            [ Html.Attributes.type_ "datetime-local"
                            , Html.Attributes.value todoForm.dueAt
                            , onInput <| \newDueAt -> SetNewTodo { todoForm | dueAt = newDueAt }
                            ]
                            []
                        ]
                    , button
                        []
                        [ text "Add todo" ]
                    ]
                , div [] <| List.map viewTodo model.todos
                ]
            }

        Nothing ->
            { title = "Login"
            , body = loginForm model
            }


viewTodo : TodoWithId -> Html Msg
viewTodo { todo, id } =
    case todo of
        Just { title, description, dueAt } ->
            div []
                [ h2 [] [ text title ], p [] [ text description ], button [ onClick <| DeleteTodo id ] [ text "Remove" ] ]

        Nothing ->
            text ""


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

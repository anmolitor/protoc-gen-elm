use std::collections::HashMap;
use std::sync::{Arc, Mutex, PoisonError, RwLock};

use tonic::{transport::Server, Request, Response, Status};

use todos::todo_service_server::{TodoService, TodoServiceServer};
use todos::{
    AddTodoRequest, AddTodoResponse, DeleteTodoRequest, DeleteTodoResponse, GetTodosRequest,
    GetTodosResponse, Todo, TodoWithId,
};

pub mod todos {
    tonic::include_proto!("todos");
}

#[derive(Clone)]
pub struct TodoServiceImpl {
    todos: Arc<RwLock<Todos>>,
    next_id: Arc<Mutex<u64>>,
}

type UserId = String;

type Todos = HashMap<u64, (Todo, UserId)>;

#[tonic::async_trait]
impl TodoService for TodoServiceImpl {
    async fn get_todos(
        &self,
        request: Request<GetTodosRequest>,
    ) -> Result<Response<GetTodosResponse>, Status> {
        let GetTodosRequest { user_id } = request.into_inner();
        let todos: Vec<TodoWithId> = self
            .todos
            .read()
            .map_err(into_status)?
            .iter()
            .filter(|(_, (_, todo_user_id))| &user_id == todo_user_id)
            .map(|(id, (todo, _))| TodoWithId {
                todo: Some(todo.clone()),
                id: *id,
            })
            .collect();

        let response = GetTodosResponse { todos };
        Ok(Response::new(response))
    }

    async fn add_todo(
        &self,
        request: Request<AddTodoRequest>,
    ) -> Result<Response<AddTodoResponse>, Status> {
        let mut next_id = self.next_id.lock().map_err(into_status)?;
        let todo_id = *next_id;
        *next_id += 1;
        let mut todos = self.todos.write().map_err(into_status)?;
        let AddTodoRequest { todo, user_id } = request.into_inner();

        if let Some(todo) = todo {
            todos.insert(todo_id, (todo, user_id));
            let response = AddTodoResponse { todo_id };
            return Ok(Response::new(response));
        }
        Err(Status::invalid_argument("No todo given."))
    }

    async fn delete_todo(
        &self,
        request: Request<DeleteTodoRequest>,
    ) -> Result<Response<DeleteTodoResponse>, Status> {
        let mut todos = self.todos.write().map_err(into_status)?;
        let DeleteTodoRequest { todo_id } = request.into_inner();
        todos.remove(&todo_id);

        Ok(Response::new(DeleteTodoResponse {}))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "127.0.0.1:3000".parse().unwrap();

    let todo_service = TodoServiceImpl {
        next_id: Arc::new(Mutex::new(0)),
        todos: Arc::new(RwLock::new(HashMap::new())),
    };
    let todo_service_server = TodoServiceServer::new(todo_service);

    println!("GreeterServer listening on {}", addr);

    Server::builder()
        // GrpcWeb is over http1 so we must enable it.
        .accept_http1(true)
        .add_service(tonic_web::enable(todo_service_server))
        .serve(addr)
        .await?;

    Ok(())
}

fn into_status<T>(_err: PoisonError<T>) -> Status {
    Status::internal("Read/Write Lock Error")
}

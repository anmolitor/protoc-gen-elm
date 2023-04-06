# Simple gRPC example

This example uses NodeJS on the backend and Envoy as a proxy between frontend and backend.

## Start the proxy

```
docker run -d -v "$(pwd)"/envoy.yaml:/etc/envoy/envoy.yaml:ro -p 8080:8080 -p 9901:9901 envoyproxy/envoy:v1.22.0
```

This starts Envoy on port 8080. Based on the `envoy.yaml` configuration in this directory, it will forward requests to port 9090.

## Start the backend

```
node server.js
```

This will start a NodeJS server built with `@grpc/grpc-js` on port 9090.
It will add "Hello " in front of any name it gets via the `SayHello` method
defined in the `greeter.proto` file.

It loads the `greeter.proto` file dynamically, so no need for a build step here.

## Build the frontend

If you cloned this repo:

```
npm run build:frontend
```

If you are building your own application:
Make sure that `protoc-gen-elm` is on the PATH, either via 
dev dependency or global installation and run

```
protoc --elm_out=src greeter.proto
```

## Start the frontend

Run 

```
elm reactor
```

then navigate to `http://localhost:8000` and click on `src/Main.elm`




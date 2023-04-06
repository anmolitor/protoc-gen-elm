var PROTO_PATH = __dirname + "/greeter.proto";

const assert = require("assert");
const grpc = require("@grpc/grpc-js");
const protoLoader = require("@grpc/proto-loader");
const packageDefinition = protoLoader.loadSync(PROTO_PATH, {
  keepCase: true,
  longs: String,
  enums: String,
  defaults: true,
  oneofs: true,
});
const protoDescriptor = grpc.loadPackageDefinition(packageDefinition);
const { Greeter } = protoDescriptor.hello;

function doSayHello(call, callback) {
  callback(null, {
    message: "Hello " + call.request.name,
  });
}

function getServer() {
  var server = new grpc.Server();
  server.addService(Greeter.service, {
    sayHello: doSayHello,
  });
  return server;
}

var server = getServer();
console.log("Starting server on port 9090.");
server.bindAsync(
  "0.0.0.0:9090",
  grpc.ServerCredentials.createInsecure(),
  (err, port) => {
    assert.ifError(err);
    server.start();
  }
);

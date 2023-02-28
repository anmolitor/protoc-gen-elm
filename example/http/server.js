#!/usr/bin/env node

const ProtoBuf = require('protobufjs');
const Http = require('http');
const assert = require('assert')
const path = require('path');

const ApiFactory = ProtoBuf.loadSync(path.join(__dirname, 'greeter.proto'));
assert.ok(ApiFactory);

const Api = ApiFactory.resolve();
assert.ok(Api);
assert.ok(Api.Greeter);
assert.ok(Api.HelloRequest);
assert.ok(Api.HelloResponse);

const http = Http.createServer(function(req, res) {
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Headers', '*');
    if ( req.method === 'OPTIONS' ) {
      res.writeHead(200);
      res.end();
      return;
    }

    const buffers = [];
    req.on('data', function(data) {
        buffers.push(data)
    });

    req.on('end', function() {
        const data = Buffer.concat(buffers);
        console.log('[on:message]', data);

        const protoReq = Api.HelloRequest.decode(data);
        const protoRes = Api.HelloResponse.encode({
          message: "Hello, " + protoReq.name + "!"
        });

        res.end(protoRes.finish().toString('binary'));
    });
});

http.listen(8001);

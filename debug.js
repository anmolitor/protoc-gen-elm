#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const Elm = require("./elm.debug.js").Elm;

const app = Elm.DebugMain.init({
  flags: {},
});

app.ports.debug.subscribe((msg) => {
  fs.appendFileSync("debug.log", `\n[${new Date().toISOString()}]: ${msg}`);
});

function sendToGenerator(request) {
  return new Promise((resolve) => {
    const responseHandler = (response) => {
      resolve(Buffer.from(response, "base64"));
      app.ports.response.unsubscribe(responseHandler);
    };
    app.ports.response.subscribe(responseHandler);
    app.ports.request.send(request.toString("base64"));
  });
}

const chunks = [];
process.stdin.on("data", (chunk) => {
  chunks.push(chunk);
});
process.stdin.on("end", () => {
  const request = Buffer.concat(chunks);
  sendToGenerator(request).then((response) => process.stdout.write(response));
});

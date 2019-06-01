#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const Elm = require('./elm.min.js').Elm;

function getPluginVersion() {
    const file = path.resolve(__dirname, 'package.json');
    const nodeJson = JSON.parse(fs.readFileSync(file, 'utf8'));
    return nodeJson.version;
}

function getLibraryVersion() {
    let version = 'latest';
    try {
      const file = path.resolve(__dirname, 'elm.json');
      const elmJson = JSON.parse(fs.readFileSync(file, 'utf8'));
      version = elmJson.dependencies.direct['eriktim/elm-protocol-buffers'] || version;
    } catch (e) {
      // not fatal
    }
    return version;
}

const pluginVersion = getPluginVersion();
const libraryVersion = getLibraryVersion();
const app = Elm.Main.init({flags: {plugin: pluginVersion, library: libraryVersion}});

function sendToGenerator(request) {
  return new Promise((resolve) => {
    const responseHandler = (response) => {
      resolve(Buffer.from(response, 'base64'));
      app.ports.response.unsubscribe(responseHandler);
    };
    app.ports.response.subscribe(responseHandler);
    app.ports.request.send(request.toString('base64'));
  });
}

const chunks = [];
process.stdin.on('data', (chunk) => {
  chunks.push(chunk);
});
process.stdin.on('end', () => {
  const request = Buffer.concat(chunks);
  sendToGenerator(request)
    .then(response => process.stdout.write(response));
});

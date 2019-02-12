#!/usr/bin/env node

const fs = require('fs');
const through2 = require('through2');
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

const generator = through2((data, enc, cb) => {
  const responseHandler = (response) => {
    cb(null, Buffer.from(response, 'base64'));
    app.ports.response.unsubscribe(responseHandler);
  };
  app.ports.response.subscribe(responseHandler);
  app.ports.request.send(data.toString('base64'));
});

process.stdin.pipe(generator).pipe(process.stdout);

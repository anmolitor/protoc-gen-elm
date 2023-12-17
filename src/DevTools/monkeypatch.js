import { Elm } from "./DevToolsWorker.elm";

/**
 * This is a XMLHttpRequest interceptor that collects url, request body and response body.
 * The technique used here wraps the lifecycle methods 'open', 'send' and 'loadend'
 * to call the original code but also save some data for later use.
 *
 * The way this then interacts with the dev tools is the following:
 * We generate an additional elm worker which accepts bytes (as int[]) over a port and
 * uses the generated decoders to convert the data into elm data types.
 * We decide which decoder to use based on the url of the request.
 *
 * The elm worker then uses the generated (currently TODO) JSON encoders to convert the data type into JSON format
 * and returns that as Json.Encode.Value back to Javascript.
 *
 * In JS we then convert our data into something the grpc-dev-tools extension can work with:
 * 1. We create a client
 * 2. We pass the client to enableDevTools([client])
 * 3. The client has a client_ property which has a rpcCall function which we call by passing the
 *    collected data from the interceptor and the decoded data from the elm worker.
 *    The signature is rpcCall(method, requestObject, metadata(?), info(?), responseCallback).
 *    The responseCallback can just be an empty function: () => {}
 */

const worker = Elm.DevToolsWorker.init();

const workerDecoder = (workerRequest) =>
  new Promise((resolve, reject) => {
    function subscription(workerResponse) {
      worker.ports.out.unsubscribe(this);
      if (workerResponse.state === "success") {
        resolve(workerResponse.json);
      }
      reject(workerResponse.state);
    }
    worker.ports.out.subscribe(subscription);
    worker.ports.inc.send(workerRequest);
  });

let response;
const client = {
  client_: {
    rpcCall: (method, req, metadata, info, callback) => {
      // first argument is error
      callback(null, response);
    },
  },
};
const enableDevTools = window.__GRPCWEB_DEVTOOLS__ || (() => {});
enableDevTools([client]);

function wrapToObject(jsonObj) {
  return {
    toObject() {
      return jsonObj;
    },
  };
}

const bufferToArr = (buffer) => {
  const bytes = new Uint8Array(buffer);
  const arr = new Array(bytes.length);
  bytes.forEach((value, index) => (arr[index] = value));
  return arr;
};

function applyMonkeypatch(decoder) {
  const ORIGINAL = XMLHttpRequest;
  window.XMLHttpRequest = function () {
    let serviceAndMethod;
    let reqBody;

    const original = new ORIGINAL();
    original.addEventListener("loadend", async () => {
      if (
        original.getResponseHeader("content-type") ===
        "application/grpc-web+proto"
      ) {
        const reqAsJson = await decoder({
          url: serviceAndMethod,
          isRequest: true,
          bytes: bufferToArr(reqBody.buffer),
        });
        const resAsJson = await decoder({
          url: serviceAndMethod,
          isRequest: false,
          bytes: bufferToArr(original.response),
        });
        response = wrapToObject(resAsJson);
        client.client_.rpcCall(
          serviceAndMethod,
          wrapToObject(reqAsJson),
          null,
          null,
          () => {}
        );
      }
    });

    const originalOpen = original.open.bind(original);
    original.open = function (httpMethod, url, async, username, password) {
      serviceAndMethod = url;

      return originalOpen(httpMethod, url, async, username, password);
    };

    const originalSend = original.send.bind(original);
    original.send = function (body) {
      reqBody = body;
      originalSend(body);
    };

    return original;
  };
}

applyMonkeypatch(workerDecoder);

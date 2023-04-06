import cp from "child_process";
import path from "path";
import { promisify } from "util";
import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import which from "which";

const protocPlugin = (protoPath, extraArgs, files) => {
  return {
    name: "run-protoc-plugin",
    async buildStart() {
      const protocPath = await which("protoc");
      const includePath = path.join(protocPath, "..", "..", "include");
      console.log(includePath);
      await promisify(cp.exec)(
        `protoc ${extraArgs} --proto_path="${protoPath}" --proto_path="${includePath}" ${files.join(
          " "
        )}`
      );
    },
  };
};

export default defineConfig({
  build: {
    rollupOptions: {
      input: {
        main: path.resolve(__dirname, "index.html"),
      },
    },
  },
  plugins: [
    // Convert Elm files to JS
    elmPlugin(),
    protocPlugin(__dirname, "--elm_out=generated", [
      "todos.proto",
      "google/protobuf/timestamp.proto",
    ]),
  ],
  server: {
    host: true,
    proxy: {
      "/todos": "http://127.0.0.1:3000",
    },
  },
});

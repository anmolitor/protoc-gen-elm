import cp from "child_process";
import fs from "fs";
import path from "path";
import { promisify } from "util";
import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

const protocPlugin = (protoPath, extraArgs, files) => {
  return {
    name: "run-protoc-plugin",
    async buildStart() {
      await promisify(fs.rm)(path.join(__dirname, "generated", "Proto"), {
        recursive: true,
        force: true,
      });
      await promisify(cp.exec)(
        `protoc ${extraArgs} --proto_path="${protoPath}" ${files.join(" ")}`
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
    elmPlugin(),
    protocPlugin(__dirname, "--elm_out=generated", ["todos.proto"]),
  ],
  server: {
    host: true,
    proxy: {
      "/todos": "http://127.0.0.1:3000",
    },
  },
});

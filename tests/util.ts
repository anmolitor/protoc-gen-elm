import cp from "child_process";
import path from "path";

const protoPath = path.join(__dirname, "proto");
export const generatedPath = path.join(__dirname, "..", "generated");

const exec = (command: string): Promise<void> =>
  new Promise((resolve, reject) => {
    cp.exec(command, (err) => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });

export const runPlugin = async (protoFileOrFiles: string | string[]) => {
  const args = Array.isArray(protoFileOrFiles)
    ? protoFileOrFiles.join(" ")
    : protoFileOrFiles;
  await exec(
    `protoc --plugin="protoc-gen-elm=${path.resolve(
      __dirname,
      "..",
      "index.js"
    )}" --proto_path=${protoPath} --elm_out=${generatedPath} ${args}`
  );
};

export const compileElm = async (elmFileOrFiles: string | string[]) => {
  const outputFilenames = Array.isArray(elmFileOrFiles)
    ? elmFileOrFiles
    : [elmFileOrFiles];
  // check that the generated files compile
  await exec(
    `elm make ${outputFilenames
      .map((filename) => path.join(generatedPath, filename))
      .join(" ")}`
  );
};

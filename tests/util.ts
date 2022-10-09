import cp from "child_process";
import { Stats } from "fs";
import fs from "fs/promises";
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

export const runPluginForAllFiles = async () => {
  const files = await getProtoFilesRecursive([protoPath]);
  const withoutProtoPathPrefix = files.map((file) =>
    file.replace(`${protoPath}${path.sep}`, "")
  );
  await runPlugin(withoutProtoPathPrefix);
};

const getProtoFilesRecursive = async (dirs: string[]): Promise<string[]> => {
  const promises = dirs.map((dir) =>
    fs
      .readdir(dir)
      .then((files) => files.map((fileName) => path.join(dir, fileName)))
  );
  const filePaths = (await Promise.all(promises)).flatMap((a) => a);
  const withStats: [string, Stats][] = await Promise.all(
    filePaths.map(async (filePath) => [filePath, await fs.stat(filePath)])
  );
  const protoFilePaths = withStats
    .filter(
      ([filePath, stats]) => stats.isFile() && filePath.endsWith(".proto")
    )
    .map(([filePath]) => filePath);
  const nestedDirs = withStats
    .filter(([, stats]) => stats.isDirectory())
    .map(([filePath]) => filePath);

  if (nestedDirs.length > 0) {
    return [...protoFilePaths, ...(await getProtoFilesRecursive(nestedDirs))];
  }
  return protoFilePaths;
};

const runPlugin = async (protoFileOrFiles: string | string[]) => {
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

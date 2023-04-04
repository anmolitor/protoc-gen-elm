import cp from "child_process";
import path from "path";

const protoPath = path.join(__dirname, "proto");
export const generatedPath = path.join(__dirname, "..", "generated");

const exec = (exe: string, args: string[] = []): Promise<void> => {
  return new Promise<void>((resolve, reject) => {
    const handleExitCode = (exitCode: number | null | undefined) => {
      if (exitCode) {
        reject(new Error(`Exit code: ${exitCode}`));
        return;
      }
      resolve();
    };
    const spawnedProcess = cp.spawn(exe, args);
    spawnedProcess.stdout.pipe(process.stdout);
    spawnedProcess.stderr.pipe(process.stderr);
    spawnedProcess.on("close", handleExitCode);
    spawnedProcess.on("exit", handleExitCode);
    spawnedProcess.on("disconnect", handleExitCode);
  });
};

export const compileElm = async (elmFileOrFiles: string | string[]) => {
  const outputFilenames = Array.isArray(elmFileOrFiles)
    ? elmFileOrFiles
    : [elmFileOrFiles];
  // check that the generated files compile
  await exec("elm", [
    "make",
    ...outputFilenames.map((filename) => path.join(generatedPath, filename)),
  ]);
};

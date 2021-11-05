import cp from "child_process";
import { Readable } from "stream";

const waitForMs = (ms: number) =>
  new Promise((resolve) => {
    setTimeout(resolve, ms);
  });

interface Cancelable<T> {
  promise: Promise<string>;
  cancel: () => void;
}

/**
 * Sets up event listeners on the given stream, such that
 * it resolves when
 *  1. at least one `data` event has occured
 *  2. there was no further `data` event in a timespan of 50ms
 *
 * @param stream
 * @returns The data received from one or more `data` events
 */
const waitForOutput = (stream: Readable): Cancelable<string> => {
  let resolve: (data: string) => void = () => {};
  let data = "";

  let timer = 0;
  const startTimer = () => {
    const handle = setInterval(() => {
      timer += 10;
      if (timer > 50) {
        stream.off("data", onData);
        clearInterval(handle);
        resolve(data);
      }
    }, 10);
  };
  const onData = (chunk: Buffer) => {
    if (data == "") {
      startTimer();
    } else {
      timer = 0;
    }
    data += chunk.toString("utf-8");
  };
  return {
    promise: new Promise((res) => {
      resolve = res;
      stream.on("data", onData);
    }),
    cancel: () => stream.off("data", onData),
  };
};

export interface Repl {
  importModules: (...moduleNames: string[]) => Promise<void>;
  write: (command: string) => Promise<string>;
  writeMultiple: (...commands: string[]) => Promise<string[]>;
  getFreshVariable: () => string;
  stop: () => void;
}

/**
 * Creates a child process running the elm repl.
 *
 * You can import modules using `repl.importModule`.
 * > repl.importModule('Protobuf.Encode')
 *
 * You can run commands in the repl using `repl.write`. Commands will have newlines appended automatically.
 * The output of the command will get returned to you, if a command resulted in an error, the functions rejects with the given error.
 * Caveat: Commands that do not produce any output from the repl will result in a timeout.
 *
 * > repl.write('1 + 1')
 *
 * Commands that assign variables are prone to race conditions, so you should use
 * `getFreshVariable` to get an unused one.
 */
export const startRepl = async (): Promise<Repl> => {
  const replProcess = cp.spawn("elm", ["repl"]);
  replProcess.stdout.pipe(process.stdout);
  replProcess.stderr.pipe(process.stderr);

  const write = async (input: string): Promise<string> => {
    const stdoutCancelable = waitForOutput(replProcess.stdout);
    const stderrCancelable = waitForOutput(replProcess.stderr);
    const answer = Promise.race([
      stdoutCancelable.promise,
      stderrCancelable.promise.then((err) => {
        throw new Error(err);
      }),
      // timeout if repl does not respond
      waitForMs(2000).then(() => {
        throw new Error(
          `Timed out when waiting for output of command '${input}'`
        );
      }),
    ]);
    process.stdout.write(input + "\n");
    replProcess.stdin.write(input + "\n");
    const awaited = await answer;
    stdoutCancelable.cancel();
    stderrCancelable.cancel();
    return awaited;
  };

  const writeMultiple = async (...commands: string[]) => {
    const results = [];
    for (const command of commands) {
      results.push(await write(command));
    }
    return results;
  };

  const importModules = async (...moduleNames: string[]) => {
    await writeMultiple(
      ...moduleNames.map((moduleName) => `import ${moduleName}\n`)
    );
  };

  const stop = (): Promise<void> =>
    new Promise((resolve, reject) => {
      replProcess.on("exit", (err) => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      });
      replProcess.stdin.write(":exit\n");
    });

  await waitForMs(1000);

  const varGen = variableGenerator();

  return {
    write,
    writeMultiple,
    importModules,
    getFreshVariable: () => varGen.next().value,
    stop,
  };
};

const variableGenerator = function* () {
  let start = 0;
  while (true) {
    yield "x" + start++;
  }
};

import cp from "child_process";
import { Readable } from "stream";

const waitForMs = (ms: number) =>
  new Promise((resolve) => {
    setTimeout(resolve, ms);
  });

/**
 * Sets up event listeners on the given stream, such that
 * it resolves when
 *  1. at least one `data` event has occured
 *  2. there was no further `data` event in a timespan of 50ms
 *
 * @param stream
 * @returns The data received from one or more `data` events
 */
const waitForOutput = async (stream: Readable): Promise<string> =>
  new Promise((resolve) => {
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
    stream.on("data", onData);
  });

interface Repl {
  importModules: (...moduleNames: string[]) => Promise<void>;
  write: (command: string) => Promise<string>;
  writeMultiple: (...commands: string[]) => Promise<string[]>;
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
 */
export const withRepl = async (
  useRepl: (repl: Repl) => Promise<void>
): Promise<void> => {
  const repl = cp.spawn("elm", ["repl"]);
  repl.stdout.pipe(process.stdout);
  repl.stderr.pipe(process.stderr);

  const write = (input: string): Promise<string> => {
    const answer = Promise.race([
      waitForOutput(repl.stdout),
      waitForOutput(repl.stderr).then((err) => {
        throw new Error(err);
      }),
      // timeout if repl does not respond
      waitForMs(500).then(() => {
        throw new Error(
          `Timed out when waiting for output of command '${input}'`
        );
      }),
    ]);
    process.stdout.write(input + "\n");
    repl.stdin.write(input + "\n");
    return answer;
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

  const stop = () => repl.stdin.write(":exit\n");

  await waitForMs(500);

  const replInterface = {
    write,
    writeMultiple,
    importModules,
  };

  try {
    await useRepl(replInterface);
    stop();
  } catch (err) {
    stop();
    throw err;
  }
};

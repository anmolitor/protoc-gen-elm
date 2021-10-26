const cp = require("child_process");
const path = require("path");
const fs = require("fs");
const { promisify } = require("util");

const protoPath = path.join(__dirname, "proto");
const generatedPath = path.join(__dirname, "generated");

const cases = [
  { input: "single_enum.proto", output: "Proto/SingleEnum.elm" },
  { input: "basic_message.proto", output: "Proto/BasicMessage.elm" },
  { input: "oneof.proto", output: "Proto/Oneof.elm" },
];

const exec = (command) =>
  new Promise((resolve, reject) => {
    cp.exec(command, (err) => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });

const readFile = (filePath) =>
  promisify(fs.readFile)(filePath, { encoding: "utf-8" });

describe("protoc-plugin-elm", () => {
  beforeAll((done) => {
    fs.mkdir(generatedPath, { recursive: true }, done);
  });

  afterAll((done) => {
    fs.rm(generatedPath, { recursive: true, force: true }, done);
  });

  test.each(cases)(
    "generates expected code for $input",
    async ({ input, output }) => {
      console.log(`Starting ${input} generation`);
      await exec(
        `protoc --plugin="protoc-gen-elm=index.js" --proto_path=${protoPath} --elm_out=tests/generated ${input}`
      );
      const expectedPath = path.join(generatedPath, output);
      await exec(`elm make ${expectedPath}`);
      const fileContent = await readFile(expectedPath);
      expect(fileContent).toMatchSnapshot();
    }
  );
});

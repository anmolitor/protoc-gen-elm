const cp = require("child_process");
const path = require("path");
const fs = require("fs");
const { promisify } = require("util");

const protoPath = path.join(__dirname, "proto");
const generatedPath = path.join(__dirname, "..", "generated");

const cases = [
  { input: "single_enum.proto", output: "Proto/SingleEnum.elm" },
  { input: "basic_message.proto", output: "Proto/BasicMessage.elm" },
  { input: "oneof.proto", output: "Proto/Oneof.elm" },
  {
    input: ["imported.proto", "importing.proto"],
    output: ["Proto/Importing.elm", "Proto/Imported.elm"],
  },
  { input: "multiple_oneof.proto", output: "Proto/MultipleOneof.elm" },
  { input: "package.proto", output: "Any/Package.elm" },
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
    fs.rm(generatedPath, { recursive: true, force: true }, () => {
      fs.mkdir(generatedPath, { recursive: true }, done);
    });
  });

  test.each(cases)(
    "generates expected code for $input",
    async ({ input, output }) => {
      console.log(`Starting ${input} generation`);
      const args = Array.isArray(input) ? input.join(" ") : input;
      await exec(
        `protoc --plugin="protoc-gen-elm=${path.resolve(
          __dirname,
          "..",
          "index.js"
        )}" --proto_path=${protoPath} --elm_out=${generatedPath} ${args}`
      );

      const outputFilenames = Array.isArray(output) ? output : [output];
      // check that the generated files compile
      await exec(
        `elm make ${outputFilenames
          .map((filename) => path.join(generatedPath, filename))
          .join(" ")}`
      );

      // compare with previously generated file as a regression test
      const generatedContent = await Promise.all(
        outputFilenames.map(async (filename) => {
          const outputPath = path.join(generatedPath, filename);
          return readFile(outputPath);
        })
      );
      expect(generatedContent.join("\n")).toMatchSnapshot();
    }
  );
});

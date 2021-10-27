import {
  compileElm,
  getGeneratedFileContents,
  runPlugin,
} from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin(["imported.proto", "importing.proto"]));
  const expectedElmFileNames = ["Proto/Imported.elm", "Proto/Importing.elm"];

  it("generates a valid elm file for imported.proto and importing.proto", async () => {
    await compileElm(expectedElmFileNames);
  });

  it("generates the expected code for imported.proto and importing.proto", async () => {
    const generatedContent = await getGeneratedFileContents(
      expectedElmFileNames
    );
    expect(generatedContent).toMatchSnapshot();
  });
});

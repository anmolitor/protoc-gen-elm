import {
  runPlugin,
  compileElm,
  getGeneratedFileContents,
} from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() =>
    runPlugin(["subdir/imported.proto", "subdir/importing.proto"])
  );
  const expectedElmFileName = [
    "Proto/Subdir/Imported.elm",
    "Proto/Subdir/Importing.elm",
  ];

  it("generates a valid elm file for files in subdirectory", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates the expected code for files in subdirectory", async () => {
    const generatedContent = await getGeneratedFileContents(
      expectedElmFileName
    );
    expect(generatedContent).toMatchSnapshot();
  });
});

import {
  compileElm,
  getGeneratedFileContents,
  runPlugin,
} from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin(["package.proto", "subdir/package.proto"]));
  const expectedElmFileName = [
    "Proto/Package.elm",
    "Pkg/Subdir/Package.elm",
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

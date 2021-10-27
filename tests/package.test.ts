const {
  runPlugin,
  compileElm,
  getGeneratedFileContents,
} = require("./snapshot_test_base");

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("package.proto"));
  const expectedElmFileName = "Any/Package.elm";

  it("generates a valid elm file for package.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates the expected code for package.proto", async () => {
    const generatedContent = await getGeneratedFileContents(
      expectedElmFileName
    );
    expect(generatedContent).toMatchSnapshot();
  });
});

const {
  runPlugin,
  compileElm,
  getGeneratedFileContents,
} = require("./snapshot_test_base");

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("single_enum.proto"));
  const expectedElmFileName = "Proto/SingleEnum.elm";

  it("generates a valid elm file for single_enum.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates the expected code for single_enum.proto", async () => {
    const generatedContent = await getGeneratedFileContents(expectedElmFileName);
    expect(generatedContent).toMatchSnapshot();
  });
});

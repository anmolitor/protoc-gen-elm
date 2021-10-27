import {
  runPlugin,
  compileElm,
  getGeneratedFileContents,
} from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("oneof.proto"));
  const expectedElmFileName = "Proto/Oneof.elm";

  it("generates a valid elm file for oneof.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates the expected code for oneof.proto", async () => {
    const generatedContent = await getGeneratedFileContents(
      expectedElmFileName
    );
    expect(generatedContent).toMatchSnapshot();
  });
});

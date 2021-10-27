import {
  compileElm,
  getGeneratedFileContents,
  runPlugin,
} from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("multiple_oneof.proto"));
  const expectedElmFileName = "Proto/MultipleOneof.elm";

  it("generates a valid elm file for multiple_oneof.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates the expected code for multiple_oneof.proto", async () => {
    const generatedContent = await getGeneratedFileContents(
      expectedElmFileName
    );
    expect(generatedContent).toMatchSnapshot();
  });
});

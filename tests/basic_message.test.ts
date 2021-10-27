import {
  runPlugin,
  compileElm,
  getGeneratedFileContents,
} from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("basic_message.proto"));
  const expectedElmFileName = "Proto/BasicMessage.elm";

  it("generates a valid elm file for basic_message.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates the expected code for basic_message.proto", async () => {
    const generatedContent = await getGeneratedFileContents(
      expectedElmFileName
    );
    expect(generatedContent).toMatchSnapshot();
  });
});

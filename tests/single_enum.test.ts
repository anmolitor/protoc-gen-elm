import { withRepl } from "./repl";
import {
  compileElm,
  getGeneratedFileContents,
  runPlugin,
} from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("single_enum.proto"));
  const expectedElmFileName = "Proto/SingleEnum.elm";

  it("generates a valid elm file for single_enum.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates working encoders and decoders", async () => {
    await withRepl(async (repl) => {
      await repl.importModules(
        "Proto.SingleEnum",
        "Protobuf.Decode",
        "Protobuf.Encode"
      );
      const output = await repl.write(
        "Proto.SingleEnum.encodeAnEnum Proto.SingleEnum.OptionB |> Protobuf.Encode.encode |> Protobuf.Decode.decode Proto.SingleEnum.decodeAnEnum"
      );
      expect(output).toEqual(expect.stringMatching(/Just.+OptionB/));
    });
  });

  xit("generates the expected code for single_enum.proto", async () => {
    const generatedContent = await getGeneratedFileContents(
      expectedElmFileName
    );
    expect(generatedContent).toMatchSnapshot();
  });
});

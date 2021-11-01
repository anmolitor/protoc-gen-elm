import { withRepl } from "./repl";
import { compileElm, runPlugin } from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("basic_message.proto"));
  const expectedElmFileName = "Proto/BasicMessage.elm";

  it("generates a valid elm file for basic_message.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates the expected code for basic_message.proto", () =>
    withRepl(async (repl) => {
      await repl.importModules(
        "Proto.BasicMessage as P",
        "Protobuf.Decode as D",
        "Protobuf.Encode as E"
      );
      await repl.write('x = P.BasicMessage "hi" 5 6.0 True');
      const output = await repl.write(
        "(P.encodeBasicMessage x |> E.encode |> D.decode P.decodeBasicMessage) == Just x"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    }));
});

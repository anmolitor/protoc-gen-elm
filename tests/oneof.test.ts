import { withRepl } from "./repl";
import { compileElm, runPlugin } from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("oneof.proto"));
  const expectedElmFileName = "Proto/Oneof.elm";

  it("generates a valid elm file for oneof.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates the expected code for oneof.proto", () =>
    withRepl(async (repl) => {
      await repl.importModules(
        "Proto.Oneof as P",
        "Protobuf.Decode as D",
        "Protobuf.Encode as E"
      );
      await repl.write('x = { msg = Just <| P.OneOfMsgAString "test" }');
      const output = await repl.write(
        "(P.encodeOneOf x |> E.encode |> D.decode P.decodeOneOf) == Just x"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    }));
});

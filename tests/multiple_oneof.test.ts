import { withRepl } from "./repl";
import { compileElm, runPlugin } from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("multiple_oneof.proto"));
  const expectedElmFileName = "Proto/MultipleOneof.elm";

  it("generates a valid elm file for multiple_oneof.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates working code for multiple_oneof.proto", async () =>
    withRepl(async (repl) => {
      await repl.importModules(
        "Protobuf.Decode as D",
        "Protobuf.Encode as E",
        "Proto.MultipleOneof as P"
      );
      await repl.write('x = { msg = Just <| P.Oneof1MsgOptionA "a" }');
      await repl.write(
        "P.encodeOneof1 x |> E.encode |> D.decode P.decodeOneof1"
      );
    }));
});

import { withRepl } from "./repl";
import { compileElm, runPlugin } from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin("package.proto"));
  const expectedElmFileName = "Proto/Package.elm";

  it("generates a valid elm file for package.proto", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates working code for package.proto", () =>
    withRepl(async (repl) => {
      await repl.importModules(
        "Protobuf.Decode as D",
        "Protobuf.Encode as E",
        "Proto.Package as P"
      );
      const output = await repl.write(
        "(P.encodeTest P.Test |> E.encode |> D.decode P.decodeTest) == Just P.Test"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    }));
});

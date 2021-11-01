import { withRepl } from "./repl";
import { compileElm, runPlugin } from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin(["package.proto", "subdir/package.proto"]));
  const expectedElmFileName = ["Proto/Package.elm", "Proto/Subdir/Package.elm"];

  it("generates a valid elm file for files in subdirectory", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates working code for files in subdirectory", () =>
    withRepl(async (repl) => {
      await repl.importModules(
        "Protobuf.Decode as D",
        "Protobuf.Encode as E",
        "Proto.Subdir.Package as P",
        "Proto.Package"
      );
      await repl.write("x = P.Something (Just Proto.Package.Test) True");
      const output = await repl.write(
        "(P.encodeSomething x |> E.encode |> D.decode P.decodeSomething) == Just x"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    }));
});

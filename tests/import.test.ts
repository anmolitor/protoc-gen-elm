import { withRepl } from "./repl";
import { compileElm, runPlugin } from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() => runPlugin(["imported.proto", "importing.proto"]));
  const expectedElmFileNames = ["Proto/Imported.elm", "Proto/Importing.elm"];

  it("generates a valid elm file for imported.proto and importing.proto", async () => {
    await compileElm(expectedElmFileNames);
  });

  it("generates working code for imported.proto", () =>
    withRepl(async (repl) => {
      await repl.importModules(
        "Protobuf.Encode as E",
        "Protobuf.Decode as D",
        "Proto.Imported as Imported"
      );
      await repl.write('x = { first = "test", second = True }');
      const output = await repl.write(
        "(Imported.encodeImported x |> E.encode |> D.decode Imported.decodeImported) == Just x"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    }));

  it("generates working code for importing.proto", () =>
    withRepl(async (repl) => {
      await repl.importModules(
        "Protobuf.Encode as E",
        "Protobuf.Decode as D",
        "Proto.Imported as Imported",
        "Proto.Importing as Importing"
      );
      await repl.write('y = { first = "test", second = True }');
      await repl.write('x = { normalProperty = "a", nestedProperty = Just y }');
      const output = await repl.write(
        "(Importing.encodeNested x |> E.encode |> D.decode Importing.decodeNested) == Just x"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    }));
});

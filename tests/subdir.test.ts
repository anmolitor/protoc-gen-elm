import { withRepl } from "./repl";
import { compileElm, runPlugin } from "./snapshot_test_base";

describe("protoc-gen-elm", () => {
  beforeAll(() =>
    runPlugin(["subdir/imported.proto", "subdir/importing.proto"])
  );
  const expectedElmFileName = [
    "Proto/Subdir/Imported.elm",
    "Proto/Subdir/Importing.elm",
  ];

  it("generates a valid elm file for files in subdirectory", async () => {
    await compileElm(expectedElmFileName);
  });

  it("generates working code for files in subdirectory", () =>
    withRepl(async (repl) => {
      await repl.importModules(
        "Protobuf.Decode as D",
        "Protobuf.Encode as E",
        "Proto.Subdir.Imported as Imported",
        "Proto.Subdir.Importing as Importing"
      );
      await repl.write(
        'x = Importing.Nested "b" (Just <| Imported.SubImported "a" False)'
      );
      const output = await repl.write(
        "(Importing.encodeNested x |> E.encode |> D.decode Importing.decodeNested) == Just x"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    }));
});

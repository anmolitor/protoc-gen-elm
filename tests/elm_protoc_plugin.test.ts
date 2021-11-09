import fs from "fs";
import { Repl, startRepl } from "./repl";
import { compileElm, generatedPath, runPlugin } from "./util";

describe("protoc-gen-elm", () => {
  let repl: Repl;

  beforeAll(async () => {
    console.log("Cleaning /generated folder");
    fs.rmSync(generatedPath, { recursive: true, force: true });
    fs.mkdirSync(generatedPath);

    repl = await startRepl();
    await repl.importModules("Protobuf.Decode as D", "Protobuf.Encode as E");
    console.log("Started elm repl.");
  });

  afterAll(() => repl.stop());

  describe("single enum", () => {
    beforeAll(() => runPlugin("single_enum.proto"));
    const expectedElmFileName = "Proto/SingleEnum.elm";

    it("generates a valid elm file for single_enum.proto", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working encoders and decoders", async () => {
      await repl.importModules("Proto.SingleEnum");
      const output = await repl.write(
        "Proto.SingleEnum.encodeAnEnum Proto.SingleEnum.AnEnum_OptionB |> E.encode |> D.decode Proto.SingleEnum.decodeAnEnum"
      );
      expect(output).toEqual(expect.stringMatching(/Just.+AnEnum_OptionB/));
    });
  });

  describe("basic message", () => {
    beforeAll(() => runPlugin("basic_message.proto"));
    const expectedElmFileName = "Proto/BasicMessage.elm";

    it("generates a valid elm file for basic_message.proto", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates the expected code for basic_message.proto", async () => {
      await repl.importModules("Proto.BasicMessage");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = Proto.BasicMessage.BasicMessage "hi" 5 6.0 True`
      );
      const output = await repl.write(
        `(Proto.BasicMessage.encodeBasicMessage ${freshVar} |> E.encode |> D.decode Proto.BasicMessage.decodeBasicMessage) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("oneof", () => {
    beforeAll(() => runPlugin("oneof.proto"));
    const expectedElmFileName = "Proto/Oneof.elm";

    it("generates a valid elm file for oneof.proto", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates the expected code for oneof.proto", async () => {
      await repl.importModules("Proto.Oneof");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { msg = Just <| Proto.Oneof.OneOfMsgAString "test" }`
      );
      const output = await repl.write(
        `(Proto.Oneof.encodeOneOf ${freshVar} |> E.encode |> D.decode Proto.Oneof.decodeOneOf) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("multiple oneof", () => {
    beforeAll(() => runPlugin("multiple_oneof.proto"));
    const expectedElmFileName = "Proto/MultipleOneof.elm";

    it("generates a valid elm file for multiple_oneof.proto", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for multiple_oneof.proto", async () => {
      await repl.importModules("Proto.MultipleOneof");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { msg = Just <| Proto.MultipleOneof.Oneof1MsgOptionA "a" }`
      );
      await repl.write(
        `(Proto.MultipleOneof.encodeOneof1 ${freshVar} |> E.encode |> D.decode Proto.MultipleOneof.decodeOneof1) == Just ${freshVar}`
      );
    });
  });

  describe("package", () => {
    beforeAll(() => runPlugin("package.proto"));
    const expectedElmFileName = "Proto/Package.elm";

    it("generates a valid elm file for package.proto", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for package.proto", async () => {
      await repl.importModules("Proto.Package");
      const output = await repl.write(
        "(Proto.Package.encodeTest Proto.Package.Test |> E.encode |> D.decode Proto.Package.decodeTest) == Just Proto.Package.Test"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("import", () => {
    beforeAll(() => runPlugin(["imported.proto", "importing.proto"]));
    const expectedElmFileNames = ["Proto/Imported.elm", "Proto/Importing.elm"];

    it("generates a valid elm file for imported.proto and importing.proto", async () => {
      await compileElm(expectedElmFileNames);
    });

    it("generates working code for imported.proto", async () => {
      await repl.importModules("Proto.Imported");
      const freshVar = repl.getFreshVariable();
      await repl.write(`${freshVar} = { first = "test", second = True }`);
      const output = await repl.write(
        `(Proto.Imported.encodeImported ${freshVar} |> E.encode |> D.decode Proto.Imported.decodeImported) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("generates working code for importing.proto", async () => {
      await repl.importModules("Proto.Imported", "Proto.Importing");
      const inner = repl.getFreshVariable();
      const outer = repl.getFreshVariable();
      await repl.write(`${inner} = { first = "test", second = True }`);
      await repl.write(
        `${outer} = { normalProperty = "a", nestedProperty = Just ${inner} }`
      );
      const output = await repl.write(
        `(Proto.Importing.encodeNested ${outer} |> E.encode |> D.decode Proto.Importing.decodeNested) == Just ${outer}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("subdirectory", () => {
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

    it("generates working code for files in subdirectory", async () => {
      await repl.importModules(
        "Proto.Subdir.Imported",
        "Proto.Subdir.Importing"
      );
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = Proto.Subdir.Importing.Nested "b" (Just <| Proto.Subdir.Imported.SubImported "a" False)`
      );
      const output = await repl.write(
        `(Proto.Subdir.Importing.encodeNested ${freshVar} |> E.encode |> D.decode Proto.Subdir.Importing.decodeNested) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("subdirectory imports", () => {
    beforeAll(() => runPlugin(["package.proto", "subdir/package.proto"]));
    const expectedElmFileName = [
      "Proto/Package.elm",
      "Proto/Subdir/Package.elm",
    ];

    it("generates a valid elm file for files in subdirectory", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for files in subdirectory", async () => {
      await repl.importModules("Proto.Subdir.Package", "Proto.Package");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = Proto.Subdir.Package.Something (Just Proto.Package.Test) True`
      );
      const output = await repl.write(
        `(Proto.Subdir.Package.encodeSomething ${freshVar} |> E.encode |> D.decode Proto.Subdir.Package.decodeSomething) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("maps", () => {
    beforeAll(() => runPlugin("map.proto"));
    const expectedElmFileName = "Proto/Map.elm";

    it("generates a valid elm file for maps", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for maps", async () => {
      await repl.importModules("Proto.Map", "Dict");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = Proto.Map.Bar (Dict.singleton "test" (Just <| Proto.Map.Foo "hi")) (Dict.fromList [(1, "a"), (5, "b")])`
      );
      const output = await repl.write(
        `(Proto.Map.encodeBar ${freshVar} |> E.encode |> D.decode Proto.Map.decodeBar) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("nested declarations", () => {
    beforeAll(() => runPlugin("nested.proto"));
    const expectedElmFileName = "Proto/Nested.elm";

    it("generates a valid elm file for nested messages and enums", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for nested messages and enums", async () => {
      await repl.importModules("Proto.Nested");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = Proto.Nested.Test Proto.Nested.TopLevel_LevelOne_LevelTwo_EnumLevelTwo_A`
      );
      const output = await repl.write(
        `(Proto.Nested.encodeTest ${freshVar} |> E.encode |> D.decode Proto.Nested.decodeTest) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("recursive declarations", () => {
    beforeAll(() =>
      runPlugin([
        "recursive.proto",
        "recursive_importing.proto",
        "recursive_imported.proto",
      ])
    );

    it("generates a valid elm file for recursive messages", async () => {
      await compileElm("Proto/Recursive.elm");
    });

    it("generates a valid elm file for imported recursive messages", async () => {
      await compileElm([
        "Proto/RecursiveImported.elm",
        "Proto/RecursiveImporting.elm",
      ]);
    });

    it("generates working code for recursive messages", async () => {
      await repl.importModules("Proto.Recursive");
      const innerRec = repl.getFreshVariable();
      const other = repl.getFreshVariable();
      const outerRec = repl.getFreshVariable();
      await repl.write(`${innerRec} = { rec = [], other = Nothing }`);
      await repl.write(`${other} = { rec = Just ${innerRec} }`);
      await repl.write(
        `${outerRec} = { rec = [Proto.Recursive.Recursive_ ${innerRec}], other = Just (Proto.Recursive.Other_ ${other}) }`
      );

      const output = await repl.write(
        `(Proto.Recursive.encodeRecursive ${outerRec} |> E.encode |> D.decode Proto.Recursive.decodeRecursive) == Just ${outerRec}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("weird names", () => {
    beforeAll(() => runPlugin("weird_names.proto"));
    const expectedElmFileName = "Proto/WeirdNames.elm";

    it("generates a valid elm file even with weird casing conventions", async () => {
      await compileElm(expectedElmFileName);
    });
  });

  describe("proto2 enums", () => {
    beforeAll(() => runPlugin("proto2_enum.proto"));
    const expectedElmFileName = "Proto/Proto2Enum.elm";

    it("generates a valid elm file for proto2 enum", async () => {
      await compileElm(expectedElmFileName);
    });
  });

  describe("proto2 required", () => {
    beforeAll(() => runPlugin("proto2_required.proto"));
    const expectedElmFileName = "Proto/Proto2Required.elm";

    it("generates a valid elm file for proto2 enum", async () => {
      await compileElm(expectedElmFileName);
    });
  });
});

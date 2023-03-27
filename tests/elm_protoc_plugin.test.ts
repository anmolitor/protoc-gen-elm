import fs from "fs";
import { Repl, startRepl } from "./repl";
import { makeRoundtripRunner, RoundtripRunner } from "./roundtrip";
import { compileElm, generatedPath, runPluginSelectively } from "./util";

jest.setTimeout(20000);

describe("protoc-gen-elm", () => {
  let repl: Repl;
  let roundtripRunner: RoundtripRunner;

  beforeAll(async () => {
    console.log("Cleaning /generated folder");
    fs.rmSync(generatedPath, { recursive: true, force: true });
    fs.mkdirSync(generatedPath);
    // await runPluginForAllFiles();
    await runPluginSelectively([
      "single_enum.proto",
      "basic_message.proto",
      "oneof.proto",
      "multiple_oneof.proto",
    ]);

    repl = await startRepl();
    await repl.importModules("Protobuf.Decode as D", "Protobuf.Encode as E");
    console.log("Started elm repl.");
    roundtripRunner = makeRoundtripRunner(repl);
  });

  afterAll(() => repl.stop());

  describe("single enum", () => {
    const expectedElmFileName = "Proto.elm";

    it("generates a valid elm file for single_enum.proto", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working encoders and decoders", async () => {
      await repl.importModules("Proto");
      const output = await repl.write(
        "Proto.encodeAnEnum Proto.AnEnum_OptionB |> E.encode |> D.decode Proto.decodeAnEnum"
      );
      expect(output).toEqual(expect.stringMatching(/Just.+AnEnum_OptionB/));
    });
  });

  describe("basic message", () => {
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

    it("is compatable with protobufjs", async () => {
      const msg = {
        stringProperty: "str",
        intProperty: 42,
        floatProperty: 3.14,
        boolProperty: true,
      };
      await roundtripRunner(
        { protoFileName: "basic_message", messageName: "BasicMessage" },
        msg,
        (actual) => expect(actual).toMatchCloseTo(msg)
      );
    });
  });

  describe("oneof", () => {
    const expectedElmFileName = "Proto/Oneof.elm";

    it("generates a valid elm file for oneof.proto", async () => {
      await compileElm(expectedElmFileName);
      await compileElm("Proto/Oneof/OneOf.elm");
    });

    it("generates the expected code for oneof.proto", async () => {
      await repl.importModules("Proto.Oneof", "Proto.Oneof.OneOf");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { msg = Just <| Proto.Oneof.OneOf.AString "test" }`
      );
      const output = await repl.write(
        `(Proto.Oneof.encodeOneOf ${freshVar} |> E.encode |> D.decode Proto.Oneof.decodeOneOf) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("is compatable with protobufjs", async () => {
      await roundtripRunner(
        {
          protoFileName: "oneof",
          messageName: "OneOf",
          elmModuleName: "Oneof",
        },
        { anInt: 69 }
      );
    });
  });

  describe("multiple oneof", () => {
    const expectedElmFileName = "Proto/MultipleOneof.elm";

    it("generates a valid elm file for multiple_oneof.proto", async () => {
      await compileElm(expectedElmFileName);
      await compileElm("Proto/MultipleOneof/Oneof1.elm");
      await compileElm("Proto/MultipleOneof/Oneof2.elm");
    });

    it("generates working code for multiple_oneof.proto", async () => {
      await repl.importModules(
        "Proto.MultipleOneof",
        "Proto.MultipleOneof.Oneof1",
        "Proto.MultipleOneof.Oneof2"
      );
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { msg = Just <| Proto.MultipleOneof.Oneof1.OptionA "a" }`
      );
      await repl.write(
        `(Proto.MultipleOneof.encodeOneof1 ${freshVar} |> E.encode |> D.decode Proto.MultipleOneof.decodeOneof1) == Just ${freshVar}`
      );
    });
  });

  // describe("package", () => {
  //   const expectedElmFileName = "Proto/Package.elm";

  //   it("generates a valid elm file for package.proto", async () => {
  //     await compileElm(expectedElmFileName);
  //   });

  //   it("generates working code for package.proto", async () => {
  //     await repl.importModules("Proto.Package");
  //     const output = await repl.write(
  //       "(Proto.Package.encodeTest Proto.Package.Test |> E.encode |> D.decode Proto.Package.decodeTest) == Just Proto.Package.Test"
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });
  // });

  // describe("import", () => {
  //   const expectedElmFileNames = ["Proto/Imported.elm", "Proto/Importing.elm"];

  //   it("generates a valid elm file for imported.proto and importing.proto", async () => {
  //     await compileElm(expectedElmFileNames);
  //   });

  //   it("generates working code for imported.proto", async () => {
  //     await repl.importModules("Proto.Imported");
  //     const freshVar = repl.getFreshVariable();
  //     await repl.write(`${freshVar} = { first = "test", second = True }`);
  //     const output = await repl.write(
  //       `(Proto.Imported.encodeImported ${freshVar} |> E.encode |> D.decode Proto.Imported.decodeImported) == Just ${freshVar}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });

  //   it("generates working code for importing.proto", async () => {
  //     await repl.importModules("Proto.Imported", "Proto.Importing");
  //     const inner = repl.getFreshVariable();
  //     const outer = repl.getFreshVariable();
  //     await repl.write(`${inner} = { first = "test", second = True }`);
  //     await repl.write(
  //       `${outer} = { normalProperty = "a", nestedProperty = Just ${inner} }`
  //     );
  //     const output = await repl.write(
  //       `(Proto.Importing.encodeNested ${outer} |> E.encode |> D.decode Proto.Importing.decodeNested) == Just ${outer}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });
  // });

  // describe("enum imports", () => {
  //   const expectedElmFileNames = [
  //     "Proto/ImportedEnum.elm",
  //     "Proto/ImportingEnum.elm",
  //   ];

  //   it("generates a valid elm file for imported_enum.proto and importing_enum.proto", async () => {
  //     await compileElm(expectedElmFileNames);
  //   });

  //   it("generates working code for imported_enum.proto", async () => {
  //     await repl.importModules("Proto.ImportedEnum");
  //     const freshVar = repl.getFreshVariable();
  //     await repl.write(
  //       `${freshVar} = Proto.ImportedEnum.SomeEnum_OptionAImported`
  //     );
  //     const output = await repl.write(
  //       `(Proto.ImportedEnum.encodeSomeEnum ${freshVar} |> E.encode |> D.decode Proto.ImportedEnum.decodeSomeEnum) == Just ${freshVar}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });

  //   it("generates working code for importing_enum.proto", async () => {
  //     await repl.importModules("Proto.ImportedEnum", "Proto.ImportingEnum");
  //     const inner = repl.getFreshVariable();
  //     const outer = repl.getFreshVariable();
  //     await repl.write(
  //       `${inner} = Proto.ImportedEnum.SomeEnum_OptionBImported`
  //     );
  //     await repl.write(`${outer} = { someEnum = ${inner} }`);
  //     const output = await repl.write(
  //       `(Proto.ImportingEnum.encodeMsg ${outer} |> E.encode |> D.decode Proto.ImportingEnum.decodeMsg) == Just ${outer}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });
  // });

  // describe("subdirectory", () => {
  //   const expectedElmFileName = [
  //     "Proto/Subdir/Imported.elm",
  //     "Proto/Subdir/Importing.elm",
  //   ];

  //   it("generates a valid elm file for files in subdirectory", async () => {
  //     await compileElm(expectedElmFileName);
  //   });

  //   it("generates working code for files in subdirectory", async () => {
  //     await repl.importModules(
  //       "Proto.Subdir.Imported",
  //       "Proto.Subdir.Importing"
  //     );
  //     const freshVar = repl.getFreshVariable();
  //     await repl.write(
  //       `${freshVar} = Proto.Subdir.Importing.NestedSubDir "b" (Just <| Proto.Subdir.Imported.SubImported "a" False)`
  //     );
  //     const output = await repl.write(
  //       `(Proto.Subdir.Importing.encodeNestedSubDir ${freshVar} |> E.encode |> D.decode Proto.Subdir.Importing.decodeNestedSubDir) == Just ${freshVar}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });
  // });

  // describe("subdirectory imports", () => {
  //   const expectedElmFileName = [
  //     "Proto/Package.elm",
  //     "Proto/Subdir/Package.elm",
  //   ];

  //   it("generates a valid elm file for files in subdirectory", async () => {
  //     await compileElm(expectedElmFileName);
  //   });

  //   it("generates working code for files in subdirectory", async () => {
  //     await repl.importModules("Proto.Subdir.Package", "Proto.Package");
  //     const freshVar = repl.getFreshVariable();
  //     await repl.write(
  //       `${freshVar} = Proto.Subdir.Package.Something (Just Proto.Package.Test) True`
  //     );
  //     const output = await repl.write(
  //       `(Proto.Subdir.Package.encodeSomething ${freshVar} |> E.encode |> D.decode Proto.Subdir.Package.decodeSomething) == Just ${freshVar}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });
  // });

  // describe("maps", () => {
  //   const expectedElmFileName = "Proto/Map.elm";

  //   it("generates a valid elm file for maps", async () => {
  //     await compileElm(expectedElmFileName);
  //   });

  //   it("generates working code for maps", async () => {
  //     await repl.importModules("Proto.Map", "Dict");
  //     const freshVar = repl.getFreshVariable();
  //     await repl.write(
  //       `${freshVar} = Proto.Map.Bar (Dict.singleton "test" (Just <| Proto.Map.Foo "hi")) (Dict.fromList [(1, "a"), (5, "b")])`
  //     );
  //     const output = await repl.write(
  //       `(Proto.Map.encodeBar ${freshVar} |> E.encode |> D.decode Proto.Map.decodeBar) == Just ${freshVar}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });

  //   it("is compatable with protobufjs", async () => {
  //     await roundtripRunner(
  //       { protoFileName: "map", messageName: "Bar", elmModuleName: "Map" },
  //       {
  //         foos: { a: { abc: "test" }, cd: { abc: "bla" } },
  //         idk: { 1: "one", 5: "five" },
  //       }
  //     );
  //   });
  // });

  // describe("map_in_package", () => {
  //   const expectedElmFileName = "Proto/MapInPackage.elm";

  //   it("generates a valid elm file for maps", async () => {
  //     await compileElm(expectedElmFileName);
  //   });

  //   it("generates working code for maps", async () => {
  //     await repl.importModules("Proto.MapInPackage", "Dict");
  //     const freshVar = repl.getFreshVariable();
  //     await repl.write(
  //       `${freshVar} = Proto.MapInPackage.Bar (Dict.singleton "test" (Just <| Proto.MapInPackage.Foo "hi"))`
  //     );
  //     const output = await repl.write(
  //       `(Proto.MapInPackage.encodeBar ${freshVar} |> E.encode |> D.decode Proto.MapInPackage.decodeBar) == Just ${freshVar}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });
  // });

  // describe("nested declarations", () => {
  //   const expectedElmFileName = "Proto/Nested.elm";

  //   it("generates a valid elm file for nested messages and enums", async () => {
  //     await compileElm(expectedElmFileName);
  //   });

  //   it("generates working code for nested messages and enums", async () => {
  //     await repl.importModules("Proto.Nested");
  //     const freshVar = repl.getFreshVariable();
  //     await repl.write(
  //       `${freshVar} = Proto.Nested.Test Proto.Nested.TopLevel_LevelOne_LevelTwo_EnumLevelTwo_A`
  //     );
  //     const output = await repl.write(
  //       `(Proto.Nested.encodeTest ${freshVar} |> E.encode |> D.decode Proto.Nested.decodeTest) == Just ${freshVar}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });
  // });

  // describe("recursive declarations", () => {
  //   it("generates a valid elm file for recursive messages", async () => {
  //     await compileElm("Proto/Recursive.elm");
  //   });

  //   it("generates a valid elm file for imported recursive messages", async () => {
  //     await compileElm([
  //       "Proto/RecursiveImported.elm",
  //       "Proto/RecursiveImporting.elm",
  //     ]);
  //   });

  //   it("generates working code for recursive messages", async () => {
  //     await repl.importModules("Proto.Recursive");
  //     const innerRec = repl.getFreshVariable();
  //     const other = repl.getFreshVariable();
  //     const outerRec = repl.getFreshVariable();
  //     await repl.write(`${innerRec} = { rec = [], other = Nothing }`);
  //     await repl.write(`${other} = { rec = Just ${innerRec} }`);
  //     await repl.write(
  //       `${outerRec} = { rec = [Proto.Recursive.Recursive_ ${innerRec}], other = Just (Proto.Recursive.Other_ ${other}) }`
  //     );

  //     const output = await repl.write(
  //       `(Proto.Recursive.encodeRecursive ${outerRec} |> E.encode |> D.decode Proto.Recursive.decodeRecursive) == Just ${outerRec}`
  //     );
  //     expect(output).toEqual(expect.stringContaining("True"));
  //   });
  // });

  // describe("weird names", () => {
  //   const expectedElmFileName = "Proto/WeirdNames.elm";

  //   it("generates a valid elm file even with weird casing conventions", async () => {
  //     await compileElm(expectedElmFileName);
  //   });
  // });

  // describe("proto2 enums", () => {
  //   const expectedElmFileName = "Proto/Proto2Enum.elm";

  //   it("generates a valid elm file for proto2 enum", async () => {
  //     await compileElm(expectedElmFileName);
  //   });
  // });

  // describe("proto2 required", () => {
  //   const expectedElmFileName = "Proto/Proto2Required.elm";

  //   it("generates a valid elm file for proto2 enum", async () => {
  //     await compileElm(expectedElmFileName);
  //   });
  // });

  // describe("proto2 group", () => {
  //   const expectedElmFileName = "Proto/Proto2Group.elm";

  //   it("generates a valid elm file for proto2 group", async () => {
  //     await compileElm(expectedElmFileName);
  //   });
  // });

  // describe("oneof with embedded types", () => {
  //   const expectedElmFileName = "Proto/OneofEmbedded.elm";

  //   it("generates a valid elm file for embedded types", async () => {
  //     await compileElm(expectedElmFileName);
  //   });
  // });

  // describe("multiple imports", () => {
  //   it("generates a valid elm file for multiple imports", async () => {
  //     await compileElm([
  //       "Proto/MultipleImports1.elm",
  //       "Proto/MultipleImports2.elm",
  //       "Proto/MultipleImports3.elm",
  //     ]);
  //   });
  // });

  // describe("nested oneofs", () => {
  //   it("generates a valid elm file for proto2 group", async () => {
  //     await compileElm(["Proto/NestedOneofs.elm"]);
  //   });
  // });

  // describe("int types", () => {
  //   it("generates a valid elm file for ints", async () => {
  //     await compileElm(["Proto/Ints.elm"]);
  //   });

  //   it("is compatable with protobufjs", async () => {
  //     await roundtripRunner(
  //       { protoFileName: "ints", messageName: "Ints" },
  //       {
  //         int32: 123,
  //         sint32: 123,
  //         sfixed32: 123,
  //         uint32: 123,
  //         fixed32: 123,
  //         int64: long.fromInt(2 ^ 33),
  //         sint64: long.fromInt(2 ^ 33),
  //         sfixed64: long.fromInt(2 ^ 33),
  //         uint64: long.fromInt(2 ^ 33, true),
  //         fixed64: long.fromInt(2 ^ 33, true),
  //       }
  //     );
  //   });
  // });
});

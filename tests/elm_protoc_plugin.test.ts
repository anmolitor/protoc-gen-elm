import long from "long";
import { Repl, startRepl } from "./repl";
import { RoundtripRunner, makeRoundtripRunner } from "./roundtrip";
import { compileElm } from "./util";

jest.setTimeout(100_000);

describe("protoc-gen-elm", () => {
  let repl: Repl;
  let roundtripRunner: RoundtripRunner;

  beforeAll(async () => {
    repl = await startRepl();
    await repl.importModules(
      "Protobuf.Decode as D",
      "Protobuf.Encode as E",
      "Json.Decode as JD",
      "Json.Encode as JE"
    );
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
      await repl.importModules("Proto.AnEnum");
      const output = await repl.write(
        "Proto.AnEnum.encodeAnEnum Proto.AnEnum.OptionB |> E.encode |> D.decode Proto.AnEnum.decodeAnEnum"
      );
      expect(output).toEqual(expect.stringMatching(/Just.+OptionB/));
    });

    it("roundtrips to json", async () => {
      await repl.importModules("Proto.AnEnum");
      const msg = repl.getFreshVariable();
      await repl.write(`${msg} = Proto.AnEnum.OptionB`);
      const output = await repl.write(
        `JD.decodeString Proto.AnEnum.jsonDecodeAnEnum (JE.encode 0 (Proto.AnEnum.jsonEncodeAnEnum ${msg})) == Ok ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("decoding json accepts enum names", async () => {
      await repl.importModules("Proto.AnEnum");
      const enumNameOutput = await repl.write(
        `JD.decodeString Proto.AnEnum.jsonDecodeAnEnum "\\"Option_B\\"" == Ok Proto.AnEnum.OptionB`
      );
      expect(enumNameOutput).toEqual(expect.stringContaining("True"));
    });

    it("decoding json accepts integer values", async () => {
      await repl.importModules("Proto.AnEnum");
      const enumNameOutput = await repl.write(
        `JD.decodeString Proto.AnEnum.jsonDecodeAnEnum "2" == Ok Proto.AnEnum.OPTIONC`
      );
      expect(enumNameOutput).toEqual(expect.stringContaining("True"));
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
        `${freshVar} = { stringProperty = "hi", intProperty = 5, floatProperty = 6.0, boolProperty = True }`
      );
      const output = await repl.write(
        `(Proto.BasicMessage.encodeBasicMessage ${freshVar} |> E.encode |> D.decode Proto.BasicMessage.decodeBasicMessage) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("roundtrips to json", async () => {
      await repl.importModules("Proto.BasicMessage");
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { stringProperty = "hi", intProperty = 5, floatProperty = 6.0, boolProperty = True }`
      );
      const json = repl.getFreshVariable();
      await repl.write(
        `${json} = Proto.BasicMessage.jsonEncodeBasicMessage ${msg} |> JE.encode 0`
      );
      const output = await repl.write(
        `JD.decodeString Proto.BasicMessage.jsonDecodeBasicMessage ${json} == Ok ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("roundtrips to json", async () => {
      await repl.importModules("Proto.BasicMessage");
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { stringProperty = "hi", intProperty = 5, floatProperty = 6.0, boolProperty = True }`
      );
      const json = repl.getFreshVariable();
      await repl.write(
        `${json} = Proto.BasicMessage.jsonEncodeBasicMessage ${msg} |> JE.encode 0`
      );
      const output = await repl.write(
        `JD.decodeString Proto.BasicMessage.jsonDecodeBasicMessage ${json} == Ok ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("decoding json accepts missing fields by using default values", async () => {
      await repl.importModules("Proto.BasicMessage");
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { stringProperty = "", intProperty = 0, floatProperty = 0, boolProperty = False }`
      );
      const output = await repl.write(
        `JD.decodeString Proto.BasicMessage.jsonDecodeBasicMessage "{}" == Ok ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("decoding json accepts null fields by using default values", async () => {
      await repl.importModules("Proto.BasicMessage");
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { stringProperty = "", intProperty = 0, floatProperty = 0, boolProperty = False }`
      );
      const output = await repl.write(
        `JD.decodeString Proto.BasicMessage.jsonDecodeBasicMessage "{ \\"stringProperty\\": null, \\"intProperty\\": null, \\"floatProperty\\": null, \\"boolProperty\\": null }" == Ok ${msg}`
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
        {
          protoFileName: "basic_message",
          messageName: "BasicMessage",
          elmModuleName: "Proto.BasicMessage",
        },
        msg,
        (actual) => expect(actual).toMatchCloseTo(msg)
      );
    });
  });

  describe("oneof", () => {
    const expectedElmFileName = "Proto/Oneof.elm";

    it("generates a valid elm file for oneof.proto", async () => {
      await compileElm(expectedElmFileName);
      await compileElm("Proto/Oneof/OneOf/Msg.elm");
    });

    it("generates the expected code for oneof.proto", async () => {
      await repl.importModules("Proto.Oneof", "Proto.Oneof.OneOf.Msg");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { msg = Just <| Proto.Oneof.OneOf.Msg.AString "test" }`
      );
      let output = await repl.write(
        `(Proto.Oneof.encodeOneOf ${freshVar} |> E.encode |> D.decode Proto.Oneof.decodeOneOf) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("is compatable with protobufjs", async () => {
      await roundtripRunner(
        {
          protoFileName: "oneof",
          messageName: "OneOf",
          elmModuleName: "Proto.Oneof",
        },
        { anInt: 69 }
      );
    });

    it("roundtrips to json", async () => {
      await repl.importModules("Proto.Oneof", "Proto.Oneof.OneOf.Msg");
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { msg = Just <| Proto.Oneof.OneOf.Msg.AString "test" }`
      );
      const json = repl.getFreshVariable();
      await repl.write(
        `${json} = Proto.Oneof.jsonEncodeOneOf ${msg} |> JE.encode 0`
      );
      const output = await repl.write(
        `JD.decodeString Proto.Oneof.jsonDecodeOneOf ${json} == Ok ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("multiple oneof", () => {
    const expectedElmFileName = "Proto/MultipleOneof.elm";

    it("generates a valid elm file for multiple_oneof.proto", async () => {
      await compileElm([
        expectedElmFileName,
        "Proto/MultipleOneof/Oneof1/Msg.elm",
        "Proto/MultipleOneof/Oneof2/Msg.elm",
        "Proto/MultipleOneof/Oneof2/Msg2.elm",
        "Proto/MultipleOneof/Oneof2/Msg3.elm",
      ]);
    });

    it("generates working code for multiple_oneof.proto", async () => {
      await repl.importModules(
        "Proto.MultipleOneof",
        "Proto.MultipleOneof.Oneof1.Msg"
      );
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { msg = Just <| Proto.MultipleOneof.Oneof1.Msg.OptionA "a" }`
      );
      await repl.write(
        `(Proto.MultipleOneof.encodeOneof1 ${freshVar} |> E.encode |> D.decode Proto.MultipleOneof.decodeOneof1) == Just ${freshVar}`
      );
    });
  });

  describe("package", () => {
    const expectedElmFileName = "Proto/Any.elm";

    it("generates a valid elm file for package.proto", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for package.proto", async () => {
      await repl.importModules("Proto.Any");
      const output = await repl.write(
        "(Proto.Any.encodeTest {} |> E.encode |> D.decode Proto.Any.decodeTest) == Just {}"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("nested package", () => {
    const expectedElmFileName = "Proto/Some/Nested/Stuff/Test.elm";

    it("generates a valid elm file for nested_package.proto", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for nested_package.proto", async () => {
      await repl.importModules("Proto.Some.Nested.Stuff.Test");
      const output = await repl.write(
        "(Proto.Some.Nested.Stuff.Test.encodeTest Proto.Some.Nested.Stuff.Test.A |> E.encode |> D.decode Proto.Some.Nested.Stuff.Test.decodeTest) == Just Proto.Some.Nested.Stuff.Test.A"
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("import", () => {
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

  describe("enum imports", () => {
    const expectedElmFileNames = [
      "Proto/ImportedEnum/SomeEnum.elm",
      "Proto/ImportingEnum.elm",
    ];

    it("generates a valid elm file for imported_enum.proto and importing_enum.proto", async () => {
      await compileElm(expectedElmFileNames);
    });

    it("generates working code for imported_enum.proto", async () => {
      await repl.importModules("Proto.ImportedEnum.SomeEnum");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = Proto.ImportedEnum.SomeEnum.OptionAImported`
      );
      const output = await repl.write(
        `(Proto.ImportedEnum.SomeEnum.encodeSomeEnum ${freshVar} |> E.encode |> D.decode Proto.ImportedEnum.SomeEnum.decodeSomeEnum) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("generates working code for importing_enum.proto", async () => {
      await repl.importModules(
        "Proto.ImportingEnum",
        "Proto.ImportedEnum.SomeEnum"
      );
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { someEnum = Proto.ImportedEnum.SomeEnum.OptionBImported }`
      );
      const output = await repl.write(
        `(Proto.ImportingEnum.encodeMsg ${msg} |> E.encode |> D.decode Proto.ImportingEnum.decodeMsg) == Just ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("roundtrips to json", async () => {
      await repl.importModules(
        "Proto.ImportingEnum",
        "Proto.ImportedEnum.SomeEnum"
      );
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { someEnum = Proto.ImportedEnum.SomeEnum.OptionBImported }`
      );
      const output = await repl.write(
        `(Proto.ImportingEnum.jsonEncodeMsg ${msg} |> JE.encode 0 |> JD.decodeString Proto.ImportingEnum.jsonDecodeMsg) == Ok ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("json decode null or missing enum field produces default value", async () => {
      await repl.importModules(
        "Proto.ImportingEnum",
        "Proto.ImportedEnum.SomeEnum"
      );
      const msgWithDefaultEnum = repl.getFreshVariable();
      await repl.write(
        `${msgWithDefaultEnum} = { someEnum = Proto.ImportedEnum.SomeEnum.OptionAImported }`
      );
      const nullOutput = await repl.write(
        `JD.decodeString Proto.ImportingEnum.jsonDecodeMsg """{ "someEnum": null }""" == Ok ${msgWithDefaultEnum}`
      );
      expect(nullOutput).toEqual(expect.stringContaining("True"));
      const missingOutput = await repl.write(
        `JD.decodeString Proto.ImportingEnum.jsonDecodeMsg "{}" == Ok ${msgWithDefaultEnum}`
      );
      expect(missingOutput).toEqual(expect.stringContaining("True"));
    });
  });

  describe("subdirectory", () => {
    const expectedElmFileName = [
      "Proto/Subimported.elm",
      "Proto/Subimporting.elm",
    ];

    it("generates a valid elm file for files in subdirectory", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for files in subdirectory", async () => {
      await repl.importModules("Proto.Subimported", "Proto.Subimporting");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { normalProperty = "b", nestedProperty = Just { first = "a", second = False } }`
      );
      const output = await repl.write(
        `(Proto.Subimporting.encodeNestedSubDir ${freshVar} |> E.encode |> D.decode Proto.Subimporting.decodeNestedSubDir) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("subdirectory imports", () => {
    const expectedElmFileName = ["Proto/Any.elm", "Proto/Pkg.elm"];

    it("generates a valid elm file for files in subdirectory", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for files in subdirectory", async () => {
      await repl.importModules("Proto.Any", "Proto.Pkg");
      const freshVar = repl.getFreshVariable();
      await repl.write(`${freshVar} = { sub = Just {}, other = True }`);
      const output = await repl.write(
        `(Proto.Pkg.encodeSomething ${freshVar} |> E.encode |> D.decode Proto.Pkg.decodeSomething) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("maps", () => {
    it("generates a valid elm file for maps", async () => {
      await compileElm(["Proto/Map.elm"]);
    });

    it("generates working code for maps", async () => {
      await repl.importModules("Proto", "Dict");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { foos = Dict.singleton "test" { abc = "hi" }, idk = Dict.fromList [(1, "a"), (5, "b")] }`
      );
      const output = await repl.write(
        `(Proto.encodeBar ${freshVar} |> E.encode |> D.decode Proto.decodeBar) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("generates working code for int64 maps", async () => {
      await repl.importModules("Proto", "Dict");
      const freshVar = repl.getFreshVariable();
      await repl.write(`${freshVar} = { idk = Dict.singleton (1, 2) "test" }`);
      const output = await repl.write(
        `(Proto.encodeInt64Map ${freshVar} |> E.encode |> D.decode Proto.decodeInt64Map) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("roundtrips to json", async () => {
      await repl.importModules("Proto", "Dict");
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { foos = Dict.singleton "test" { abc = "hi" }, idk = Dict.fromList [(1, "a"), (5, "b")] }`
      );
      const json = repl.getFreshVariable();
      await repl.write(`${json} = Proto.jsonEncodeBar ${msg} |> JE.encode 0`);

      const output = await repl.write(
        `JD.decodeString Proto.jsonDecodeBar ${json} == Ok ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("int64 map roundtrips to json", async () => {
      await repl.importModules("Proto", "Dict");
      const msg = repl.getFreshVariable();
      await repl.write(`${msg} = { idk = Dict.singleton (1, 2) "test" }`);
      const json = repl.getFreshVariable();
      await repl.write(
        `${json} = Proto.jsonEncodeInt64Map ${msg} |> JE.encode 0`
      );
      const output = await repl.write(
        `JD.decodeString Proto.jsonDecodeInt64Map ${json} == Ok ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("json decode null or missing defaults to empty map", async () => {
      await repl.importModules("Proto", "Dict");
      const expected = repl.getFreshVariable();
      await repl.write(`${expected} = { foos = Dict.empty, idk = Dict.empty }`);
      const output = await repl.write(
        `JD.decodeString Proto.jsonDecodeBar """{ "foos": null }""" == Ok ${expected}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("is compatable with protobufjs", async () => {
      await roundtripRunner(
        { protoFileName: "map", messageName: "Bar", elmModuleName: "Proto" },
        {
          foos: { a: { abc: "test" }, cd: { abc: "bla" } },
          idk: { 1: "one", 5: "five" },
        }
      );
    });
  });

  describe("map_in_package", () => {
    const expectedElmFileName = "Proto/Map.elm";

    it("generates a valid elm file for maps", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for maps", async () => {
      await repl.importModules("Proto.Map", "Dict");
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { foos = Dict.singleton "test" { abc = "hi" } }`
      );
      const output = await repl.write(
        `(Proto.Map.encodeBar ${freshVar} |> E.encode |> D.decode Proto.Map.decodeBar) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("nested declarations", () => {
    const expectedElmFileName = "Proto/Nested.elm";

    it("generates a valid elm file for nested messages and enums", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates working code for nested messages and enums", async () => {
      await repl.importModules(
        "Proto.Nested",
        "Proto.Nested.TopLevel",
        "Proto.Nested.TopLevel.LevelOne",
        "Proto.Nested.TopLevel.LevelOne.EnumLevelOne",
        "Proto.Nested.TopLevel.LevelOne.LevelTwo.EnumLevelTwo"
      );
      const freshVar = repl.getFreshVariable();
      await repl.write(
        `${freshVar} = { property = Proto.Nested.TopLevel.LevelOne.LevelTwo.EnumLevelTwo.A }`
      );
      const output = await repl.write(
        `(Proto.Nested.encodeTest ${freshVar} |> E.encode |> D.decode Proto.Nested.decodeTest) == Just ${freshVar}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("recursive declarations", () => {
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
      await repl.write(
        `${other} = { rec = Just <| Proto.Recursive.wrapRecursive ${innerRec} }`
      );
      await repl.write(
        `${outerRec} = { rec = [Proto.Recursive.wrapRecursive ${innerRec}], other = Just (Proto.Recursive.wrapOther ${other}) }`
      );

      const output = await repl.write(
        `(Proto.Recursive.encodeRecursive ${outerRec} |> E.encode |> D.decode Proto.Recursive.decodeRecursive) == Just ${outerRec}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("weird names", () => {
    const expectedElmFileName = "Proto/WeirdNames.elm";

    it("generates a valid elm file even with weird casing conventions", async () => {
      await compileElm(expectedElmFileName);
    });
  });

  describe("proto2 enums", () => {
    const expectedElmFileName = "Proto/Proto2Enum/Proto2.elm";

    it("generates a valid elm file for proto2 enum", async () => {
      await compileElm(expectedElmFileName);
    });
  });

  describe("proto2 required", () => {
    const expectedElmFileName = "Proto/Proto2Required.elm";

    it("generates a valid elm file for proto2 enum", async () => {
      await compileElm(expectedElmFileName);
    });
  });

  describe("proto2 group", () => {
    const expectedElmFileName = "Proto/Proto2Group.elm";

    it("generates a valid elm file for proto2 group", async () => {
      await compileElm(expectedElmFileName);
    });
  });

  describe("proto3 optional", () => {
    const expectedElmFileName = "Proto/Proto3Optional.elm";

    it("generates a valid elm file for proto3 optional", async () => {
      await compileElm(expectedElmFileName);
    });

    it("generates the expected api", async () => {
      await repl.importModules(
        "Proto.Proto3Optional",
        "Proto.Proto3Optional.AnEnum"
      );
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { field = Just "", field2 = Nothing, field3 = Just <| Proto.Proto3Optional.AnEnum.A }`
      );

      const output = await repl.write(
        `(Proto.Proto3Optional.encodeWithOptional ${msg} |> E.encode |> D.decode Proto.Proto3Optional.decodeWithOptional) == Just ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("is compatible wih protobufjs", async () => {
      await roundtripRunner(
        {
          protoFileName: "proto3_optional",
          messageName: "WithOptional",
          elmModuleName: "Proto.Proto3Optional",
        },
        { field: undefined, field2: 0 }
      );
    });
  });

  describe("oneof with embedded types", () => {
    const expectedElmFileName = "Proto/OneofEmbedded.elm";

    it("generates a valid elm file for embedded types", async () => {
      await compileElm(expectedElmFileName);
    });
  });

  describe("oneof named 'result'", () => {
      const expectedElmFileName = "Proto/OneofResult.elm";

      it("generates a valid elm file for a oneof named 'result'", async () => {
          await compileElm(expectedElmFileName);
      });
  })

  describe("oneof with enums", () => {
    it("generates working code for a oneof using enums", async () => {
      await repl.importModules(
        "Proto.EnumOf",
        "Proto.EnumOf.MyEnum",
        "Proto.EnumOf.EnumOneOf.YourEnum",
        "Proto.EnumOf.EnumOneOf.Msg"
      );
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { msg = Just <| Proto.EnumOf.EnumOneOf.Msg.MyEnum Proto.EnumOf.MyEnum.A }`
      );

      const output = await repl.write(
        `(Proto.EnumOf.encodeEnumOneOf ${msg} |> E.encode |> D.decode Proto.EnumOf.decodeEnumOneOf) == Just ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("multiple imports", () => {
    it("generates a valid elm file for multiple imports", async () => {
      await compileElm([
        "Proto/MultipleImports1.elm",
        "Proto/MultipleImports2.elm",
      ]);
    });
  });

  describe("nested oneofs", () => {
    it("generates a valid elm file for nested oneofs", async () => {
      await compileElm(["Proto/NestedOneofs.elm"]);
    });
  });

  describe("recursive oneofs", () => {
    it("generates a valid elm file for recursive oneofs", async () => {
      await compileElm(["Proto/RecursiveOneof.elm"]);
    });

    it("wrapper types work correctly for direct recursion", async () => {
      await repl.importModules(
        "Proto.RecursiveOneof",
        "Proto.RecursiveOneof.Rec.Msg"
      );
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { msg = Just <| Proto.RecursiveOneof.Rec.Msg.Rec <| Proto.RecursiveOneof.wrapRec { msg = Nothing } }`
      );

      const output = await repl.write(
        `(Proto.RecursiveOneof.encodeRec ${msg} |> E.encode |> D.decode Proto.RecursiveOneof.decodeRec) == Just ${msg}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("wrapper types work correctly for two stage recursion", async () => {
      await repl.importModules(
        "Proto.RecursiveOneof",
        "Proto.RecursiveOneof.LayerTwo.Msg"
      );
      const layerOne = repl.getFreshVariable();
      await repl.write(
        `${layerOne} = Proto.RecursiveOneof.LayerTwo.Msg.LayerOne <| Proto.RecursiveOneof.wrapLayerOne { layerTwo = [] }`
      );
      const layerTwo = repl.getFreshVariable();
      await repl.write(
        `${layerTwo} = Proto.RecursiveOneof.wrapLayerTwo { msg = Just ${layerOne} }`
      );
      const outerLayerOne = repl.getFreshVariable();
      await repl.write(`${outerLayerOne} = { layerTwo = [${layerTwo}] }`);

      const output = await repl.write(
        `(Proto.RecursiveOneof.encodeLayerOne ${outerLayerOne} |> E.encode |> D.decode Proto.RecursiveOneof.decodeLayerOne) == Just ${outerLayerOne}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("int types", () => {
    it("generates a valid elm file for ints", async () => {
      await compileElm(["Proto/Ints.elm"]);
    });

    it("is compatable with protobufjs", async () => {
      await roundtripRunner(
        {
          protoFileName: "ints",
          messageName: "Ints",
          elmModuleName: "Proto.Ints",
        },
        {
          int32: 123,
          sint32: 123,
          sfixed32: 123,
          uint32: 123,
          fixed32: 123,
          int64: long.fromInt(2 ^ 33),
          sint64: long.fromInt(2 ^ 33),
          sfixed64: long.fromInt(2 ^ 33),
          uint64: long.fromInt(2 ^ 33, true),
          fixed64: long.fromInt(2 ^ 33, true),
        }
      );
    });

    it("json decoding accepts numbers and strings", async () => {
      let json = JSON.stringify({
        int32: 123,
        sint32: "-123",
        sfixed32: "-456",
        uint32: 456,
        fixed32: "456",
        int64: 8589934592,
        sint64: "-8589934592",
        sfixed64: "-18589934592",
        uint64: 18589934592,
        fixed64: "18589934592",
      }).replace(/"/g, '\\"');
      await repl.importModules("Proto.Ints", "Protobuf.Types.Int64 as Int64");
      const expected =
        "{ int32 = 123, sint32 = -123, sfixed32 = -456, uint32 = 456, fixed32 = 456, " +
        "int64 = Int64.fromInts 2 0, sint64 = Int64.fromInts -2 0, sfixed64 = Int64.fromInts -5 -1410065408, " +
        "uint64 = Int64.fromInts 4 1410065408, fixed64 = Int64.fromInts 4 1410065408 }";
      const output = await repl.write(
        `JD.decodeString Proto.Ints.jsonDecodeInts "${json}" == Ok ${expected}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("grpc", () => {
    it("generates a valid elm file for grpc.proto", async () => {
      await compileElm([
        "Proto/SomeGrpc/OtherService.elm",
        "Proto/SomeGrpc/GrpcService.elm",
      ]);
    });

    it("integrates with elm-grpc", async () => {
      await repl.importModules("Proto.SomeGrpc.GrpcService", "Grpc");
      await repl.write(
        "Grpc.new Proto.SomeGrpc.GrpcService.getOrders {} |> Grpc.toCmd identity"
      );
    });
  });

  describe("repeated", () => {
    it("json decoding accepts null as an empty list", async () => {
      await repl.importModules("Proto.Rep");
      const output = await repl.write(
        `Ok { ints = [] } == JD.decodeString Proto.Rep.jsonDecodeRepeat "{\\"ints\\":null}"`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("floats", () => {
    it("json decoding accepts exponent notation", async () => {
      await repl.importModules("Proto.Float");
      const output = await repl.write(
        `Ok { f = 3.14e3, d = 2.02e5 } == JD.decodeString Proto.Float.jsonDecodeFloats "{\\"f\\":3.14e3,\\"d\\":2.02e5}"`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("json decoding accepts strings", async () => {
      await repl.importModules("Proto.Float");
      const output = await repl.write(
        `Ok { f = 3.14, d = 2.02e5 } == JD.decodeString Proto.Float.jsonDecodeFloats "{\\"f\\":\\"3.14\\",\\"d\\":\\"2.02e5\\"}"`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("json decoding accepts Infinity/-Infinity", async () => {
      await repl.importModules("Proto.Float");
      const output = await repl.write(
        `Ok { f = 1/0, d = -1/0 } == JD.decodeString Proto.Float.jsonDecodeFloats "{\\"f\\":\\"Infinity\\",\\"d\\":\\"-Infinity\\"}"`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("json decoding accepts 'NaN'", async () => {
      await repl.importModules("Proto.Float");
      const output = await repl.write(
        `Ok True == (JD.decodeString Proto.Float.jsonDecodeFloats "{\\"f\\":\\"NaN\\",\\"d\\":\\"NaN\\"}" |> Result.map (.f >> isNaN))`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("json name", () => {
    it("respects the json_name property when encoding/decoding from json", async () => {
      await repl.importModules(
        "Proto.JsonName",
        "Proto.JsonName.CustomJsonNames.O"
      );
      const msg = repl.getFreshVariable();
      await repl.write(
        `${msg} = { protoIntName = 42, o = Just <| Proto.JsonName.CustomJsonNames.O.OptB "test" }`
      );
      const jsonOutput = await repl.write(
        `Proto.JsonName.jsonEncodeCustomJsonNames ${msg} |> JE.encode 0`
      );
      expect(jsonOutput).toEqual(
        expect.stringContaining(
          `{\\"my_int_name\\":42,\\"my_opt_b\\":\\"test\\"}`
        )
      );
      const output = await repl.write(
        `Ok ${msg} == (Proto.JsonName.jsonEncodeCustomJsonNames ${msg} |> JD.decodeValue Proto.JsonName.jsonDecodeCustomJsonNames)`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });
  });

  describe("well-known-types", () => {
    it("json timestamp encoding", async () => {
      await repl.importModules("Proto.Time");
      const timestamp = repl.getFreshVariable();
      const jsIsoTimestamp = new Date().toISOString();
      await repl.write(
        `${timestamp} = JD.decodeString Proto.Time.jsonDecodeStamp """{ "timestamp": "${jsIsoTimestamp}" }"""`
      );
      const output = await repl.write(
        `Result.map (Proto.Time.jsonEncodeStamp >> JE.encode 0) ${timestamp}`
      );
      expect(output).toEqual(
        expect.stringContaining(`{\\"timestamp\\":\\"${jsIsoTimestamp}\\"}`)
      );
    });

    it("json duration encoding", async () => {
      await repl.importModules("Proto.Time");
      const duration = repl.getFreshVariable();
      const durationInSeconds = "1234567.891s";
      await repl.write(
        `${duration} = JD.decodeString Proto.Time.jsonDecodeDurationMsg """{ "duration": "${durationInSeconds}" }"""`
      );
      const output = await repl.write(
        `Result.map (Proto.Time.jsonEncodeDurationMsg >> JE.encode 0) ${duration}`
      );
      expect(output).toEqual(
        expect.stringContaining(`{\\"duration\\":\\"${durationInSeconds}\\"}`)
      );
    });

    it("json field mask encoding", async () => {
      await repl.importModules("Proto.Mask");
      const mask = repl.getFreshVariable();
      const fieldMaskInCommaSeperatedForm =
        "fieldA.fieldB,fieldC,fieldD.fieldE.fieldF";
      await repl.write(
        `${mask} = JD.decodeString Proto.Mask.jsonDecodeWithMask """{ "mask": "${fieldMaskInCommaSeperatedForm}" }"""`
      );
      const output = await repl.write(
        `Result.map (Proto.Mask.jsonEncodeWithMask >> JE.encode 0) ${mask}`
      );
      expect(output).toEqual(
        expect.stringContaining(
          `{\\"mask\\":\\"${fieldMaskInCommaSeperatedForm}\\"}`
        )
      );
    });

    it("json wrapper types encoding", async () => {
      await repl.importModules(
        "Proto.Wrapper",
        "Protobuf.Types.Int64 as Int64",
        "Bytes.Encode"
      );
      const wrapper = repl.getFreshVariable();
      await repl.write(
        `${wrapper} = { bool = Just { value =  True }, int32 = Just { value = -42 }, int64 = Just { value = Int64.fromInts -5 123 }, uint32 = Just { value = 543 }, uint64 = Just { value = Int64.fromInts 12 34 }, bytes = Just { value = Bytes.Encode.encode (Bytes.Encode.unsignedInt8 7) }, float = Just { value = 7.89 }, double = Just { value = 8.91 }, string = Just { value = "a string" } }`
      );

      const json = repl.getFreshVariable();
      await repl.write(
        `${json} = Proto.Wrapper.jsonEncodeWrapper ${wrapper} |> JE.encode 0`
      );
      const jsonOutput = await repl.write(`${json}`);
      expect(jsonOutput).toEqual(
        expect.stringContaining(
          `{\\"bool\\":true,\\"int32\\":-42,\\"int64\\":\\"-21474836357\\",\\"uint32\\":543,\\"uint64\\":\\"51539607586\\",\\"bytes\\":\\"Bw==\\",\\"float\\":7.89,\\"double\\":8.91,\\"string\\":\\"a string\\"}`
        )
      );
      const output = await repl.write(
        `Ok ${wrapper} == JD.decodeString Proto.Wrapper.jsonDecodeWrapper ${json}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("json wrapper types encoding allows null", async () => {
      await repl.importModules(
        "Proto.Wrapper",
        "Protobuf.Types.Int64 as Int64",
        "Bytes.Encode"
      );
      const wrapper = repl.getFreshVariable();
      await repl.write(
        `${wrapper} = { bool = Nothing, int32 = Nothing, int64 = Nothing, uint32 = Nothing, uint64 = Nothing, bytes = Nothing, float = Nothing, double = Nothing, string = Nothing }`
      );

      const json = repl.getFreshVariable();
      await repl.write(
        `${json} = Proto.Wrapper.jsonEncodeWrapper ${wrapper} |> JE.encode 0`
      );
      const jsonOutput = await repl.write(`${json}`);
      expect(jsonOutput).toEqual(
        expect.stringContaining(
          `{\\"bool\\":null,\\"int32\\":null,\\"int64\\":null,\\"uint32\\":null,\\"uint64\\":null,\\"bytes\\":null,\\"float\\":null,\\"double\\":null,\\"string\\":null}`
        )
      );
      const output = await repl.write(
        `Ok ${wrapper} == JD.decodeString Proto.Wrapper.jsonDecodeWrapper ${json}`
      );
      expect(output).toEqual(expect.stringContaining("True"));
    });

    it("json value type encodes/decodes any json", async () => {
      const anyJson = JSON.stringify({
        hi: ["test", 5, true, null, 6.8, { obj: "abc" }],
        wow: null,
      }).replace(/"/g, '\\"');
      await repl.importModules("Proto.Google.Protobuf");
      const jsonOutput = await repl.write(
        `JD.decodeString Proto.Google.Protobuf.jsonDecodeValue "${anyJson}" |> Result.map (Proto.Google.Protobuf.jsonEncodeValue >> JE.encode 0)`
      );
      expect(jsonOutput).toEqual(expect.stringContaining(anyJson));
    });
  });
});

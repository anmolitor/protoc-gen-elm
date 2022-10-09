import Parser from "parsimmon";
import path from "path";
import Protobuf from "protobufjs";
import stripAnsi from "strip-ansi";
import { Repl } from "./repl";

import { toBeDeepCloseTo, toMatchCloseTo } from "jest-matcher-deep-close-to";
expect.extend({ toBeDeepCloseTo, toMatchCloseTo });

const arrToBytes = (arr: number[]): Uint8Array => {
  const bytes = new Uint8Array(new ArrayBuffer(arr.length));
  arr.forEach((value, index) => (bytes[index] = value));
  return bytes;
};

const bufferToArr = (buffer: ArrayBuffer): number[] => {
  const bytes = new Uint8Array(buffer);
  const arr = new Array(bytes.length);
  bytes.forEach((value, index) => (arr[index] = value));
  return arr;
};

const replArrParser: Parser.Parser<number[]> = Parser.string("Just ")
  .then(
    Parser.takeWhile((char) => char !== "]").map((arr) => JSON.parse(`${arr}]`))
  )
  .skip(Parser.all);

export type RoundtripRunner = (
  opts: { protoFileName: string; messageName: string; elmModuleName?: string },
  testObj: any
) => Promise<void>;

export const makeRoundtripRunner =
  (repl: Repl): RoundtripRunner =>
  async (
    { protoFileName, messageName, elmModuleName = messageName },
    testObj
  ) => {
    const root = await Protobuf.load(
      path.join(__dirname, "proto", `${protoFileName}.proto`)
    );
    const Message = root.lookupType(messageName);
    Message.verify(testObj);
    const bytes = Message.encode(Message.create(testObj)).finish();
    const asArray = bufferToArr(bytes);
    await repl.importModules(`Proto.${elmModuleName}`, "ByteUtil");
    const result = await repl
      .write(
        `ByteUtil.makeRoundtrip Proto.${elmModuleName}.decode${messageName} Proto.${elmModuleName}.encode${messageName} [${asArray}]`
      )
      .then(stripAnsi);
    const arrayAfterwards = replArrParser.tryParse(result);
    expect(arrayAfterwards).toEqual(asArray);
    const decodedMessage = Message.toObject(
      Message.decode(arrToBytes(arrayAfterwards))
    );
    expect(decodedMessage).toMatchCloseTo(testObj);
  };

import fs from "fs";
import { generatedPath } from "./snapshot_test_base";

console.log("Running before script");

export default () => {
  fs.rmSync(generatedPath, { recursive: true, force: true });
  fs.mkdirSync(generatedPath);
};

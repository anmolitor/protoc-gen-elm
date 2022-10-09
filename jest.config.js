/** @type {import('ts-jest/dist/types').InitialOptionsTsJest} */
module.exports = {
  preset: "ts-jest",
  testEnvironment: "node",
  transform: {
    "^.+(t|j)sx?$": "ts-jest", // transpile both `ts` + `js` files
  },
  transformIgnorePatterns: [
    "`/node_modules/(?!strip)`"
  ],
};

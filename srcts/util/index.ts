// What the client's modules share: helpers for plain data, for the text of
// messages and for errors, and the validator of a JSON Schema.

export {
  asError,
  frozenCopy,
  getAt,
  isObject,
  jsonEqual,
  listing,
  logicalOf,
  messageOf,
  numberOf,
  own,
  isPackageName,
  packageOf,
  pathText,
  quoted,
  report,
  setAt,
  without,
} from "./util.ts";
export { compile } from "./validate.ts";
export type { Validate } from "./validate.ts";

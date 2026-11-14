// What the client's modules share: helpers for plain data, for the text of
// messages and for errors, what raises the notifications of the board, and the
// validator of a JSON Schema.

export {
  asError,
  asLogical,
  asNumber,
  errorMessage,
  frozenCopy,
  getAt,
  isObject,
  jsonEqual,
  listing,
  own,
  isPackageName,
  packageFromId,
  pathText,
  quoted,
  report,
  setAt,
  without,
} from "./util.ts";
export {
  checkRaised,
  coreNotification,
  isBoardObject,
  notifyNothing,
  packageModule,
  raiseNothing,
  repliedNothing,
} from "./notification.ts";
export type { Concerns, Notify, Raise, Replied } from "./notification.ts";
export { compile } from "./validate.ts";
export type { Validate } from "./validate.ts";

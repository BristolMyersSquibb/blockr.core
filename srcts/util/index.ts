// What the client's modules share: helpers for plain data, for the text of
// messages and for errors, what raises the notifications of the board, the
// keyboard's shortcuts, the validator of a JSON Schema, the board's blocks and
// the block classes as core's board actions and their forms name, list and
// check them, and a module whose code loads as it first renders.

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
  setAt,
  without,
} from "./util.ts";
export {
  checkRaised,
  coreNotification,
  givenNotifier,
  holdsObject,
  isBoardObject,
  notifyNothing,
  raiseNothing,
  refusedNothing,
  repliedNothing,
  sourceModule,
} from "./notification.ts";
export type { Concerns, Notify, Raise, Refused, Replied } from "./notification.ts";
export { ariaShortcuts, matchesShortcut, onMac, shortcutLabel } from "./shortcuts.ts";
export { blockClasses, blockName, classInputs, inputNameError, isVariadicBlock, namesInto } from "./blocks.ts";
export type { ClassItem } from "./blocks.ts";
export { loadedModule } from "./modules.ts";
export { compile } from "./validate.ts";
export type { Validate } from "./validate.ts";

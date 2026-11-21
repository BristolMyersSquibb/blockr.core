// What a caller in plain JS gives core's board actions, checked, as the types
// of CoreApi would check it: a target, and the element that the user triggered
// from or in which core renders an advanced action.

import { isBoardObject, isObject } from "../util/index.ts";
import type { Target } from "./actions.ts";

// The target that a caller gives, copied, so that no board action changes what
// the next one gets. It throws for anything else, as a plugin in plain JS may
// give.
export function targetGiven(given: unknown): Target {
  if (isObject(given) && Object.keys(given).length === 1) {
    if (given.board === true) return Object.freeze({ board: true });
    const { selection } = given;
    if (Array.isArray(selection) && selection.every(isBoardObject)) {
      return Object.freeze({ selection: Object.freeze(selection.map((object) => Object.freeze({ ...object }))) });
    }
    if (isBoardObject(given)) return Object.freeze({ ...given });
  }
  throw new TypeError(
    `A board action acts on the board, as { board: true }, on a board object, as { block: "a" }, or on a selection of them, as { selection: [{ block: "a" }, { link: "ab" }] }, not ${JSON.stringify(given)}.`,
  );
}

// Whether a value is an element that the user can trigger from, which takes
// the keyboard's focus and has a box, or in which core renders an advanced
// action. An element of another window counts, which is no HTMLElement of
// this one.
export function isElement(value: unknown): value is HTMLElement {
  return (
    isObject(value) &&
    typeof (value as { focus?: unknown }).focus === "function" &&
    typeof (value as { getBoundingClientRect?: unknown }).getBoundingClientRect === "function"
  );
}

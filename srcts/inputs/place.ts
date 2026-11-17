// Where the list of a select sits in the window,
// in a browser without CSS anchor positioning, where script places it
// (./select.tsx).
// The list sits 4px below the select's field and lines up with its left edge.
// It sits above the field where it fits there but not below.
// It is at least as wide as the field, and at least MIN_WIDTH wide.
// It keeps 8px from the window's edges.
// Core's stylesheet places it the same way where the browser has anchor
// positioning (../runtime/index.css).

export interface Box {
  readonly left: number;
  readonly top: number;
  readonly bottom: number;
  readonly width: number;
}

export interface Size {
  readonly width: number;
  readonly height: number;
}

export interface Spot {
  readonly left: number;
  readonly top: number;
}

// The narrowest the list gets, in pixels, as the design system's dropdown of
// a field.
export const MIN_WIDTH = 190;

const GAP = 4;
const EDGE = 8;

// The width that the list takes at least, for the field given.
export function listWidth(field: Box): number {
  return Math.max(field.width, MIN_WIDTH);
}

// Where the list of the size given sits by the field, in a window of the size
// given.
export function listSpot(field: Box, list: Size, view: Size): Spot {
  const below = field.bottom + GAP;
  const above = field.top - GAP - list.height;
  const top = below + list.height > view.height - EDGE && above >= EDGE ? above : below;
  return {
    left: clamp(field.left, EDGE, view.width - EDGE - list.width),
    top: clamp(top, EDGE, view.height - EDGE - list.height),
  };
}

// The value nearest to `value` from `low` to `high`, or `low` where `high` is
// below it, as for a list taller than the window.
function clamp(value: number, low: number, high: number): number {
  return Math.max(low, Math.min(value, high));
}

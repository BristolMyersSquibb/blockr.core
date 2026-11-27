// The colours that a new stack starts from, which Create stack's quick action
// and the form of a stack offer (./stacks.ts, ./stack-form.tsx).

import type { Stack } from "../types/board.ts";

// ColorBrewer's Set2, of which a new stack takes the first that no stack has.
export const PALETTE = ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"] as const;

// The first colour of the palette that no stack has, or else the one that
// follows the number of stacks round the palette.
export function suggestedColor(stacks: readonly Stack[]): string {
  const used = new Set(stacks.map((stack) => stack.color?.toLowerCase()));
  return PALETTE.find((color) => !used.has(color)) ?? PALETTE[stacks.length % PALETTE.length] ?? PALETTE[0];
}

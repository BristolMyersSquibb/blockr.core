// The hook that says whether a card gives its block's output a height of its
// own.
// The card follows the block's status and the notifications about it through
// useStatus() and useNotified() of core's React SDK.

import { useState } from "react";
import type { BlockStatus } from "../scheduler/index.ts";

// Whether the card gives the block's output a height of its own: from the
// first time that the block reads ready on, for as long as the card is drawn.
// Core asks R for an output only once the block reads ready and the output's
// element measures more than 0 by 0, so the element measures nothing before,
// and a block that fails or reads invalid at once leaves no empty room.
// Core then keeps drawing the last output whatever the block's status, so the
// output keeps its height as the block fails or waits, and does not jump.
export function useSized(status: BlockStatus | undefined): boolean {
  const ready = status?.status === "ready";
  const [sized, setSized] = useState(ready);
  if (ready && !sized) setSized(true);
  return sized || ready;
}

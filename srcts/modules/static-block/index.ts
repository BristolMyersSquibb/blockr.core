// The UI of the static block, which draws nothing:
// an app script gives the block its expression,
// since no UI sets the code a block runs.

import type { BlockModule } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

const module: BlockModule<State> = {
  render: () => undefined,
};

export default module;

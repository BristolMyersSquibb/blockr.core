// The UI of the row-bind block, which draws nothing,
// since the block has no state args.

import type { BlockModule } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

const module: BlockModule<State> = {
  render: () => undefined,
};

export default module;

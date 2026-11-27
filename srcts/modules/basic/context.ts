// What every component of the basic frontend reads: CoreApi, the toasts, what
// is in view, which plugins' sections are collapsed and which stacks' groups.
// The frontend runs its edits with runCommand() of ../../ui.

import { createContext, useContext } from "react";
import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { InView } from "./demand.ts";
import type { Sections } from "./plugins.tsx";
import type { Toasts } from "./toasts.ts";

export interface FrontEnd {
  readonly core: CoreApi;
  readonly toasts: Toasts;
  readonly inView: InView;
  readonly sections: Sections<PluginClassId>;
  readonly groups: Sections;
}

export const FrontEndContext = createContext<FrontEnd | null>(null);

export function useFrontEnd(): FrontEnd {
  const given = useContext(FrontEndContext);
  if (given === null) throw new Error("A component of the basic frontend is drawn outside of it.");
  return given;
}

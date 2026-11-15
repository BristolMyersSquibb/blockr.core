// What every component of the basic frontend reads: CoreApi, the toasts and
// what is in view. The frontend runs its edits with runCommand() of ../../ui.

import { createContext, useContext } from "react";
import type { CoreApi } from "../../types/core-api.ts";
import type { InView } from "./demand.ts";
import type { Toasts } from "./toasts.ts";

export interface FrontEnd {
  readonly core: CoreApi;
  readonly toasts: Toasts;
  readonly inView: InView;
}

export const FrontEndContext = createContext<FrontEnd | null>(null);

export function useFrontEnd(): FrontEnd {
  const given = useContext(FrontEndContext);
  if (given === null) throw new Error("A component of the basic frontend is drawn outside of it.");
  return given;
}

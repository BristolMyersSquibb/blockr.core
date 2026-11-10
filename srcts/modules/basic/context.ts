// What every component of the basic frontend reads: CoreApi, the notices,
// what is in view, and how it runs an edit.

import { createContext, useContext } from "react";
import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import type { InView } from "./demand.ts";
import type { Notices } from "./notices.ts";

export interface FrontEnd {
  readonly core: CoreApi;
  readonly notices: Notices;
  readonly inView: InView;
}

export const FrontEndContext = createContext<FrontEnd | null>(null);

export function useFrontEnd(): FrontEnd {
  const given = useContext(FrontEndContext);
  if (given === null) throw new Error("A component of the basic frontend is drawn outside of it.");
  return given;
}

// Runs one of the commands that the command registry holds, as one step of
// the user, and resolves whether it ran, telling the user why where the
// command registry or the store refused it.
export function runCommand(front: Pick<FrontEnd, "core" | "notices">, id: string, args?: Json): Promise<boolean> {
  return front.core.commands.run(id, args).then(
    () => true,
    (error: unknown) => {
      front.notices.failed(error);
      return false;
    },
  );
}

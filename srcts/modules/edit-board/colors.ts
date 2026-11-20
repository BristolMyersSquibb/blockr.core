// The colours of core's React SDK, with which the stacks draw a stack's
// colour: a swatch in the table, and a colour picker in the form.
// The Edit board's module loads as the page starts, and the page loads the
// colours only once a module imports them (../../colors).
// So the stacks import the colours as they first show, rather than the module
// as it loads, and a page loads them only where the stacks show.
// Until they load, the stacks draw core's own swatch in their place.
// Where they fail to load, the stacks keep that swatch and log why.
// The next time the stacks show, they import the colours again.

import { useEffect, useState } from "react";
import type * as Colors from "../../colors/index.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { asError } from "../../util/index.ts";

export type ColorsModule = typeof Colors;

let loading: Promise<ColorsModule> | undefined;
let loaded: ColorsModule | null = null;

function load(): Promise<ColorsModule> {
  loading ??= import("../../colors/index.ts").then(
    (module) => {
      loaded = module;
      return module;
    },
    (error: unknown) => {
      loading = undefined;
      throw error;
    },
  );
  return loading;
}

// The colours, which the first call imports, or null until they have loaded.
export function useColors(core: CoreApi): ColorsModule | null {
  const [colors, setColors] = useState(loaded);
  useEffect(() => {
    if (colors !== null) return;
    let mounted = true;
    load().then(
      (module) => {
        if (mounted) setColors(module);
      },
      (error: unknown) => {
        core.log("blockr.core").error(asError(error));
      },
    );
    return () => {
      mounted = false;
    };
  }, [colors, core]);
  return colors;
}

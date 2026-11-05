// The hooks through which a module written in React follows its host, as
// React's useSyncExternalStore follows a store: the host's getters give a
// snapshot, which stays the same object until the host announces a change.

import { useCallback, useSyncExternalStore } from "react";
import type { Condition } from "../protocol/index.ts";
import type { OptionId } from "../types/board.ts";
import type { OutputHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";

// What an output host holds at one moment.
export interface OutputSnapshot {
  readonly output: Json;
  readonly body: Blob | null;
  readonly params: Json;
  readonly options: Readonly<Record<OptionId, Json>>;
  readonly loading: boolean;
  readonly error: Condition | null;
}

// The last snapshot of each host, which a component reads again until one of
// its parts changes.
const snapshots = new WeakMap<OutputHost, OutputSnapshot>();

// What the output host holds, for a renderer written in React, which draws
// again as the host announces "output".
export function useOutput(host: OutputHost): OutputSnapshot {
  const subscribe = useCallback((notify: () => void) => host.on("output", notify), [host]);
  const read = useCallback(() => snapshotOf(host), [host]);
  return useSyncExternalStore(subscribe, read);
}

function snapshotOf(host: OutputHost): OutputSnapshot {
  const next: OutputSnapshot = {
    output: host.output(),
    body: host.body(),
    params: host.params(),
    options: host.options(),
    loading: host.loading(),
    error: host.error(),
  };
  const last = snapshots.get(host);
  if (last !== undefined && (Object.keys(next) as (keyof OutputSnapshot)[]).every((key) => last[key] === next[key])) {
    return last;
  }
  snapshots.set(host, next);
  return next;
}

// What core gives a front-end, an extension and a startup module: `CoreApi`,
// through which they work on the board, as Front-ends and extension points
// has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-a-front-end).
// The modules that core mounts inside a front-end's layout get a host instead
// (./hosts.ts).

import type { Commands } from "../commands/commands.ts";
import type { Lock } from "../protocol/messages.ts";
import type { Scheduler } from "../scheduler/scheduler.ts";
import type { Options } from "../store/registrations.ts";
import type { Store } from "../store/store.ts";
import type { BlockId, OptionId, RegistryId } from "./board.ts";
import type { Json } from "./json.ts";
import type { RegistryEntry } from "./registry.ts";

export interface CoreApi {
  readonly store: Store;
  // The command registry, for menus and shortcuts.
  readonly commands: Commands;
  // The scheduler's methods for front-ends and extensions. Core alone calls
  // the others.
  readonly scheduler: Pick<Scheduler, "demand" | "evaluate" | "invalidate" | "status" | "subscribe">;
  // The block registry's entries by registry id, for pickers, which change as
  // another board opens.
  blockRegistry(): BlockRegistry;
  // The board options that packages define, by id.
  readonly options: Options;
  // The blocks that a locked board leaves open, from R's reply to `hello`.
  readonly lock: Lock | null;
  // What renderers report that the user did, such as a selection, for
  // extensions to act on.
  readonly outputEvents: OutputEvents;
  // An extension's request, by the extension's id.
  request<T = Json>(extension: string, name: string, args?: Json, opts?: RequestOpts): Promise<T | Blob>;
  // A module another package offers, by id.
  module<T = unknown>(id: string): Promise<T>;
  // A blockr option, asked of R once per page, or the fallback where R has
  // none.
  blockrOption<T extends Json>(name: string, fallback: T): Promise<T>;
  // The block's UI module, its output, and a board option's UI or the
  // renderer R picked for it, each mounted in the element until the cleanup
  // that the call returns runs.
  mountBlock(el: HTMLElement, block: BlockId): () => void;
  mountOutput(el: HTMLElement, block: BlockId): () => void;
  mountOption(el: HTMLElement, option: OptionId): () => void;
}

export type BlockRegistry = Readonly<Record<RegistryId, RegistryEntry>>;

export interface RequestOpts {
  // The blocks it reads, which core has evaluated first.
  reads?: BlockId[];
  // Gives it up, as a cancel does on the wire.
  signal?: AbortSignal;
  // Hears each progress note that R sends for it.
  progress?(note: { value?: number; message?: string }): void;
}

export interface OutputEvents {
  // Calls `fn` with each event that a renderer emits.
  subscribe(fn: (event: { block: BlockId; name: string; payload: Json }) => void): () => void;
}

// What core gives a front-end, an extension and a startup module: `CoreApi`,
// through which they work on the board, as Front-ends and extension points
// has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-a-front-end).
// Core makes one for each board that opens, and ends it as the next board
// opens, with the signal that the startup modules and the front-end get
// beside it. The modules that core mounts inside a front-end's layout get a
// host instead (./hosts.ts).

import type { Commands } from "../commands/index.ts";
import type { Lock } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Options, PluginClass, Store } from "../store/index.ts";
import type { BlockId, OptionId, PluginId, RegistryId } from "./board.ts";
import type { Json } from "./json.ts";
import type { RegistryEntry } from "./registry.ts";

export interface CoreApi {
  // The store of the board. Its open() starts core again in place for the
  // board given, which opens with a CoreApi of its own, once this store has
  // checked the board, and a reply to `hello` given opens its board as a JSON
  // board's text would.
  readonly store: Store;
  // The command registry, for menus and shortcuts.
  readonly commands: Commands;
  // The scheduler's methods for front-ends and extensions. Core alone calls
  // the others.
  readonly scheduler: Pick<Scheduler, "demand" | "evaluate" | "invalidate" | "status" | "subscribe">;
  // The block registry's entries by registry id, for pickers, as R's reply to
  // `hello` gives them for the board.
  blockRegistry(): BlockRegistry;
  // The board options that packages define, by id.
  readonly options: Options;
  // The plugin classes that packages define, each of which a package's
  // startup module registers under the class's id.
  readonly plugins: {
    register(def: PluginClass): () => void;
  };
  // The blocks that a locked board leaves open, from R's reply to `hello`.
  readonly lock: Lock | null;
  // What renderers report that the user did, such as a selection, for
  // extensions to act on.
  readonly outputEvents: OutputEvents;
  // A request of a plugin of the board, by the plugin's id, which the method
  // of R's plugin_requests() for the plugin's class answers. Its args go to
  // the request's function by name. Core has the blocks that it reads
  // evaluated first, and keeps none of its answers, since it may write.
  request<T = Json>(plugin: PluginId, name: string, args?: Readonly<Record<string, Json>>, opts?: RequestOpts): Promise<T | Blob>;
  // A module another package offers, by id.
  module<T = unknown>(id: string): Promise<T>;
  // A blockr option, asked of R once per page, or the fallback where nothing
  // sets it. It rejects where R cannot give the value, as for a blockr option
  // whose R option and environment variable disagree, and where the value does
  // not convert to the fallback's type.
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
  // A file it sends, which R hands the request's function in its argument
  // `file`.
  file?: File;
  // Hears each progress note that R sends for it.
  progress?(note: { value?: number; message?: string }): void;
}

export interface OutputEvents {
  // Calls `fn` with each event that a renderer emits.
  subscribe(fn: (event: { block: BlockId; name: string; payload: Json }) => void): () => void;
}

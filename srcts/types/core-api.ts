// What core gives a frontend, a plugin and a startup module: `CoreApi`,
// through which they work on the board, as Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-plugins).
// Core makes one for each board that opens, and ends it as the next board
// opens, with the signal that the startup modules, the module of every plugin
// and every plugin class's start get beside it. The modules that core mounts
// inside a frontend's layout get a host instead (./hosts.ts), but for the
// module of a plugin, which gets CoreApi.

import type { Commands } from "../commands/index.ts";
import type { Lock, Plugin } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Options, PluginClass, Storage, Store } from "../store/index.ts";
import type { BlockId, OptionId, PluginClassId, RegistryId } from "./board.ts";
import type { Rendered } from "./hosts.ts";
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
  // The scheduler's methods for frontends and plugins. Core alone calls
  // the others.
  readonly scheduler: Pick<Scheduler, "demand" | "evaluate" | "invalidate" | "status" | "subscribe">;
  // The block registry's entries by registry id, for pickers, as R's reply to
  // `hello` gives them for the board.
  blockRegistry(): BlockRegistry;
  // The board options that packages define, by id, and the value of each, as
  // the board holds it, which is undefined where the board holds none.
  readonly options: Options & {
    value(option: OptionId): Json | undefined;
  };
  // The plugin classes that packages define, and the app's plugins, which
  // run on every board that the app opens, each named by the id of its class
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#running-a-plugin).
  readonly plugins: {
    // Registers a plugin class under the class's id, as a package's startup
    // module does, and returns what unregisters it.
    register(def: PluginClass): () => void;
    // The app's plugins whose classes are registered, by the ids of their
    // classes, each with its class's name, as R's reply to `hello` names
    // them. It returns the same object until the registered classes change,
    // and an empty one while no board is open.
    running(): Readonly<Record<PluginClassId, Plugin>>;
    // The definition of the class of a plugin that runs. It throws for a
    // plugin that the app does not run.
    pluginClass(plugin: PluginClassId): PluginClass;
    // What a frontend's save and open controls call: the storage of the app's
    // storage plugin, or undefined where the app runs none, where a frontend
    // draws neither control.
    storage(): Storage | undefined;
  };
  // The locks that the app sets, from R's reply to `hello`: whether the board
  // lock and the load lock are on. The store's locked() says which blocks the
  // board lock leaves open.
  readonly lock: Lock;
  // What renderers report that the user did, such as a selection, for
  // plugins to act on.
  readonly outputEvents: OutputEvents;
  // The notes that plugins send, and the one in which core names what the
  // store dropped as a board opened, which the frontend hears and draws.
  readonly notifications: {
    // Hands the note to each subscriber. Core keeps no note, so a note sent
    // before anything subscribes reaches nobody.
    notify(note: Note): void;
    subscribe(fn: (note: Note) => void): () => void;
  };
  // Which block has focus, which the frontend sets as the user moves into a
  // block, and which a plugin such as the assistant reads to tell which block
  // the user is looking at. Core holds it for the page, outside the board, so
  // that nothing saves or undoes it, and no block has focus as a board opens.
  readonly focus: {
    // The block that has focus, or null for none.
    get(): BlockId | null;
    // Gives the block focus, or with null takes focus from every block. It
    // throws for a block that the board does not hold.
    set(block: BlockId | null): void;
    // Calls `fn` with the block that has focus whenever that changes, as where
    // the block with focus leaves the board, which leaves no block with focus.
    subscribe(fn: (block: BlockId | null) => void): () => void;
  };
  // A request of a plugin that runs, by the id of the plugin's class, which
  // the method of R's plugin_requests() for the class answers. Its args go to
  // the request's function by name. Core has the blocks that it reads
  // evaluated first, and keeps none of its answers, since it may write.
  request<T = Json>(plugin: PluginClassId, name: string, args?: Readonly<Record<string, Json>>, opts?: RequestOpts): Promise<T | Blob>;
  // A module another package offers, by id.
  module<T = unknown>(id: string): Promise<T>;
  // A blockr option as R sent it, asked of R once per page, or undefined where
  // nothing sets it. The caller converts and checks the value, as one that an
  // environment variable sets arrives as a string, and takes a fallback of its
  // own where nothing sets it. It rejects where R cannot give the value, as for
  // a blockr option whose R option and environment variable disagree.
  blockrOption(name: string): Promise<Json | undefined>;
  // The block's UI module, its output, and a board option's UI or the
  // renderer R picked for it, each mounted in the element until the cleanup
  // that the call returns runs.
  mountBlock(el: HTMLElement, block: BlockId): () => void;
  mountOutput(el: HTMLElement, block: BlockId): () => void;
  mountOption(el: HTMLElement, option: OptionId): () => void;
  // The `ui` module of the class of a plugin that runs, by the id of the
  // class, mounted in the element that a frontend places the plugin in, until
  // the cleanup that the call returns runs. It throws for a plugin that the
  // app does not run, or whose class has no `ui`.
  mountPlugin(el: HTMLElement, plugin: PluginClassId): () => void;
}

// A note that a plugin sends, which the frontend draws.
export interface Note {
  readonly message: string;
  readonly severity: "message" | "warning" | "error";
}

// What a plugin's module exports as its default, beside the JS API major it
// was built against, as `export const api = 1`: the `ui` of a plugin class,
// which a frontend places with mountPlugin(), or its `frontend`, which draws
// the page. Core calls render() with the element to draw in, CoreApi, the id
// of the class of the plugin it draws and a signal, which it aborts when it
// unmounts the module, as the contract of a block's UI has it with a host in
// place of CoreApi (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#plugin-classes).
// So one module draws the plugin of its class, from the state that the board
// holds under that id, and never names its own id. The render() function may
// be async and may return its cleanup.
export interface PluginModule {
  render(args: { el: HTMLElement; core: CoreApi; plugin: PluginClassId; signal: AbortSignal }): Rendered;
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

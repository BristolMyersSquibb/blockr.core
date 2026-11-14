// What core gives a frontend and a plugin: `CoreApi`, through which they work
// on the board, as Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-plugins).
// Core makes one for each board that opens, and ends it as the next board
// opens, with the signal that the load() of the app's storage plugin, the `ui`
// and the `frontend` of every plugin class and every plugin class's start get
// beside it. The modules that core mounts inside a frontend's layout get a host
// instead (./hosts.ts), but for the `ui` of a plugin class, which gets
// CoreApi.

import type { Commands } from "../commands/index.ts";
import type { Lock, Phase } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Options, PluginClass, Storage, Store } from "../store/index.ts";
import type { BlockId, LinkId, OptionId, PluginClassId, RegistryId, StackId } from "./board.ts";
import type { Rendered } from "./hosts.ts";
import type { Json } from "./json.ts";
import type { Log } from "./log.ts";
import type { RegistryEntry } from "./registry.ts";

export interface CoreApi {
  // The store of the board. Its open() starts core again in place for the
  // board given, which opens with a CoreApi of its own, once this store has
  // checked the board, and a reply to `hello` given opens its board as a JSON
  // board's text would. Where this store refuses the board, core tells the
  // user why through a notification of the code `board_open_failed`, and
  // open() rejects with the reason. Where it refuses a delta, an undo or a
  // redo of the origin "user", core tells the user why through a notification
  // of the code `edit_refused`, and the call still throws the reason.
  readonly store: Store;
  // The command registry, for menus and shortcuts. Where it or the store
  // refuses a command of the origin "user", core tells the user why through a
  // notification of the code `edit_refused`, and run() still rejects with the
  // reason. An edit of another origin, such as the assistant's, only rejects
  // or throws.
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
    // Registers a plugin class under the class's id, as core does with the
    // default export of the module that R names for the class of each of the
    // app's plugins, and returns what unregisters it.
    register(def: PluginClass): () => void;
    // The app's plugins whose classes are registered, by the ids of their
    // classes, each with its class's name, as R's reply to `hello` names
    // them. It returns the same object until the registered classes change,
    // and an empty one while no board is open.
    appPlugins(): Readonly<Record<PluginClassId, Plugin>>;
    // The definition of the class of a plugin that runs. It throws for a
    // plugin that the app does not run.
    pluginClass(plugin: PluginClassId): PluginClass;
    // The storage of the app's storage plugin, whose controls a frontend
    // draws, or undefined where the app runs none, where a frontend draws no
    // storage control.
    storage(): Storage | undefined;
  };
  // The locks that the app sets, from R's reply to `hello`: whether the board
  // lock and the load lock are on. The store's locked() says which blocks the
  // board lock leaves open.
  readonly lock: Lock;
  // What renderers report that the user did, such as a selection, for
  // plugins to act on.
  readonly outputEvents: OutputEvents;
  // Every notification of the open board, whoever raised it, which a frontend
  // draws as it chooses, as Notifications has it
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications):
  // one for each condition of each block's status, one for each condition of
  // R's reply to a request that fails, those that core raises where it fails
  // at something that the user asked for or that the board needs, and those
  // that the modules of packages raise, a plugin's through notify() and one
  // that core mounts through its host. Core holds them outside the board, in
  // the order they came, so that one raised before the frontend draws still
  // reaches it. It withdraws those that concern a block, a link or a stack as
  // that leaves the board, and every one as the next board opens.
  readonly notifications: {
    // Raises a notification of the plugin of the class `plugin`, which
    // concerns the plugin unless it names another board object, and holds
    // until the function that it returns runs. It throws for one whose members
    // are of the wrong kind. One about a block, a link or a stack that the
    // board no longer holds holds nothing, since the work that raises it can
    // outlive what it concerns, which core logs at debug. Core logs at error one
    // from a plugin that the app does not run or about a board object that the
    // board has never held, since only a mistake in the plugin names one.
    notify(plugin: PluginClassId, notification: Raised): () => void;
    // Those that hold, the same array until one comes or goes.
    get(): readonly Notification[];
    // Calls `fn` with those that hold whenever one comes or goes, as React's
    // useSyncExternalStore follows a store with get().
    subscribe(fn: (all: readonly Notification[]) => void): () => void;
  };
  // A log whose lines name the package, as `core.log("blockr.dock").warn(...)`
  // writes a warning of blockr.dock, which the page's logger writes out, to
  // the browser's console or to an OpenTelemetry collector
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#logging).
  // It throws for a package that is not named as R names one.
  log(pkg: string): Log;
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

// One of the app's plugins, as appPlugins() gives it under the id of its
// class: the name of its class, which titles its panel, as "Document".
export interface Plugin {
  readonly name: string;
}

// A message, warning or error that the user should see, whoever raised it.
// It says who raised it and what it concerns apart, so that a frontend places
// it by either as it chooses: on the card of the block it concerns, next to
// the plugin that raised it, or as a toast whatever it concerns.
// The browser has a global of the same name, for desktop notifications, whose
// constructor a module that imports this type still calls as
// `new Notification()`, since the type has no value.
export interface Notification {
  // The same while it holds, so that a frontend knows which it has shown.
  readonly id: string;
  readonly severity: Severity;
  readonly message: string;
  // Who raised it.
  readonly source: NotificationSource;
  // The board object that it concerns, or none for the board as a whole.
  readonly about?: BoardObject;
}

export type Severity = "message" | "warning" | "error";

// A board object, by its id: a block, a link, a stack, a board option, or one
// of the app's plugins, by the id of its class.
export type BoardObject =
  | { readonly block: BlockId }
  | { readonly link: LinkId }
  | { readonly stack: StackId }
  | { readonly option: OptionId }
  | { readonly plugin: PluginClassId };

// Who raised a notification: R, core or a package's module in the client.
// Each kind names the package's module behind the notification in `module`,
// where a module is behind it, so that a frontend reads which module a
// notification is of in one way, whoever raised it.
export type NotificationSource = RSource | CoreSource | ModuleSource;

// A package's module in the client, as a notification's source names it: a
// plugin class's module, or one that core mounts for a block or a board
// option. A block has two modules, the UI module of its class and the renderer
// of its output. A board option has two as well: its own module, and the
// renderer that the app may name to draw its value in place of its UI.
export interface SourceModule {
  // The board object whose module it is: a plugin, a block or a board option.
  readonly object: { readonly plugin: PluginClassId } | { readonly block: BlockId } | { readonly option: OptionId };
  // The renderer, by its id, where the module is the renderer of the block's
  // output or the renderer that draws the board option. A module without it
  // is the block's UI module, the board option's own module or the plugin
  // class's module.
  readonly renderer?: string;
  // The module's package, where core knows it, which it does but for a block
  // class defined outside a package.
  readonly package?: string;
}

// R, in a condition of its reply to what core asked of it for the board object
// that the notification concerns.
export interface RSource {
  readonly kind: "r";
  // The condition's class vector, as ["simpleWarning", "warning", "condition"].
  readonly class: readonly string[];
  readonly request: RRequest;
  // The module that made the request, for a request:
  // - Of a block class, the block's UI module.
  // - Of an output class or of a renderer, the renderer of the block's output.
  // - Of a board option, the board option's own module or the renderer that
  //   draws it.
  // - Of a plugin, the plugin class's module.
  // An evaluation and an output have none, since core asks R for them.
  readonly module?: SourceModule;
}

// What core asked of R: a block's evaluation or its output, or a request, by
// its type and name. The module of the source names the renderer of a
// renderer's own request.
export type RRequest =
  // The phase that raised the condition, which one that R raises itself, as
  // for an evaluation that a lock refuses, lacks.
  | { readonly type: "evaluate"; readonly phase?: EvaluatePhase }
  | { readonly type: "output" }
  | { readonly type: "block_class" | "output_class" | "renderer" | "option" | "plugin"; readonly name: string };

// The phases of a block's evaluation, of which R names one in each condition
// it raises there.
export type EvaluatePhase = Extract<Phase, "expr" | "inputs" | "block_eval" | "result_summary">;

// Core, where it fails at something that the user asked for or that the board
// needs.
export interface CoreSource {
  readonly kind: "core";
  readonly code: CoreCode;
  // The module whose code failed, where a module's code failed:
  // - A module that cannot load or draw.
  // - The board option's own module, whose default failed or does not fit its
  //   schema, or whose onChange() threw.
  // - The plugin class's module, whose start threw.
  // - The storage plugin's module, whose load() failed or named a board that
  //   does not open.
  readonly module?: SourceModule;
}

// What core's own notification is of.
export type CoreCode =
  // A module that fails to load or to draw.
  | "module_failed"
  // A board option that does not go onto the board.
  | "board_option_failed"
  // A board option's onChange() that throws.
  | "on_change_failed"
  // A plugin's start that throws.
  | "plugin_start_failed"
  // A board that does not open.
  | "board_open_failed"
  // An edit of the origin "user" that the store or the command registry
  // refuses.
  | "edit_refused"
  // What the store dropped as the board opened.
  | "board_objects_dropped"
  // A package that R runs at another version than the page loaded.
  | "page_reload_needed"
  // An evaluation that the transport could not send to R.
  | "evaluation_unsent"
  // An ask of a block's output that got no reply, or an error reply to one
  // that holds no condition.
  | "output_unanswered";

// A package's module in the client: a plugin class's, one that core mounts for
// a block or a board option, its UI module or a renderer, or a board option's,
// whose onChange() raises it.
export interface ModuleSource {
  readonly kind: "module";
  // The module that raised it.
  readonly module: SourceModule;
  // The package's own code for what it is of, as "render_failed".
  readonly code: string;
}

// What a package's module gives to raise a notification, from which core
// makes the notification, naming who raised it.
export interface Raised {
  readonly severity: Severity;
  readonly message: string;
  // The package's own code for what it is of, as "render_failed", by which a
  // frontend that knows the package draws it.
  readonly code: string;
  // The board object that it concerns: the one whose module raises it unless
  // given, or null for the board as a whole.
  readonly about?: BoardObject | null;
}

// What a plugin's module exports as its default, beside the JS API major it
// was built against, as `export const api = 1`: the `ui` of a plugin class,
// which a frontend places with mountPlugin(), or its `frontend`, which draws
// the page and gets more (FrontendModule, below). Core calls render() with
// the element to draw in, CoreApi, the id of the class of the plugin it draws
// and a signal, which it aborts when it unmounts the module, as the contract
// of a block's UI has it with a host in place of CoreApi
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#plugin-classes).
// So one module draws the plugin of its class, from the state that the board
// holds under that id, and never names its own id. The render() function may
// be async and may return its cleanup.
export interface PluginModule {
  render(args: { el: HTMLElement; core: CoreApi; plugin: PluginClassId; signal: AbortSignal }): Rendered;
}

// What the module of a frontend exports as its default, the `frontend` of its
// plugin class, which draws the page. Core calls its render() as it calls that
// of a plugin's module, with the page's element, and with `args` beside: what
// the frontend reads of the arguments that `serve()` hands it, by name, as the
// method of R's frontend_args() for the frontend's class returned them, which
// R's reply to `hello` carries in `frontend`. So no board sets them, and every
// board that the app opens gets the same. They are empty for core's basic
// frontend, which takes none
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#the-frontend).
export interface FrontendModule {
  render(args: {
    el: HTMLElement;
    core: CoreApi;
    plugin: PluginClassId;
    signal: AbortSignal;
    args: Readonly<Record<string, Json>>;
  }): Rendered;
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

// What core gives a frontend and a plugin: `CoreApi`, through which they work
// on the board, as Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-plugins).
// Core makes one for each board that opens, and ends it as the next board
// opens, with the signal that the load() of the app's storage plugin, the `ui`
// and the `frontend` of every plugin class and every plugin class's start get
// beside it. The modules that core mounts inside a frontend's layout get a host
// instead (./hosts.ts), but for the `ui` of a plugin class, which gets
// CoreApi.

import type { BoardAction, CommandCall, Target } from "../actions/index.ts";
import type { Commands } from "../commands/index.ts";
import type { Lock, Phase } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Delta, Options, PluginClass, Storage, Store } from "../store/index.ts";
import type { BlockId, BoardActionId, LinkId, OptionId, PluginClassId, RegistryId, RendererId, StackId } from "./board.ts";
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
  // redo, of whatever origin, core tells the user why through a notification
  // of the code `board_edit_failed`, and the call still throws the reason.
  // Where its read() leaves anything out of the board that it reads, core
  // tells the user what through a notification of the code
  // `board_objects_dropped`, as it does as a board opens.
  // A call whose caller shows the notification itself, or hands it on, passes
  // a notifier in its options, which takes the notification in place of core
  // (Notifier, below).
  readonly store: Store;
  // The command registry, for menus and shortcuts. Where it or the store
  // refuses a command, or a batch, of whatever origin, core tells the user
  // why through a notification of the code `board_edit_failed`, and
  // run() still rejects with the reason, unless the call's notifier takes the
  // notification, as for the store.
  readonly commands: Commands;
  // The board actions: what users trigger from menus, buttons and gestures, on
  // the board, on one of its board objects, on an input of a block or on a
  // selection of board objects, which run commands of any package, each batch
  // as one undo step of the user
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#a-board-action).
  // An input's target names the block and the input, as
  // { block: "b", input: "y" }, with the input "" for the next place of a
  // variadic block. Core hands it only to a board action whose `on` names
  // inputs.
  // Core registers its own and those that the classes of the app's plugins
  // list in `boardActions`, for each board that opens.
  readonly boardActions: {
    // The board actions that a menu of the target offers: each whose `on`
    // names the kind of the target, and whose `offers` passes for it or which
    // has none. Core's come first, in the order of their table, then each
    // package's, in the order of the app's plugins and as its class lists
    // them. Of the board actions that name a role, it gives only the one that
    // plays the role in the app, in the place of the role's default
    // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#roles).
    // It throws for what is no target, and gives none while the store holds
    // no board.
    on(target: Target): readonly BoardAction[];
    // Opens the menu of the target at the anchor, with a row for each board
    // action that on() gives the target, showing its label, its icon, its
    // detail for the target and its category, in core's menu, which core draws
    // whichever package opens it, and runs by the rules of a quick action
    // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-menus-of-board-objects).
    // Picking a row closes the menu and runs the board action's run, as
    // trigger() does, or opens its quick action as the menu's next level, in
    // which a pick that returns a batch closes the menu and runs the batch.
    // It opens the board action's advanced action instead where the board
    // action has no quick action, or its quick gives no level for the target.
    // A click on the open icon of a level of a quick action, or of an item,
    // closes the menu and opens the advanced action too.
    // Each batch runs with the commands that `extend` returns at its end, as
    // one step of the user. Where the store or the command registry refuses
    // the batch, core tells the user why through a notification of the code
    // `board_edit_failed`, and where `run` throws, rejects or returns no
    // batch, through one of the code `board_action_failed`. The notifier takes
    // either in place of core, with the board action. For an advanced action,
    // `extend` and the notifier take part until it first ends, and only on the
    // target that it opened on, so that a place that the user pinned, which
    // mounts it again or on another target, runs the batches there without
    // them. It opens no menu where on() gives the target none, and it closes
    // the menu that is open. The keyboard's focus returns to `el` as the menu
    // closes. It throws for what is no target, and for options without `el`
    // and `anchor`.
    menu(
      target: Target,
      opts: {
        // The element that the user triggered from, and the box at which the
        // menu opens, such as the pointer of a right click.
        el: HTMLElement;
        anchor: DOMRect;
        extend?(deltas: readonly Delta[]): readonly CommandCall[];
        notifier?(notifications: readonly Notification[], action: BoardAction): void;
        // Shows each advanced action that the menu opens in room of the
        // caller's, in place of the frontend's place, as the frontend's
        // `advanced` member does (FrontendModule, below).
        advanced?: FrontendModule["advanced"];
      },
    ): void;
    // Triggers the board action with the package and the id given on the
    // target, as { package: "blockr.core", id: "rename_action" }, as a button
    // or a gesture does, or the one that plays the role with the id given, a
    // string, as "append_block_role", so that a caller that relies on what a
    // board action does meets the app's pick. It runs nothing where on() leaves the board
    // action out for the target, as under the board lock for every board
    // action of core's but Copy block ID, and nothing for the id of a board
    // action that names a role that it does not play. Otherwise:
    // - Where the board action asks the user nothing, it runs the batch that
    //   its `run` returns, or that a promise of its `run` resolves with, with
    //   the commands that `extend` returns at its end, as one step of the user.
    //   Where `run` throws, rejects or returns no batch, core tells the user
    //   through a notification of the code `board_action_failed`, about the
    //   plugin whose class lists the board action.
    // - Where the board action has a quick action, it opens the quick action
    //   at `anchor`, or at the box of `el`, as a menu of core's, which runs the
    //   batch of a row as menu() does, and closes the menu that is open. The
    //   keyboard's focus returns to `el` as the menu closes.
    // - Where the board action has an advanced action alone, or its quick
    //   gives no level for the target, it opens the advanced action in its
    //   place: the caller's `advanced`, or else the frontend's place, or else
    //   core's dialog, from which the keyboard's focus returns to `el`.
    // Where the store or the command registry refuses the batch of a run or a
    // row, core tells the user why through a notification of the code
    // `board_edit_failed`. The notifier takes either notification in place of
    // core, with the board action. It resolves once the batch has run, or
    // once the menu has closed without one, and rejects with the refusal or
    // the failure. For an advanced action, it resolves once the advanced
    // action ends: once its done() has run a batch, or its place has closed
    // without one. A batch that the store refuses there leaves the advanced
    // action open, whose done() rejects with the refusal. The caller's
    // `extend` and notifier take part until then, on the target that the
    // advanced action opened on, as menu() has it. It rejects for an id
    // that no board action or role has, for what is no target, and for
    // options without `el`.
    trigger(
      id: { readonly package: string; readonly id: string } | string,
      target: Target,
      opts: {
        el: HTMLElement;
        anchor?: DOMRect;
        // The caller's commands, which run after the batch in the same step,
        // for the deltas that the batch will apply, as core.commands.plan()
        // returns them, such as the position of a block that the batch adds.
        extend?(deltas: readonly Delta[]): readonly CommandCall[];
        notifier?(notifications: readonly Notification[], action: BoardAction): void;
        advanced?: FrontendModule["advanced"];
      },
    ): Promise<void>;
    // Renders the advanced action of the board action with the package and the
    // id given on the target in the element, or of the one that plays the role
    // with the id given, which a plugin gives as room of its own, as the Edit board
    // plugin shows the advanced action of Add block, until the function that
    // it returns runs. The advanced action gets `picked` where
    // given. There its done() runs the batch as one step of the user, and core
    // renders the advanced action again, so that it is ready for the next
    // edit. Where a board object that the target names leaves the board, core
    // unmounts it and leaves the element empty. It renders nothing where on()
    // leaves the board action out for the target, and nothing for the id of a
    // board action that names a role that it does not play. It throws for an
    // id that no board action or role has, for a board action without an
    // advanced action, for what is no target or no element, and once the
    // next board has opened.
    mountAdvanced(el: HTMLElement, id: { readonly package: string; readonly id: string } | string, target: Target, picked?: Json): () => void;
  };
  // The scheduler's methods for frontends and plugins. Core alone calls
  // the others.
  readonly scheduler: Pick<Scheduler, "demand" | "evaluate" | "invalidate" | "status" | "subscribe">;
  // The block registry's entries by registry id, for pickers, as R's reply to
  // `hello` gives them for the board, in R's order.
  blockRegistry(): BlockRegistry;
  // The board options that packages define, by id, and the value of each, as
  // the board holds it, which is undefined where the board holds none.
  readonly options: Options & {
    value(option: OptionId): Json | undefined;
  };
  // The app's plugins, which run on every board that the app opens, each
  // named by the id of its class, and the definitions of their classes
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#running-a-plugin).
  // Core registers the class of each of the app's plugins itself, from the
  // default export of the module that R names for it.
  readonly plugins: {
    // The app's plugins, by the ids of their classes, each with its class's
    // name, as R's reply to `hello` names them, in the order of the rows that
    // `serve()` takes. It returns the same map while the board is open, and an
    // empty one while no board is open.
    appPlugins(): ReadonlyMap<PluginClassId, Plugin>;
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
  // The board objects that the user selected, which whatever selects sets, as
  // the DAG does as the user selects nodes, links and stacks. The DAG opens
  // the menu of a selection with what it holds, and a frontend that pins its
  // place for advanced actions may follow it, as the dock's sidebar does
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action).
  // Core's basic frontend selects nothing. Core holds the selection for the
  // page, outside the board, as it holds focus, so that nothing saves or
  // undoes it, and nothing is selected as a board opens.
  readonly selection: {
    // The board objects that the user selected, the same array until they
    // change, which is empty where nothing is selected.
    get(): readonly BoardObject[];
    // Selects the board objects given in place of those selected, or with an
    // empty array selects nothing. A board object given twice is selected
    // once, and the same board objects in another order change nothing, so
    // that get() still gives the same array. It throws for what is no array
    // of board objects, and for a board object that the board does not hold:
    // a block, a link or a stack that the board lacks, a board option whose
    // value the board lacks, or a plugin that the app does not run. The
    // selection then stays as it was.
    set(objects: readonly BoardObject[]): void;
    // Calls `fn` with the board objects selected whenever they change, as
    // where a selected board object leaves the board, which leaves the
    // selection with it.
    subscribe(fn: (objects: readonly BoardObject[]) => void): () => void;
  };
  // Which blocks and plugins are on screen, which the user sees now as the
  // frontend lays them out, and which a plugin such as the assistant reads to
  // tell what the user sees: under the dock, the panel that each tab group of
  // the active view shows and that of each open rail, and under core's basic
  // frontend, the cards and sections in the browser's viewport
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#what-a-frontend-offers-other-plugins).
  // Core holds it for the page, outside the board, as it holds focus.
  // Core asks R for a block's output only while it holds the block or the
  // plugin whose UI holds the output (mountOutput(), below).
  readonly onScreen: OnScreen;
  // Has the frontend show a block or a plugin in its own way, as the DAG shows
  // a block as the user selects its node
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#showing-a-block-or-a-plugin).
  // It throws for a block that the board does not hold, and for a plugin that
  // the app does not run or whose class has no `ui`. It gives a block focus,
  // and calls the show() of the frontend's module where the module has one,
  // resolving once that has, and rejecting with why where that fails. Under
  // every frontend, showing takes no undo step,
  // leaves the keyboard's focus where it is, stays within what the frontend
  // shows now and changes nothing that a lock fixes. Once the next board has
  // opened, it shows nothing.
  show(object: { readonly block: BlockId } | { readonly plugin: PluginClassId }): Promise<void>;
  // A request of a plugin that runs, by the id of the plugin's class, which
  // the method of R's plugin_requests() for the class answers. Its args go to
  // the request's function by name. Core has the blocks that it reads
  // evaluated first, and keeps none of its answers, since it may write. Each
  // condition of R's reply, ok or error, raises a notification about the
  // plugin, unless the request's notifier takes them.
  request<T = Json>(plugin: PluginClassId, name: string, args?: Readonly<Record<string, Json>>, opts?: RequestOpts): Promise<T | Blob>;
  // A module another package offers, by its package and its id within it, as
  // { package: "blockr.core", id: "controls" }, which a caller writes as it is.
  module<T = unknown>(id: { readonly package: string; readonly id: string }): Promise<T>;
  // A blockr option as R sent it, asked of R once per page, or undefined where
  // nothing sets it. The caller converts and checks the value, as one that an
  // environment variable sets arrives as a string, and takes a fallback of its
  // own where nothing sets it. It rejects where R cannot give the value, as for
  // a blockr option whose R option and environment variable disagree.
  blockrOption(name: string): Promise<Json | undefined>;
  // A new id, such as "calm_heron", that `taken` does not hold, in the shape
  // that the blockr option rand_id names, as core's commands mint ids. It
  // serves a caller that names an object before a batch adds it. A batch hands
  // no value from one command to the next, so a caller that adds a block and
  // links it in one batch mints the block's id first, as the block picker
  // does.
  mintId(taken: (id: string) => boolean): string;
  // The block's UI module, its output, and a board option's UI or the
  // renderer R picked for it, each mounted in the element until the cleanup
  // that the call returns runs.
  // Core asks R for the output only while the block reads ready and the
  // output is on screen, as far as core can tell. It mounts the renderer that
  // draws the output only while the output is on screen, whatever the
  // block's status. The output is on screen while both of these hold:
  // 1. The element measures more than 0 by 0. So an output whose element a
  //    frontend or a plugin takes out of the layout waits, as one in a closed
  //    section of a card does.
  // 2. CoreApi's onScreen holds what the element sits in, or is null, as
  //    where the frontend has said nothing. Core tells what the element sits
  //    in from the elements in which it mounted the UIs of plugins:
  //    - An element inside a plugin's UI sits in that plugin.
  //    - An element on the page that the frontend draws, outside the UI of
  //      every other plugin, sits on its block's card or panel, so onScreen
  //      has to hold the block.
  //    - Core cannot tell where an element outside the page shows, as one in
  //      a popover at the end of the page's body. So this check holds for it.
  // An output that core has drawn stays drawn when one of these stops
  // holding. Core asks for a newer one once both hold again.
  // Core also asks while a request of the output waits for the output of the
  // block's revision, wherever the element measures more than 0 by 0,
  // whatever onScreen holds, as a request that reads blocks has them
  // evaluated even off screen.
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
  readonly renderer?: RendererId;
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
  // - The plugin class's module, whose start threw, or the run of whose board
  //   action failed.
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
  // An edit that the store or the command registry refuses.
  | "board_edit_failed"
  // A board action whose run throws, rejects or returns no batch.
  | "board_action_failed"
  // What the store dropped as the board opened, or left out of a board that
  // its read() read.
  | "board_objects_dropped"
  // A package that R runs at another version than the page loaded.
  | "page_reload_needed"
  // An evaluation that the transport could not send to R.
  | "evaluation_unsent"
  // An ask of a block's output that got no reply, or an error reply to one
  // that holds no condition.
  | "output_unanswered"
  // A copy to the clipboard that the browser refuses, as of a block's id.
  | "copy_failed";

// Why core could not get or draw a block's output where R gave no error, as
// the output host's error() gives it in place of R's condition. Its code is
// that of the notification that core raises for the same failure:
// output_unanswered for an ask that got no reply or an error reply that holds
// no error, and board_option_failed where a board option of the block's
// outputs does not go onto the board.
export interface CoreFailure {
  readonly kind: "core";
  readonly code: Extract<CoreCode, "output_unanswered" | "board_option_failed">;
  readonly message: string;
}

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
// Beside render(), the module may give show(), which core calls as a plugin
// asks to show a block or a plugin through CoreApi's show(), with the board
// object, the page's element and CoreApi, and which may be async. A frontend
// that keeps every block and plugin in view leaves it out.
// It may give `advanced` too, which shows an advanced action in the
// frontend's place, as the dock shows one in its sidebar
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#what-a-frontend-gives).
// Core calls it with the board action's id, its package and its id within
// the package, as core's table holds them, a title, the target, a mount() and
// a close(). The frontend renders the advanced action in its place
// through mount(), until the cleanup that mount() returns runs, and calls
// close() as the user closes the place. It may pass mount() another target,
// on which core renders the same board action where a menu of the target
// offers it, and otherwise renders nothing, so that a pinned place may move on
// to a block that a batch added, or follow core.selection. Each mount renders
// the advanced action afresh.
// It returns a function, which core calls once done() ends the advanced
// action, with the deltas of the batch that done() ran, or with none where it
// ran none. The frontend then closes its place, or keeps it open and mounts
// the advanced action again, as a place that the user pinned does. A frontend
// without it leaves every advanced action to core's dialog.
export interface FrontendModule {
  render(args: {
    el: HTMLElement;
    core: CoreApi;
    plugin: PluginClassId;
    signal: AbortSignal;
    args: Readonly<Record<string, Json>>;
  }): Rendered;
  show?(args: {
    object: { readonly block: BlockId } | { readonly plugin: PluginClassId };
    el: HTMLElement;
    core: CoreApi;
  }): void | Promise<void>;
  advanced?(args: {
    readonly id: BoardActionId;
    readonly title: string;
    readonly target: Target;
    readonly mount: (el: HTMLElement, target?: Target) => () => void;
    readonly close: () => void;
  }): (deltas?: readonly Delta[]) => void;
}

// Which blocks and plugins are on screen, which the frontend sets as what it
// shows changes. It is null until the frontend sets it, so that a plugin tells
// a frontend that says nothing from one that shows nothing, and it goes back
// to null as the next board opens.
export interface OnScreen {
  // The blocks and plugins that the user sees, the same object until they
  // change, or null where the frontend has said nothing.
  get(): { readonly blocks: readonly BlockId[]; readonly plugins: readonly PluginClassId[] } | null;
  // The frontend's: the blocks and plugins that it now has on screen, by their
  // ids and the ids of the plugins' classes, which it sets again as what it
  // shows changes, as when a block leaves the board. It throws for what is no
  // such pair of arrays of ids.
  set(shown: { readonly blocks: readonly BlockId[]; readonly plugins: readonly PluginClassId[] }): void;
  // Calls `fn` with them whenever they change.
  subscribe(fn: (shown: { readonly blocks: readonly BlockId[]; readonly plugins: readonly PluginClassId[] } | null) => void): () => void;
}

export type BlockRegistry = ReadonlyMap<RegistryId, RegistryEntry>;

export interface RequestOpts {
  // The blocks it reads, which core has evaluated first.
  reads?: BlockId[];
  // Gives it up, as a cancel does on the wire.
  signal?: AbortSignal;
  // A file it sends, which R hands the request's function in its argument
  // `file`, and a block class's request in the argument that its
  // `new_block_request()` names.
  file?: File;
  // Hears each progress note that R sends for it.
  progress?(note: { value?: number; message?: string }): void;
  // Takes the notifications of each of R's replies to it, ok or error, one for
  // each condition of the reply, and none for a reply without conditions
  // (Notifier, below).
  // A reply of error still rejects with its conditions.
  // A plugin passes it only for a request of its own, since core cannot tell
  // which plugin calls.
  notifier?(notifications: readonly Notification[]): void;
}

// Takes the notifications that a call would raise, for a caller that shows
// the user why the call failed itself, or hands that on, as the assistant
// hands it to the model, so that the user is told once
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#taking-a-calls-notifications).
// A caller passes it in the options of a request, of an edit through the
// store or the command registry, of the store's open() or read(), or of a
// host's submit(). Core then raises none of those notifications, and leaves
// those that earlier calls raised as they are, since another caller may have
// made those calls. Core hands it the notifications of each reply to a
// request, and the notification of a refused edit or board before the call
// rejects or throws, which it still does. It hands it the notification of
// what the store's read() left out of a board before read() resolves.
// Core logs what a notifier throws, and the call ends as it would have.
export type Notifier = (notifications: readonly Notification[]) => void;

export interface OutputEvents {
  // Calls `fn` with each event that a renderer emits.
  subscribe(fn: (event: { block: BlockId; name: string; payload: Json }) => void): () => void;
}

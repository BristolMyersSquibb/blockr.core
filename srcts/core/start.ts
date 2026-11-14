// Starting core on a page, as Starting the page has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#starting-the-page):
// 1. The protocol's client sends each session's hello as R starts the session.
// 2. Once R has answered the first, core asks R for the blockr options that its
//    scheduler reads, for rand_id, the shape of the ids that commands mint,
//    and for those that set the page's logger up (../log/settings.ts), sets
//    the logger up, and creates for the board that it opens the store, the
//    notifications of the board (./notifications.ts) and the command
//    registry.
// 3. It imports the module of the class of each of the app's plugins and of
//    each board option that R's reply names, each once per page, and
//    registers the default export of each under the id that the reply gives
//    it, with the commands of each plugin class. It raises a notification for
//    a module that fails to load, or whose class or board option the store
//    refuses, and carries on, so that the board opens without what that module
//    defines.
// 4. It creates the scheduler and CoreApi.
// 5. Unless the load lock is on, it calls the load() of the storage of the
//    app's storage plugin, where it has one, which may resolve with the text of
//    a board to open in place of the one that R sent, such as the board that
//    the page's URL names. Where it rejects, or resolves with no JSON board,
//    or that board fails to open, core raises a notification of why and opens
//    R's.
// 6. It imports the UI module of each block class of which that board holds a
//    block below the class's version, for the class's migration
//    (./migrations.ts).
// 7. It opens the board that R sent, or the one that load() named in its
//    place, and either runs the app's plugins, which R names in its reply, each
//    with the state that the board holds for it. Where the store dropped
//    anything as the board opened, core raises a notification that names it.
// 8. As the store opens the board, core adds to it the board options that the
//    classes of the app's plugins require (./plugins.ts).
// 9. Once the board options that the app's frontend requires are on the
//    board, core calls the render() of the frontend, the plugin that runs whose
//    class has `frontend`, such as core's basic frontend, with what R's reply
//    holds in `frontend`, and starts the scheduler once render() has returned,
//    so that the scheduler sends R nothing before. Where the app runs no
//    frontend, the page shows why in place of the board.
// 10. Core then calls the start of each plugin whose class has one, so that
//    what the frontend first shows comes before what the plugins ask for as
//    they start.
//
// A block that serve() serves alone comes as a board of one block, which no
// frontend draws, as Data passed to serve() has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#data-passed-to-serve).
// Core then starts in the same order, with three differences:
// - In step 5, it calls no load(), since R names no plugin for a block that
//   serve() serves alone, and opens the board that R sent.
// - In step 8, it adds no board options for plugins. The board still takes
//   dark_mode, as every board does, since its onChange() switches the page's
//   theme (./core.ts).
// - In step 9, it draws no frontend, but the block in its card, which demands
//   the block, and starts the scheduler once the card has mounted the block.
//
// A board opened later, through the store's open() that CoreApi gives, starts
// core again in place, as Opening a board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board):
// 1. The store on the page checks the board as it would open it, once core
//    has imported the UI modules of the block classes that it migrates. A
//    board that it refuses leaves the page as it was, and open() rejects with
//    the reason.
// 2. One abort ends everything that core runs for the board on the page: the
//    frontend and what it mounted, every plugin that runs, the store with its
//    undo history, the command registry and the plugin classes, the board
//    options, the scheduler with its demands, which block has focus, and what
//    core keeps of outputs and answers.
// 3. Core sends another hello, which R takes as a fresh start of the session,
//    so that R drops every result and request of the board before.
// 4. Once R has answered it, core starts as the page does, from step 2 on,
//    with the board given in place of R's, which no load() replaces. Where that
//    board fails to open after all, core opens the board before in the new
//    store, raises a notification of why, and open() rejects with the reason.
// Only what is fixed for the page outlives an opening: the code and
// stylesheets that have loaded, the page's element, the transport with its
// session, the protocol's client, the blockr options that core has read, the
// locks, the migrations that core imported, and the revisions given, so that
// the next scheduler gives a block id a revision above any it had.
//
// Core takes a later reply to hello, as of a later session or of the hello with
// which core starts again, in one of three ways:
// 1. Where R speaks another major of the protocol in it, the protocol's client
//    stops and sends R nothing more, not even what it held for the session.
//    Core then ends what it runs for the board, as stop() does, and says what
//    to do, as for the first reply.
// 2. Where it names a package at another version than the page loaded, core
//    asks for the page to be reloaded, through a notification that each board
//    that opens raises again while the versions differ, and goes on with the
//    reply it used before: for the modules and renderers it loads, and for the
//    board that opens next. So it loads no module of that package from the new
//    reply. The loader imports no module of that package that it has not
//    imported already, rather than load a second copy of the package.
// 3. Otherwise core goes on with the new reply.
// The reply to a later session's hello also has the scheduler give every block
// a new revision, since the new session holds no results.

import { createCommands } from "../commands/index.ts";
import type { Commands } from "../commands/index.ts";
import { createLoader, ModuleError, moduleExports } from "../mount/index.ts";
import type { LoaderOptions, Measure } from "../mount/index.ts";
import { createClient, PROTOCOL_VERSION, sameMajor } from "../protocol/index.ts";
import type { Capability, HelloReply, Reply, Transport } from "../protocol/index.ts";
import { counter, createScheduler } from "../scheduler/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import { createStore, StoreError } from "../store/index.ts";
import type { Dropped, Option, PluginClass, Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi, FrontendModule, Notification } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { fallBack, log, logSettings, setUpLogging } from "../log/index.ts";
import type { OtelModule } from "../log/index.ts";
import { asError, coreNotification, errorMessage, isObject, own, packageFromId, quoted } from "../util/index.ts";
import type { Concerns, Raise } from "../util/index.ts";
import { createBlockrOptions } from "./blockr-options.ts";
import { createCore } from "./core.ts";
import { createMigrations } from "./migrations.ts";
import { createNotifications } from "./notifications.ts";
import type { Holder } from "./notifications.ts";

// The capabilities that core's client speaks.
const CAPABILITIES: Capability[] = ["cancel", "interrupt", "progress"];

// The frontend that draws a board that opens: what draws the page, its class's
// `frontend`, the id of the frontend's class, which names the plugin that it
// is, and what it reads of the arguments that `serve()` hands it, which R's
// reply to `hello` holds in `frontend`. Or, where the app runs no frontend,
// why, which the page shows in place of the board.
export type Front =
  | {
      readonly frontend: FrontendModule;
      readonly plugin: PluginClassId;
      readonly args: Readonly<Record<string, Json>>;
    }
  | { readonly none: string };

// What draws the frontends of the boards that open, which the page gives, and
// a CoreApi for tests may leave out.
export interface FrontEnds {
  // Takes the frontend that draws the board that opens, and ends the frontend
  // on the page, if any. Core calls it once the store has opened a board.
  // Where the signal has aborted, as another board opened meanwhile, it leaves
  // the page to that board.
  prepare(front: Front, signal: AbortSignal): void;
  // Calls the render() of the frontend that prepare() took, with the element,
  // CoreApi, the id of the frontend's class, a signal and what the frontend
  // reads of the arguments of `serve()`, and resolves once render() has
  // returned or resolved. Where board options that the frontend requires did
  // not go onto the board, or the app runs no frontend, it shows why in the
  // element instead, and calls no render(). The frontend ends as the signal
  // aborts, as the next board opens.
  render(core: CoreApi, signal: AbortSignal, failures: readonly string[]): Promise<void>;
  // Draws the board's one block in its card, for a block that serve() serves
  // alone, and returns once the card has mounted the block. The card ends as
  // the signal aborts, as the next board opens.
  card?(core: CoreApi, signal: AbortSignal): void;
}

// A package that a later reply names at another version than the page loaded.
export interface Changed {
  readonly package: string;
  readonly loaded: string;
  readonly now: string;
}

export interface StartOptions {
  readonly transport: Transport;
  // How the loader imports modules and puts stylesheets on the page, as on the
  // page unless given.
  readonly loader?: Omit<LoaderOptions, "modules" | "changed">;
  readonly frontEnds?: FrontEnds;
  // Whether the board is a block that serve() serves alone, which no frontend
  // draws, but the card that `frontEnds` gives.
  readonly alone?: boolean;
  readonly measure?: Measure;
  // Shows why core cannot start, or why it stopped, as where R speaks another
  // major of the protocol in the first session or a later one, or cannot give
  // a blockr option that core reads as it starts, or gives one that the
  // scheduler, the command registry or the page's logger does not take, or
  // why no board is open once another failed to open. Core logs why where it
  // is not given.
  readonly fail?: (error: Error) => void;
  // Imports core's OpenTelemetry module, as the page does, where the blockr
  // options have the page send its log records to a collector
  // (../log/settings.ts). Where it is not given, core writes to the console,
  // and logs that it has no such module.
  readonly otel?: () => Promise<OtelModule>;
}

export interface Started {
  // Resolves with CoreApi once the first board is open and its frontend has
  // rendered, or rejects with why core could not start.
  readonly ready: Promise<CoreApi>;
  // The CoreApi of the board that is open, which core makes anew for each
  // board that opens, or undefined while core starts.
  core(): CoreApi | undefined;
  // Ends what core started: it aborts what core runs for the board, and core
  // hears no later session.
  stop(): void;
}

// What core runs for one board, which the abort of its controller ends.
interface Running {
  readonly store: Store;
  readonly scheduler: Scheduler;
  readonly core: CoreApi;
  readonly notifications: Holder;
  readonly controller: AbortController;
}

// A board that opens later, as its text, and the text of the board before,
// which core opens in its place where it fails to open after all.
interface Later {
  readonly text: string;
  readonly before: string;
}

export function start(options: StartOptions): Started {
  const page = new AbortController();
  const alone = options.alone === true;
  // Says why core cannot start or stopped, and has the console write the log
  // lines that core holds where it has not set the page's logger up yet, as
  // where it fails before R gives the blockr options that set it up.
  const fail = (error: Error): void => {
    fallBack();
    if (options.fail === undefined) log.error(error);
    else options.fail(error);
  };
  // Whether core has set the page's logger up, which it does once, as the
  // first board opens.
  let logging = false;

  // The first reply to hello, whose locks hold for the page. And the latest
  // reply that names no package at another version than the page loaded,
  // whose modules and renderers core uses, and with which a board that opens
  // later starts core again.
  let first: HelloReply | undefined;
  let latest: HelloReply | undefined;
  // The version of each package that the page loaded, from the replies that
  // core adopted.
  const loaded = new Map<string, string>();
  // The packages that the last reply named at other versions than the page
  // loaded. The loader imports no module of them that it has not imported
  // already.
  let changed: readonly Changed[] = [];
  // What withdraws the notification that asks for the page to be reloaded,
  // while the open board holds one.
  let reloading: (() => void) | undefined;
  // What core runs for the board that is open.
  let running: Running | undefined;
  const holdsBoard = (): boolean => running?.store.hasBoard() === true;
  // An opening that waits for R to answer the hello with which core starts
  // again.
  let restarting: { resolve: (reply: HelloReply) => void; reject: (error: Error) => void } | undefined;
  // The revisions that the page gives, from one count for the page.
  const nextRevision = counter();

  let settle: { resolve: (core: CoreApi) => void; reject: (error: unknown) => void } | undefined;
  const ready = new Promise<CoreApi>((resolve, reject) => {
    settle = { resolve, reject };
  });
  // A page that starts nothing waits on it, which is no unhandled rejection.
  ready.catch(() => undefined);

  const client = createClient(options.transport, {
    hello: () => ({ protocol: PROTOCOL_VERSION, capabilities: CAPABILITIES }),
    session(reply) {
      heard(reply);
    },
  });
  const blockrOptions = createBlockrOptions(client);
  const loader = createLoader({
    ...options.loader,
    modules: () => latest?.modules ?? {},
    changed: (pkg) => changed.find((c) => c.package === pkg),
  });
  // The migrations of block classes' states, which core imports from the
  // classes' UI modules for the boards that need them, once per page.
  const migrations = createMigrations(loader);

  function heard(reply: Reply): void {
    if (page.signal.aborted) return;
    const error = helloError(reply);
    if (error !== undefined) {
      if (first === undefined) {
        settle?.reject(error);
        fail(error);
      } else if (restarting !== undefined) {
        const waiting = restarting;
        restarting = undefined;
        waiting.reject(error);
      } else {
        log.error(error);
      }
      return;
    }
    const hello = reply as HelloReply;
    if (!sameMajor(hello.protocol, PROTOCOL_VERSION)) {
      halt(
        new Error(
          `R speaks version ${hello.protocol} of the wire protocol, where this page speaks ${PROTOCOL_VERSION}. Reload the page, which then loads the client that R serves.`,
        ),
      );
      return;
    }
    if (first === undefined) {
      first = hello;
      adopt(hello);
      boot(hello, undefined).then(
        ({ core }) => settle?.resolve(core),
        (failure: unknown) => {
          settle?.reject(failure);
          // Core has said why already where it stopped.
          if (!page.signal.aborted) fail(asError(failure));
        },
      );
      return;
    }
    changed = changedPackages(loaded, hello);
    // The board that opens next raises the notification itself, since none is
    // open while core starts again.
    askReload(running?.notifications.raise);
    if (changed.length === 0) adopt(hello);
    if (restarting !== undefined) {
      const waiting = restarting;
      restarting = undefined;
      waiting.resolve(latest ?? first);
      return;
    }
    // A new session holds no results.
    running?.scheduler.session();
  }

  // Makes the reply the one whose modules and renderers core uses, and with
  // which the board that opens next starts, and keeps the versions of the
  // packages that it names as those the page loaded.
  function adopt(reply: HelloReply): void {
    latest = reply;
    for (const [pkg, version] of Object.entries(reply.packages)) {
      loaded.set(pkg, version);
    }
  }

  // Ends what core started: it aborts what core runs for the board, so that
  // core hears no later session, and rejects an opening that waits for R with
  // why.
  function end(why: Error): void {
    page.abort();
    const waiting = restarting;
    restarting = undefined;
    waiting?.reject(why);
  }

  // Asks for the page to be reloaded through a notification of the open
  // board, in place of the one before, while the last reply names packages at
  // other versions than the page loaded, and withdraws the one before
  // otherwise.
  function askReload(raise: Raise | undefined): void {
    const before = reloading;
    reloading = changed.length > 0 && raise !== undefined ? raise(reloadNotification(changed)) : undefined;
    before?.();
  }

  // Stops core where R speaks another major of the protocol, to which the
  // protocol's client sends nothing more: core ends what it started, as stop()
  // does, and says what to do.
  function halt(error: Error): void {
    end(error);
    settle?.reject(error);
    fail(error);
  }

  // Starts core for one board, with what R's reply to hello holds, as the page
  // starts and as each later board opens: the store, the notifications of the
  // board, the command registry, the scheduler and CoreApi, each of which the
  // abort of the opening's controller ends. It registers the plugin classes and
  // board options that the reply names, opens the board in the store, or as the
  // page starts the one that the app's storage plugin names in its place,
  // raises the notification that names what the store dropped, draws the board
  // with its frontend, starts the scheduler, then starts the app's plugins, and
  // resolves with CoreApi and what the store dropped.
  async function boot(reply: HelloReply, later: Later | undefined): Promise<{ core: CoreApi; dropped: Dropped[] }> {
    // The opening ends as the next board opens, or as the page stops.
    const controller = new AbortController();
    const { signal } = controller;
    const ending = (): void => {
      controller.abort();
    };
    if (page.signal.aborted) ending();
    page.signal.addEventListener("abort", ending, { once: true });
    signal.addEventListener(
      "abort",
      () => {
        page.signal.removeEventListener("abort", ending);
      },
      { once: true },
    );

    // The blockr options that the scheduler reads, rand_id, the shape of the
    // ids that commands mint, and those that set the page's logger up, asked
    // at once, in one request, before the scheduler starts and the commands
    // run, and kept for the page.
    // Each goes as R sent it to the scheduler or the command registry, which
    // converts and checks it, and takes its fallback where nothing sets it.
    // As the first board opens, core sets the page's logger up with the other
    // three (../log/settings.ts), which hands it the lines that core held.
    const [evaluationQueue, backgroundEvaluationDelay, gateVisibility, randId, logLevel, logger, otelClientEndpoint] = await Promise.all([
      blockrOptions.read("evaluation_queue"),
      blockrOptions.read("background_evaluation_delay"),
      blockrOptions.read("gate_visibility"),
      blockrOptions.read("rand_id"),
      blockrOptions.read("log_level"),
      blockrOptions.read("logger"),
      blockrOptions.read("otel_client_endpoint"),
    ]);
    if (!logging) {
      const settings = logSettings({ logLevel, logger, otelClientEndpoint }, typeof document === "undefined" ? undefined : document.baseURI);
      logging = true;
      setUpLogging(settings, () => options.transport.sessionId?.(), options.otel ?? noOtel);
    }

    const store: Store = createStore({ hello: reply, signal, migration: (id) => migrations.get(id) });
    // The notifications of the board, made before core imports the modules
    // that the reply names, so that the frontend hears of one that fails.
    const notifications = createNotifications(store, { signal, plugins: reply.plugins });
    const { raise } = notifications;
    askReload(raise);
    const commands = createCommands(store, {
      signal,
      lock: reply.lock,
      randId,
      blockrOption: (name) => blockrOptions.read(name),
    });
    await register(reply, store, commands, signal, raise);
    // R binds inputs itself for the board that it sent alone.
    const bound = later === undefined ? reply.bound : undefined;
    const scheduler = createScheduler({
      store,
      client,
      evaluationQueue,
      backgroundEvaluationDelay,
      gateVisibility,
      nextRevision,
      signal,
      ...(bound === undefined ? {} : { bound }),
    });
    const { api: core, plugins } = createCore({
      store: {
        ...store,
        open: (given) => reopen(given, controller),
      },
      commands,
      scheduler,
      loader,
      blockrOptions,
      lock: (first ?? reply).lock,
      renderers: () => latest?.renderers ?? {},
      plugins: reply.plugins,
      options: reply.options,
      notifications,
      signal,
      ...(options.measure === undefined ? {} : { measure: options.measure }),
    });
    running = { store, scheduler, core, notifications, controller };

    let dropped: Dropped[] = [];
    let failure: Error | undefined;
    if (later === undefined) {
      const instead = alone ? undefined : await named(reply, store, core, signal, raise);
      await migrations.prepare(instead?.text ?? reply.board, reply.block_registry);
      if (instead === undefined) {
        dropped = await store.open(reply);
      } else {
        try {
          dropped = await store.open(instead.text);
        } catch (error) {
          log.error(asError(error));
          raise(
            coreNotification(
              "board_open_failed",
              "error",
              `The board that the storage plugin "${instead.plugin}" named did not open, so core opens the board that R sent: ${errorMessage(error)}`,
              { about: { plugin: instead.plugin }, package: packageFromId(instead.plugin) },
            ),
          );
          await migrations.prepare(reply.board, reply.block_registry);
          dropped = await store.open(reply);
        }
      }
    } else {
      try {
        await migrations.prepare(later.text, reply.block_registry);
        dropped = await store.open(later.text);
      } catch (error) {
        failure = asError(error);
        try {
          await store.open(later.before);
        } catch (again) {
          log.error(asError(again));
          throw failure;
        }
        // The opening has ended the code that called open(), which rejects,
        // so core tells the user itself.
        raise(
          coreNotification(
            "board_open_failed",
            "error",
            `The board did not open, so core opened the board before it again: ${failure.message}`,
          ),
        );
      }
    }
    if (dropped.length > 0) raise(droppedNotification(dropped));

    if (alone) {
      try {
        options.frontEnds?.card?.(core, signal);
      } finally {
        scheduler.start();
      }
    } else {
      // The frontend that draws the board, once the board options that it
      // requires are on the board.
      const front = plugins.frontend();
      options.frontEnds?.prepare(
        front === undefined
          ? { none: noFrontend(reply, core) }
          : { frontend: front.frontend, plugin: front.plugin, args: reply.frontend },
        signal,
      );
      const failures = await (front?.added ?? []);
      try {
        await options.frontEnds?.render(core, signal, failures);
      } finally {
        scheduler.start();
      }
    }
    await plugins.start();
    if (failure !== undefined) throw failure;
    return { core, dropped };
  }

  // Registers in the store the class of each of the app's plugins and each
  // board option that the reply names, from the module that R serves for it,
  // which the loader imports once per page, and the commands of each plugin
  // class in the command registry. It raises a notification for a module that
  // fails to load and for a default export that the store refuses, which
  // concerns the plugin class or the board option, with the module's package,
  // names a command that the registry refuses, and carries on, so that the
  // board opens without them.
  async function register(
    reply: HelloReply,
    store: Store,
    commands: Commands,
    signal: AbortSignal,
    raise: Raise,
  ): Promise<void> {
    const failed = (error: unknown, concerns: Concerns): void => {
      log.error(asError(error));
      if (!signal.aborted) raise(coreNotification("module_failed", "error", errorMessage(error), concerns));
    };
    const [defs, opts] = await Promise.all([
      Promise.all(
        Object.entries(reply.plugins).map(([id, entry]) =>
          listed(entry, packageFromId(id), (error) => {
            failed(error, { about: { plugin: id }, package: packageFromId(id) });
          }),
        ),
      ),
      Promise.all(
        Object.entries(reply.options).map(([id, entry]) =>
          listed(entry, entry.package, (error) => {
            failed(error, { about: { option: id }, package: entry.package });
          }),
        ),
      ),
    ]);
    if (signal.aborted) return;
    Object.entries(reply.options).forEach(([id, entry], i) => {
      const def = opts[i];
      if (def === undefined) return;
      try {
        store.options.register({ ...(def as unknown as Option), id });
      } catch (error) {
        failed(error, { about: { option: id }, package: entry.package });
      }
    });
    Object.keys(reply.plugins).forEach((id, i) => {
      const def = defs[i];
      if (def === undefined) return;
      let cls: PluginClass;
      try {
        cls = { ...(def as unknown as PluginClass), id };
        store.plugins.register(cls);
      } catch (error) {
        failed(error, { about: { plugin: id }, package: packageFromId(id) });
        return;
      }
      for (const command of cls.commands ?? []) {
        try {
          commands.register(command);
        } catch (error) {
          log.error(new Error(`A command of the plugin class "${id}" is refused: ${errorMessage(error)}`, { cause: error }));
        }
      }
    });
  }

  // The default export of the module that R serves for a plugin class or a
  // board option, or undefined where it fails to load or exports none, which
  // `failed` hears.
  async function listed(
    source: { readonly module: string; readonly css?: string },
    pkg: string | null,
    failed: (error: unknown) => void,
  ): Promise<Readonly<Record<string, unknown>> | undefined> {
    try {
      const at = source.css === undefined ? { module: source.module } : { module: source.module, css: source.css };
      return listedDefault(await loader.load(at, pkg), source.module, pkg);
    } catch (error) {
      failed(error);
      return undefined;
    }
  }

  // The text of the board that the app's storage plugin names in place of the
  // one that R sent, as the page starts, with the id of the plugin's class, or
  // undefined for R's: core asks for none under the load lock, which fixes the
  // app to R's board, and keeps R's where load() rejects or resolves with no
  // JSON board, raising a notification of why, which concerns the package of
  // the plugin's class.
  async function named(
    reply: HelloReply,
    store: Store,
    core: CoreApi,
    signal: AbortSignal,
    raise: Raise,
  ): Promise<{ readonly text: string; readonly plugin: PluginClassId } | undefined> {
    if (reply.lock.load) return undefined;
    const classes = store.plugins.registered();
    const plugin = Object.keys(reply.plugins).find((id) => own(classes, id)?.storage !== undefined);
    const storage = plugin === undefined ? undefined : own(classes, plugin)?.storage;
    if (plugin === undefined || storage?.load === undefined) return undefined;
    const refuse = (error: Error): void => {
      log.error(error);
      raise(coreNotification("board_open_failed", "error", error.message, { about: { plugin }, package: packageFromId(plugin) }));
    };
    let text: unknown;
    try {
      text = await storage.load({ core, plugin, signal });
    } catch (error) {
      refuse(
        new Error(`The load() of the storage plugin "${plugin}" failed, so core opens the board that R sent: ${errorMessage(error)}`, {
          cause: error,
        }),
      );
      return undefined;
    }
    if (text === undefined) return undefined;
    if (typeof text !== "string") {
      refuse(
        new Error(
          `The load() of the storage plugin "${plugin}" resolved with something other than the text of a JSON board, so core opens the board that R sent.`,
        ),
      );
      return undefined;
    }
    try {
      JSON.parse(text);
    } catch (error) {
      refuse(
        new Error(`The storage plugin "${plugin}" named a board that holds no JSON, so core opens the board that R sent: ${errorMessage(error)}`),
      );
      return undefined;
    }
    return { text, plugin };
  }

  // Openings run one at a time.
  let queue: Promise<unknown> = Promise.resolve();

  // Opens a board in place of the one that the store of the opening holds,
  // whose controller is given, by starting core again for it.
  function reopen(given: string | HelloReply, of: AbortController): Promise<Dropped[]> {
    const run = queue.then(() => reopening(given, of));
    queue = run.catch(() => undefined);
    return run;
  }

  async function reopening(given: string | HelloReply, of: AbortController): Promise<Dropped[]> {
    const refused = "The store refused to open the board";
    const current = running;
    if (current?.controller !== of || of.signal.aborted) {
      throw new StoreError(refused, ["Another board opened since, so this store opens no board."]);
    }
    if (!current.store.hasBoard()) {
      throw new StoreError(refused, ["The store holds no board yet, so it opens none in place of it."]);
    }
    const text = typeof given === "string" ? given : JSON.stringify(given.board);

    await migrations.prepare(text, current.store.blockRegistry());
    await current.store.check(text);
    const before = current.store.save();

    of.abort();
    running = undefined;
    try {
      const reply = await afresh();
      const { dropped } = await boot(reply, { text, before });
      return dropped;
    } catch (error) {
      if (!page.signal.aborted && !holdsBoard()) fail(asError(error));
      throw error;
    }
  }

  // Sends the hello with which core starts again, and resolves with R's reply
  // to it, or to the hello of the next session where the session ends first.
  function afresh(): Promise<HelloReply> {
    return new Promise((resolve, reject) => {
      if (page.signal.aborted) {
        reject(new Error("Core has stopped."));
        return;
      }
      restarting = { resolve, reject };
      client.hello();
    });
  }

  return {
    ready,
    core: () => running?.core,
    stop() {
      end(new Error("Core has stopped."));
    },
  };
}

// Why a reply to hello does not let core start: it is no reply that R sends
// to hello.
function helloError(reply: Reply): Error | undefined {
  if (reply.status !== "ok" || !("block_registry" in reply)) {
    const said = "conditions" in reply ? reply.conditions.map((c) => c.message).join(" ") : "";
    return new Error(`R replied ${reply.status} to the client's hello. ${said}`.trim());
  }
  return undefined;
}

// Why the app runs no frontend, which the page shows in place of the board: no
// plugin that the reply names is a frontend whose class core registered from
// the module that R names for it.
function noFrontend(reply: HelloReply, core: CoreApi): string {
  const missing = Object.keys(reply.plugins).filter((id) => !Object.hasOwn(core.plugins.appPlugins(), id));
  const why =
    missing.length === 0
      ? "none of the app's plugins is a frontend"
      : `core registered no class of the app's ${missing.length === 1 ? "plugin" : "plugins"} ${quoted(missing)}`;
  return `The app runs no frontend that draws the board, since ${why}.`;
}

// The packages that a reply names at another version than the page loaded.
function changedPackages(loaded: ReadonlyMap<string, string>, reply: HelloReply): Changed[] {
  return Object.entries(reply.packages).flatMap(([pkg, now]) => {
    const before = loaded.get(pkg);
    return before === undefined || before === now ? [] : [{ package: pkg, loaded: before, now }];
  });
}

// The notification that asks the user to reload the page, as R now runs other
// versions of packages whose code the page holds, which concerns the board and
// names the packages. A frontend offers to reload the page, which then loads
// the code that R serves, rather than load a second copy beside the first. The
// page carries on meanwhile, since a reload loses the board unless it is saved.
function reloadNotification(changed: readonly Changed[]): Omit<Notification, "id"> {
  const versions = changed.map((c) => `${c.package} ${c.loaded} to ${c.now}`).join(", ");
  return coreNotification(
    "page_reload_needed",
    "warning",
    `R now runs other versions of packages than this page loaded (${versions}). Save the board, then reload the page.`,
  );
}

// The notification that names what the store dropped as a board opened, one
// line for each, with the package of each that names one, as Board objects has
// it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#objects-that-cannot-be-rebuilt).
// One notification covers the whole opening, which concerns the board.
function droppedNotification(dropped: readonly Dropped[]): Omit<Notification, "id"> {
  const lines = dropped.map((item) => {
    if ("option" in item) return `- board option "${item.option}"`;
    if ("block" in item) return `- block "${item.block}" (${item.registry_id})`;
    if ("plugin" in item) return `- plugin "${item.plugin}"`;
    return `- package ${item.package} ${item.version}`;
  });
  return coreNotification("board_objects_dropped", "warning", ["The board opened without:", ...lines].join("\n"));
}

// The default export of a module that R serves for a plugin class or a board
// option, which declares the major of core's JS API that it was built against.
function listedDefault(exports: unknown, module: string, pkg: string | null): Readonly<Record<string, unknown>> {
  const def = moduleExports(exports, module, pkg).default;
  if (!isObject(def)) {
    throw new ModuleError(module, pkg, "has no default export, an object.");
  }
  return def;
}

// What core imports in place of its OpenTelemetry module where it is given
// none, as in a test.
function noOtel(): Promise<OtelModule> {
  return Promise.reject(new Error("Core was given no OpenTelemetry module to send log records with."));
}

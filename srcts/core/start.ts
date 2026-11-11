// Starting core on a page, in the order of the startup modules, as Starting
// the page has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#starting-the-page):
// 1. The protocol's client sends each session's hello as R starts the session.
// 2. Once R has answered the first, core asks R for the blockr options that its
//    scheduler reads and for rand_id, the shape of the ids that its commands
//    mint, and creates for the board that it opens the store, with the classes
//    of core's own plugins, its basic frontend and its storage plugin, the
//    command registry with core's board options, the scheduler and CoreApi.
// 3. It initializes the startup module of every package that R has loaded, one
//    at a time in R's order, awaiting each initialize(). It names one that
//    fails and carries on.
// 4. It opens the board that R sent, or in its place the one that a startup
//    module resolves with, unless the load lock is on or two startup modules
//    resolve with one, where it keeps R's and names the packages. Either board
//    runs the app's plugins, which R names in its reply, each with the state
//    that the board holds for it.
// 5. As the store opens the board, core adds to it the board options that the
//    classes of the app's plugins require (./plugins.ts).
// 6. Once the board is open, core imports the module of the app's frontend,
//    the plugin that runs whose class has `frontend`, such as core's basic
//    frontend. Once the board options that the frontend requires are on the
//    board, core calls the frontend's render(), and starts the scheduler once
//    render() has returned, so that the scheduler sends R nothing before. It
//    then sends, through `notifications`, the note that names what the store
//    dropped, which the frontend shows. Where the app runs no frontend, the
//    page shows why in place of the board.
// 7. Core then calls the start of each plugin whose class has one, so that
//    what the frontend first shows comes before what the plugins ask for as
//    they start.
//
// A block that serve() serves alone comes as a board of one block, which no
// frontend draws, as Data passed to serve() has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#data-passed-to-serve).
// Core then starts in the same order, with three differences:
// - In step 4, it opens the board that R sent, whatever board a startup
//   module resolves with.
// - In step 5, it adds no board options for plugins, since R names no plugin
//   for a block that serve() serves alone. The board still takes dark_mode, as
//   every board does, since its onChange() switches the page's theme
//   (./core.ts).
// - In step 6, it imports no frontend, but draws the block in its card, which
//   demands the block, and starts the scheduler once the card has mounted the
//   block.
//
// A board opened later, through the store's open() that CoreApi gives, starts
// core again in place, as Opening a board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board):
// 1. The store on the page checks the board as it would open it. A board that
//    it refuses leaves the page as it was, and open() rejects with the reason.
// 2. One abort ends everything that core runs for the board on the page: the
//    frontend and what it mounted, every plugin that runs, every startup
//    module, the store with its undo history, the command registry and the
//    plugin classes, the board options, the scheduler with its demands, which
//    block has focus, and what core keeps of outputs and answers.
// 3. Core sends another hello, which R takes as a fresh start of the session,
//    so that R drops every result and request of the board before.
// 4. Once R has answered it, core starts as the page does, from step 2 on,
//    with the board given in place of R's. Where that board fails to open after
//    all, core opens the board before in the new store, and open() rejects with
//    the reason.
// Only what is fixed for the page outlives an opening: the code and
// stylesheets that have loaded, the page's element, the transport with its
// session, the protocol's client, the blockr options that core has read, the
// locks, and the revisions given, so that the next scheduler gives a block id a
// revision above any it had.
//
// Core takes a later reply to hello, as of a later session or of the hello with
// which core starts again, in one of three ways:
// 1. Where R speaks another major of the protocol in it, the protocol's client
//    stops and sends R nothing more, not even what it held for the session.
//    Core then ends what it runs for the board, as stop() does, and says what
//    to do, as for the first reply.
// 2. Where it names a package at another version than the page loaded, core
//    asks for the page to be reloaded, and goes on with the reply it used
//    before: for the modules and renderers it loads, and for the board that
//    opens next. So it loads no module of that package from the new reply. The
//    loader imports no module of that package that it has not imported
//    already, rather than load a second copy of the package.
// 3. Otherwise core goes on with the new reply.
// The reply to a later session's hello also has the scheduler give every block
// a new revision, since the new session holds no results.

import { createCommands } from "../commands/index.ts";
import { API, createLoader, ModuleError } from "../mount/index.ts";
import type { Loader, LoaderOptions, Measure, Source } from "../mount/index.ts";
import { createClient, PROTOCOL_VERSION, sameMajor } from "../protocol/index.ts";
import type { Capability, HelloReply, Reply, Transport } from "../protocol/index.ts";
import { counter, createScheduler } from "../scheduler/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import { createStore, StoreError } from "../store/index.ts";
import type { Dropped, Option, Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi, Note } from "../types/core-api.ts";
import { asError, errorMessage, isObject, own, quoted, report } from "../util/index.ts";
import { createBlockrOptions } from "./blockr-options.ts";
import { createCore } from "./core.ts";
import { corePluginClasses } from "./plugins.ts";

// The capabilities that core's client speaks.
const CAPABILITIES: Capability[] = ["cancel", "interrupt", "progress"];

// The frontend that draws a board that opens: the id of its module, from where
// R serves the module, which is undefined where R serves no such module, and
// the id of the frontend's class, which names the plugin that it is. Or, where
// the app runs no frontend, why, which the page shows in place of the board.
export type Front =
  | { readonly id: string; readonly source: Source | undefined; readonly plugin: PluginClassId }
  | { readonly none: string };

// What draws the frontends of the boards that open, which the page gives, and
// a CoreApi for tests may leave out.
export interface FrontEnds {
  // Imports the module of the frontend and ends the frontend on the page, if
  // any. Core calls it once the store has opened a board. Where the signal has
  // aborted by the time the module has loaded, as another board opened
  // meanwhile, it leaves the page to that board.
  prepare(front: Front, loader: Loader, signal: AbortSignal): Promise<void>;
  // Calls the render() of the frontend that prepare() imported, with the
  // element, CoreApi, the id of the frontend's class and a signal, and
  // resolves once render() has returned or resolved. Where board options that
  // the frontend requires did not go onto the board, or the app runs no
  // frontend, it shows why in the element instead, and calls no render(). The
  // frontend ends as the signal aborts, as the next board opens.
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
  // The board options that core registers before any startup module runs.
  readonly options: (core: Pick<CoreApi, "blockrOption">) => readonly Option[];
  readonly frontEnds?: FrontEnds;
  // Whether the board is a block that serve() serves alone, which no frontend
  // draws, but the card that `frontEnds` gives.
  readonly alone?: boolean;
  readonly measure?: Measure;
  // Asks for the page to be reloaded, where a reply names a loaded package at
  // another version, rather than load a second copy of it.
  readonly reload?: (changed: readonly Changed[]) => void;
  // Shows why core cannot start, or why it stopped, as where R speaks another
  // major of the protocol in the first session or a later one, or cannot give
  // a blockr option that core reads as it starts, or gives one that the
  // scheduler or the command registry does not take, or why no board is open
  // once another failed to open.
  readonly fail?: (error: Error) => void;
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
  const fail = options.fail ?? ((error: Error) => {
    report(error);
  });

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
        report(error);
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
    if (changed.length > 0) {
      (options.reload ?? askReload)(changed);
    } else {
      adopt(hello);
    }
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

  // Stops core where R speaks another major of the protocol, to which the
  // protocol's client sends nothing more: core ends what it started, as stop()
  // does, and says what to do.
  function halt(error: Error): void {
    end(error);
    settle?.reject(error);
    fail(error);
  }

  // Starts core for one board, with what R's reply to hello holds, as the page
  // starts and as each later board opens: the store, the command registry, the
  // scheduler and CoreApi, each of which the abort of the opening's controller
  // ends. It initializes the startup modules with that CoreApi and signal,
  // opens the board in the store, draws it with its frontend, starts the
  // scheduler, sends the note that names what the store dropped, then starts
  // the app's plugins, and resolves with CoreApi and what the store dropped.
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

    // The blockr options that the scheduler reads, and rand_id, the shape of
    // the ids that core's commands mint, asked at once, in one request, before
    // the scheduler starts and the commands run, and kept for the page.
    // Each goes as R sent it to the scheduler or the command registry, which
    // converts and checks it, and takes its fallback where nothing sets it.
    const [evaluationQueue, backgroundEvaluationDelay, gateVisibility, randId] = await Promise.all([
      blockrOptions.read("evaluation_queue"),
      blockrOptions.read("background_evaluation_delay"),
      blockrOptions.read("gate_visibility"),
      blockrOptions.read("rand_id"),
    ]);

    const store: Store = createStore({ hello: reply, signal });
    for (const def of corePluginClasses()) store.plugins.register(def);
    const commands = createCommands(store, { signal, lock: reply.lock, randId });
    for (const option of options.options({ blockrOption: (name) => blockrOptions.read(name) })) {
      store.options.register(option);
    }
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
      signal,
      ...(options.measure === undefined ? {} : { measure: options.measure }),
    });
    running = { store, scheduler, core, controller };

    const named = await initialize(reply, core, signal, later === undefined);

    let dropped: Dropped[] = [];
    let failure: Error | undefined;
    if (later === undefined) {
      const instead = alone ? undefined : chosen(reply, named);
      if (instead === undefined) {
        dropped = await store.open(reply);
      } else {
        try {
          dropped = await store.open(instead);
        } catch (error) {
          report(error);
          dropped = await store.open(reply);
        }
      }
    } else {
      try {
        dropped = await store.open(later.text);
      } catch (error) {
        failure = asError(error);
        try {
          await store.open(later.before);
        } catch (again) {
          report(again);
          throw failure;
        }
      }
    }

    if (alone) {
      try {
        options.frontEnds?.card?.(core, signal);
      } finally {
        scheduler.start();
      }
    } else {
      // The frontend that draws the board, imported while the board options
      // that it requires go onto the board.
      const front = plugins.frontend();
      const [failures] = await Promise.all([
        front?.added ?? [],
        options.frontEnds?.prepare(
          front === undefined
            ? { none: noFrontend(reply, core) }
            : { id: front.module, source: own(latest?.modules ?? {}, front.module), plugin: front.plugin },
          loader,
          signal,
        ),
      ]);
      try {
        await options.frontEnds?.render(core, signal, failures);
      } finally {
        scheduler.start();
      }
      // The frontend subscribes to the notes as it draws, so this one reaches
      // it.
      if (dropped.length > 0) core.notifications.notify(droppedNote(dropped));
    }
    await plugins.start();
    if (failure !== undefined) throw failure;
    return { core, dropped };
  }

  // Initializes the startup module of each package that the reply names, one
  // at a time in R's order, with CoreApi and the signal of the board that
  // opens, and resolves with the boards that they name, where they may name
  // one, as the page starts.
  async function initialize(
    reply: HelloReply,
    core: CoreApi,
    signal: AbortSignal,
    naming: boolean,
  ): Promise<{ package: string; text: string }[]> {
    const named: { package: string; text: string }[] = [];
    for (const entry of reply.startup_modules) {
      if (signal.aborted) break;
      try {
        const source = entry.css === undefined ? { module: entry.module } : { module: entry.module, css: entry.css };
        const mod = asStartupModule(await loader.load(source, entry.package), entry.module, entry.package);
        let result: unknown;
        try {
          result = await mod.initialize({ core, signal });
        } catch (error) {
          throw new ModuleError(entry.module, entry.package, `failed in initialize(): ${errorMessage(error)}`, { cause: error });
        }
        if (typeof result === "string") {
          if (naming) {
            named.push({ package: entry.package, text: result });
          } else {
            report(
              new Error(
                `The startup module of ${entry.package} named a board to open as another board opened, which core ignores, since only a startup module that runs as the page starts names one.`,
              ),
            );
          }
        } else if (result !== undefined) {
          throw new ModuleError(entry.module, entry.package, "resolved initialize() with something other than the text of a JSON board.");
        }
      } catch (error) {
        report(error);
      }
    }
    return named;
  }

  // The text of the board that a startup module named in place of the board
  // that R sent, or undefined where core opens R's. The store opens it with
  // the reply, so that it runs the app's plugins, which the reply names.
  function chosen(reply: HelloReply, named: readonly { package: string; text: string }[]): string | undefined {
    const [only, ...more] = named;
    if (only === undefined) return undefined;
    const packages = quoted(named.map((entry) => entry.package));
    if (more.length > 0) {
      report(new Error(`The startup modules of ${packages} each named a board to open, so core opens the board that R sent.`));
      return undefined;
    }
    if (reply.lock.load) {
      report(
        new Error(
          `The load lock is on, so core opens the board that R sent rather than the one that ${only.package} named.`,
        ),
      );
      return undefined;
    }
    try {
      JSON.parse(only.text);
    } catch (error) {
      report(new Error(`The startup module of ${only.package} named a board that holds no JSON, so core opens the board that R sent: ${errorMessage(error)}`));
      return undefined;
    }
    return only.text;
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
  if (reply.status !== "ok" || !("startup_modules" in reply)) {
    const said = "conditions" in reply ? reply.conditions.map((c) => c.message).join(" ") : "";
    return new Error(`R replied ${reply.status} to the client's hello. ${said}`.trim());
  }
  return undefined;
}

// Why the app runs no frontend, which the page shows in place of the board: no
// plugin that the reply names is a frontend whose class a package registered.
function noFrontend(reply: HelloReply, core: CoreApi): string {
  const missing = Object.keys(reply.plugins).filter((id) => !Object.hasOwn(core.plugins.running(), id));
  const why =
    missing.length === 0
      ? "none of the app's plugins is a frontend"
      : `no package registered the ${missing.length === 1 ? "class" : "classes"} of the app's ${missing.length === 1 ? "plugin" : "plugins"} ${quoted(missing)}`;
  return `The app runs no frontend that draws the board, since ${why}.`;
}

// The packages that a reply names at another version than the page loaded.
function changedPackages(loaded: ReadonlyMap<string, string>, reply: HelloReply): Changed[] {
  return Object.entries(reply.packages).flatMap(([pkg, now]) => {
    const before = loaded.get(pkg);
    return before === undefined || before === now ? [] : [{ package: pkg, loaded: before, now }];
  });
}

// Asks the user to reload the page, as R now runs other versions of packages
// whose code the page holds.
function askReload(changed: readonly Changed[]): void {
  report(
    new Error(
      `R now runs ${changed.map((c) => `${c.package} ${c.now}`).join(", ")}, where the page loaded ${changed.map((c) => `${c.package} ${c.loaded}`).join(", ")}. Reload the page.`,
    ),
  );
}

// The note that names what the store dropped as a board opened, one line for
// each, as Board objects has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#objects-that-cannot-be-rebuilt).
function droppedNote(dropped: readonly Dropped[]): Note {
  const lines = dropped.map((item) => {
    if ("option" in item) return `- board option "${item.option}"`;
    if ("block" in item) return `- block "${item.block}" (${item.registry_id})`;
    if ("plugin" in item) return `- plugin "${item.plugin}"`;
    return `- package ${item.package} ${item.version}`;
  });
  return { message: ["The board opened without:", ...lines].join("\n"), severity: "warning" };
}

// What a startup module exports as its default.
interface StartupModule {
  initialize(args: { core: CoreApi; signal: AbortSignal }): unknown;
}

function asStartupModule(exports: unknown, module: string, pkg: string): StartupModule {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new ModuleError(module, pkg, "declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new ModuleError(
      module,
      pkg,
      `was built against major ${String(exports.api)} of core's JS API, where core's is ${String(API)}.`,
    );
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.initialize !== "function") {
    throw new ModuleError(module, pkg, "has no default export with an initialize() function.");
  }
  return mod as unknown as StartupModule;
}

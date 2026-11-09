// Starting core on a page, in the order of the startup modules, as Starting
// the page has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#starting-the-page):
// 1. The protocol's client sends each session's hello as R starts the session.
// 2. Once R has answered the first, core asks R for the blockr options that its
//    scheduler reads, and creates for the board that it opens the store, the
//    command registry with core's board options, the scheduler and CoreApi.
// 3. It initializes the startup module of every package that R has loaded, one
//    at a time in R's order, awaiting each initialize(). It names one that
//    fails and carries on. Core records the package of each board option:
//    the package of the startup module whose initialize() it awaits as the
//    board option is registered, or blockr.core for core's own. A second
//    registration of the board option's id names that package.
// 4. It opens the board that R sent, or in its place the one that a startup
//    module resolves with, unless the load lock is on or two startup modules
//    resolve with one, where it keeps R's and names the packages. A board that
//    a startup module names holds the plugins of the board that R sent, each
//    with the state that the named board gives it.
// 5. As the store opens the board, core imports the module of core's basic
//    front-end, which draws every board. Once the board is open, it calls the
//    front-end's render(), and starts the scheduler once render() has returned,
//    so that the scheduler sends R nothing before.
//
// A board opened later, through the store's open() that CoreApi gives, starts
// core again in place, as Opening a board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board):
// 1. The store on the page checks the board as it would open it. A board that
//    it refuses leaves the page as it was, and open() rejects with the reason.
// 2. One abort ends everything that core runs for the board on the page: the
//    front-end and what it mounted, every startup module, the store with its
//    undo history, the command registry, the board options, the scheduler with
//    its demands, and what core keeps of outputs and answers.
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
// The reply to a later session's hello, for the board that is open, has the
// scheduler give every block a new revision, and makes core ask for the page to
// be reloaded where it names a loaded package at another version.

import { createCommands } from "../commands/index.ts";
import { API, createLoader, ModuleError } from "../mount/index.ts";
import type { Loader, LoaderOptions, Measure, Source } from "../mount/index.ts";
import { createClient, PROTOCOL_VERSION } from "../protocol/index.ts";
import type { Capability, HelloReply, Reply, Transport } from "../protocol/index.ts";
import { counter, createScheduler } from "../scheduler/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import { createStore, StoreError } from "../store/index.ts";
import type { Dropped, Option, Store } from "../store/index.ts";
import type { CoreApi } from "../types/core-api.ts";
import { asError, isObject, messageOf, own, quoted, report } from "../util/index.ts";
import { createBlockrOptions } from "./blockr-options.ts";
import { createCore } from "./core.ts";

// The capabilities that core's client speaks.
const CAPABILITIES: Capability[] = ["cancel", "interrupt", "progress"];

// The id of core's basic front-end, which draws every board.
export const BASIC = "blockr.core-basic";

// The package of core's own board options.
const CORE = "blockr.core";

// What draws the front-ends of the boards that open, which the page gives, and
// a CoreApi for tests may leave out.
export interface FrontEnds {
  // Imports the module of the front-end, by its id, from where R serves it,
  // which is undefined where R serves no such module, and ends the front-end
  // on the page, if any. Core calls it as the store opens a board, once the
  // startup modules have initialized.
  prepare(front: { readonly id: string; readonly source: Source | undefined }, loader: Loader): Promise<void>;
  // Calls the render() of the front-end that prepare() imported, with CoreApi,
  // and resolves once render() has returned or resolved. The front-end ends as
  // the signal aborts, as the next board opens.
  render(core: CoreApi, signal: AbortSignal): Promise<void>;
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
  readonly loader?: Omit<LoaderOptions, "modules">;
  // The board options that core registers before any startup module runs.
  readonly options: (core: Pick<CoreApi, "blockrOption">) => readonly Option[];
  readonly frontEnds?: FrontEnds;
  readonly measure?: Measure;
  // Asks for the page to be reloaded, where a reply names a loaded package at
  // another version, rather than load a second copy of it.
  readonly reload?: (changed: readonly Changed[]) => void;
  // Shows why core cannot start, as where R speaks another major of the
  // protocol, or why no board is open once another failed to open.
  readonly fail?: (error: Error) => void;
}

export interface Started {
  // Resolves with CoreApi once the first board is open and its front-end has
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

// The package whose startup module registers now, for one opening: blockr.core
// while core registers its own board options, the package of the startup
// module whose initialize() core awaits, and null otherwise.
interface Registering {
  package: string | null;
}

export function start(options: StartOptions): Started {
  const page = new AbortController();
  const fail = options.fail ?? ((error: Error) => {
    report(error);
  });

  // The first reply to hello, whose package versions are those the page
  // loaded and whose locks hold for the page, and the latest, whose modules
  // and renderers core uses.
  let first: HelloReply | undefined;
  let latest: HelloReply | undefined;
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
  const loader = createLoader({ ...options.loader, modules: () => latest?.modules ?? {} });

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
    latest = hello;
    if (first === undefined) {
      first = hello;
      boot(hello, undefined).then(
        ({ core }) => settle?.resolve(core),
        (failure: unknown) => {
          settle?.reject(failure);
          fail(asError(failure));
        },
      );
      return;
    }
    const changed = changedPackages(first, hello);
    if (changed.length > 0) (options.reload ?? askReload)(changed);
    if (restarting !== undefined) {
      const waiting = restarting;
      restarting = undefined;
      waiting.resolve(hello);
      return;
    }
    // A new session holds no results.
    running?.scheduler.session();
  }

  // Starts core for one board, with what R's reply to hello holds, as the page
  // starts and as each later board opens: the store, the command registry, the
  // scheduler and CoreApi, each of which the abort of the opening's controller
  // ends. It initializes the startup modules with that CoreApi and signal,
  // opens the board in the store, draws it with its front-end and starts the
  // scheduler, and resolves with CoreApi and what the store dropped.
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

    // The blockr options that the scheduler reads, asked at once, in one
    // request, before the scheduler starts, and kept for the page.
    const [evaluationQueue, backgroundEvaluationDelay, gateVisibility] = await Promise.all([
      blockrOptions.read("evaluation_queue", 2),
      blockrOptions.read("background_evaluation_delay", Infinity),
      blockrOptions.read("gate_visibility", true),
    ]);

    // The store keeps this package with each board option registered.
    const registering: Registering = { package: CORE };
    const store: Store = createStore({
      hello: reply,
      loaded: () => prepare(),
      signal,
      registering: () => registering.package,
    });
    const commands = createCommands(store, { signal, lock: reply.lock });
    for (const option of options.options({ blockrOption: (name, fallback) => blockrOptions.read(name, fallback) })) {
      store.options.register(option);
    }
    registering.package = null;
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
    const core = createCore({
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
      signal,
      ...(options.measure === undefined ? {} : { measure: options.measure }),
    });
    running = { store, scheduler, core, controller };

    const named = await initialize(reply, core, signal, later === undefined, registering);

    let dropped: Dropped[] = [];
    let failure: Error | undefined;
    if (later === undefined) {
      const instead = chosen(reply, named);
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

    try {
      await options.frontEnds?.render(core, signal);
    } finally {
      scheduler.start();
    }
    if (failure !== undefined) throw failure;
    return { core, dropped };
  }

  // Initializes the startup module of each package that the reply names, one
  // at a time in R's order, with CoreApi and the signal of the board that
  // opens, and resolves with the boards that they name, where they may name
  // one, as the page starts. While it awaits a module's initialize(), it has
  // `registering` name the module's package.
  async function initialize(
    reply: HelloReply,
    core: CoreApi,
    signal: AbortSignal,
    naming: boolean,
    registering: Registering,
  ): Promise<{ package: string; text: string }[]> {
    const named: { package: string; text: string }[] = [];
    for (const entry of reply.startup_modules) {
      if (signal.aborted) break;
      try {
        const source = entry.css === undefined ? { module: entry.module } : { module: entry.module, css: entry.css };
        const mod = startupOf(await loader.load(source, entry.package), entry.module, entry.package);
        let result: unknown;
        registering.package = entry.package;
        try {
          result = await mod.initialize({ core, signal });
        } catch (error) {
          throw new ModuleError(entry.module, entry.package, `failed in initialize(): ${messageOf(error)}`, { cause: error });
        } finally {
          registering.package = null;
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
  // the reply, so that it holds the plugins of the board that R sent.
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
      report(new Error(`The startup module of ${only.package} named a board that holds no JSON, so core opens the board that R sent: ${messageOf(error)}`));
      return undefined;
    }
    return only.text;
  }

  // What the store runs once R has replied, before it migrates or drops
  // anything, once the startup modules have initialized: the import of core's
  // basic front-end, which draws every board.
  async function prepare(): Promise<void> {
    await options.frontEnds?.prepare({ id: BASIC, source: own(latest?.modules ?? {}, BASIC) }, loader);
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
      page.abort();
      const waiting = restarting;
      restarting = undefined;
      waiting?.reject(new Error("Core has stopped."));
    },
  };
}

// Why a reply to hello does not let core start: it is no reply that R sends
// to hello, or R speaks another major of the protocol.
function helloError(reply: Reply): Error | undefined {
  if (reply.status !== "ok" || !("startup_modules" in reply)) {
    const said = "conditions" in reply ? reply.conditions.map((c) => c.message).join(" ") : "";
    return new Error(`R replied ${reply.status} to the client's hello. ${said}`.trim());
  }
  const theirs = reply.protocol.split(".")[0];
  const ours = PROTOCOL_VERSION.split(".")[0];
  if (theirs !== ours) {
    return new Error(
      `R speaks version ${reply.protocol} of the wire protocol, where this page speaks ${PROTOCOL_VERSION}. Reload the page, which then loads the client that R serves.`,
    );
  }
  return undefined;
}

// The packages that a reply names at another version than the first reply.
function changedPackages(first: HelloReply, reply: HelloReply): Changed[] {
  return Object.entries(reply.packages).flatMap(([pkg, now]) => {
    const loaded = own(first.packages, pkg);
    return loaded === undefined || loaded === now ? [] : [{ package: pkg, loaded, now }];
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

// What a startup module exports as its default.
interface StartupModule {
  initialize(args: { core: CoreApi; signal: AbortSignal }): unknown;
}

function startupOf(exports: unknown, module: string, pkg: string): StartupModule {
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

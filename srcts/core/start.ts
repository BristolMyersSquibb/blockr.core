// Starting core on a page, in the order of the startup modules, as Starting
// the page has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#starting-the-page):
// 1. The protocol's client sends each session's hello as R starts the session.
// 2. Once R has answered the first, core asks R for the blockr options that its
//    scheduler reads, and creates the store, the command registry with core's
//    board options, the scheduler and CoreApi.
// 3. It initializes the startup module of every package that R has loaded, one
//    at a time in R's order, awaiting each initialize(). It names one that
//    fails and carries on.
// 4. It opens the board that R sent, or in its place the one that a startup
//    module resolves with, unless the board is locked or two startup modules
//    resolve with one, where it keeps R's and names the packages.
// 5. As the store opens a board, core imports the module of the front-end of
//    the class that the board is built as and ends the front-end on the page.
//    Once the board is open, it calls the front-end's render(), and starts the
//    scheduler once render() has returned, so that the scheduler sends R
//    nothing before.
//
// A board opened later goes the same way, through the store's open(), which
// CoreApi gives: the store awaits what core runs as R has replied, before it
// migrates or drops anything, and the scheduler holds its evaluations until
// the front-end's render() has returned, as at the start
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#switching-front-ends).
// The reply to a later session's hello has the scheduler give every block a
// new revision, and makes core ask for the page to be reloaded where it names a
// loaded package at another version.

import { createCommands } from "../commands/index.ts";
import { API, createLoader, ModuleError } from "../mount/index.ts";
import type { Loader, LoaderOptions, Measure, Source } from "../mount/index.ts";
import { createClient, PROTOCOL_VERSION } from "../protocol/index.ts";
import type { Capability, HelloReply, Reply, Transport } from "../protocol/index.ts";
import { createScheduler } from "../scheduler/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import { createStore } from "../store/index.ts";
import type { Dropped, Option, Store } from "../store/index.ts";
import type { CoreApi } from "../types/core-api.ts";
import { isObject, messageOf, own, quoted, report } from "../util/index.ts";
import { createBlockrOptions } from "./blockr-options.ts";
import { createCore } from "./core.ts";

// The capabilities that core's client speaks.
const CAPABILITIES: Capability[] = ["cancel", "interrupt", "progress"];

// What draws the front-ends of the boards that open, which the page gives, and
// a CoreApi for tests may leave out.
export interface FrontEnds {
  // Imports the module of the front-end, by its id, from where R serves it,
  // which is undefined where R serves no such module, and ends the front-end
  // on the page, whether it is another or the same. Core calls it as the store
  // opens a board, once the startup modules that the opening brings have
  // initialized, with why no front-end draws the board where R names none for
  // its class.
  prepare(
    front: { readonly id: string; readonly source: Source | undefined } | { readonly error: Error },
    loader: Loader,
  ): Promise<void>;
  // Calls the render() of the front-end that prepare() imported, and resolves
  // once render() has returned or resolved.
  render(core: CoreApi): Promise<void>;
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
  // protocol.
  readonly fail?: (error: Error) => void;
}

export interface Started {
  // Resolves with CoreApi once the first board is open and its front-end has
  // rendered, or rejects with why core could not start.
  readonly ready: Promise<CoreApi>;
  // Ends what core started: it aborts the startup modules' signal, and core
  // hears no later session.
  stop(): void;
}

// What core holds once R has answered the first hello.
interface Running {
  readonly store: Store;
  readonly scheduler: Scheduler;
  readonly core: CoreApi;
}

export function start(options: StartOptions): Started {
  const page = new AbortController();
  const fail = options.fail ?? ((error: Error) => {
    report(error);
  });

  // The first reply to hello, whose package versions are those the page
  // loaded, and the latest, whose modules and renderers core uses.
  let first: HelloReply | undefined;
  let latest: HelloReply | undefined;
  let running: Running | undefined;
  const initialized = new Set<string>();

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
      } else {
        report(error);
      }
      return;
    }
    const hello = reply as HelloReply;
    latest = hello;
    if (first === undefined) {
      first = hello;
      boot(hello).then(
        (core) => settle?.resolve(core),
        (failure: unknown) => {
          settle?.reject(failure);
          fail(failure instanceof Error ? failure : new Error(messageOf(failure)));
        },
      );
      return;
    }
    // A new session holds no results.
    running?.scheduler.session();
    const changed = changedPackages(first, hello);
    if (changed.length > 0) (options.reload ?? askReload)(changed);
  }

  async function boot(reply: HelloReply): Promise<CoreApi> {
    // The blockr options that the scheduler reads, asked at once, in one
    // request, before the scheduler starts.
    const [evaluationQueue, backgroundEvaluationDelay, gateVisibility] = await Promise.all([
      blockrOptions.read("evaluation_queue", 2),
      blockrOptions.read("background_evaluation_delay", Infinity),
      blockrOptions.read("gate_visibility", true),
    ]);

    const store = createStore({ loaded });
    const commands = createCommands(store);
    for (const option of options.options({ blockrOption: (name, fallback) => blockrOptions.read(name, fallback) })) {
      store.options.register(option);
    }
    const scheduler = createScheduler({
      store,
      client,
      evaluationQueue,
      backgroundEvaluationDelay,
      gateVisibility,
      ...(reply.bound === undefined ? {} : { bound: reply.bound }),
    });
    const core = createCore({
      store: { ...store, open },
      commands,
      scheduler,
      loader,
      blockrOptions,
      lock: reply.lock,
      renderers: () => latest?.renderers ?? {},
      ...(options.measure === undefined ? {} : { measure: options.measure }),
    });
    running = { store, scheduler, core };

    const named = await initialize(reply, true);
    const instead = chosen(reply, named);
    if (instead === undefined) {
      await open(reply);
    } else {
      try {
        await open(instead);
      } catch (error) {
        report(error);
        await open(reply);
      }
    }
    return core;
  }

  // Initializes the startup module of each package that the reply names and
  // that core has not initialized yet, one at a time in R's order, and
  // resolves with the boards that they name, where they may name one.
  async function initialize(reply: HelloReply, naming: boolean): Promise<{ package: string; text: string }[]> {
    const core = running?.core;
    if (core === undefined) return [];
    const named: { package: string; text: string }[] = [];
    for (const entry of reply.startup_modules) {
      if (initialized.has(entry.package)) continue;
      initialized.add(entry.package);
      try {
        const source = entry.css === undefined ? { module: entry.module } : { module: entry.module, css: entry.css };
        const mod = startupOf(await loader.load(source, entry.package), entry.module, entry.package);
        let result: unknown;
        try {
          result = await mod.initialize({ core, signal: page.signal });
        } catch (error) {
          throw new ModuleError(entry.module, entry.package, `failed in initialize(): ${messageOf(error)}`, { cause: error });
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

  // The reply with the board that a startup module named in place of the
  // board that R sent, or undefined where core opens R's.
  function chosen(reply: HelloReply, named: readonly { package: string; text: string }[]): HelloReply | undefined {
    const [only, ...more] = named;
    if (only === undefined) return undefined;
    const packages = quoted(named.map((entry) => entry.package));
    if (more.length > 0) {
      report(new Error(`The startup modules of ${packages} each named a board to open, so core opens the board that R sent.`));
      return undefined;
    }
    if (reply.lock !== null) {
      report(new Error(`The board is locked, so core opens the board that R sent rather than the one that ${only.package} named.`));
      return undefined;
    }
    let board: unknown;
    try {
      board = JSON.parse(only.text);
    } catch (error) {
      report(new Error(`The startup module of ${only.package} named a board that holds no JSON, so core opens the board that R sent: ${messageOf(error)}`));
      return undefined;
    }
    return { ...reply, board: board as HelloReply["board"] };
  }

  // What the store runs once R has replied to an opening, before it migrates
  // or drops anything: the startup modules that the opening brings, which define
  // their board classes, and then the import of the front-end of the class
  // that the store builds the board as, which ends the one on the page.
  async function loaded(reply: HelloReply, classes: readonly string[]): Promise<void> {
    const current = running;
    if (current === undefined) return;
    current.scheduler.hold();
    await initialize(reply, false);
    await prepare(current, classes);
  }

  // Imports the front-end of the class that a board of these classes is built
  // as, which ends the one on the page.
  async function prepare(current: Running, classes: readonly string[]): Promise<void> {
    if (options.frontEnds === undefined) return;
    prepared++;
    let id: string;
    try {
      id = current.store.frontEnd(classes);
    } catch (error) {
      await options.frontEnds.prepare({ error: error instanceof Error ? error : new Error(messageOf(error)) }, loader);
      return;
    }
    await options.frontEnds.prepare({ id, source: own(latest?.modules ?? {}, id) }, loader);
  }

  // Openings run one at a time, each through the store, and then draw the
  // board that the store holds with the front-end that the opening imported.
  // Where the opening failed once it had ended the front-end on the page, core
  // draws the board that the store still holds with that board's front-end.
  let queue: Promise<unknown> = Promise.resolve();
  // How many times core has imported a front-end, which tells an opening that
  // failed whether it ended the front-end on the page.
  let prepared = 0;

  function open(given: string | HelloReply): Promise<Dropped[]> {
    const run = queue.then(() => opening(given));
    queue = run.catch(() => undefined);
    return run;
  }

  async function opening(given: string | HelloReply): Promise<Dropped[]> {
    const current = running;
    if (current === undefined) throw new Error("Core opens a board only once R has answered its hello.");
    const before = prepared;
    try {
      const dropped = await current.store.open(given);
      await draw(current);
      return dropped;
    } catch (error) {
      if (prepared !== before && current.store.hasBoard()) {
        await prepare(current, current.store.board().class);
        await draw(current);
      } else {
        current.scheduler.start();
      }
      throw error;
    }
  }

  async function draw(current: Running): Promise<void> {
    try {
      await options.frontEnds?.render(current.core);
    } finally {
      current.scheduler.start();
    }
  }

  return {
    ready,
    stop() {
      page.abort();
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

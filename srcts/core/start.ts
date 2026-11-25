// Starting core on a page, as Starting the page has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#starting-the-page):
// 1. The protocol's client sends each session's hello as R starts the session.
// 2. Once R has answered the first, core asks R for the blockr options that its
//    scheduler reads, for rand_id, the shape of the ids that commands mint,
//    for those that set the page's logger up (../log/settings.ts), for
//    board_actions, which picks the board action that plays a role, and for
//    board_action_menu, which names the module that draws the menus of board
//    actions (../actions/module.ts). It sets the logger up, and creates for
//    the board that it opens the store, the notifications of the board
//    (./notifications.ts) and the command registry.
// 3. It imports the module of the class of each of the app's plugins and of
//    each board option that R's reply names, each once per page, and
//    registers the default export of each under the id that the reply gives
//    it, with the commands, the board actions and the roles of each plugin
//    class. Core opens no board where a module fails to load, where the store
//    refuses the class or the board option that it defines, where its class
//    lists a board action or a role that breaks a rule, or where the command
//    registry refuses a command of its class. The page then shows why in
//    place of the board, naming each such module and its package. Saving a
//    board that opened without such a module would lose what the board holds
//    for the module's plugin or board option.
//    Meanwhile it loads the module that board_action_menu names. Where that
//    module cannot draw the menus, as where no package offers it, core opens
//    no board and shows why in its place, since the blockr option is the
//    app's rather than a board's.
// 4. Once every module is registered, it chooses the board action that plays
//    each role: the one that board_actions picks for the role, or else the
//    role's default (../actions/actions.ts). Where a pick of board_actions
//    fails the check of a player, or names a board action or a role that no
//    package registers, core opens no board, and the page shows why in its
//    place, since board_actions holds for every board of the app.
// 5. It creates the scheduler and CoreApi.
// 6. Unless the load lock is on, it calls the load() of the storage of the
//    app's storage plugin, where it has one, which may resolve with the text of
//    a board to open in place of the one that R sent, such as the board that
//    the page's URL names. Where it rejects, or resolves with no JSON board,
//    or that board fails to open, core raises a notification of why and opens
//    R's.
// 7. It imports the UI module of each block class of which that board holds a
//    block below the class's version, for the class's migration
//    (./migrations.ts).
// 8. It opens the board that R sent, or the one that load() named in its
//    place, and either runs the app's plugins, which R names in its reply, each
//    with the state that the board holds for it. Where the store dropped
//    anything as the board opened, core raises a notification that names it.
// 9. As the store opens the board, core adds to it the board options that the
//    classes of the app's plugins require (./plugins.ts).
// 10. Once the board options that the app's frontend requires are on the
//    board, core calls the render() of the frontend, the plugin that runs whose
//    class has `frontend`, such as core's basic frontend, with what R's reply
//    holds in `frontend`, and starts the scheduler once render() has returned,
//    so that the scheduler sends R nothing before. Where the app runs no
//    frontend, the page shows why in place of the board.
// 11. Core then calls the start of each plugin whose class has one, so that
//    what the frontend first shows comes before what the plugins ask for as
//    they start.
//
// A block that serve() serves alone comes as a board of one block, which no
// frontend draws, as Data passed to serve() has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#data-passed-to-serve).
// Core then starts in the same order, with four differences:
// - In step 4, it takes no picks of board_actions, so that core's own board
//   actions play core's roles, since no menu of board actions shows and R
//   names no plugin whose board action a pick could name.
// - In step 6, it calls no load(), since R names no plugin for a block that
//   serve() serves alone, and opens the board that R sent.
// - In step 9, it adds no board options for plugins. The board still takes
//   dark_mode, as every board does, since its onChange() switches the page's
//   theme (./core.ts).
// - In step 10, it draws no frontend, but the block in its card, which demands
//   the block, and starts the scheduler once the card has mounted the block.
//
// A board opened later, through the store's open() that CoreApi gives, starts
// core again in place, as Opening a board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board):
// 1. The store on the page checks the board as it would open it, once core
//    has imported the UI modules of the block classes that it migrates. A
//    board that it refuses leaves the page as it was: core raises a
//    notification of why, or hands it to the notifier that the caller of
//    open() passes, and open() rejects with the reason.
// 2. One abort ends everything that core runs for the board on the page: the
//    frontend and what it mounted, every plugin that runs, the store with its
//    undo history, the command registry, the board actions and the plugin
//    classes, the board options, the scheduler with its demands, which block
//    has focus, and what core keeps of outputs and answers.
// 3. Core sends another hello, which R takes as a fresh start of the session,
//    so that R drops every result and request of the board before.
// 4. Once R has answered it, core starts as the page does, from step 2 on,
//    with the board given in place of R's, which no load() replaces. Where that
//    board fails to open after all, core opens the board before in the new
//    store, raises a notification of why, and open() rejects with the reason.
//    It raises that notification whatever notifier the caller passed, since
//    the opening has ended the code that called open().
// Only what is fixed for the page outlives an opening: the code and
// stylesheets that have loaded, the page's element, the transport with its
// session, the protocol's client, the blockr options that core has read, the
// locks, the migrations that core imported, and the revisions given, so that
// the next scheduler gives a block id a revision above any it had.
//
// Before the store's check() or read() that CoreApi gives, core imports the
// UI modules of the block classes whose older states the text holds, as it
// does before an opening.
// So both migrate those states as opening the text would.
// Where read() leaves anything out of the board it resolves with, core raises
// a notification that names it, as it does as a board opens. It raises it
// anew for each read, so that a frontend that has shown it shows it again.
// A caller that shows it itself passes a notifier, which takes it instead.
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
//
// Core records a span of the page's start and of each later opening, as
// Tracing has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing),
// `start` and `open`, with a span inside for each step: `hello`, from sending
// the hello to R's reply, `modules`, importing the modules of the plugin
// classes and board options that the reply names and the module that draws
// the menus of board actions, `storage.load`, the load() of the storage
// plugin, `store.open`, each call of the store's open(), and `render`, the
// frontend's render(). It notes when each step starts and ends,
// and records the spans once the opening is done, and once the page's tracing
// is set up, which it sets up with the page's logger as the first board opens
// (../trace/settings.ts). The span of a later opening goes under the span that
// was active as the store's open() was called.

import { createBoardActions, MENU_OPTION, menuModule, showDialog } from "../actions/index.ts";
import type { BoardActions, Place, ShowDialog, ShowMenu } from "../actions/index.ts";
import { createCommands } from "../commands/index.ts";
import type { Commands } from "../commands/index.ts";
import { createLoader, ModuleError, moduleExports } from "../mount/index.ts";
import type { LoaderOptions, Measure, Source } from "../mount/index.ts";
import { createClient, PROTOCOL_VERSION, sameMajor } from "../protocol/index.ts";
import type { Capability, HelloReply, Reply, Transport } from "../protocol/index.ts";
import { counter, createScheduler } from "../scheduler/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import { createStore, StoreError } from "../store/index.ts";
import type { Dropped, Option, PluginClass, Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi, FrontendModule, Notification, Notifier } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { fallBack, log, logSettings, serviceName, setUpLogging } from "../log/index.ts";
import type { OtelModule } from "../log/index.ts";
import { setUpTracing, steps, tracesUrl } from "../trace/index.ts";
import type { Steps } from "../trace/index.ts";
import { asError, coreNotification, errorMessage, givenNotifier, isObject, own, packageFromId, sourceModule } from "../util/index.ts";
import type { Raise } from "../util/index.ts";
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
  // The page's element, in which they draw, by which core tells that an
  // output's element sits on the page (./core.ts).
  readonly el?: HTMLElement;
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
  // Calls the show() of the module of the frontend that render() drew for the
  // board of the signal, where it has one, with the board object, the element
  // and CoreApi, and resolves once that has, or rejects with why it failed. It
  // does nothing where that frontend is not drawn, as once the next board has
  // opened.
  show(object: Parameters<CoreApi["show"]>[0], signal: AbortSignal): Promise<void>;
  // The `advanced` member of the module of the frontend that render() drew
  // for the board of the signal, which shows an advanced action in the
  // frontend's place, called as a method of the module. It is undefined where
  // that frontend is not drawn or gives none, so that core shows the advanced
  // action in its dialog.
  advanced?(signal: AbortSignal): Place | undefined;
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
  // Draws each menu of board actions that CoreApi opens, in place of the module
  // that the blockr option board_action_menu names, as the CoreApi for tests
  // records each menu without drawing it. Where it is given, core loads no
  // module for the menus.
  readonly showMenu?: ShowMenu;
  // Draws core's dialog, which shows each advanced action that neither the
  // caller nor the frontend shows, core's own unless given, as the CoreApi for
  // tests records each without drawing it.
  readonly showDialog?: ShowDialog;
  // Shows why core cannot start, or why it stopped, as where R speaks another
  // major of the protocol in the first session or a later one, or cannot give
  // a blockr option that core reads as it starts, or gives one that the
  // scheduler, the command registry or the page's logger does not take, or
  // one in board_actions that picks a board action that cannot play its role,
  // or where the module that board_action_menu names cannot draw the menus of
  // board actions, or where the module of a plugin class or a board option
  // fails, as step 3 above has it, or why no board is open once another
  // failed to open. Core logs why where it is not given.
  readonly fail?: (error: Error) => void;
  // Imports core's OpenTelemetry module, as the page does, where the blockr
  // options have the page send its log records or its spans to a collector
  // (../log/settings.ts, ../trace/settings.ts). Where it is not given, core
  // writes to the console, records no spans, and logs that it has no such
  // module.
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

// The module that R serves for a plugin class or a board option, by the id of
// the class or the board option, with where R serves it and its package, and
// with its default export, or why it fails to load or exports none.
type Listed = { readonly id: string; readonly source: Source; readonly pkg: string | null } & (
  | { readonly def: Readonly<Record<string, unknown>> }
  | { readonly failure: Error }
);

export function start(options: StartOptions): Started {
  const page = new AbortController();
  const alone = options.alone === true;
  // The steps of the page's start, whose span runs from now on.
  const starting = steps("start");
  // When the protocol's client last sent a hello, and when R last answered
  // one, which the steps of an opening note as its hello.
  let helloSent: number | undefined;
  let helloAnswered: number | undefined;
  // Settles once the page's tracing is set up, or once core knows that the
  // page records no spans, which core sets up with the page's logger.
  let tracing: Promise<void> = Promise.resolve();
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
    hello: () => {
      helloSent = performance.now();
      return { protocol: PROTOCOL_VERSION, capabilities: CAPABILITIES };
    },
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
    helloAnswered = performance.now();
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
      noteHello(starting);
      boot(hello, undefined, starting).then(
        ({ core }) => {
          starting.record(tracing);
          settle?.resolve(core);
        },
        (failure: unknown) => {
          starting.record(tracing, failure);
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

  // Notes the hello that R answered last as a step of the opening, from when
  // the client sent it.
  function noteHello(opening: Steps): void {
    const end = helloAnswered ?? performance.now();
    opening.noted("hello", helloSent ?? end, end);
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
  // board, the command registry, the board actions, the scheduler and CoreApi,
  // each of which the abort of the opening's controller ends. It registers the
  // plugin classes and board options that the reply names, opens the board in
  // the store, or as the page starts the one that the app's storage plugin
  // names in its place, raises the notification that names what the store
  // dropped, draws the board with its frontend, starts the scheduler, then
  // starts the app's plugins, and resolves with CoreApi and what the store
  // dropped. It notes its steps in `opening`.
  async function boot(
    reply: HelloReply,
    later: Later | undefined,
    opening: Steps,
  ): Promise<{ core: CoreApi; dropped: Dropped[] }> {
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
    // ids that commands mint, and those that set the page's logger and its
    // tracing up, asked at once, in one request, before the scheduler starts
    // and the commands run, and kept for the page.
    // Each goes as R sent it to the scheduler or the command registry, which
    // converts and checks it, and takes its fallback where nothing sets it.
    // CoreApi gets rand_id too. It converts the value as the command registry
    // does, so that its mintId() mints ids in the same shape.
    // As the first board opens, core sets the page's logger and its tracing up
    // with the other four (../log/settings.ts, ../trace/settings.ts), and the
    // logger writes the lines that core held.
    // The same request asks for board_actions, which picks the board action
    // that plays a role, and for board_action_menu, which names the module
    // that draws the menus of board actions.
    const [
      evaluationQueue,
      backgroundEvaluationDelay,
      gateVisibility,
      randId,
      logLevel,
      logger,
      otelClientEndpoint,
      otelClientServiceName,
      picks,
      boardActionMenu,
    ] = await Promise.all([
      blockrOptions.read("evaluation_queue"),
      blockrOptions.read("background_evaluation_delay"),
      blockrOptions.read("gate_visibility"),
      blockrOptions.read("rand_id"),
      blockrOptions.read("log_level"),
      blockrOptions.read("logger"),
      blockrOptions.read("otel_client_endpoint"),
      blockrOptions.read("otel_client_service_name"),
      blockrOptions.read("board_actions"),
      blockrOptions.read(MENU_OPTION),
    ]);
    if (!logging) {
      const base = typeof document === "undefined" ? undefined : document.baseURI;
      const settings = logSettings({ logLevel, logger, otelClientEndpoint }, base);
      const service = serviceName(otelClientServiceName);
      const sessionId = (): string | undefined => options.transport.sessionId?.();
      logging = true;
      setUpLogging(settings, service, sessionId, options.otel ?? noOtel);
      tracing = setUpTracing(tracesUrl(otelClientEndpoint, base), service, sessionId, options.otel ?? noOtel);
    }

    // The module that draws the menus loads while core imports the modules that
    // the reply names, once per page, as the loader keeps what it imported.
    // Its failure fails the opening once those have registered.
    const drawing = options.showMenu === undefined ? menuModule(loader, boardActionMenu) : Promise.resolve(options.showMenu);
    drawing.catch(() => undefined);

    const store: Store = createStore({ hello: reply, signal, migration: (id) => migrations.get(id) });
    // The notifications of the board.
    const notifications = createNotifications(store, { signal, plugins: reply.plugins });
    const { raise } = notifications;
    askReload(raise);
    const commands = createCommands(store, {
      signal,
      lock: reply.lock,
      randId,
      blockrOption: (name) => blockrOptions.read(name),
    });
    const boardActions = createBoardActions({
      raise,
      preflight: (id, args) => commands.preflight(id, args),
      blockRegistry: () => store.blockRegistry(),
    });
    const showMenu = await opening.step("modules", async () => {
      await register(reply, store, commands, boardActions, signal);
      return drawing;
    });
    // Once every module is registered, the board action that plays each role.
    // A wrong pick of board_actions opens no board, as a module that cannot
    // draw the menus does.
    boardActions.choosePlayers(alone ? undefined : picks);
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
        open: (given, opts) => reopen(given, controller, opts),
        check: (text) => afterMigrations(text, store, () => store.check(text)),
        read: (text, opts) => readBoard(text, store, notifications, opts),
      },
      commands,
      boardActions,
      showMenu,
      showDialog: options.showDialog ?? showDialog,
      advanced: () => options.frontEnds?.advanced?.(signal),
      scheduler,
      loader,
      blockrOptions,
      randId,
      lock: (first ?? reply).lock,
      renderers: () => latest?.renderers ?? {},
      plugins: reply.plugins,
      options: reply.options,
      notifications,
      signal,
      show: (object) => options.frontEnds?.show(object, signal) ?? Promise.resolve(),
      ...(options.measure === undefined ? {} : { measure: options.measure }),
      ...(options.frontEnds?.el === undefined ? {} : { page: options.frontEnds.el }),
    });
    running = { store, scheduler, core, notifications, controller };

    let dropped: Dropped[] = [];
    let failure: Error | undefined;
    if (later === undefined) {
      const instead = alone ? undefined : await named(reply, store, core, signal, raise, opening);
      await migrations.prepare(instead?.text ?? reply.board, reply.block_registry);
      if (instead === undefined) {
        dropped = await opening.step("store.open", () => store.open(reply));
      } else {
        try {
          dropped = await opening.step("store.open", () => store.open(instead.text));
        } catch (error) {
          log.error(asError(error));
          raise(
            coreNotification(
              "board_open_failed",
              "error",
              `The board that the storage plugin "${instead.plugin}" named did not open, so core opens the board that R sent: ${errorMessage(error)}`,
              { module: sourceModule({ plugin: instead.plugin }, packageFromId(instead.plugin)) },
            ),
          );
          await migrations.prepare(reply.board, reply.block_registry);
          dropped = await opening.step("store.open", () => store.open(reply));
        }
      }
    } else {
      try {
        await migrations.prepare(later.text, reply.block_registry);
        dropped = await opening.step("store.open", () => store.open(later.text));
      } catch (error) {
        failure = asError(error);
        try {
          await opening.step("store.open", () => store.open(later.before));
        } catch (again) {
          log.error(asError(again));
          throw failure;
        }
        // Core tells the user why, and logs why at debug alone, as it does
        // where the store on the page refuses the board (reopening(), below),
        // and open() rejects.
        const why = `The board did not open, so core opened the board before it again: ${failure.message}`;
        log.debug(why);
        raise(coreNotification("board_open_failed", "error", why));
      }
    }
    if (dropped.length > 0) raise(droppedNotification(dropped, OPENED_WITHOUT));

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
        front === undefined ? { none: NO_FRONTEND } : { frontend: front.frontend, plugin: front.plugin, args: reply.frontend },
        signal,
      );
      const failures = await (front?.added ?? []);
      try {
        await opening.step("render", async () => options.frontEnds?.render(core, signal, failures));
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
  // which the loader imports once per page, with the board actions and the
  // roles of each plugin class, and its commands in the command registry.
  // Core refuses what a module defines where the store refuses the class or
  // the board option, where one of the class's board actions or roles breaks
  // a rule (../actions/actions.ts), or where the command registry refuses one
  // of the class's commands. Once it has tried every module, it throws where
  // a module failed to load or core refused what it defines, naming each such
  // module with its package, so that core opens no board.
  async function register(
    reply: HelloReply,
    store: Store,
    commands: Commands,
    boardActions: BoardActions,
    signal: AbortSignal,
  ): Promise<void> {
    const [classes, options] = await Promise.all([
      Promise.all(Object.entries(reply.plugins).map(([id, entry]) => listed(id, entry, packageFromId(id)))),
      Promise.all(Object.entries(reply.options).map(([id, entry]) => listed(id, entry, entry.package))),
    ]);
    if (signal.aborted) return;
    // Why each module failed, one for each module.
    const failures: Error[] = [];
    // Why core refuses what the module defines, naming the module and its
    // package.
    const refuse = ({ source, pkg }: Listed, what: string, why: string): void => {
      failures.push(new ModuleError(source.module, pkg, `defines ${what} that core refuses: ${why}`));
    };
    for (const module of options) {
      if ("failure" in module) {
        failures.push(module.failure);
        continue;
      }
      try {
        store.options.register({ ...(module.def as unknown as Option), id: module.id });
      } catch (error) {
        refuse(module, "a board option", errorMessage(error));
      }
    }
    for (const module of classes) {
      if ("failure" in module) {
        failures.push(module.failure);
        continue;
      }
      const { id } = module;
      let cls: PluginClass;
      try {
        cls = { ...(module.def as unknown as PluginClass), id };
        store.plugins.register(cls);
        boardActions.register(id, cls.boardActions, cls.roles);
      } catch (error) {
        refuse(module, "a plugin class", errorMessage(error));
        continue;
      }
      // Why the command registry refuses each command that it refuses, which
      // it checks one by one.
      const refused = (cls.commands ?? []).flatMap((command) => {
        try {
          commands.register(command);
          return [];
        } catch (error) {
          return [errorMessage(error)];
        }
      });
      if (refused.length > 0) refuse(module, "a plugin class", refused.join(" "));
    }
    if (failures.length > 0) throw new Error([...failures.map(({ message }) => message), noBoard(failures.length)].join("\n"));
  }

  // The module that R serves for the plugin class or the board option of the
  // id given, with its default export, or why it fails to load or exports
  // none, which names the module and its package.
  async function listed(id: string, source: Source, pkg: string | null): Promise<Listed> {
    try {
      const at = source.css === undefined ? { module: source.module } : { module: source.module, css: source.css };
      return { id, source, pkg, def: listedDefault(await loader.load(at, pkg), source.module, pkg) };
    } catch (error) {
      return { id, source, pkg, failure: asError(error) };
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
    opening: Steps,
  ): Promise<{ readonly text: string; readonly plugin: PluginClassId } | undefined> {
    if (reply.lock.load) return undefined;
    const classes = store.plugins.registered();
    const plugin = Object.keys(reply.plugins).find((id) => own(classes, id)?.storage !== undefined);
    const storage = plugin === undefined ? undefined : own(classes, plugin)?.storage;
    if (plugin === undefined || storage?.load === undefined) return undefined;
    const refuse = (error: Error): void => {
      log.error(error);
      raise(coreNotification("board_open_failed", "error", error.message, { module: sourceModule({ plugin }, packageFromId(plugin)) }));
    };
    let text: unknown;
    try {
      text = await opening.step("storage.load", async () => storage.load?.({ core, plugin, signal }));
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

  // What the store's check() or read() resolves with, once core has imported
  // the UI modules of the block classes whose older states the text holds, as
  // it does before an opening.
  async function afterMigrations<T>(text: string, store: Store, run: () => Promise<T>): Promise<T> {
    await migrations.prepare(text, store.blockRegistry());
    return run();
  }

  // What the store's read() resolves with, once core has imported the
  // migrations that the text needs. Where the read left anything out of the
  // board, core tells the user what, anew for each read, or hands that
  // notification to the notifier in `opts`, before read() resolves.
  // A notifier that is no function is refused at once, with a TypeError,
  // before anything is read.
  async function readBoard(text: string, store: Store, notifications: Holder, opts: Parameters<Store["read"]>[1]): ReturnType<Store["read"]> {
    const notifier = givenNotifier(opts, "read()");
    const read = await afterMigrations(text, store, () => store.read(text));
    if (read.dropped.length > 0) notifications.raiseAgain(droppedNotification(read.dropped, READ_WITHOUT), notifier);
    return read;
  }

  // Openings run one at a time.
  let queue: Promise<unknown> = Promise.resolve();

  // Opens a board in place of the one that the store of the opening holds,
  // whose controller is given, by starting core again for it. Where the store
  // on the page refuses the board, the notifier in `opts` takes why, in place
  // of core raising it.
  function reopen(given: string | HelloReply, of: AbortController, opts: Parameters<Store["open"]>[1]): Promise<Dropped[]> {
    let notifier: Notifier | undefined;
    try {
      notifier = givenNotifier(opts, "open()");
    } catch (error) {
      return Promise.reject(asError(error));
    }
    // The span of the opening runs from now, under the span active now.
    const opening = steps("open");
    const run = queue.then(() => reopening(given, of, opening, notifier));
    queue = run.catch(() => undefined);
    return run;
  }

  async function reopening(given: string | HelloReply, of: AbortController, opening: Steps, notifier: Notifier | undefined): Promise<Dropped[]> {
    const refused = "The store refused to open the board";
    const current = running;
    if (current?.controller !== of || of.signal.aborted) {
      const error = new StoreError(refused, ["Another board opened since, so this store opens no board."]);
      opening.record(tracing, error);
      throw error;
    }
    if (!current.store.hasBoard()) {
      const error = new StoreError(refused, ["The store holds no board yet, so it opens none in place of it."]);
      opening.record(tracing, error);
      throw error;
    }
    const text = typeof given === "string" ? given : JSON.stringify(given.board);

    let before: string;
    try {
      try {
        await afterMigrations(text, current.store, () => current.store.check(text));
      } catch (error) {
        // The board on the page stays open. Core tells the user why the store
        // refused the board, anew for each refusal, so that no caller of
        // open() needs to, or hands that to the caller's notifier, and logs
        // why at debug alone, as it logs a refused edit (./core.ts). Then
        // open() rejects with the reason.
        const why = errorMessage(error);
        log.debug(why);
        current.notifications.raiseAgain(coreNotification("board_open_failed", "error", why), notifier);
        throw error;
      }
      before = current.store.save();
    } catch (error) {
      opening.record(tracing, error);
      throw error;
    }

    of.abort();
    running = undefined;
    try {
      const reply = await afresh();
      noteHello(opening);
      const { dropped } = await boot(reply, { text, before }, opening);
      opening.record(tracing);
      return dropped;
    } catch (error) {
      opening.record(tracing, error);
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

// Why the app runs no frontend, which the page shows in place of the board.
const NO_FRONTEND = "The app runs no frontend that draws the board, since none of the app's plugins is a frontend.";

// What the page shows in place of the board after why each module failed,
// given how many modules failed.
function noBoard(failed: number): string {
  return failed === 1
    ? "Core opens no board without this module, since saving the board would lose what it holds for the module's plugin or board option."
    : "Core opens no board without these modules, since saving the board would lose what it holds for their plugins and board options.";
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

// The first lines of the notification that names what the store dropped: as
// a board opened, and as its read() read a board, which another board opens
// none of.
const OPENED_WITHOUT = "The board opened without:";
const READ_WITHOUT = "Core read a board and left out:";

// The notification that names what the store dropped, under the first line
// given, one line for each, with the package of each that names one, as Board
// objects has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#objects-that-cannot-be-rebuilt).
// One notification covers the whole opening, or the whole read, which
// concerns the board.
function droppedNotification(dropped: readonly Dropped[], first: string): Omit<Notification, "id"> {
  const lines = dropped.map((item) => {
    if ("option" in item) return `- board option "${item.option}"`;
    if ("block" in item) return `- block "${item.block}" (${item.registry_id})`;
    if ("plugin" in item) return `- plugin "${item.plugin}"`;
    return `- package ${item.package} ${item.version}`;
  });
  return coreNotification("board_objects_dropped", "warning", [first, ...lines].join("\n"));
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
  return Promise.reject(new Error("Core was given no OpenTelemetry module to send log records or spans with."));
}

// A CoreApi for tests, whose store, command registry and scheduler are core's
// own and whose replies from R come from the fixtures of the wire protocol,
// those that both test suites round-trip, so that a frontend or a plugin runs
// in a test runner on a board, needing neither R nor a browser
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).
//
// Core starts as it does on the page: it imports the module of the class of
// each of the app's plugins and of each board option that R's reply names,
// those that the test gives and core's own, registers them, opens the board of
// R's reply to hello, draws it with the app's frontend where the test gives an
// element to draw in, and runs the app's plugins, which the reply names, their
// start, update and migrate included. The frontend is the app's plugin whose
// class has `frontend`, core's basic frontend for the reply of the fixture
// hello. A board that the test opens later through the store's open() starts
// core again, as on the page.
// Where one of those modules fails to load, or core refuses what it defines,
// core opens no board, as on the page, and stubCore() rejects with why, so
// that a package's test fails at once where one of its modules breaks a rule.
// R, played from the fixtures, replies to the client's hello with the
// reply of the fixture hello unless given, and drops what it holds at a later
// hello, as R does. It evaluates every block as ok, makes a block's output
// from the sample outputs of its class's renderer, and answers a request with
// what the test gives.
// Core runs each menu of board actions as on the page, and draws none: the
// test opens the menu of a target, or takes the menu that is open, and picks
// its rows by their labels (./menu.ts). Core shows each advanced action in its
// dialog, as on the page, where neither the caller nor the frontend shows it,
// and draws no modal: the test takes the advanced action that the dialog
// shows, and reads the element in which core renders it (./advanced.ts).

import type { Target } from "../actions/index.ts";
import { pageFrontEnds, start } from "../core/index.ts";
import { replaceLogger } from "../log/index.ts";
// The stubs serve the modules of core's own plugin classes, which the build
// bundles on their own, as the page imports them.
// eslint-disable-next-line no-restricted-imports
import basicFrontend from "../modules/basic/index.tsx";
// eslint-disable-next-line no-restricted-imports
import editBoard from "../modules/edit-board/index.tsx";
// eslint-disable-next-line no-restricted-imports
import fileStorage from "../modules/file-storage/index.ts";
import type { Measure } from "../mount/index.ts";
import { coreOptions } from "../options/index.ts";
import type {
  ClientMessage,
  Condition,
  EvaluateReply,
  EvaluateRequest,
  Fixture,
  HelloReply,
  OutputInfo,
  OutputReply,
  OutputRequest,
  Reply,
  Request,
  Transport,
  TransportListener,
} from "../protocol/index.ts";
import { appPlugins, blockRegistry } from "../protocol/index.ts";
import type { BlockId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { LogLine } from "../types/log.ts";
import type { BlockOutput, OutputFixtures } from "../types/outputs.ts";
import { errorMessage, interned, isObject, nestedValue } from "../util/index.ts";
import { stubDialogs } from "./advanced.ts";
import type { StubAdvanced } from "./advanced.ts";
import { stubMenus } from "./menu.ts";
import type { StubMenu } from "./menu.ts";
import { acting, actingNow } from "./util.ts";
// The stubs ship the fixtures that R and both test suites read from inst/.
// eslint-disable-next-line no-restricted-imports
import helloFixture from "../../inst/protocol/fixtures/hello.json" with { type: "json" };
// eslint-disable-next-line no-restricted-imports
import tableFixtures from "../../inst/outputs/fixtures/table-output.json" with { type: "json" };
// eslint-disable-next-line no-restricted-imports
import textFixtures from "../../inst/outputs/fixtures/text-output.json" with { type: "json" };
// eslint-disable-next-line no-restricted-imports
import plotFixtures from "../../inst/outputs/fixtures/plot-output.json" with { type: "json" };

// R's reply to hello in the fixture hello: the board of a core app, with the
// block registry of core's block classes.
export const HELLO = (helloFixture as unknown as Fixture).r[0]?.[0] as HelloReply;

// The sample outputs of core's output classes, by the package and the id of the
// renderer that core gives the class.
const SAMPLES: Readonly<Record<string, Readonly<Record<string, OutputFixtures>>>> = {
  "blockr.core": {
    table_output: tableFixtures as unknown as OutputFixtures,
    text_output: textFixtures,
    plot_output: plotFixtures,
  },
};

// The modules of core's own plugin classes, by the package and the id of each
// class, which the stub serves where R's reply names the class and the test
// gives no module of its own, as the page imports them.
const CORE_CLASSES: Readonly<Record<string, Readonly<Record<string, unknown>>>> = {
  "blockr.core": {
    basic_frontend_plugin: { api: 1, default: basicFrontend },
    file_storage_plugin: { api: 1, default: fileStorage },
    edit_board_plugin: { api: 1, default: editBoard },
  },
};

// What an element measures for an output's info: a panel of 640 by 400 CSS
// pixels on a display of pixel ratio 1, in a light theme.
export const INFO: OutputInfo = Object.freeze({
  width: 640,
  height: 400,
  pixel_ratio: 1,
  bg: "#ffffff",
  fg: "#212529",
  accent: "#0d6efd",
  font: "sans-serif",
});

// What R answers to a request, given the request and the file it sends, if
// any: a value or a file, or undefined for no answer, which R replies to with
// an error. A function that throws has R reply error with what it threw.
export type Answer = (
  request: Request,
  body: Blob | undefined,
) => Json | Blob | undefined | Promise<Json | Blob | undefined>;

export interface StubCoreOptions {
  // R's reply to hello, that of the fixture hello unless given.
  readonly hello?: HelloReply;
  // The values that R holds of blockr options, by name. R holds none unless
  // given, so that the code that reads them takes its fallbacks.
  readonly blockrOptions?: Readonly<Record<string, Json>>;
  // What each module that core imports exports, by the URL that R serves it
  // at, or by the id of a board option, for its module.
  // The stub gives the modules of core's own plugin classes and board options,
  // as the page imports them, unless the test gives its own. A module that the
  // test leaves out fails to load, so that stubCore() rejects with why core
  // opens no board.
  readonly modules?: Readonly<Record<string, unknown>>;
  // What each module exports that R's reply names by a package and an id, by
  // the package and then by the id: the module of the class of one of the
  // app's plugins, and a module that packages offer each other's UIs, as
  // { "blockr.test": { notes_plugin: { api: 1, default: … }, menu: … } }.
  // R serves a module that the test gives here and that R's reply does not
  // name as one that packages offer each other, which it then names so.
  readonly offered?: Readonly<Record<string, Readonly<Record<string, unknown>>>>;
  // What R replies to an evaluation, beside its id, block and revision, ok
  // with no conditions unless given.
  readonly evaluate?: (request: EvaluateRequest) => Partial<Omit<EvaluateReply, "id" | "block" | "revision">> | undefined;
  // The output that R makes, and the messages and warnings that R's reply
  // carries with it, as the output's coercion raised them.
  // R makes the first sample output of the renderer that the block's class
  // names unless given, and its reply carries no conditions unless given.
  readonly output?: (
    request: OutputRequest,
  ) => { output: BlockOutput & Record<string, Json>; body?: Blob; conditions?: readonly Condition[] } | undefined;
  // What R answers to a request of a block class, an output class, a
  // renderer, a board option or a plugin.
  readonly answer?: Answer;
  // The messages and warnings that R's reply to such a request carries, as
  // the request's function raised them, and none unless given.
  // The reply carries them whether `answer` answers or throws.
  // In a reply of error, they come before what `answer` threw, as R records
  // each condition in the order it is raised.
  // A reply to a request that `answer` gives no answer to carries none,
  // since R then runs no function.
  readonly conditions?: (request: Request) => readonly Condition[] | undefined;
  // The element in which core draws the board with its frontend. Without
  // one, core draws no frontend and lets the scheduler send once the board is
  // open.
  readonly el?: HTMLElement;
  // Measures an output's element, as INFO for every element unless given.
  readonly measure?: Measure;
  // The kinds of request that the transport refuses to send, with an error, as
  // a closed transport does. It sends every kind unless given.
  readonly refuse?: readonly Request["kind"][];
}

export interface StubCore {
  // The CoreApi of the board that is open, which core makes anew for each
  // board that opens.
  readonly core: CoreApi;
  // Each message that core sent R, in order, each session's hello included.
  readonly sent: readonly Request[];
  // Resolves once R has replied to every request that core sent, and core has
  // sent nothing more for a few turns of the event loop, as the scheduler
  // sends what it plans with a timer. It needs real timers.
  // Where the test runs in React's act() environment, as blockr.ui's stubs set
  // it up, each turn runs inside act(), so that React draws what R's replies
  // change without warning of an update outside act().
  readonly settled: () => Promise<void>;
  // Each line that core and the modules on the page logged while the stub ran,
  // at every level, in the order they came. Each goes on to the logger that
  // the page would write it with too, the console unless a test put another
  // in its place.
  readonly logged: readonly LogLine[];
  // Opens the menu of the target, as core.boardActions.menu() does, at the top
  // left corner of the page's body, to which the keyboard's focus returns as
  // the menu closes, and returns the menu, which the test drives as a user
  // would, picking rows by their labels. It throws where on() gives the target
  // no board action, so that no menu opens. Without a target, it gives the
  // menu that is open, as trigger() or a frontend opened it, or undefined
  // where none is.
  menu(target: Target, opts?: MenuOptions): StubMenu;
  menu(): StubMenu | undefined;
  // The advanced action that core's dialog shows, as a menu, trigger() or a
  // frontend opened it, or undefined where the dialog shows none.
  advanced(): StubAdvanced | undefined;
  // Ends what core started, and what the stub logged with it.
  readonly stop: () => void;
}

// What the menu of a target takes beside where it opens: the caller's
// `extend` and its notifier.
type MenuOptions = Omit<Parameters<CoreApi["boardActions"]["menu"]>[1], "el" | "anchor">;

export async function stubCore(options: StubCoreOptions = {}): Promise<StubCore> {
  const base = options.hello ?? HELLO;
  const given = options.modules ?? {};
  const offered = options.offered ?? {};
  // The modules that the test gives by a package and an id that R's reply
  // names for no plugin and no module, which R then serves at a URL of their
  // own, as modules that packages offer each other's UIs.
  const plugins = appPlugins(base);
  const modules: Record<string, Record<string, { module: string; css?: string }>> = Object.fromEntries(
    Object.entries(base.modules).map(([pkg, byId]) => [pkg, { ...byId }]),
  );
  for (const [pkg, byId] of Object.entries(offered)) {
    for (const id of Object.keys(byId)) {
      const key = interned(pkg, id);
      if (plugins.has(key) || nestedValue(base.modules, key) !== undefined) continue;
      (modules[pkg] ??= {})[id] = { module: moduleUrl(pkg, id) };
    }
  }
  const hello: HelloReply = { ...base, modules };
  // What each URL that R's reply names serves: what the test gives by the URL
  // or by the id that the reply names it under, or else core's own module.
  const named = (url: string): unknown => {
    if (Object.hasOwn(given, url)) return given[url];
    const ids = [
      ...hello.plugins.filter((entry) => entry.module === url).map((entry) => interned(entry.package, entry.id)),
      ...Object.entries(hello.modules).flatMap(([pkg, byId]) =>
        Object.entries(byId)
          .filter(([, source]) => source.module === url)
          .map(([id]) => interned(pkg, id)),
      ),
    ];
    const options = Object.entries(hello.options)
      .filter(([, entry]) => entry.module === url)
      .map(([id]) => id);
    const id = ids.find((key) => nestedValue(offered, key) !== undefined);
    if (id !== undefined) return nestedValue(offered, id);
    const option = options.find((key) => Object.hasOwn(given, key));
    if (option !== undefined) return given[option];
    const cls = ids.find((key) => nestedValue(CORE_CLASSES, key) !== undefined);
    if (cls !== undefined) return nestedValue(CORE_CLASSES, cls);
    const core = options.find((key) => Object.hasOwn(coreOptions, key));
    return core === undefined ? undefined : { api: 1, default: coreOptions[core] };
  };

  const logged: LogLine[] = [];
  const restore = replaceLogger((line) => {
    logged.push(line);
  }, true);
  const r = fixtureR(hello, options);
  const menus = stubMenus();
  const dialogs = stubDialogs();
  const started = start({
    transport: r.transport,
    loader: {
      importer(url) {
        const exports = named(url);
        if (exports !== undefined) return Promise.resolve(exports);
        return Promise.reject(new TypeError(`The test gives no module ${url}.`));
      },
      stylesheet: () => Promise.resolve(),
    },
    ...(options.el === undefined ? {} : { frontEnds: pageFrontEnds(options.el) }),
    measure: options.measure ?? { info: () => INFO, watch: () => () => undefined },
    showMenu: menus.show,
    showDialog: dialogs.show,
  });

  let first: CoreApi;
  try {
    // Inside act(), as each turn of settled(), so that React draws the
    // frontend before the test's first step.
    first = await acting(() => started.ready);
  } catch (error) {
    restore();
    throw error;
  }
  const core = (): CoreApi => started.core() ?? first;
  function menu(target: Target, opts?: MenuOptions): StubMenu;
  function menu(): StubMenu | undefined;
  function menu(target?: Target, opts: MenuOptions = {}): StubMenu | undefined {
    if (target === undefined) return menus.open();
    const before = menus.open();
    core().boardActions.menu(target, { ...opts, el: document.body, anchor: new DOMRect(0, 0, 0, 0) });
    const opened = menus.open();
    if (opened === undefined || opened === before) {
      throw new Error(`No board action applies to the target ${JSON.stringify(target)}, so that no menu opens.`);
    }
    return opened;
  }
  return {
    get core() {
      return core();
    },
    menu,
    advanced: () => dialogs.open(),
    sent: r.sent,
    settled: async () => {
      for (let quiet = 0; quiet < 3; ) {
        const before = r.sent.length;
        await acting(() => new Promise<void>((resolve) => setTimeout(resolve, 0)));
        quiet = r.pending() === 0 && r.sent.length === before ? quiet + 1 : 0;
      }
    },
    logged,
    stop: () => {
      // Inside act(), as the start, so that the frontend's React roots unmount
      // without warning of an update outside act().
      actingNow(() => {
        started.stop();
      });
      restore();
    },
  };
}

// Where R serves a module that the test gives by its package and its id, as
// "blockr.test/notes.js" for the module notes of blockr.test.
function moduleUrl(pkg: string, id: string): string {
  return `${pkg}/${id}.js`;
}

// R, played from the fixtures over a transport of its own: it starts a session
// as core listens, and replies to each request in a later task, as R would.
function fixtureR(hello: HelloReply, options: StubCoreOptions) {
  const sent: Request[] = [];
  let pending = 0;
  let listener: TransportListener | undefined;
  // The revision of each block that R holds a result of, and its class.
  const results = new Map<BlockId, { revision: number; registry_id: { readonly package: string | null; readonly id: string } }>();

  const transport: Transport = {
    send(message: ClientMessage, bodies) {
      if (message.some((request) => options.refuse?.includes(request.kind) === true)) {
        return Promise.reject(new Error("The transport is closed."));
      }
      sent.push(...message);
      for (const request of message) {
        if (request.kind === "cancel" || request.kind === "forget") {
          if (request.kind === "forget") for (const block of request.blocks) results.delete(block);
          continue;
        }
        pending++;
        setTimeout(() => {
          void replyTo(request, bodies.get(request.id)).then(([reply, body]) => {
            pending--;
            listener?.received(reply, body === undefined ? null : Promise.resolve(body));
          });
        }, 0);
      }
      return Promise.resolve();
    },
    listen(heard) {
      listener = heard;
      setTimeout(() => {
        heard.started();
      }, 0);
    },
  };

  async function replyTo(request: Exclude<Request, { kind: "cancel" | "forget" }>, body: Blob | undefined): Promise<[Reply, Blob?]> {
    const { id } = request;
    switch (request.kind) {
      case "hello":
        // A later hello starts the session afresh.
        results.clear();
        return [{ ...hello, id }];
      case "blockr_options": {
        const values = options.blockrOptions ?? {};
        return [
          {
            id,
            status: "ok",
            values: Object.fromEntries(request.names.flatMap((name) => (Object.hasOwn(values, name) ? [[name, values[name] ?? null]] : []))),
          },
        ];
      }
      case "evaluate": {
        const given = options.evaluate?.(request) ?? {};
        const reply: EvaluateReply = {
          status: "ok",
          summary: null,
          reason: null,
          conditions: [],
          stdout: "",
          stderr: "",
          ...given,
          id,
          block: request.block,
          revision: request.revision,
        };
        if (reply.status === "ok") results.set(request.block, { revision: request.revision, registry_id: request.recipe.registry_id });
        return [reply];
      }
      case "output": {
        const held = results.get(request.block);
        if (held?.revision !== request.revision) {
          return [{ id, status: "missing", missing: [{ block: request.block, revision: request.revision }] }];
        }
        const given = options.output?.(request);
        const made = given ?? sample(held.registry_id);
        if (made === undefined) return [failed(id, "output", `The fixtures hold no output of block "${request.block}".`)];
        const reply: OutputReply = { id, status: "ok", output: made.output, conditions: [...(given?.conditions ?? [])] };
        return made.body === undefined ? [reply] : [{ ...reply, body: { type: made.body.type } }, made.body];
      }
      default: {
        const raised = [...(options.conditions?.(request) ?? [])];
        let answer: Json | Blob | undefined;
        try {
          answer = await options.answer?.(request, body);
        } catch (error) {
          return [failed(id, "request", errorMessage(error), raised)];
        }
        if (answer === undefined) return [failed(id, "request", `The test gives no answer to the request ${JSON.stringify(request)}.`)];
        if (answer instanceof Blob) {
          const announced = answer instanceof File ? { type: answer.type, name: answer.name } : { type: answer.type };
          return [{ id, status: "ok", body: announced, conditions: raised }, answer];
        }
        return [{ id, status: "ok", value: answer, conditions: raised }];
      }
    }
  }

  // The first sample output of the first renderer that the block's class
  // names, drawn by that renderer.
  function sample(registryId: { readonly package: string | null; readonly id: string }): { output: BlockOutput & Record<string, Json>; body?: Blob } | undefined {
    const renderer = blockRegistry(hello).get(interned(registryId.package, registryId.id))?.renderers?.[0];
    const samples = renderer?.package == null ? undefined : nestedValue(SAMPLES, interned(renderer.package, renderer.id));
    const first = samples?.outputs[0];
    if (renderer === undefined || first === undefined) return undefined;
    const output = { ...(first.output as unknown as Record<string, Json>), renderer } as BlockOutput & Record<string, Json>;
    const announced = (first as unknown as { body?: unknown }).body;
    return isObject(announced) && typeof announced.type === "string"
      ? { output, body: new Blob([], { type: announced.type }) }
      : { output };
  }

  return { transport, sent, pending: () => pending };
}

// R's reply of error: the conditions that R recorded before the error, and
// then the error, with the message given.
function failed(id: string, phase: "output" | "request", message: string, before: readonly Condition[] = []): Reply {
  return {
    id,
    status: "error",
    conditions: [...before, { severity: "error", class: ["blockr_error", "error", "condition"], phase, message }],
  };
}

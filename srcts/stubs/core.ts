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
// R, played from the fixtures, replies to the client's hello with the
// reply of the fixture hello unless given, and drops what it holds at a later
// hello, as R does. It evaluates every block as ok, makes a block's output
// from the sample outputs of its class's renderer, and answers a request with
// what the test gives.

import { pageFrontEnds, start } from "../core/index.ts";
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
import type { BlockId, PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { BlockOutput, OutputFixtures } from "../types/outputs.ts";
import { errorMessage, isObject, own, packageFromId } from "../util/index.ts";
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

// The sample outputs of core's output classes, by the id of the renderer that
// core gives the class.
const SAMPLES: Readonly<Record<string, OutputFixtures>> = {
  "blockr.core-table_output": tableFixtures as unknown as OutputFixtures,
  "blockr.core-text_output": textFixtures,
  "blockr.core-plot_output": plotFixtures,
};

// The modules of core's own plugin classes, by the id of each class, which the
// stub serves where R's reply names the class and the test gives no module of
// its own, as the page imports them.
const CORE_CLASSES: Readonly<Record<PluginClassId, unknown>> = {
  "blockr.core-basic_frontend_plugin": { api: 1, default: basicFrontend },
  "blockr.core-file_storage_plugin": { api: 1, default: fileStorage },
  "blockr.core-edit_board_plugin": { api: 1, default: editBoard },
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
  // at, or by an id that R's reply names: the id of the class of one of the
  // app's plugins, for the module of the class, the id of a board option, for
  // its module, or the id of a module that packages offer each other's UIs,
  // such as "blockr.core-controls". R serves a module that the test gives by
  // an id of the form `{pkg}-{name}` that R's reply does not name as one that
  // packages offer each other. The stub gives the modules of core's own plugin
  // classes and board options, as the page imports them, unless the test gives
  // its own, and a module that the test leaves out fails to load.
  readonly modules?: Readonly<Record<string, unknown>>;
  // What R replies to an evaluation, beside its id, block and revision, ok
  // with no conditions unless given.
  readonly evaluate?: (request: EvaluateRequest) => Partial<Omit<EvaluateReply, "id" | "block" | "revision">> | undefined;
  // The output that R makes, the first sample output of the renderer that the
  // block's class names unless given.
  readonly output?: (request: OutputRequest) => { output: BlockOutput & Record<string, Json>; body?: Blob } | undefined;
  // What R answers to a request of a block class, an output class, a
  // renderer, a board option or a plugin.
  readonly answer?: Answer;
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
  readonly settled: () => Promise<void>;
  // Ends what core started.
  readonly stop: () => void;
}

export async function stubCore(options: StubCoreOptions = {}): Promise<StubCore> {
  const base = options.hello ?? HELLO;
  const given = options.modules ?? {};
  // The modules that the test gives by an id that R's reply does not name,
  // which R then serves at a URL of their own.
  const offered = Object.keys(given).filter(
    (id) =>
      isModuleId(id) && !Object.hasOwn(base.modules, id) && !Object.hasOwn(base.plugins, id) && !Object.hasOwn(base.options, id),
  );
  const hello: HelloReply = {
    ...base,
    modules: { ...base.modules, ...Object.fromEntries(offered.map((id) => [id, { module: moduleUrl(id) }])) },
  };
  // What each URL that R's reply names serves: what the test gives by the URL
  // or by the id that the reply names it under, or else core's own module.
  const named = (url: string): unknown => {
    if (Object.hasOwn(given, url)) return given[url];
    const ids = [
      ...Object.entries(hello.plugins).filter(([, entry]) => entry.module === url).map(([id]) => id),
      ...Object.entries(hello.options).filter(([, entry]) => entry.module === url).map(([id]) => id),
      ...Object.entries(hello.modules).filter(([, source]) => source.module === url).map(([id]) => id),
    ];
    const id = ids.find((key) => Object.hasOwn(given, key));
    if (id !== undefined) return given[id];
    const own = ids.find((key) => Object.hasOwn(CORE_CLASSES, key) || Object.hasOwn(coreOptions, key));
    if (own === undefined) return undefined;
    return Object.hasOwn(CORE_CLASSES, own) ? CORE_CLASSES[own] : { api: 1, default: coreOptions[own] };
  };

  const r = fixtureR(hello, options);
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
  });

  const first = await started.ready;
  return {
    get core() {
      return started.core() ?? first;
    },
    sent: r.sent,
    settled: async () => {
      for (let quiet = 0; quiet < 3; ) {
        const before = r.sent.length;
        await new Promise((resolve) => setTimeout(resolve, 0));
        quiet = r.pending() === 0 && r.sent.length === before ? quiet + 1 : 0;
      }
    },
    stop: () => {
      started.stop();
    },
  };
}

// Where R serves a module that the test gives by its id, `{pkg}-{name}`, as
// "blockr.test/blockr.test-notes.js" for "blockr.test-notes".
function moduleUrl(id: string): string {
  return `${packageFromId(id) ?? "app"}/${id}.js`;
}

// Whether a key of the modules that the test gives is a module's id,
// `{pkg}-{name}`, rather than a URL that R serves a module at.
function isModuleId(key: string): boolean {
  return packageFromId(key) !== null && !key.includes("/");
}

// R, played from the fixtures over a transport of its own: it starts a session
// as core listens, and replies to each request in a later task, as R would.
function fixtureR(hello: HelloReply, options: StubCoreOptions) {
  const sent: Request[] = [];
  let pending = 0;
  let listener: TransportListener | undefined;
  // The revision of each block that R holds a result of, and its class.
  const results = new Map<BlockId, { revision: number; registry_id: string }>();

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
        const made = options.output?.(request) ?? sample(held.registry_id);
        if (made === undefined) return [failed(id, "output", `The fixtures hold no output of block "${request.block}".`)];
        const reply: OutputReply = { id, status: "ok", output: made.output };
        return made.body === undefined ? [reply] : [{ ...reply, body: { type: made.body.type } }, made.body];
      }
      default: {
        let answer: Json | Blob | undefined;
        try {
          answer = await options.answer?.(request, body);
        } catch (error) {
          return [failed(id, "request", errorMessage(error))];
        }
        if (answer === undefined) return [failed(id, "request", `The test gives no answer to the request ${JSON.stringify(request)}.`)];
        if (answer instanceof Blob) {
          const announced = answer instanceof File ? { type: answer.type, name: answer.name } : { type: answer.type };
          return [{ id, status: "ok", body: announced }, answer];
        }
        return [{ id, status: "ok", value: answer }];
      }
    }
  }

  // The first sample output of the renderer that the block's class names,
  // drawn by that renderer.
  function sample(registryId: string): { output: BlockOutput & Record<string, Json>; body?: Blob } | undefined {
    const renderer = own(hello.block_registry, registryId)?.renderer;
    const samples = renderer === undefined ? undefined : own(SAMPLES, renderer.id);
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

function failed(id: string, phase: "output" | "request", message: string): Reply {
  return {
    id,
    status: "error",
    conditions: [{ severity: "error", class: ["blockr_error", "error", "condition"], phase, message }],
  };
}

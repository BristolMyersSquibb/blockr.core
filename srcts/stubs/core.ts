// A CoreApi for tests, whose store, command registry and scheduler are core's
// own and whose replies from R come from the fixtures of the wire protocol,
// those that both test suites round-trip, so that a front-end, an extension or
// a startup module runs in a test runner on a board, needing neither R nor a
// browser (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).
//
// Core starts as it does on the page: it initializes the startup modules
// given, in their order, opens the board of R's reply to hello, and draws it
// with the module given for its front-end where the test gives an element to
// draw in. R, played from the fixtures, replies to the client's hello with the
// reply of the fixture hello unless given, evaluates every block as ok, makes
// a block's output from the sample outputs of its class's renderer, and answers
// a request with what the test gives.

import { pageFrontEnds, start } from "../core/index.ts";
import type { Measure } from "../mount/index.ts";
import { coreDefinitions } from "../options/index.ts";
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
import type { BlockId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { OptionModule } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { BlockOutput, OutputFixtures } from "../types/outputs.ts";
import { isObject, messageOf, own } from "../util/index.ts";
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
  // given, so that core takes the fallbacks.
  readonly blockrOptions?: Readonly<Record<string, Json>>;
  // What each module that core imports exports, by the URL that R serves it
  // at or by the id of a module that packages offer each other, a front-end
  // among them, such as "blockr.core-basic", which draws a board of core's
  // class. A module that the test leaves out fails to load.
  readonly modules?: Readonly<Record<string, unknown>>;
  // What the startup module of each package exports, by package, in the order
  // in which core initializes them, after those that the reply names.
  readonly startup?: Readonly<Record<string, unknown>>;
  // What R replies to an evaluation, beside its id, block and revision, ok
  // with no conditions unless given.
  readonly evaluate?: (request: EvaluateRequest) => Partial<Omit<EvaluateReply, "id" | "block" | "revision">> | undefined;
  // The output that R makes, the first sample output of the renderer that the
  // block's class names unless given.
  readonly output?: (request: OutputRequest) => { output: BlockOutput & Record<string, Json>; body?: Blob } | undefined;
  // What R answers to a request of a block class, an output class, a
  // renderer, a board option or a type that a package adds.
  readonly answer?: Answer;
  // The element in which core draws the board with its front-end. Without
  // one, core draws no front-end and lets the scheduler send once the board is
  // open.
  readonly el?: HTMLElement;
  // Measures an output's element, as INFO for every element unless given.
  readonly measure?: Measure;
}

export interface StubCore {
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

// The UI module of a board option that the CoreApi for tests registers in
// place of the React UIs of core's, which draws nothing.
const noUi: OptionModule = { render: () => undefined };

export async function stubCore(options: StubCoreOptions = {}): Promise<StubCore> {
  const startup = options.startup ?? {};
  const base = options.hello ?? HELLO;
  const hello: HelloReply = {
    ...base,
    packages: { ...base.packages, ...Object.fromEntries(Object.keys(startup).map((pkg) => [pkg, "1.0.0"])) },
    startup_modules: [
      ...base.startup_modules,
      ...Object.keys(startup).map((pkg) => ({ package: pkg, module: startupUrl(pkg) })),
    ],
  };
  const modules: Readonly<Record<string, unknown>> = {
    ...options.modules,
    ...Object.fromEntries(Object.entries(startup).map(([pkg, exports]) => [startupUrl(pkg), exports])),
  };

  const r = fixtureR(hello, options);
  const started = start({
    transport: r.transport,
    loader: {
      importer(url) {
        if (Object.hasOwn(modules, url)) return Promise.resolve(modules[url]);
        const id = Object.entries(hello.modules).find(([, source]) => source.module === url)?.[0];
        if (id !== undefined && Object.hasOwn(modules, id)) return Promise.resolve(modules[id]);
        return Promise.reject(new TypeError(`The test gives no module ${url}.`));
      },
      stylesheet: () => Promise.resolve(),
    },
    options: (core) => coreDefinitions(core).map((definition) => ({ ...definition, ui: noUi })),
    ...(options.el === undefined ? {} : { frontEnds: pageFrontEnds(options.el) }),
    measure: options.measure ?? { info: () => INFO, watch: () => () => undefined },
  });

  const core = await started.ready;
  return {
    core,
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

function startupUrl(pkg: string): string {
  return `${pkg}/startup.js`;
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
          conditions: [],
          omitted: { warning: 0, message: 0 },
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
          return [failed(id, "request", messageOf(error))];
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
    omitted: { warning: 0, message: 0 },
  };
}

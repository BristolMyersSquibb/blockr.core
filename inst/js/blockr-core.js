// srcts/scheduler/scheduler.ts
import { ROOT_CONTEXT } from "@opentelemetry/api";
import { action, batch, computed, effect, signal, untracked } from "./vendor-858b8c85.js";

// srcts/graph/graph.ts
function createGraph(board) {
  const blocks = Object.keys(board.blocks);
  const into = /* @__PURE__ */ new Map();
  const sources = /* @__PURE__ */ new Map();
  const targets = /* @__PURE__ */ new Map();
  for (const id2 of blocks) {
    into.set(id2, []);
    sources.set(id2, []);
    targets.set(id2, []);
  }
  for (const [id2, link] of Object.entries(board.links)) {
    const linksIn = into.get(link.to);
    const from = sources.get(link.to);
    const to = targets.get(link.from);
    if (linksIn === void 0 || from === void 0 || to === void 0) continue;
    linksIn.push(id2);
    from.push(link.from);
    to.push(link.to);
  }
  const upstream = once(sources);
  const downstream = once(targets);
  const order = topological(blocks, sources, targets);
  let cycle;
  return {
    order,
    linksInto: (id2) => into.get(id2) ?? [],
    upstream: (id2) => upstream.get(id2) ?? [],
    downstream: (id2) => downstream.get(id2) ?? [],
    withUpstream: (ids2) => closure(ids2, upstream),
    withDownstream: (ids2) => closure(ids2, downstream),
    cycle() {
      cycle ??= { found: findCycle(board, targets) };
      return cycle.found;
    },
    path: (from, to) => findPath(from, to, downstream)
  };
}
function once(ends) {
  return new Map([...ends].map(([id2, blocks]) => [id2, [...new Set(blocks)]]));
}
function closure(ids2, steps2) {
  const reached = /* @__PURE__ */ new Set();
  const todo = [...ids2].filter((id2) => steps2.has(id2));
  for (const id2 of todo) reached.add(id2);
  for (const id2 of todo) {
    for (const next of steps2.get(id2) ?? []) {
      if (reached.has(next)) continue;
      reached.add(next);
      todo.push(next);
    }
  }
  return reached;
}
function topological(blocks, sources, targets) {
  const left2 = new Map(blocks.map((id2) => [id2, sources.get(id2)?.length ?? 0]));
  const order = blocks.filter((id2) => left2.get(id2) === 0);
  for (const id2 of order) {
    for (const next of targets.get(id2) ?? []) {
      const count = (left2.get(next) ?? 0) - 1;
      left2.set(next, count);
      if (count === 0) order.push(next);
    }
  }
  const placed = new Set(order);
  return [...order, ...blocks.filter((id2) => !placed.has(id2))];
}
function findCycle(board, targets) {
  const done = /* @__PURE__ */ new Set();
  const path = [];
  const onPath = /* @__PURE__ */ new Set();
  function visit(block) {
    if (onPath.has(block)) return path.slice(path.indexOf(block));
    if (done.has(block)) return void 0;
    path.push(block);
    onPath.add(block);
    for (const next of targets.get(block) ?? []) {
      const cycle = visit(next);
      if (cycle !== void 0) return cycle;
    }
    path.pop();
    onPath.delete(block);
    done.add(block);
    return void 0;
  }
  const starts = new Set(Object.values(board.links).map((link) => link.from));
  for (const block of starts) {
    if (!targets.has(block)) continue;
    const cycle = visit(block);
    if (cycle !== void 0) return cycle;
  }
  return void 0;
}
function findPath(from, to, downstream) {
  if (!downstream.has(from)) return void 0;
  const before = /* @__PURE__ */ new Map([[from, void 0]]);
  const todo = [from];
  for (const block of todo) {
    if (block === to) {
      const path = [block];
      for (let at = before.get(block); at !== void 0; at = before.get(at)) path.unshift(at);
      return path;
    }
    for (const next of downstream.get(block) ?? []) {
      if (before.has(next)) continue;
      before.set(next, block);
      todo.push(next);
    }
  }
  return void 0;
}

// srcts/protocol/client.ts
import { context as context3, SpanKind, SpanStatusCode as SpanStatusCode2 } from "@opentelemetry/api";

// srcts/util/util.ts
function own(record, key2) {
  return Object.hasOwn(record, key2) ? record[key2] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}
function asError(error) {
  return error instanceof Error ? error : new Error(String(error));
}
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}
function jsonEqual(a, b) {
  if (a === b) return true;
  if (Array.isArray(a)) {
    return Array.isArray(b) && a.length === b.length && a.every((x, i) => jsonEqual(x, b[i]));
  }
  if (isObject(a) && isObject(b)) {
    const keys = Object.keys(a);
    return keys.length === Object.keys(b).length && keys.every((key2) => Object.hasOwn(b, key2) && jsonEqual(a[key2], b[key2]));
  }
  return false;
}
function without(record, keys) {
  if (!keys.some((key2) => Object.hasOwn(record, key2))) return record;
  return Object.fromEntries(
    Object.entries(record).filter(([key2]) => !keys.includes(key2))
  );
}
function frozenCopy(x) {
  return freeze(structuredClone(x));
}
function freeze(x) {
  if (typeof x === "object" && x !== null) {
    for (const value2 of Object.values(x)) freeze(value2);
    Object.freeze(x);
  }
  return x;
}
function listing(items) {
  if (items.length <= 1) return items.join("");
  return `${items.slice(0, -1).join(", ")} and ${items.at(-1) ?? ""}`;
}
function quoted(items) {
  return listing(items.map((item) => `"${item}"`));
}
function packageFromId(id2) {
  const at = id2.indexOf("-");
  if (at < 0) return null;
  const pkg = id2.slice(0, at);
  return isPackageName(pkg) ? pkg : null;
}
function isPackageName(name3) {
  return typeof name3 === "string" && /^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$/.test(name3);
}
function asNumber(text) {
  const number = text.trim() === "" ? NaN : Number(text);
  return Number.isFinite(number) ? number : NaN;
}
function asLogical(text) {
  const trimmed = text.trim();
  if (["TRUE", "true", "True", "T"].includes(trimmed)) return true;
  if (["FALSE", "false", "False", "F"].includes(trimmed)) return false;
  return void 0;
}

// srcts/util/notification.ts
var raiseNothing = () => () => void 0;
var repliedNothing = () => void 0;
var notifyNothing = () => () => void 0;
var refusedNothing = () => void 0;
function givenNotifier(opts, call) {
  if (opts?.notifier === void 0) return void 0;
  if (typeof opts.notifier !== "function") throw new TypeError(`The \`notifier\` of ${call} is a function.`);
  return opts.notifier.bind(opts);
}
function sourceModule(object2, pkg, renderer) {
  return {
    object: object2,
    ...renderer === void 0 ? {} : { renderer },
    ...pkg === void 0 || pkg === null ? {} : { package: pkg }
  };
}
function coreNotification(code, severity, message, concerns = {}) {
  const { module } = concerns;
  const about = concerns.about ?? module?.object;
  return {
    severity,
    message,
    source: { kind: "core", code, ...module === void 0 ? {} : { module } },
    ...about === void 0 ? {} : { about }
  };
}
var SEVERITIES = ["message", "warning", "error"];
var KINDS = ["block", "link", "stack", "option", "plugin"];
function isBoardObject(value2) {
  if (!isObject(value2)) return false;
  const keys = Object.keys(value2);
  const [kind] = keys;
  if (keys.length !== 1 || kind === void 0 || !KINDS.includes(kind)) return false;
  const id2 = value2[kind];
  return typeof id2 === "string" && id2 !== "";
}
function checkRaised(notification) {
  const given = notification;
  if (!isObject(given)) {
    throw new TypeError(`A notification is an object, not ${JSON.stringify(given)}.`);
  }
  if (!SEVERITIES.includes(given.severity)) {
    throw new TypeError(
      `The severity of a notification is "message", "warning" or "error", not ${JSON.stringify(given.severity)}.`
    );
  }
  if (typeof given.message !== "string") {
    throw new TypeError(`The message of a notification is a string, not ${JSON.stringify(given.message)}.`);
  }
  if (typeof given.code !== "string" || given.code === "") {
    throw new TypeError(
      `The code of a notification is a non-empty string, as "render_failed", not ${JSON.stringify(given.code)}.`
    );
  }
  const { about } = given;
  if (about !== void 0 && about !== null && !isBoardObject(about)) {
    throw new TypeError(
      `A notification concerns one board object, as { block: "a" }: a block, a link, a stack, a board option or a plugin, or null for the board, not ${JSON.stringify(about)}.`
    );
  }
  return notification;
}

// srcts/util/validate.ts
import { Validator } from "./vendor-858b8c85.js";
function compile(what, schema, fails = "fails its schema") {
  let validator;
  try {
    validator = new Validator(schema, schemaDraft(schema), false);
  } catch (error) {
    throw new Error(`The schema of ${what} cannot be read: ${errorMessage(error)}`, {
      cause: error
    });
  }
  return (value2) => {
    const result = validator.validate(value2);
    return result.valid ? [] : describe(result.errors, fails);
  };
}
function schemaDraft(schema) {
  const declared = typeof schema.$schema === "string" ? schema.$schema : "";
  if (declared.includes("draft-04")) return "4";
  if (declared.includes("draft-07")) return "7";
  if (declared.includes("2019-09")) return "2019-09";
  return "2020-12";
}
var wrappers = /* @__PURE__ */ new Set([
  "properties",
  "items",
  "prefixItems",
  "additionalItems",
  "$ref",
  "$dynamicRef",
  "$recursiveRef",
  "allOf",
  "false"
]);
function describe(errors, fails) {
  return errors.filter((error) => !wrappers.has(error.keyword)).map((error) => {
    const at = error.instanceLocation.replace(/^#/, "");
    return `${fails}${at === "" ? "" : ` at ${at}`}: ${error.error}`;
  });
}

// srcts/log/line.ts
function writingLog(pkg, write2) {
  const line = (level, message, attributes) => {
    const error = level === "error" && message instanceof Error ? message : void 0;
    write2({
      level,
      message: error === void 0 ? String(message) : error.message,
      package: pkg,
      ...error === void 0 ? {} : { error },
      attributes: isObject(attributes) ? { ...attributes } : {},
      time: Date.now()
    });
  };
  return Object.freeze({
    error(message, attributes) {
      line("error", message, attributes);
    },
    warn(message, attributes) {
      line("warn", message, attributes);
    },
    info(message, attributes) {
      line("info", message, attributes);
    },
    debug(message, attributes) {
      line("debug", message, attributes);
    },
    trace(message, attributes) {
      line("trace", message, attributes);
    }
  });
}

// srcts/log/log.ts
var THRESHOLDS = ["fatal", "error", "warn", "info", "debug", "trace"];
var logging;
var awaited = false;
var held = [];
var replaced = [];
function reaches(level, lowest) {
  return THRESHOLDS.indexOf(level) <= THRESHOLDS.indexOf(lowest);
}
function write(line, at = replaced.length - 1) {
  const test = replaced[at];
  if (test !== void 0) {
    test.logger(line);
    if (test.passOn) write(line, at - 1);
  } else if (logging === void 0) {
    held.push(line);
  } else if (reaches(line.level, logging.lowest)) {
    logging.logger(line);
  }
}
function setLogging(lowest, logger) {
  logging = { lowest, logger };
  awaited = false;
  const lines = held;
  held = [];
  for (const line of lines) write(line);
}
function awaitLogger() {
  awaited = true;
}
function fallBack() {
  if (logging === void 0 && !awaited) setLogging("info", consoleLogger);
}
function createLog(pkg) {
  return writingLog(pkg, write);
}
var log = /* @__PURE__ */ createLog("blockr.core");
var consoleLogger = (line) => {
  const write2 = consoleWriter(line.level);
  const prefix = line.package === null ? void 0 : `[${line.package}]`;
  if (line.error === void 0) {
    write2("%s", prefix === void 0 ? line.message : `${prefix} ${line.message}`);
  } else if (prefix === void 0) {
    write2(line.error);
  } else {
    write2("%s", prefix, line.error);
  }
};
function consoleWriter(level) {
  switch (level) {
    case "error":
      return (...data) => {
        console.error(...data);
      };
    case "warn":
      return (...data) => {
        console.warn(...data);
      };
    case "info":
      return (...data) => {
        console.info(...data);
      };
    case "debug":
    case "trace":
      return (...data) => {
        console.debug(...data);
      };
  }
}

// srcts/log/settings.ts
var LOGGERS = ["console", "otel", "condition"];
function logSettings(values2, base) {
  const lowest = logLevel(values2.logLevel);
  const endpoint = values2.otelClientEndpoint === void 0 ? void 0 : collectorUrl(values2.otelClientEndpoint, "v1/logs", base);
  const logger = values2.logger;
  if (logger !== void 0 && (typeof logger !== "string" || !LOGGERS.includes(logger))) {
    throw new TypeError(`The blockr option logger is ${JSON.stringify(logger)}, where it takes "console", "otel" or "condition".`);
  }
  if (logger === "otel" && endpoint === void 0) {
    throw new Error(
      'The blockr option logger is "otel", but the blockr option otel_client_endpoint names no collector to send the log records of the client to.'
    );
  }
  const sends = logger === "otel" || logger === void 0 && endpoint !== void 0;
  return sends && endpoint !== void 0 ? { lowest, url: endpoint } : { lowest };
}
function logLevel(value2) {
  if (value2 === void 0) return "info";
  const named4 = typeof value2 === "string" && value2 !== "" ? THRESHOLDS.filter((level2) => level2.startsWith(value2)) : [];
  const [level] = named4;
  if (named4.length === 1 && level !== void 0) return level;
  throw new TypeError(
    `The blockr option log_level is ${JSON.stringify(value2)}, where it takes "fatal", "error", "warn", "info", "debug" or "trace".`
  );
}
function collectorUrl(value2, path, base) {
  let url;
  try {
    url = typeof value2 === "string" && value2 !== "" ? new URL(value2, base) : void 0;
  } catch {
    url = void 0;
  }
  if (url === void 0 || url.protocol !== "http:" && url.protocol !== "https:") {
    throw new TypeError(
      `The blockr option otel_client_endpoint is ${JSON.stringify(value2)}, where it takes the URL of an OpenTelemetry collector, over HTTP or HTTPS.`
    );
  }
  url.pathname = `${url.pathname.replace(/\/$/, "")}/${path}`;
  return url.href;
}
function serviceName(value2) {
  if (value2 === void 0) return "blockr";
  if (typeof value2 !== "string" || value2 === "") {
    throw new TypeError(
      `The blockr option otel_client_service_name is ${JSON.stringify(value2)}, where it takes the name of a service, as a string that is not empty.`
    );
  }
  return value2;
}
function setUpLogging(settings, serviceName2, sessionId, load) {
  const { lowest, url } = settings;
  if (url === void 0) {
    setLogging(lowest, consoleLogger);
    return;
  }
  awaitLogger();
  void (async () => {
    let logger;
    try {
      logger = (await load()).otelLogger({ url, serviceName: serviceName2, sessionId });
    } catch (error) {
      setLogging(lowest, consoleLogger);
      log.error(
        new Error(`Core could not set up its OpenTelemetry logger, so the page writes its log lines to the console: ${errorMessage(error)}`, {
          cause: error
        })
      );
      return;
    }
    setLogging(lowest, logger);
  })();
}

// srcts/trace/trace.ts
import { context, isSpanContextValid, SpanStatusCode, trace } from "@opentelemetry/api";
var SCOPE = "blockr.core";
function tracer() {
  return trace.getTracer(SCOPE);
}
function traceparent(span) {
  if (!span.isRecording()) return void 0;
  const { traceId, spanId, traceFlags } = span.spanContext();
  if (!isSpanContextValid({ traceId, spanId, traceFlags })) return void 0;
  return `00-${traceId}-${spanId}-${(traceFlags & 255).toString(16).padStart(2, "0")}`;
}
function keepContext() {
  const kept = context.active();
  return (fn) => context.with(kept, fn);
}
function activeSpanContext() {
  const span = trace.getActiveSpan();
  if (span?.isRecording() !== true) return void 0;
  const spanContext = span.spanContext();
  return isSpanContextValid(spanContext) ? spanContext : void 0;
}
function failSpan(span, error) {
  const failure = asError(error);
  span.recordException(failure);
  span.setStatus({ code: SpanStatusCode.ERROR, message: failure.message });
}

// srcts/trace/steps.ts
import { context as context2, trace as trace2 } from "@opentelemetry/api";
function steps(name3, start2 = performance.now(), parent = context2.active()) {
  const taken = [];
  let recorded = false;
  return {
    async step(stepName, fn) {
      const step2 = { name: stepName, start: performance.now() };
      taken.push(step2);
      try {
        return await fn();
      } catch (error) {
        step2.error = error;
        throw error;
      } finally {
        step2.end = performance.now();
      }
    },
    noted(stepName, stepStart, end) {
      taken.push({ name: stepName, start: stepStart, end });
    },
    record(ready, failure) {
      if (recorded) return;
      recorded = true;
      const end = performance.now();
      const write2 = () => {
        const whole = tracer().startSpan(name3, { startTime: start2 }, parent);
        const inside = trace2.setSpan(parent, whole);
        for (const step2 of taken) {
          const span = tracer().startSpan(step2.name, { startTime: step2.start }, inside);
          if (step2.error !== void 0) failSpan(span, step2.error);
          span.end(step2.end ?? end);
        }
        if (failure !== void 0) failSpan(whole, failure);
        whole.end(end);
      };
      void ready.then(write2, write2);
    }
  };
}

// srcts/trace/settings.ts
function tracesUrl(otelClientEndpoint, base) {
  return otelClientEndpoint === void 0 ? void 0 : collectorUrl(otelClientEndpoint, "v1/traces", base);
}
async function setUpTracing(url, serviceName2, sessionId, load) {
  if (url === void 0) return;
  try {
    (await load()).otelTracing({ url, serviceName: serviceName2, sessionId });
  } catch (error) {
    log.error(
      new Error(`Core could not set up its OpenTelemetry tracing, so the page records no spans: ${errorMessage(error)}`, {
        cause: error
      })
    );
  }
}

// srcts/protocol/client.ts
var SessionEndedError = class extends Error {
  constructor(id2) {
    super(`The session ended before R replied to request ${id2}.`);
    this.name = "SessionEndedError";
  }
};
function sameMajor(a, b) {
  return a.split(".")[0] === b.split(".")[0];
}
function createClient(transport, options) {
  let next = 0;
  let state2 = "none";
  let stopped;
  let shared = /* @__PURE__ */ new Set();
  const waiting2 = /* @__PURE__ */ new Map();
  let held2 = [];
  const sending = /* @__PURE__ */ new Set();
  let ends = 0;
  function mint() {
    next += 1;
    return String(next);
  }
  function dispatch(outbound) {
    const plain = outbound.filter((entry) => entry.call?.body === void 0);
    if (plain.length > 0) {
      transmit(plain, /* @__PURE__ */ new Map());
    }
    for (const entry of outbound) {
      if (entry.call?.body !== void 0) {
        transmit([entry], /* @__PURE__ */ new Map([[entry.call.id, entry.call.body]]));
      }
    }
  }
  function transmit(outbound, bodies) {
    const calls = outbound.flatMap((entry) => entry.call ?? []);
    for (const call of calls) {
      waiting2.set(call.id, call);
    }
    const sent2 = transport.send(
      outbound.map((entry) => entry.message),
      bodies
    ).then(
      () => {
        for (const call of calls) {
          call.sent = true;
          if (call.aborted) cancel(call);
        }
      },
      (error) => {
        for (const call of calls) {
          if (waiting2.get(call.id) === call) {
            waiting2.delete(call.id);
            call.settle({ error: asError(error) });
          }
        }
      }
    );
    if (bodies.size > 0) {
      sending.add(sent2);
      void sent2.finally(() => sending.delete(sent2));
    }
  }
  function cancel(call) {
    if (waiting2.get(call.id) !== call || !shared.has("cancel")) return;
    quietly(transport.send([{ kind: "cancel", request: call.id }], /* @__PURE__ */ new Map()));
  }
  function abort(entry, call) {
    const index = held2.indexOf(entry);
    if (index >= 0) {
      held2.splice(index, 1);
      call.settle(cancelled(call.id));
    } else if (call.sent) {
      cancel(call);
    } else {
      call.aborted = true;
    }
  }
  function queue(entry) {
    if (stopped !== void 0) {
      entry.call?.settle({ error: stopped });
    } else if (state2 === "ready") {
      dispatch([entry]);
    } else {
      held2.push(entry);
    }
  }
  function end() {
    state2 = "none";
    shared = /* @__PURE__ */ new Set();
    ends += 1;
    const lost = [...waiting2.values()];
    waiting2.clear();
    for (const call of lost) {
      call.settle({ error: new SessionEndedError(call.id) });
    }
  }
  function stop(theirs, ours) {
    stopped = new Error(
      `R speaks version ${theirs} of the wire protocol, where the client speaks ${ours}, so the client sends R nothing.`
    );
    end();
    const lost = held2;
    held2 = [];
    for (const { call } of lost) {
      call?.settle({ error: stopped });
    }
  }
  function sendHello() {
    const named4 = options.hello();
    const id2 = mint();
    const call = {
      id: id2,
      body: void 0,
      progress: void 0,
      sent: false,
      aborted: false,
      settle(result) {
        if ("error" in result) {
          if (!(result.error instanceof SessionEndedError)) log.error(asError(result.error));
          return;
        }
        const { reply } = result;
        if (reply.status === "ok" && "capabilities" in reply) {
          if (sameMajor(reply.protocol, named4.protocol)) {
            const theirs = new Set(reply.capabilities);
            shared = new Set(named4.capabilities.filter((c) => theirs.has(c)));
            state2 = "ready";
            const outbound = held2;
            held2 = [];
            dispatch(outbound);
          } else {
            stop(reply.protocol, named4.protocol);
          }
        }
        guard(() => {
          options.session(reply);
        });
      }
    };
    transmit([{ message: { ...named4, id: id2, kind: "hello" }, call }], /* @__PURE__ */ new Map());
  }
  transport.listen({
    started() {
      if (stopped !== void 0) return;
      end();
      state2 = "started";
      sendHello();
    },
    ended() {
      end();
    },
    received(item, body) {
      const call = waiting2.get(item.id);
      if (call === void 0) return;
      if (!("status" in item)) {
        const note = item.progress;
        guard(() => {
          call.progress?.(note);
        });
        return;
      }
      waiting2.delete(item.id);
      if (body === null) {
        call.settle({ reply: item, body: null });
      } else {
        body.then(
          (blob2) => {
            call.settle({ reply: item, body: blob2 });
          },
          (error) => {
            call.settle({ error: asError(error) });
          }
        );
      }
    }
  });
  return {
    request(request, { body, progress, signal: signal5, span: spanned } = {}) {
      const id2 = mint();
      const span = requestSpan(request, spanned);
      const parent = span === void 0 ? void 0 : traceparent(span);
      const message = {
        ...request,
        id: id2,
        ...parent === void 0 ? {} : { traceparent: parent },
        ...body === void 0 ? {} : { body: announce(body) }
      };
      return new Promise((resolve, reject) => {
        const onAbort2 = () => {
          abort(entry, call);
        };
        const call = {
          id: id2,
          body,
          progress,
          sent: false,
          aborted: false,
          settle(result) {
            signal5?.removeEventListener("abort", onAbort2);
            if (span !== void 0) endRequestSpan(span, result);
            if ("error" in result) {
              reject(result.error);
            } else {
              resolve(result);
            }
          }
        };
        const entry = { message, call };
        if (signal5?.aborted === true) {
          call.settle(cancelled(id2));
          return;
        }
        signal5?.addEventListener("abort", onAbort2, { once: true });
        queue(entry);
      });
    },
    forget(blocks) {
      queue({ message: { kind: "forget", blocks } });
    },
    capabilities: () => shared,
    hello() {
      if (stopped !== void 0) return;
      const runs = state2 !== "none";
      const lost = held2;
      held2 = [];
      end();
      for (const { call } of lost) {
        call?.settle({ error: new SessionEndedError(call.id) });
      }
      if (!runs) return;
      state2 = "started";
      if (sending.size === 0) {
        sendHello();
        return;
      }
      const at = ends;
      void Promise.allSettled([...sending]).then(() => {
        if (ends === at) sendHello();
      });
    }
  };
}
function requestSpan(request, given = {}) {
  const named4 = spanName(request);
  if (named4 === void 0) return void 0;
  return tracer().startSpan(
    named4,
    {
      kind: SpanKind.CLIENT,
      attributes: { ...requestAttributes(request), ...given.attributes },
      links: [...given.links ?? []]
    },
    given.parent ?? context3.active()
  );
}
function spanName(request) {
  switch (request.kind) {
    case "evaluate":
    case "output":
      return request.kind;
    case "request":
      return `request ${request.name ?? ""}`;
    default:
      return void 0;
  }
}
function requestAttributes(request) {
  switch (request.kind) {
    case "evaluate":
      return {
        "blockr.block.id": request.block,
        "blockr.block.revision": request.revision,
        "blockr.block.registry_id": request.recipe.registry_id
      };
    case "output":
      return { "blockr.block.id": request.block, "blockr.block.revision": request.revision };
    case "request": {
      const named4 = { "blockr.request.type": request.type, "blockr.request.name": request.name };
      switch (request.type) {
        case "block_class":
          return { ...named4, "blockr.block.id": request.block, "blockr.block.registry_id": request.registry_id };
        case "option":
          return { ...named4, "blockr.option.id": request.option };
        case "plugin":
          return { ...named4, "blockr.plugin.class_id": request.plugin };
        case "output_class":
          return { ...named4, "blockr.block.id": request.output.block, "blockr.block.revision": request.output.revision };
        case "renderer":
          return {
            ...named4,
            "blockr.block.id": request.output.block,
            "blockr.block.revision": request.output.revision,
            "blockr.renderer.id": request.renderer
          };
      }
      break;
    }
    default:
      break;
  }
  return {};
}
function endRequestSpan(span, result) {
  if ("error" in result) {
    failSpan(span, result.error);
  } else {
    span.setAttribute("blockr.reply.status", result.reply.status);
    if (result.reply.status === "error") {
      const conditions = "conditions" in result.reply ? result.reply.conditions : [];
      const message = conditions.find((condition) => condition.severity === "error")?.message;
      span.setStatus({ code: SpanStatusCode2.ERROR, ...message === void 0 ? {} : { message } });
    }
  }
  span.end();
}
function announce(body) {
  const type = body.type === "" ? "application/octet-stream" : body.type;
  return body instanceof File ? { type, name: body.name } : { type };
}
function cancelled(id2) {
  return { reply: { id: id2, status: "cancelled" }, body: null };
}
function guard(fn) {
  try {
    fn();
  } catch (error) {
    log.error(asError(error));
  }
}
function quietly(sent2) {
  sent2.catch(() => void 0);
}

// srcts/protocol/messages.ts
var PROTOCOL_VERSION = "1.0";

// srcts/scheduler/status.ts
var NO_CONDITIONS = Object.freeze([]);
function replyOutcome(reply, revision) {
  if (reply.status === "cancelled" || reply.status === "missing") return void 0;
  const evaluated = reply;
  return {
    status: reply.status,
    revision,
    summary: evaluated.summary ?? null,
    reason: evaluated.reason ?? null,
    conditions: evaluated.conditions ?? NO_CONDITIONS,
    stdout: evaluated.stdout ?? "",
    stderr: evaluated.stderr ?? ""
  };
}
function unsent(revision, error) {
  return { revision, message: `The evaluation could not be sent to R: ${error}` };
}
function unevaluated(revision, running2) {
  return {
    status: "unevaluated",
    revision,
    evaluated: null,
    running: running2,
    conditions: NO_CONDITIONS,
    stdout: "",
    stderr: "",
    summary: null,
    reason: null,
    unsent: null
  };
}
function failedToSend(failure, revision, evaluated, running2) {
  return {
    status: failure.revision === revision ? "failed" : "stale",
    revision,
    evaluated,
    running: running2,
    conditions: NO_CONDITIONS,
    stdout: "",
    stderr: "",
    summary: null,
    reason: null,
    unsent: failure.message
  };
}
var REPLIED = {
  ok: "ready",
  error: "failed",
  invalid: "invalid",
  waiting: "waiting"
};
function replied(outcome, revision, running2) {
  return {
    status: outcome.revision !== revision ? "stale" : REPLIED[outcome.status],
    revision,
    evaluated: outcome.revision,
    running: running2,
    conditions: outcome.conditions,
    stdout: outcome.stdout,
    stderr: outcome.stderr,
    summary: outcome.summary,
    reason: outcome.reason,
    unsent: null
  };
}
function waiting(why, revision, evaluated, running2) {
  return {
    status: "waiting",
    revision,
    evaluated,
    running: running2,
    conditions: NO_CONDITIONS,
    stdout: "",
    stderr: "",
    summary: null,
    reason: why,
    unsent: null
  };
}
function waitingFor(what) {
  if ("upstream" in what) {
    const blocks = what.upstream.length === 1 ? "block" : "blocks";
    return `This block is waiting for ${blocks} ${quoted(what.upstream)}.`;
  }
  if (what.unlinked.includes("...")) {
    return "This block is waiting for a block to be linked to it.";
  }
  const inputs = what.unlinked.length === 1 ? "input" : "inputs";
  return `This block is waiting for its ${inputs} ${quoted(what.unlinked)} to be linked.`;
}
function sameStatus(a, b) {
  return a.status === b.status && a.revision === b.revision && a.evaluated === b.evaluated && a.running === b.running && a.stdout === b.stdout && a.stderr === b.stderr && a.summary === b.summary && a.reason === b.reason && a.unsent === b.unsent && a.conditions.length === b.conditions.length && a.conditions.every((condition, i) => condition === b.conditions[i]);
}
function blockStatuses(input2) {
  const statuses = /* @__PURE__ */ new Map();
  const changed = [];
  for (const id2 of input2.graph.order) {
    const revision = input2.revisions.get(id2);
    if (revision === void 0) continue;
    const outcome = input2.outcomes.get(id2);
    const failure = input2.unsent.get(id2);
    const running2 = input2.running.has(id2);
    const reason = keptWaiting(input2.inputs.get(id2), statuses);
    const now = reason !== void 0 ? waiting(reason, revision, outcome?.revision ?? null, running2) : failure !== void 0 ? failedToSend(failure, revision, outcome?.revision ?? null, running2) : outcome === void 0 ? unevaluated(revision, running2) : replied(outcome, revision, running2);
    const before = input2.before.get(id2);
    if (before !== void 0 && sameStatus(before, now)) {
      statuses.set(id2, before);
    } else {
      statuses.set(id2, Object.freeze(now));
      changed.push(id2);
    }
  }
  for (const id2 of input2.before.keys()) {
    if (!statuses.has(id2)) changed.push(id2);
  }
  return { statuses, changed };
}
function keptWaiting(inputs, statuses) {
  const unlinked = inputs?.unlinked ?? [];
  if (unlinked.length > 0) return waitingFor({ unlinked });
  const reads = (inputs?.recipe ?? []).flatMap((read) => read.block ?? []);
  const blocked = [...new Set(reads)].filter((up) => {
    const status = statuses.get(up)?.status;
    return status === "failed" || status === "invalid" || status === "waiting";
  });
  return blocked.length > 0 ? waitingFor({ upstream: blocked }) : void 0;
}
function waitingError(id2, status) {
  const why = status.reason ?? "This block is waiting.";
  return new Error(
    `${why.replace(/^This block/, `Block "${id2}"`)} Its evaluation with parameters is not sent.`
  );
}

// srcts/scheduler/evaluations.ts
function createOneTime(blocks, params, priority, statuses, inputs) {
  if (params === void 0) {
    const onBoard = new Set(blocks.filter((id2) => statuses.has(id2)));
    if (onBoard.size === 0) {
      return new Error(`The board holds none of the blocks ${quoted(blocks)} to evaluate.`);
    }
    return { blocks: onBoard, priority };
  }
  if (blocks.length === 0) return new Error("An evaluation with parameters names no block.");
  const absent = blocks.filter((id2) => !statuses.has(id2));
  if (absent.length > 0) {
    const named4 = absent.length === 1 ? "block" : "blocks";
    return new Error(`The board holds no ${named4} ${quoted(absent)} to evaluate.`);
  }
  const parts = /* @__PURE__ */ new Map();
  for (const id2 of blocks) {
    const status = statuses.get(id2);
    if (status !== void 0 && keptWaiting(inputs.get(id2), statuses) !== void 0) {
      return waitingError(id2, status);
    }
    parts.set(id2, { recipe: void 0, sent: void 0, result: void 0 });
  }
  return { priority, params, parts };
}
function settle(oneTimes, statuses) {
  const settled2 = [];
  for (const [id2, oneTime] of oneTimes) {
    if ("params" in oneTime) continue;
    const blocks = [...oneTime.blocks];
    if (blocks.some((block) => !current(statuses.get(block)))) continue;
    const result = {};
    for (const block of blocks) {
      const status = statuses.get(block);
      if (status !== void 0) result[block] = status;
    }
    settled2.push([id2, { statuses: result }]);
  }
  return { oneTimes: without2(oneTimes, settled2.map(([id2]) => id2)), settled: settled2 };
}
function left(oneTimes, gone) {
  const kept = /* @__PURE__ */ new Map();
  const settled2 = [];
  for (const [id2, oneTime] of oneTimes) {
    if (!("params" in oneTime)) {
      const blocks = new Set([...oneTime.blocks].filter((block) => !gone.has(block)));
      if (blocks.size === 0) {
        settled2.push([id2, { error: new Error("Every block of the one-time evaluation left the board.") }]);
      } else {
        kept.set(id2, blocks.size === oneTime.blocks.size ? oneTime : { ...oneTime, blocks });
      }
      continue;
    }
    const early = [...oneTime.parts].find(
      ([block, part]) => gone.has(block) && part.sent === void 0 && part.result === void 0
    );
    if (early === void 0) {
      kept.set(id2, oneTime);
    } else {
      const error = new Error(
        `Block "${early[0]}" left the board before its evaluation with parameters was sent.`
      );
      settled2.push([id2, { error }]);
    }
  }
  return { oneTimes: kept, settled: settled2 };
}
function sent(oneTimes, id2, block, evaluation) {
  return withPart(oneTimes, id2, block, { recipe: evaluation.recipe, sent: evaluation.id });
}
function answered(oneTimes, id2, block, reply, outcome, revision) {
  const oneTime = oneTimes.get(id2);
  if (oneTime === void 0 || !("params" in oneTime)) return { oneTimes, settled: [] };
  const part = oneTime.parts.get(block);
  if (part === void 0) return { oneTimes, settled: [] };
  if (outcome !== void 0) {
    const result = Object.freeze(replied(outcome, revision, false));
    const parts = new Map(oneTime.parts).set(block, { ...part, result });
    const results = [...parts].flatMap(
      ([at, { result: status }]) => status === void 0 ? [] : [[at, status]]
    );
    if (results.length < parts.size) {
      return { oneTimes: new Map(oneTimes).set(id2, { ...oneTime, parts }), settled: [] };
    }
    return {
      oneTimes: without2(oneTimes, [id2]),
      settled: [[id2, { statuses: Object.fromEntries(results) }]]
    };
  }
  if (reply.status === "missing") {
    return { oneTimes: withPart(oneTimes, id2, block, { recipe: void 0, sent: void 0 }), settled: [] };
  }
  const error = new Error(
    `R dropped the evaluation of block "${block}" with parameters, as it does for a block that leaves the board.`
  );
  return { oneTimes: without2(oneTimes, [id2]), settled: [[id2, { error }]] };
}
function neededBy(oneTimes, graph) {
  const needs = [];
  for (const oneTime of oneTimes.values()) {
    if (!("params" in oneTime)) {
      needs.push({ blocks: oneTime.blocks, priority: oneTime.priority });
      continue;
    }
    for (const [block, part] of oneTime.parts) {
      if (part.result !== void 0) continue;
      const reads = part.recipe === void 0 ? graph.upstream(block) : part.recipe.inputs.flatMap((input2) => input2.block ?? []);
      needs.push({ blocks: reads, priority: oneTime.priority });
    }
  }
  return needs;
}
function pendingEvaluations(oneTimes) {
  const pending = [];
  for (const [id2, oneTime] of oneTimes) {
    if (!("params" in oneTime)) continue;
    for (const [block, part] of oneTime.parts) {
      if (part.sent === void 0 && part.result === void 0) {
        pending.push({ oneTime: id2, block, priority: oneTime.priority });
      }
    }
  }
  return pending;
}
function current(status) {
  return status?.status !== "stale" && status?.status !== "unevaluated";
}
function withPart(oneTimes, id2, block, change) {
  const oneTime = oneTimes.get(id2);
  if (oneTime === void 0 || !("params" in oneTime)) return oneTimes;
  const part = oneTime.parts.get(block);
  if (part === void 0) return oneTimes;
  const parts = new Map(oneTime.parts).set(block, { ...part, ...change });
  return new Map(oneTimes).set(id2, { ...oneTime, parts });
}
function without2(oneTimes, ids2) {
  if (ids2.length === 0) return oneTimes;
  const kept = new Map(oneTimes);
  for (const id2 of ids2) kept.delete(id2);
  return kept;
}

// srcts/scheduler/needs.ts
function neededBlocks(graph, needs) {
  const onBoard = new Set(graph.order);
  const found = /* @__PURE__ */ new Map();
  const raise = (id2, priority) => {
    if (!onBoard.has(id2)) return;
    const was = found.get(id2);
    if (was === void 0 || priority > was) found.set(id2, priority);
  };
  for (const need of needs) {
    for (const id2 of need.blocks) raise(id2, need.priority);
  }
  for (const id2 of [...graph.order].reverse()) {
    const priority = found.get(id2);
    if (priority === void 0) continue;
    for (const up of graph.upstream(id2)) raise(up, priority);
  }
  return found;
}

// srcts/scheduler/plan.ts
var GIVE_WAY = 5e3;
function evaluationPlan(input2) {
  const { graph, revisions, statuses, needs, now } = input2;
  const sent2 = [...input2.sent];
  const asked = [...input2.asked];
  const pending = [...input2.pending];
  const index = new Map(graph.order.map((id2, i) => [id2, i]));
  const deadlines = [];
  const cancel = [];
  const cancelled2 = /* @__PURE__ */ new Set();
  const planned = /* @__PURE__ */ new Set();
  const evaluationPriority = (evaluation) => evaluation.oneTime !== void 0 ? evaluation.oneTime.priority : needs.get(evaluation.block) ?? (evaluation.background ? -1 : void 0);
  const inFlight = (id2, at) => planned.has(id2) || sent2.some(
    (evaluation) => evaluation.block === id2 && evaluation.revision === at && !evaluation.cancelled && !cancelled2.has(evaluation.id)
  );
  const needsEvaluation = (id2) => {
    const status = statuses.get(id2)?.status;
    return status === "stale" || status === "unevaluated";
  };
  const available = (id2) => {
    const at = revisions.get(id2);
    return at !== void 0 && (statuses.get(id2)?.status === "ready" || inFlight(id2, at));
  };
  const waitedSince = (evaluation, priority) => {
    let since;
    for (const other of asked) {
      if (other.id > evaluation.id) since = Math.min(since ?? other.at, other.at);
    }
    for (const other of sent2) {
      if (other.id <= evaluation.id) continue;
      if ((evaluationPriority(other) ?? -Infinity) > priority) {
        since = Math.min(since ?? other.at, other.at);
      }
    }
    return since;
  };
  const paramsFor = (id2, at) => pending.some((item) => item.block === id2) || sent2.some(
    (evaluation) => evaluation.oneTime !== void 0 && evaluation.block === id2 && evaluation.revision === at
  );
  let oldest = Infinity;
  for (const item of [...sent2, ...asked]) oldest = Math.min(oldest, item.id);
  const drop = (evaluation) => {
    cancel.push(evaluation.id);
    cancelled2.add(evaluation.id);
  };
  for (const evaluation of sent2) {
    if (evaluation.cancelled || evaluation.oneTime !== void 0) continue;
    const at = revisions.get(evaluation.block);
    if (at === void 0) continue;
    if (at !== evaluation.revision) {
      drop(evaluation);
      continue;
    }
    const priority = evaluationPriority(evaluation);
    if (priority === void 0) {
      if (evaluation.id !== oldest) drop(evaluation);
      continue;
    }
    if (!input2.interrupt || priority >= 0) continue;
    const since = waitedSince(evaluation, priority);
    if (since === void 0) continue;
    if (now - since >= GIVE_WAY) drop(evaluation);
    else deadlines.push(since + GIVE_WAY);
  }
  const queued = [];
  for (const [id2, priority] of needs) {
    const at = revisions.get(id2);
    if (at === void 0 || !needsEvaluation(id2)) continue;
    if (inFlight(id2, at) || paramsFor(id2, at)) continue;
    queued.push({ block: id2, priority });
  }
  for (const item of pending) {
    queued.push({ block: item.block, priority: item.priority, oneTime: item.oneTime });
  }
  queued.sort(
    (a, b) => (a.priority === b.priority ? 0 : a.priority > b.priority ? -1 : 1) || (index.get(a.block) ?? 0) - (index.get(b.block) ?? 0)
  );
  let slots = input2.queue + 1 - sent2.length;
  const send = [];
  const reject = [];
  const rejected = /* @__PURE__ */ new Set();
  if (!input2.holding) {
    for (const item of queued) {
      if (item.oneTime !== void 0) {
        if (rejected.has(item.oneTime)) continue;
        const status = statuses.get(item.block);
        if (status !== void 0 && keptWaiting(input2.inputs.get(item.block), statuses) !== void 0) {
          reject.push({ oneTime: item.oneTime, error: waitingError(item.block, status) });
          rejected.add(item.oneTime);
          continue;
        }
      }
      if (slots <= 0 || !graph.upstream(item.block).every(available)) continue;
      send.push(
        item.oneTime === void 0 ? { block: item.block } : { block: item.block, oneTime: item.oneTime }
      );
      planned.add(item.block);
      slots -= 1;
    }
  }
  const busy = asked.length > 0 || sent2.some((evaluation) => (evaluationPriority(evaluation) ?? 0) >= 0) || queued.some((item) => item.priority >= 0);
  let idleSince = input2.idleSince;
  if (busy || input2.holding || !Number.isFinite(input2.delay)) {
    idleSince = void 0;
  } else {
    idleSince ??= now;
    const due = idleSince + input2.delay * 1e3;
    if (now < due) {
      deadlines.push(due);
    } else if (slots > 0 && !sent2.some((evaluation) => evaluation.background)) {
      const block = inBackground();
      if (block !== void 0) send.push({ block, background: true });
    }
  }
  return {
    cancel,
    send,
    reject,
    idleSince,
    wakeAt: deadlines.length > 0 ? Math.min(...deadlines) : void 0
  };
  function inBackground() {
    for (const id2 of graph.order) {
      if (needs.has(id2) || !needsEvaluation(id2)) continue;
      const at = revisions.get(id2);
      if (at === void 0 || inFlight(id2, at)) continue;
      if (graph.upstream(id2).every((up) => statuses.get(up)?.status === "ready")) return id2;
    }
    return void 0;
  }
}

// srcts/scheduler/recipes.ts
function inputsByBlock(board, graph, registry, bound) {
  const inputs = /* @__PURE__ */ new Map();
  for (const [id2, block] of Object.entries(board.blocks)) {
    const declared = own(registry, block.registry_id)?.inputs ?? [];
    const links = graph.linksInto(id2).flatMap((link) => own(board.links, link) ?? []);
    inputs.set(
      id2,
      declared.includes("...") ? variadic(declared, links, bound) : fixed(declared, links, bound)
    );
  }
  return inputs;
}
function fixed(declared, links, bound) {
  const recipe = [];
  const unlinked = [];
  for (const name3 of declared) {
    const link = links.find((candidate) => candidate.input === name3);
    if (link !== void 0) recipe.push({ input: name3, block: link.from });
    else if (bound.has(name3)) recipe.push({ input: name3 });
    else unlinked.push(name3);
  }
  return { recipe, unlinked };
}
function variadic(declared, links, bound) {
  const recipe = links.map((link) => ({ input: link.input, block: link.from }));
  for (const name3 of bound) {
    if (!links.some((link) => link.input === name3)) recipe.push({ input: name3 });
  }
  const unlinked = recipe.length === 0 ? ["..."] : [];
  for (const name3 of declared) {
    if (name3 === "...") continue;
    if (!links.some((link) => link.input === name3) && !bound.has(name3)) unlinked.push(name3);
  }
  return { recipe, unlinked };
}
function sameInputs(a, b) {
  return a.length === b.length && a.every((input2, i) => input2.input === b[i]?.input && input2.block === b[i].block);
}

// srcts/scheduler/revisions.ts
function counter(last = 0) {
  return () => {
    if (last >= Number.MAX_SAFE_INTEGER) {
      throw new Error(
        "The page has used up its revisions, 2^53 - 1 of them, so the scheduler has stopped. Reload the page to start again."
      );
    }
    last += 1;
    return last;
  };
}
function opening(graph, next) {
  return new Map(graph.order.map((id2) => [id2, next()]));
}
function revise(revisions, before, after, removed, next) {
  const gone = [...revisions.keys()].filter(
    (id2) => !Object.hasOwn(after.board.blocks, id2) || removed.has(id2)
  );
  const left2 = new Set(gone);
  const moved = /* @__PURE__ */ new Set();
  for (const id2 of after.graph.order) {
    const was = left2.has(id2) ? void 0 : own(before.board.blocks, id2);
    const now = own(after.board.blocks, id2);
    if (was === void 0 || now === void 0) {
      moved.add(id2);
      continue;
    }
    const same2 = was.registry_id === now.registry_id && (was.state === now.state || jsonEqual(was.state, now.state)) && sameInputs(before.inputs.get(id2)?.recipe ?? [], after.inputs.get(id2)?.recipe ?? []);
    if (!same2) moved.add(id2);
  }
  const kept = new Map([...revisions].filter(([id2]) => !left2.has(id2)));
  return { revisions: bump(after.graph, kept, moved, next), gone };
}
function bump(graph, revisions, roots, next) {
  const hit = graph.withDownstream(roots);
  const result = /* @__PURE__ */ new Map();
  for (const id2 of graph.order) {
    const at = revisions.get(id2);
    result.set(id2, at === void 0 || hit.has(id2) ? next() : at);
  }
  return result;
}

// srcts/scheduler/scheduler.ts
var NO_BOARD = { blocks: {}, links: {} };
function createScheduler(options) {
  const { store, client } = options;
  const queue = setting(options.evaluationQueue, 2, "evaluation_queue", true);
  const delay = setting(options.backgroundEvaluationDelay, Infinity, "background_evaluation_delay");
  const gated = flag(options.gateVisibility, true, "gate_visibility");
  const board = signal(void 0);
  const registry = signal({});
  const bound = signal(new Set(options.bound ?? []));
  const revisions = signal(/* @__PURE__ */ new Map());
  const outcomes = signal(/* @__PURE__ */ new Map());
  const unsentEvaluations = signal(/* @__PURE__ */ new Map());
  const sent2 = signal(/* @__PURE__ */ new Map());
  const asked = signal(/* @__PURE__ */ new Map());
  const demands = signal(/* @__PURE__ */ new Map());
  const oneTimes = signal(/* @__PURE__ */ new Map());
  const holding = signal(true);
  const graph = computed(() => createGraph(board.value ?? NO_BOARD));
  const inputs = computed(
    () => inputsByBlock(board.value ?? NO_BOARD, graph.value, registry.value, bound.value)
  );
  let last = /* @__PURE__ */ new Map();
  const statuses = computed(() => {
    last = blockStatuses({
      graph: graph.value,
      revisions: revisions.value,
      outcomes: outcomes.value,
      unsent: unsentEvaluations.value,
      inputs: inputs.value,
      running: running(sent2.value),
      before: last
    }).statuses;
    return last;
  });
  const needs = computed(
    () => neededBlocks(graph.value, [
      ...[...demands.value.values()].map((demand) => ({
        blocks: demand.blocks.value,
        priority: demand.priority
      })),
      ...neededBy(oneTimes.value, graph.value)
    ])
  );
  const state2 = computed(() => [
    board.value,
    registry.value,
    bound.value,
    revisions.value,
    outcomes.value,
    unsentEvaluations.value,
    sent2.value,
    asked.value,
    demands.value,
    oneTimes.value,
    holding.value
  ]);
  const controllers = /* @__PURE__ */ new Map();
  const settles = /* @__PURE__ */ new Map();
  const subscribers = /* @__PURE__ */ new Set();
  const causes = /* @__PURE__ */ new Map();
  let ids2 = 0;
  const count = options.nextRevision ?? counter();
  let stopped;
  const ended = () => options.signal?.aborted === true;
  const endedError = () => new Error("Another board opened.");
  let idleSince;
  let planned;
  let passTimer;
  let wakeTimer;
  function revision() {
    try {
      return count();
    } catch (error) {
      stopped = error;
      throw error;
    }
  }
  function setRevisions(next) {
    const before = revisions.peek();
    revisions.value = next;
    const cause = activeSpanContext();
    for (const [id2, at] of next) {
      if (before.get(id2) === at) continue;
      if (cause === void 0) causes.delete(id2);
      else causes.set(id2, cause);
    }
    for (const id2 of [...causes.keys()]) {
      if (!next.has(id2)) causes.delete(id2);
    }
  }
  const opened = action((next) => {
    registry.value = store.blockRegistry();
    board.value = next;
    setRevisions(opening(graph.value, revision));
  });
  const applied = action((delta, next) => {
    const before = { board: board.value ?? next, inputs: inputs.value };
    board.value = next;
    const revised = revise(
      revisions.value,
      before,
      { board: next, inputs: inputs.value, graph: graph.value },
      new Set(delta.blocks?.rm ?? []),
      revision
    );
    setRevisions(revised.revisions);
    if (revised.gone.length === 0) return;
    const gone = new Set(revised.gone);
    outcomes.value = new Map([...outcomes.value].filter(([id2]) => !gone.has(id2)));
    unsentEvaluations.value = new Map([...unsentEvaluations.value].filter(([id2]) => !gone.has(id2)));
    update(left(oneTimes.value, gone));
    client.forget(revised.gone);
  });
  const lacking = action((missing) => {
    const lost = /* @__PURE__ */ new Set();
    for (const { block, revision: at } of missing) {
      if (revisions.value.get(block) === at && statuses.value.get(block)?.status === "ready") {
        lost.add(block);
      }
    }
    if (lost.size > 0) setRevisions(bump(graph.value, revisions.value, lost, revision));
  });
  function update(next) {
    oneTimes.value = next.oneTimes;
    for (const [id2, result] of next.settled) {
      const settle2 = settles.get(id2);
      settles.delete(id2);
      settle2?.(result);
    }
  }
  const end = action((id2, result) => {
    if (!oneTimes.value.has(id2)) return;
    const rest = new Map(oneTimes.value);
    rest.delete(id2);
    update({ oneTimes: rest, settled: [[id2, result]] });
  });
  function blockRecipe(id2) {
    const block = board.value === void 0 ? void 0 : own(board.value.blocks, id2);
    const recipe = inputs.value.get(id2)?.recipe;
    if (block === void 0 || recipe === void 0) return void 0;
    return {
      registry_id: block.registry_id,
      state: { ...block.state },
      inputs: recipe.map(
        (input2) => input2.block === void 0 ? { input: input2.input } : { input: input2.input, block: input2.block, revision: revisions.value.get(input2.block) ?? 0 }
      )
    };
  }
  function send(item) {
    const recipe = blockRecipe(item.block);
    const at = revisions.value.get(item.block);
    const block = board.value === void 0 ? void 0 : own(board.value.blocks, item.block);
    if (recipe === void 0 || at === void 0 || block === void 0) return;
    let oneTime;
    if (item.oneTime !== void 0) {
      const found = oneTimes.value.get(item.oneTime);
      if (found === void 0 || !("params" in found)) return;
      oneTime = { id: item.oneTime, priority: found.priority, params: found.params };
    }
    const id2 = ++ids2;
    sent2.value = new Map(sent2.value).set(id2, {
      id: id2,
      at: Date.now(),
      block: item.block,
      revision: at,
      oneTime: oneTime === void 0 ? void 0 : { id: oneTime.id, priority: oneTime.priority },
      background: item.background === true,
      cancelled: false
    });
    if (oneTime !== void 0) {
      oneTimes.value = sent(oneTimes.value, oneTime.id, item.block, { id: id2, recipe });
    }
    const controller = new AbortController();
    controllers.set(id2, controller);
    const cause = causes.get(item.block);
    client.request(
      {
        kind: "evaluate",
        block: item.block,
        revision: at,
        locked: block.locked,
        recipe,
        ...oneTime === void 0 ? {} : { params: oneTime.params }
      },
      {
        signal: controller.signal,
        span: { parent: ROOT_CONTEXT, ...cause === void 0 ? {} : { links: [{ context: cause }] } }
      }
    ).then(
      ({ reply }) => {
        answered2(id2, reply);
      },
      (error) => {
        unanswered(id2, error);
      }
    );
  }
  function cancel(id2) {
    const evaluation = sent2.value.get(id2);
    if (evaluation === void 0) return;
    sent2.value = new Map(sent2.value).set(id2, { ...evaluation, cancelled: true });
    controllers.get(id2)?.abort();
  }
  function done(id2) {
    const evaluation = sent2.value.get(id2);
    if (evaluation === void 0) return void 0;
    const rest = new Map(sent2.value);
    rest.delete(id2);
    sent2.value = rest;
    controllers.delete(id2);
    return evaluation;
  }
  const answered2 = action((id2, reply) => {
    const evaluation = done(id2);
    if (evaluation === void 0 || ended()) return;
    if (reply.status === "missing") lacking(reply.missing);
    const outcome = replyOutcome(reply, evaluation.revision);
    if (outcome !== void 0 && evaluation.revision === revisions.value.get(evaluation.block) && keptWaiting(inputs.value.get(evaluation.block), statuses.value) === void 0) {
      outcomes.value = new Map(outcomes.value).set(evaluation.block, outcome);
      if (unsentEvaluations.value.has(evaluation.block)) {
        const rest = new Map(unsentEvaluations.value);
        rest.delete(evaluation.block);
        unsentEvaluations.value = rest;
      }
    }
    if (evaluation.oneTime !== void 0) {
      update(
        answered(
          oneTimes.value,
          evaluation.oneTime.id,
          evaluation.block,
          reply,
          outcome,
          evaluation.revision
        )
      );
    }
  });
  const unanswered = action((id2, error) => {
    const evaluation = done(id2);
    if (evaluation === void 0 || ended()) return;
    if (evaluation.oneTime !== void 0) {
      end(evaluation.oneTime.id, { error });
    } else if (!(error instanceof SessionEndedError) && revisions.value.get(evaluation.block) === evaluation.revision) {
      unsentEvaluations.value = new Map(unsentEvaluations.value).set(
        evaluation.block,
        unsent(evaluation.revision, errorMessage(error))
      );
    }
  });
  function schedule() {
    passTimer ??= setTimeout(pass, 0);
  }
  function wake(at) {
    if (wakeTimer !== void 0) clearTimeout(wakeTimer);
    wakeTimer = void 0;
    if (at === void 0) return;
    wakeTimer = setTimeout(
      () => {
        wakeTimer = void 0;
        pass();
      },
      Math.max(0, at - Date.now())
    );
  }
  function pass() {
    passTimer = void 0;
    batch(() => {
      if (stopped === void 0 && board.value !== void 0) {
        const plan = evaluationPlan({
          graph: graph.value,
          revisions: revisions.value,
          statuses: statuses.value,
          inputs: inputs.value,
          needs: needs.value,
          sent: sent2.value.values(),
          asked: asked.value.values(),
          pending: pendingEvaluations(oneTimes.value),
          queue,
          interrupt: client.capabilities().has("interrupt"),
          holding: holding.value,
          now: Date.now(),
          idleSince,
          delay
        });
        idleSince = plan.idleSince;
        for (const id2 of plan.cancel) cancel(id2);
        for (const { oneTime, error } of plan.reject) end(oneTime, { error });
        for (const item of plan.send) send(item);
        wake(plan.wakeAt);
      }
      planned = state2.value;
    });
  }
  function resendable(request) {
    if (request.kind === "output" || request.output !== void 0) return true;
    if (request.registry_id === void 0) return false;
    const entry = own(store.blockRegistry(), request.registry_id);
    return entry === void 0 || own(entry.requests, request.name)?.action !== true;
  }
  function demanding(id2, owner, blocks, priority) {
    let demanded;
    if (typeof blocks === "function") {
      demanded = computed(() => {
        const of = board.value;
        if (of === void 0) return /* @__PURE__ */ new Set();
        return untracked(() => {
          try {
            return new Set(blocks(of));
          } catch (error) {
            log.error(
              new Error(
                `The demand of ${owner} threw, so it demands nothing until it returns: ${errorMessage(error)}`,
                { cause: error }
              )
            );
            return /* @__PURE__ */ new Set();
          }
        });
      });
    } else {
      demanded = signal(new Set(blocks));
    }
    demands.value = new Map(demands.value).set(id2, { owner, priority, blocks: demanded });
  }
  if (!gated) {
    demanding(++ids2, "blockr.core", (all) => Object.keys(all.blocks), 0);
  }
  if (store.hasBoard()) opened(store.board());
  let heard = /* @__PURE__ */ new Map();
  const dispose = effect(() => {
    const now = statuses.value;
    const settled2 = settle(oneTimes.value, now);
    if (settled2.settled.length > 0) {
      untracked(() => {
        update(settled2);
      });
    }
    const changed = changedSince(heard, now);
    heard = now;
    if (changed.length > 0) {
      untracked(() => {
        for (const fn of [...subscribers]) {
          try {
            fn(changed);
          } catch (error) {
            log.error(asError(error));
          }
        }
      });
    }
    if (state2.value !== planned) schedule();
  });
  const offStore = store.subscribe((change, next) => {
    if ("opened" in change || board.peek() === void 0) opened(next);
    else applied(change.delta, next);
  });
  function stop() {
    stopped = endedError();
    if (passTimer !== void 0) clearTimeout(passTimer);
    passTimer = void 0;
    wake(void 0);
    dispose();
    offStore();
    subscribers.clear();
    const error = endedError();
    update({ oneTimes: /* @__PURE__ */ new Map(), settled: [...oneTimes.peek().keys()].map((id2) => [id2, { error }]) });
  }
  if (ended()) stop();
  else options.signal?.addEventListener("abort", stop, { once: true });
  return {
    demand: action((owner, blocks, { priority } = {}) => {
      const id2 = ++ids2;
      demanding(id2, owner, blocks, checked(priority));
      return {
        set: action((next, { priority: given } = {}) => {
          const demand = demands.value.get(id2);
          if (demand === void 0) {
            throw new Error(`The demand of ${owner} was released, so it can no longer be set.`);
          }
          demanding(id2, owner, next, given === void 0 ? demand.priority : checked(given));
        }),
        release: action(() => {
          if (!demands.value.has(id2)) return;
          const rest = new Map(demands.value);
          rest.delete(id2);
          demands.value = rest;
        })
      };
    }),
    evaluate: action((blocks, params, { priority, signal: abort } = {}) => {
      if (ended()) return Promise.reject(endedError());
      const at = priority ?? 0;
      const invalid = priorityError(at);
      if (invalid !== void 0) return Promise.reject(invalid);
      if (abort?.aborted === true) return Promise.reject(asError(abort.reason));
      return new Promise((resolve, reject) => {
        const id2 = ++ids2;
        const onAbort2 = () => {
          end(id2, { error: abort?.reason });
        };
        abort?.addEventListener("abort", onAbort2, { once: true });
        settles.set(id2, (result) => {
          abort?.removeEventListener("abort", onAbort2);
          if ("error" in result) reject(asError(result.error));
          else resolve(result.statuses);
        });
        const oneTime = createOneTime([...new Set(blocks)], params, at, statuses.value, inputs.value);
        if (oneTime instanceof Error) {
          update({ oneTimes: oneTimes.value, settled: [[id2, { error: oneTime }]] });
        } else {
          oneTimes.value = new Map(oneTimes.value).set(id2, oneTime);
        }
      });
    }),
    invalidate: action((id2) => {
      if (!revisions.value.has(id2)) return;
      setRevisions(bump(graph.value, revisions.value, /* @__PURE__ */ new Set([id2]), revision));
    }),
    status: (id2) => statuses.peek().get(id2),
    subscribe(fn) {
      const wrapped = (changed) => {
        fn(changed);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },
    async request(request, requestOptions = {}) {
      const inCaller = keepContext();
      const block = request.kind === "output" ? own(board.peek()?.blocks ?? {}, request.block) : void 0;
      const span = {
        ...requestOptions.span,
        attributes: {
          ...block === void 0 ? {} : { "blockr.block.registry_id": block.registry_id },
          ...requestOptions.span?.attributes
        }
      };
      for (; ; ) {
        if (ended()) throw endedError();
        const id2 = ++ids2;
        asked.value = new Map(asked.peek()).set(id2, { id: id2, at: Date.now() });
        try {
          const result = await inCaller(() => client.request(request, { ...requestOptions, span }));
          if (result.reply.status === "missing") lacking(result.reply.missing);
          return result;
        } catch (error) {
          if (error instanceof SessionEndedError && resendable(request) && !ended()) continue;
          throw error;
        } finally {
          const rest = new Map(asked.peek());
          rest.delete(id2);
          asked.value = rest;
        }
      }
    },
    inputs: (id2) => inputs.peek().get(id2),
    start: action(() => {
      holding.value = false;
    }),
    session: action(() => {
      setRevisions(bump(graph.value, revisions.value, new Set(revisions.value.keys()), revision));
    })
  };
}
function running(sent2) {
  return new Set([...sent2.values()].map((evaluation) => evaluation.block));
}
function changedSince(before, now) {
  const changed = [];
  for (const [id2, status] of now) {
    if (before.get(id2) !== status) changed.push(id2);
  }
  for (const id2 of before.keys()) {
    if (!now.has(id2)) changed.push(id2);
  }
  return changed;
}
function setting(value2, fallback, name3, counts = false) {
  if (value2 === void 0) return fallback;
  const number = typeof value2 === "string" ? asNumber(value2) : value2;
  if (number === -1) return Infinity;
  if (typeof number === "number" && Number.isFinite(number) && number >= 0 && (!counts || Number.isInteger(number))) {
    return number;
  }
  throw new RangeError(
    `The blockr option ${name3} is ${shown(value2)}, where it takes ${counts ? "a whole number" : "a number"} of 0 or more, or -1.`
  );
}
function flag(value2, fallback, name3) {
  if (value2 === void 0) return fallback;
  const logical = typeof value2 === "string" ? asLogical(value2) : value2;
  if (typeof logical === "boolean") return logical;
  throw new TypeError(`The blockr option ${name3} is ${shown(value2)}, where it takes TRUE or FALSE.`);
}
function shown(value2) {
  return typeof value2 === "number" ? String(value2) : JSON.stringify(value2);
}
function checked(priority) {
  const at = priority ?? 0;
  const invalid = priorityError(at);
  if (invalid !== void 0) throw invalid;
  return at;
}
function priorityError(priority) {
  return typeof priority !== "number" || Number.isNaN(priority) ? new TypeError(`An evaluation priority is a number, not ${String(priority)}.`) : void 0;
}

// srcts/mount/answers.ts
function createAnswers(scheduler, signal5) {
  const kept = /* @__PURE__ */ new Map();
  const flights = /* @__PURE__ */ new Map();
  signal5?.addEventListener(
    "abort",
    () => {
      kept.clear();
      const given = [...flights.values()];
      flights.clear();
      for (const flight of given) flight.controller.abort();
    },
    { once: true }
  );
  scheduler.subscribe(() => {
    sweep();
  });
  function current2(request) {
    return requestReads(request).every(
      (read) => read.block === void 0 || scheduler.status(read.block)?.revision === read.revision
    );
  }
  function sweep() {
    for (const [key2, { request }] of kept) {
      if (!current2(request)) kept.delete(key2);
    }
  }
  function start2(ask, key2) {
    const controller = new AbortController();
    const progress = /* @__PURE__ */ new Set();
    const done = scheduler.request(ask.request, {
      ...ask.body === void 0 ? {} : { body: ask.body },
      signal: controller.signal,
      progress(note) {
        for (const caller of [...progress]) {
          try {
            caller.fn(note);
          } catch (error) {
            log.error(asError(error));
          }
        }
      }
    }).then((answer) => {
      if (key2 !== void 0 && ask.share === "keep" && answer.reply.status === "ok" && current2(ask.request)) {
        kept.set(key2, { answer, request: ask.request });
      }
      return answer;
    }).finally(() => {
      if (key2 !== void 0 && flights.get(key2) === flight) flights.delete(key2);
    });
    done.catch(() => void 0);
    const flight = { done, controller, progress, callers: 0 };
    if (key2 !== void 0) flights.set(key2, flight);
    return flight;
  }
  function join(flight, key2, signal6, progress) {
    flight.callers += 1;
    const listener = progress === void 0 ? void 0 : { fn: progress };
    if (listener !== void 0) flight.progress.add(listener);
    return new Promise((resolve, reject) => {
      let left2 = false;
      const leave2 = () => {
        left2 = true;
        flight.callers -= 1;
        if (listener !== void 0) flight.progress.delete(listener);
        signal6?.removeEventListener("abort", abort);
      };
      const abort = () => {
        leave2();
        reject(asError(signal6?.reason));
        if (flight.callers === 0) {
          if (key2 !== void 0 && flights.get(key2) === flight) flights.delete(key2);
          flight.controller.abort();
        }
      };
      signal6?.addEventListener("abort", abort, { once: true });
      flight.done.then(
        (answer) => {
          if (left2) return;
          leave2();
          resolve(answer);
        },
        (error) => {
          if (left2) return;
          leave2();
          reject(asError(error));
        }
      );
    });
  }
  return {
    ask(ask, { signal: signal6, progress } = {}) {
      if (signal6?.aborted === true) return Promise.reject(asError(signal6.reason));
      const key2 = ask.share === "once" ? void 0 : requestKey(ask.request);
      const answer = key2 === void 0 ? void 0 : kept.get(key2)?.answer;
      if (answer !== void 0) return Promise.resolve(answer);
      const flight = (key2 === void 0 ? void 0 : flights.get(key2)) ?? start2(ask, key2);
      return join(flight, key2, signal6, progress);
    },
    size: () => kept.size
  };
}
function requestKey(request) {
  if (request.type === "block_class") {
    return JSON.stringify(
      sorted([
        request.type,
        request.registry_id,
        request.name,
        request.args,
        request.options ?? {},
        request.reads
      ])
    );
  }
  return JSON.stringify(
    sorted([
      request.type,
      request.type === "renderer" ? request.renderer : null,
      request.output,
      request.name,
      request.args,
      request.options,
      request.params
    ])
  );
}
function requestReads(request) {
  return request.type === "block_class" ? request.reads : [request.output];
}
function sorted(x) {
  if (Array.isArray(x)) return x.map(sorted);
  if (isObject(x)) {
    const object2 = x;
    return Object.fromEntries(
      Object.keys(object2).sort().map((name3) => [name3, sorted(object2[name3] ?? null)])
    );
  }
  return x;
}

// srcts/mount/load.ts
var API = 1;
var ModuleError = class extends Error {
  module;
  package;
  constructor(module, pkg, what, options) {
    const name3 = module.replace(/[?#].*$/, "");
    super(`The module ${name3} ${pkg === null ? "from no package" : `of ${pkg}`} ${what}`, options);
    this.name = "ModuleError";
    this.module = name3;
    this.package = pkg;
  }
};
function createLoader(options = {}) {
  const importer = options.importer ?? importModule;
  const stylesheet = options.stylesheet ?? linkStylesheet;
  const modules = options.modules ?? (() => ({}));
  const changed = options.changed ?? (() => void 0);
  const imported = /* @__PURE__ */ new Map();
  const sheets = /* @__PURE__ */ new Map();
  function once2(cache, url, load2) {
    let loading = cache.get(url);
    if (loading === void 0) {
      loading = load2(url);
      cache.set(url, loading);
    }
    return loading;
  }
  async function load(source, pkg) {
    if (pkg !== null && !imported.has(source.module)) {
      const versions = changed(pkg);
      if (versions !== void 0) {
        throw new ModuleError(
          source.module,
          pkg,
          `is not loaded: R now runs ${pkg} ${versions.now}, where the page loaded ${versions.loaded}. Reload the page to load it.`
        );
      }
    }
    const sheet = source.css === void 0 ? Promise.resolve() : once2(sheets, source.css, stylesheet).catch((error) => {
      log.error(new ModuleError(source.module, pkg, `has a stylesheet that failed to load: ${errorMessage(error)}`, { cause: error }));
    });
    const exports = once2(imported, source.module, importer);
    await sheet;
    try {
      return await exports;
    } catch (error) {
      throw new ModuleError(source.module, pkg, `failed to load: ${errorMessage(error)}`, { cause: error });
    }
  }
  return {
    load,
    module(id2) {
      const source = own(modules(), id2);
      if (source === void 0) {
        return Promise.reject(new Error(`No package offers a module "${id2}".`));
      }
      return load(source, packageFromId(id2));
    }
  };
}
function importModule(url) {
  return import(
    /* @vite-ignore */
    new URL(url, document.baseURI).href
  );
}
function linkStylesheet(url) {
  return new Promise((resolve, reject) => {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = url;
    link.addEventListener("load", () => {
      resolve();
    }, { once: true });
    link.addEventListener("error", () => {
      reject(new Error(`${url} could not be loaded.`));
    }, { once: true });
    document.head.append(link);
  });
}

// srcts/mount/measure.ts
var FALLBACK = { bg: "#ffffff", fg: "#000000", accent: "#155dfc" };
var ACCENT = "--bc-color-bg-accent";
function domMeasure() {
  return {
    info(el) {
      const rect = el.getBoundingClientRect();
      const width = Math.round(rect.width);
      const height = Math.round(rect.height);
      if (width <= 0 || height <= 0) return null;
      const style = getComputedStyle(el);
      return {
        width,
        height,
        pixel_ratio: window.devicePixelRatio > 0 ? window.devicePixelRatio : 1,
        bg: visibleBackground(el) ?? FALLBACK.bg,
        fg: asHex(style.color) ?? FALLBACK.fg,
        accent: asHex(style.getPropertyValue(ACCENT).trim()) ?? FALLBACK.accent,
        font: style.fontFamily
      };
    },
    watch(el, fn) {
      const stops = [];
      if (typeof ResizeObserver === "function") {
        const resized = new ResizeObserver(() => {
          fn();
        });
        resized.observe(el);
        stops.push(() => {
          resized.disconnect();
        });
      }
      const themed = new MutationObserver(() => {
        fn();
      });
      themed.observe(document.documentElement, {
        attributes: true,
        attributeFilter: ["data-bc-theme", "class", "style"]
      });
      stops.push(() => {
        themed.disconnect();
      });
      stops.push(watchRatio(fn));
      return () => {
        for (const stop of stops) stop();
      };
    }
  };
}
function watchRatio(fn) {
  if (typeof matchMedia !== "function") return () => void 0;
  let query;
  const changed = () => {
    listen();
    fn();
  };
  function listen() {
    query?.removeEventListener("change", changed);
    query = matchMedia(`(resolution: ${String(window.devicePixelRatio)}dppx)`);
    query.addEventListener("change", changed);
  }
  listen();
  return () => {
    query?.removeEventListener("change", changed);
  };
}
var WHITE = [255, 255, 255, 1];
function visibleBackground(el) {
  const layers = [];
  for (let at = el; at !== null; at = at.parentElement) {
    const value2 = getComputedStyle(at).backgroundColor;
    if (value2 === "") continue;
    const colour = asRgba(value2);
    if (colour === null) return null;
    if (colour[3] >= 1) return hex(layers.reduceRight(over, colour));
    if (colour[3] > 0) layers.push(colour);
  }
  return hex(layers.reduceRight(over, behindPage()));
}
function behindPage() {
  const probe = document.createElement("div");
  probe.style.cssText = "display: none; background-color: Canvas";
  document.documentElement.append(probe);
  const colour = asRgba(getComputedStyle(probe).backgroundColor);
  probe.remove();
  return colour !== null && colour[3] >= 1 ? colour : WHITE;
}
function over(behind, [r, g, b, alpha]) {
  const mix = (front, back) => front * alpha + back * (1 - alpha);
  return [mix(r, behind[0]), mix(g, behind[1]), mix(b, behind[2]), 1];
}
function asHex(colour) {
  const rgba = asRgba(colour);
  return rgba === null ? null : hex(rgba);
}
var RGB = /^rgba?\(\s*(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)(?:\s*[,/]\s*(\d+(?:\.\d+)?)(%?))?\s*\)$/;
function asRgba(colour) {
  const text = colour.trim();
  if (text === "") return null;
  if (text === "transparent") return [0, 0, 0, 0];
  const long = /^#[0-9a-f]{3}$/i.test(text) ? text.replace(/[0-9a-f]/gi, "$&$&") : text;
  if (/^#[0-9a-f]{6}$/i.test(long)) {
    const [r = 0, g = 0, b = 0] = [1, 3, 5].map((at) => parseInt(long.slice(at, at + 2), 16));
    return [r, g, b, 1];
  }
  const rgb = RGB.exec(text);
  if (rgb !== null) {
    const [, r = "0", g = "0", b = "0", alpha = "1", percent] = rgb;
    return [Number(r), Number(g), Number(b), Math.min(1, Number(alpha) / (percent === "%" ? 100 : 1))];
  }
  return drawn(text);
}
function hex([r, g, b]) {
  return `#${[r, g, b].map((c) => Math.round(Math.min(255, Math.max(0, c))).toString(16).padStart(2, "0")).join("")}`;
}
function drawn(colour) {
  try {
    const context4 = document.createElement("canvas").getContext("2d", { willReadFrequently: true });
    if (context4 === null) return null;
    context4.fillStyle = "#000000";
    context4.fillStyle = colour;
    context4.fillRect(0, 0, 1, 1);
    const [r = 0, g = 0, b = 0, alpha = 0] = context4.getImageData(0, 0, 1, 1).data;
    return [r, g, b, alpha / 255];
  } catch {
    return null;
  }
}

// srcts/mount/requests.ts
var ATTEMPTS = 3;
var RequestError = class extends Error {
  status;
  conditions;
  inputs;
  blocks;
  constructor(message, fields = {}) {
    const conditions = fields.conditions ?? [];
    super([message, ...conditions.map((condition) => `- ${condition.message}`)].join("\n"));
    this.name = "RequestError";
    this.status = fields.status ?? null;
    this.conditions = conditions;
    this.inputs = fields.inputs ?? [];
    this.blocks = fields.blocks ?? [];
  }
};
async function blockRequest(deps, block, entry, name3, given, opts) {
  const spec = own(entry.requests, name3);
  if (spec === void 0) {
    throw new Error(`The block class "${entry.id}" has no request "${name3}".`);
  }
  if (!isObject(given)) {
    throw new TypeError(`The args of the request "${name3}" are an object of state args and parameters.`);
  }
  if (opts.file !== void 0 && spec.file !== true) {
    throw new TypeError(
      `The request "${name3}" of the block class "${entry.id}" takes no file, since its new_block_request() names no argument for one.`
    );
  }
  const notifier = givenNotifier(opts, `the request "${name3}" of block "${block}"`);
  const share = spec.action === true || opts.file !== void 0 ? "once" : spec.volatile === true ? "merge" : "keep";
  const what = `The request "${name3}" of block "${block}"`;
  const module = sourceModule({ block }, packageFromId(entry.id));
  const inCaller = keepContext();
  for (let attempt3 = 1; ; attempt3++) {
    const reads = await evaluateReads(deps, block, entry, name3, spec.inputs, opts.signal);
    if (!deps.bound()) throw deps.unboundError();
    const board = deps.store.board();
    const state2 = own(board.blocks, block)?.state;
    if (state2 === void 0) throw new Error(`Block "${block}" left the board, so its request "${name3}" did not go to R.`);
    const args = {};
    for (const arg of spec.state_args) {
      const value2 = own(state2, arg);
      if (value2 !== void 0 && !Object.hasOwn(given, arg)) args[arg] = value2;
    }
    for (const [arg, value2] of Object.entries(given)) {
      if (value2 !== void 0) args[arg] = value2;
    }
    const options = {};
    for (const id2 of spec.options) {
      const value2 = own(board.options, id2);
      if (value2 !== void 0) options[id2] = value2;
    }
    const request = {
      kind: "request",
      type: "block_class",
      registry_id: entry.id,
      block,
      name: name3,
      args,
      reads,
      ...spec.options.length === 0 ? {} : { options }
    };
    const { reply, body } = await inCaller(
      () => deps.answers.ask(
        { request, share, ...opts.file === void 0 ? {} : { body: opts.file } },
        {
          ...opts.signal === void 0 ? {} : { signal: opts.signal },
          ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
        }
      )
    );
    if (!deps.bound()) throw deps.unboundError();
    const conditions = "conditions" in reply ? reply.conditions : [];
    switch (reply.status) {
      case "ok":
        deps.replied(module, { type: "block_class", name: name3 }, conditions, notifier);
        if (body !== null) return body;
        return "value" in reply && reply.value !== void 0 ? reply.value : null;
      case "missing":
        if (attempt3 < ATTEMPTS) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, {
          status: "missing"
        });
      case "cancelled":
        throw new RequestError(`R cancelled the request "${name3}" of block "${block}".`, { status: "cancelled" });
      default:
        deps.replied(module, { type: "block_class", name: name3 }, conditions, notifier);
        throw new RequestError(`R replied ${reply.status} to the request "${name3}" of block "${block}".`, {
          status: reply.status,
          conditions
        });
    }
  }
}
async function evaluateReads(deps, block, entry, name3, names, signal5) {
  const what = `The request "${name3}" of block "${block}"`;
  if (names.length === 0) return [];
  const inputs = deps.scheduler.inputs(block);
  if (inputs === void 0) throw new Error(`Block "${block}" left the board, so its request "${name3}" did not go to R.`);
  const unlinked = inputs.unlinked.filter((input2) => names.includes(input2));
  if (unlinked.length > 0) {
    throw new RequestError(`${what} did not go to R, since no link feeds its ${unlinked.length === 1 ? "input" : "inputs"} ${quoted(unlinked)}.`);
  }
  const read = inputs.recipe.filter(
    (input2) => names.includes(input2.input) || names.includes("...") && !entry.inputs.includes(input2.input)
  );
  const upstream = [...new Set(read.flatMap((input2) => input2.block ?? []))];
  if (upstream.length === 0) return read.map((input2) => ({ input: input2.input }));
  const statuses = await deps.scheduler.evaluate(upstream, void 0, signal5 === void 0 ? {} : { signal: signal5 });
  const unready = read.flatMap((input2) => {
    if (input2.block === void 0) return [];
    const status = own(statuses, input2.block);
    return status === void 0 || status.status === "ready" ? [] : [{ input: input2.input, status }];
  });
  if (unready.length > 0) {
    const listed = unready.map(({ input: input2, status }) => `"${input2}" reads ${status.status}`);
    throw new RequestError(`${what} did not go to R, since an input it reads is not ready: ${listed.join(", ")}.`, {
      inputs: unready
    });
  }
  return read.map(
    (input2) => input2.block === void 0 ? { input: input2.input } : { input: input2.input, block: input2.block, revision: own(statuses, input2.block)?.revision ?? 0 }
  );
}

// srcts/mount/shared.ts
function moduleExports(exports, module, pkg) {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new ModuleError(module, pkg, "declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new ModuleError(
      module,
      pkg,
      `was built against major ${String(exports.api)} of core's JS API, where core's is ${String(API)}.`
    );
  }
  return exports;
}
function mountableModule(exports, module, pkg) {
  const mod = moduleExports(exports, module, pkg).default;
  if (!isObject(mod) || typeof mod.render !== "function") {
    throw new ModuleError(module, pkg, "has no default export with a render() function.");
  }
  return mod;
}
function optionReader(values2, named4, unnamed) {
  return (id2) => {
    if (!named4.includes(id2)) throw new Error(unnamed(id2));
    return own(values2, id2);
  };
}
function withoutOptions(module, ids2) {
  return `Without the board option${ids2.length === 1 ? "" : "s"} ${quoted(ids2)}, ${module} cannot draw.`;
}
function showError(el, message) {
  const box = document.createElement("div");
  box.setAttribute("role", "alert");
  box.className = "bc:rounded bc:border bc:border-border-danger bc:p-2 bc:font-sans bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap";
  box.textContent = message;
  el.replaceChildren(box);
}
function attempt(fn) {
  try {
    fn();
  } catch (error) {
    log.error(asError(error));
  }
}
function onAbort(signal5, fn) {
  signal5.addEventListener("abort", fn, { once: true });
  return () => {
    signal5.removeEventListener("abort", fn);
  };
}
var REPLACED = "Another board opened, so core mounts nothing more of the board before.";
function announceTo(listeners, ...args) {
  for (const fn of [...listeners]) {
    attempt(() => {
      fn(...args);
    });
  }
}

// srcts/mount/option.ts
function mountOption(deps, el, option) {
  const { store, loader } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const def = own(store.options.registered(), option);
  if (def === void 0) {
    throw new Error(`No package registers the board option "${option}" whose UI to mount.`);
  }
  const chosen = Object.entries(deps.renderers()).find(([, renderer2]) => renderer2.option === option);
  const [id2, renderer] = chosen ?? [];
  const module = id2 === void 0 ? sourceModule({ option }, deps.optionPackage(option)) : sourceModule({ option }, packageFromId(id2), id2);
  const controller = new AbortController();
  const ended = () => controller.signal.aborted;
  const listeners = /* @__PURE__ */ new Set();
  let value2;
  let seen = store.board().options;
  let cleanup;
  let stopKeeping;
  const notified = /* @__PURE__ */ new Set();
  function bound() {
    return !ended() && !deps.replaced.aborted;
  }
  const unboundError = () => new Error(`Another board opened, so the host of the board option "${option}" no longer changes it.`);
  function current2() {
    return own(store.board().options, option);
  }
  const host = {
    option,
    value: () => value2,
    on(event, fn) {
      const heard = event;
      if (heard !== "value") {
        throw new Error(`The host of a board option announces "value", not ${JSON.stringify(event)}.`);
      }
      const wrapped = () => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    async submit(next, opts) {
      if (!bound()) throw unboundError();
      const notifier = givenNotifier(opts, `a submit of the board option "${option}"`);
      try {
        await deps.commands.run("blockr.core-set_option", { id: option, value: next });
      } catch (error) {
        deps.refused(error, { option }, notifier);
        throw error;
      }
    },
    request(name3, args = {}, opts = {}) {
      if (typeof name3 !== "string" || name3 === "") {
        return Promise.reject(new TypeError("A request of a board option is named by a non-empty string."));
      }
      if (!isObject(args)) return Promise.reject(new TypeError(`The args of the request "${name3}" are an object.`));
      if (!bound()) return Promise.reject(unboundError());
      return optionRequest(deps, module, option, name3, args, opts);
    },
    notify(notification) {
      const given = checkRaised(notification);
      if (ended()) return () => void 0;
      const withdraw = deps.notify(module, given);
      const mine = () => {
        notified.delete(mine);
        withdraw();
      };
      notified.add(mine);
      return mine;
    },
    log: createLog(module.package ?? null)
  };
  const offStore = store.subscribe((_change, board) => {
    if (board.options === seen) return;
    seen = board.options;
    const next = current2();
    if (next === void 0 || next === value2) return;
    value2 = next;
    announceTo(listeners);
  });
  const offEnd = onAbort(deps.replaced, end);
  function end() {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    stopKeeping?.();
    listeners.clear();
    for (const withdraw of [...notified]) withdraw();
    if (cleanup !== void 0) attempt(cleanup);
    el.replaceChildren();
  }
  void (async () => {
    let mod;
    try {
      if (renderer === void 0 || id2 === void 0) {
        mod = def.ui;
      } else {
        const source = renderer.css === void 0 ? { module: renderer.module } : { module: renderer.module, css: renderer.css };
        mod = mountableModule(await loader.load(source, packageFromId(id2)), renderer.module, packageFromId(id2));
      }
    } catch (error) {
      fail(error);
      return;
    }
    if (ended()) return;
    const kept = deps.keepOptions([option], `the UI of the board option "${option}"`, end);
    stopKeeping = kept.stop;
    const failures = await kept.added;
    if (ended()) return;
    value2 = current2();
    if (value2 === void 0 && failures.length > 0) {
      kept.stop();
      return;
    }
    try {
      const rendered = await mod.render({ el, host, signal: controller.signal });
      if (typeof rendered !== "function") return;
      if (ended()) attempt(rendered);
      else cleanup = rendered;
    } catch (error) {
      const why = `failed in render(): ${errorMessage(error)}`;
      fail(
        renderer === void 0 || id2 === void 0 ? new Error(`The UI of the board option "${option}" ${why}`, { cause: error }) : new ModuleError(renderer.module, packageFromId(id2), why, { cause: error })
      );
    }
  })();
  function fail(error) {
    if (ended()) return;
    log.error(asError(error));
    deps.raise(coreNotification("module_failed", "error", errorMessage(error), { module }));
  }
  return end;
}
async function optionRequest(deps, module, option, name3, args, opts) {
  const notifier = givenNotifier(opts, `the request "${name3}" of the board option "${option}"`);
  const { reply, body } = await deps.scheduler.request(
    { kind: "request", type: "option", option, name: name3, args: { ...args } },
    {
      ...opts.file === void 0 ? {} : { body: opts.file },
      ...opts.signal === void 0 ? {} : { signal: opts.signal },
      ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
    }
  );
  const conditions = "conditions" in reply ? reply.conditions : [];
  switch (reply.status) {
    case "ok":
      deps.replied(module, { type: "option", name: name3 }, conditions, notifier);
      if (body !== null) return body;
      return "value" in reply && reply.value !== void 0 ? reply.value : null;
    case "cancelled":
      throw new RequestError(`R cancelled the request "${name3}" of the board option "${option}".`, { status: "cancelled" });
    default:
      deps.replied(module, { type: "option", name: name3 }, conditions, notifier);
      throw new RequestError(`R replied ${reply.status} to the request "${name3}" of the board option "${option}".`, {
        status: reply.status,
        conditions
      });
  }
}

// srcts/mount/output.ts
var SETTLE_MS = 150;
var ATTEMPTS2 = 3;
var EMPTY = Object.freeze({});
function createShelves() {
  const shelves = /* @__PURE__ */ new Map();
  const giveUp = (shelf) => {
    for (const flight of shelf.flights.values()) flight.controller.abort();
    shelf.flights.clear();
  };
  return {
    of(block) {
      let shelf = shelves.get(block);
      if (shelf === void 0) {
        shelf = { params: {}, last: void 0, notified: [], mounts: /* @__PURE__ */ new Set(), flights: /* @__PURE__ */ new Map() };
        shelves.set(block, shelf);
      }
      return shelf;
    },
    drop(block) {
      const shelf = shelves.get(block);
      if (shelf === void 0) return;
      giveUp(shelf);
      shelves.delete(block);
    }
  };
}
function coreFailure(code, message) {
  return { kind: "core", code, message };
}
function whyFailed(reply, block) {
  const conditions = "conditions" in reply ? reply.conditions : [];
  const error = conditions.find((condition) => condition.severity === "error");
  return error ?? coreFailure("output_unanswered", `R failed to make the output of block "${block}".`);
}
function mountOutput(deps, el, block) {
  const { store, scheduler, loader, measure } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const found = own(store.board().blocks, block);
  if (found === void 0) {
    throw new Error(`The board holds no block "${block}" whose output to mount.`);
  }
  const entry = own(store.blockRegistry(), found.registry_id);
  if (entry === void 0) {
    throw new Error(`The block registry has no class "${found.registry_id}" for block "${block}".`);
  }
  const shelf = deps.shelves.of(block);
  const controller = new AbortController();
  const ended = () => controller.signal.aborted;
  const outputOptions = entry.output_options ?? [];
  const registryId2 = entry.id;
  const expected = /* @__PURE__ */ new Map();
  const ready = /* @__PURE__ */ new Map();
  const preparing = /* @__PURE__ */ new Map();
  let info = measure.info(el);
  let held2;
  let asking;
  let error = null;
  let drawing;
  let options = store.board().options;
  const waiting2 = /* @__PURE__ */ new Set();
  const broken = /* @__PURE__ */ new WeakSet();
  function bound() {
    return !ended() && !deps.replaced.aborted && Object.hasOwn(store.board().blocks, block);
  }
  const unboundError = () => new Error(
    deps.replaced.aborted ? `Another board opened, so the host of the output of block "${block}" no longer asks R.` : `Block "${block}" left the board, so the host of its output no longer asks R.`
  );
  const rendererModule = (renderer) => sourceModule({ block }, packageFromId(renderer.id), renderer.id);
  function prepare(given) {
    let loading = preparing.get(given.id);
    if (loading === void 0) {
      loading = (async () => {
        const pkg = packageFromId(given.id);
        const source = given.css === void 0 ? { module: given.module } : { module: given.module, css: given.css };
        const mod = mountableModule(await loader.load(source, pkg), given.module, pkg);
        return { renderer: given, mod };
      })();
      preparing.set(given.id, loading);
    }
    return loading;
  }
  function expect(given) {
    if (expected.has(given.id)) return;
    expected.set(given.id, given);
    prepare(given).then(
      (prepared) => {
        if (ended()) return;
        ready.set(given.id, prepared);
        sync();
      },
      (failure) => {
        fail(failure, given);
      }
    );
  }
  function named4() {
    if ([...expected.keys()].some((id2) => !ready.has(id2))) return void 0;
    return outputOptions;
  }
  function values2(ids2) {
    const options2 = store.board().options;
    return Object.fromEntries(
      ids2.flatMap((id2) => {
        const value2 = own(options2, id2);
        return value2 === void 0 ? [] : [[id2, value2]];
      })
    );
  }
  function wanted() {
    if (info === null) return void 0;
    const ids2 = named4();
    if (ids2 === void 0) return void 0;
    if (ids2.some((id2) => !Object.hasOwn(store.board().options, id2))) return void 0;
    const status = scheduler.status(block);
    if (status?.status !== "ready") return void 0;
    return { asked: { revision: status.revision, options: values2(ids2), info, params: shelf.params }, ids: ids2 };
  }
  function fits2(asked, want, ids2) {
    if (asked.revision !== want.revision || !jsonEqual(asked.info, want.info)) return false;
    if (!jsonEqual(asked.params, want.params)) return false;
    return ids2.every((id2) => jsonEqual(own(asked.options, id2), own(want.options, id2)));
  }
  function sync() {
    if (ended()) return;
    const want = wanted();
    if (want !== void 0) {
      if (held2 !== void 0 && fits2(held2.asked, want.asked, want.ids)) {
        stopAsking();
      } else if (shelf.last !== void 0 && shelf.last !== held2 && fits2(shelf.last.asked, want.asked, want.ids)) {
        stopAsking();
        take(shelf.last);
      } else {
        const key2 = outputRequestKey(want.asked);
        if (asking?.key !== key2) {
          stopAsking();
          asking = join(key2, want.asked);
          announce2();
        }
      }
    }
    wake();
  }
  function stopAsking() {
    if (asking === void 0) return;
    asking.leave();
    asking = void 0;
    announce2();
  }
  function join(key2, asked) {
    let flight = shelf.flights.get(key2);
    if (flight === void 0) {
      const aborted = new AbortController();
      const done = scheduler.request(
        { kind: "output", block, revision: asked.revision, options: asked.options, info: asked.info, params: asked.params },
        { signal: aborted.signal }
      ).then((answer) => {
        if (answer.reply.status === "ok" && "output" in answer.reply && !deps.replaced.aborted) {
          shelf.last = { asked, output: answer.reply.output, body: answer.body };
        }
        heard(answer.reply);
        return answer;
      });
      const started = { done, controller: aborted, callers: 0 };
      void done.catch((failure) => {
        if (!aborted.signal.aborted) unanswered(errorMessage(failure));
      }).finally(() => {
        if (shelf.flights.get(key2) === started) shelf.flights.delete(key2);
      });
      shelf.flights.set(key2, started);
      flight = started;
    }
    const mine = flight;
    mine.callers += 1;
    let left2 = false;
    const ask = {
      key: key2,
      leave() {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (mine.callers === 0) {
          if (shelf.flights.get(key2) === mine) shelf.flights.delete(key2);
          mine.controller.abort();
        }
      }
    };
    mine.done.then(
      (answer) => {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (asking === ask) asking = void 0;
        received2(asked, answer);
      },
      (failure) => {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (asking === ask) asking = void 0;
        if (!ended()) failed2(coreFailure("output_unanswered", errorMessage(failure)));
      }
    );
    return ask;
  }
  function received2(asked, { reply, body }) {
    if (ended()) return;
    switch (reply.status) {
      case "ok":
        take({ asked, output: reply.output, body });
        return;
      case "error":
        failed2(whyFailed(reply, block));
        return;
      default:
        announce2();
    }
  }
  function heard(reply) {
    if (deps.replaced.aborted) return;
    if (reply.status === "ok" && "output" in reply) {
      hold(reply.conditions);
    } else if (reply.status === "error") {
      const why = whyFailed(reply, block);
      hold("conditions" in reply ? reply.conditions : [], "kind" in why ? [why] : []);
    }
  }
  function unanswered(message) {
    if (!deps.replaced.aborted) hold([], [coreFailure("output_unanswered", message)]);
  }
  function hold(byR, byCore = []) {
    const before = shelf.notified;
    const fromR = (condition) => deps.raise({
      severity: condition.severity,
      message: condition.message,
      source: { kind: "r", class: condition.class, request: { type: "output" } },
      about: { block }
    });
    const fromCore = (failure) => deps.raise({ severity: "error", message: failure.message, source: { kind: "core", code: failure.code }, about: { block } });
    shelf.notified = [...byR.map(fromR), ...byCore.map(fromCore)];
    for (const withdraw of before) withdraw();
  }
  function failed2(why) {
    error = why;
    announce2();
  }
  function fail(failure, by2) {
    if (ended() || broken.has(failure)) return;
    broken.add(failure);
    log.error(asError(failure));
    deps.raise(coreNotification("module_failed", "error", errorMessage(failure), { module: rendererModule(by2) }));
  }
  function take(next) {
    held2 = next;
    error = null;
    const named5 = next.output.renderer;
    expect(named5);
    if (drawing !== void 0 && drawing.prepared.renderer.id !== named5.id) unmountRenderer();
    if (drawing === void 0) {
      prepare(named5).then(
        (prepared) => {
          if (ended() || drawing !== void 0 || held2?.output.renderer.id !== prepared.renderer.id) return;
          mountRenderer(prepared);
        },
        (failure) => {
          fail(failure, named5);
        }
      );
    }
    announce2();
    wake();
  }
  function mountRenderer(prepared) {
    const signal5 = new AbortController();
    const current3 = { prepared, host: outputHost(prepared), controller: signal5, cleanup: void 0 };
    drawing = current3;
    el.replaceChildren();
    void (async () => {
      try {
        const rendered = await prepared.mod.render({ el, host: current3.host, signal: signal5.signal });
        if (typeof rendered !== "function") return;
        if (signal5.signal.aborted) attempt(rendered);
        else current3.cleanup = rendered;
      } catch (failure) {
        if (signal5.signal.aborted) return;
        const pkg = packageFromId(prepared.renderer.id);
        const why = new ModuleError(prepared.renderer.module, pkg, `failed in render(): ${errorMessage(failure)}`, {
          cause: failure
        });
        el.replaceChildren();
        fail(why, prepared.renderer);
      }
    })();
  }
  function unmountRenderer() {
    const current3 = drawing;
    if (current3 === void 0) return;
    drawing = void 0;
    current3.host.drop();
    current3.controller.abort();
    if (current3.cleanup !== void 0) attempt(current3.cleanup);
    el.replaceChildren();
  }
  function announce2() {
    drawing?.host.announce();
  }
  function wake() {
    announceTo(waiting2);
  }
  function optionsFor(asked) {
    const now = store.board().options;
    return Object.fromEntries(
      outputOptions.flatMap((id2) => {
        const value2 = asked !== void 0 && Object.hasOwn(asked.options, id2) ? asked.options[id2] : own(now, id2);
        return value2 === void 0 ? [] : [[id2, value2]];
      })
    );
  }
  function replaceParams(value2) {
    if (!bound()) return;
    const next = frozenCopy(value2);
    if (jsonEqual(next, shelf.params)) return;
    shelf.params = next;
    announceTo(shelf.mounts);
  }
  function current2(name3, signal5) {
    return new Promise((resolve, reject) => {
      const settle2 = () => {
        if (!bound()) {
          reject(unboundError());
          return true;
        }
        if (signal5?.aborted === true) {
          reject(asError(signal5.reason));
          return true;
        }
        const status = scheduler.status(block);
        if (status === void 0) {
          reject(unboundError());
          return true;
        }
        if (status.status === "failed" || status.status === "invalid" || status.status === "waiting") {
          reject(
            new RequestError(
              `The request "${name3}" of the output of block "${block}" did not go to R, since the block reads ${status.status}.`,
              { blocks: [{ block, status }] }
            )
          );
          return true;
        }
        if (status.status === "ready" && held2?.asked.revision === status.revision) {
          resolve(held2);
          return true;
        }
        return false;
      };
      if (settle2()) return;
      const check2 = () => {
        if (settle2()) stop();
      };
      const stop = () => {
        waiting2.delete(check2);
        signal5?.removeEventListener("abort", check2);
      };
      waiting2.add(check2);
      signal5?.addEventListener("abort", check2, { once: true });
    });
  }
  async function request(prepared, name3, args, opts) {
    const what = `The request "${name3}" of the output of block "${block}"`;
    const notifier = givenNotifier(opts, `the request "${name3}" of the output of block "${block}"`);
    const module = rendererModule(prepared.renderer);
    const requested2 = { type: opts.scope === "renderer" ? "renderer" : "output_class", name: name3 };
    const inCaller = keepContext();
    for (let attempt3 = 1; ; attempt3++) {
      const target2 = await current2(name3, opts.signal);
      const output = { block, revision: target2.asked.revision };
      const fields = { output, name: name3, args: { ...args }, options: target2.asked.options, params: target2.asked.params };
      const asked = opts.scope === "renderer" ? { kind: "request", type: "renderer", renderer: prepared.renderer.id, ...fields } : { kind: "request", type: "output_class", ...fields };
      const { reply, body } = await inCaller(
        () => deps.answers.ask(
          { request: asked, share: opts.file === void 0 ? "keep" : "once", ...opts.file === void 0 ? {} : { body: opts.file } },
          {
            ...opts.signal === void 0 ? {} : { signal: opts.signal },
            ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
          }
        )
      );
      if (!bound()) throw unboundError();
      const conditions = "conditions" in reply ? reply.conditions : [];
      switch (reply.status) {
        case "ok":
          deps.replied(module, requested2, conditions, notifier);
          if (body !== null) return body;
          return "value" in reply && reply.value !== void 0 ? reply.value : null;
        case "missing":
          if (attempt3 < ATTEMPTS2) continue;
          throw new RequestError(`${what} gave up, since R kept lacking the result it reads.`, { status: "missing" });
        case "cancelled":
          throw new RequestError(`R cancelled ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, { status: "cancelled" });
        default:
          deps.replied(module, requested2, conditions, notifier);
          throw new RequestError(`R replied ${reply.status} to ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, {
            status: reply.status,
            conditions
          });
      }
    }
  }
  function checkRequest(name3, args, opts) {
    if (typeof name3 !== "string" || name3 === "") return new TypeError("A request of an output is named by a non-empty string.");
    if (!isObject(args)) return new TypeError(`The args of the request "${name3}" are an object.`);
    if (!isObject(opts)) return new TypeError(`The options of the request "${name3}" are an object.`);
    const scope = opts.scope;
    if (scope !== void 0 && scope !== "class" && scope !== "renderer") {
      return new TypeError(`A request of an output has the scope "class" or "renderer", not ${JSON.stringify(scope)}.`);
    }
    return void 0;
  }
  function outputHost(prepared) {
    const listeners = /* @__PURE__ */ new Set();
    let named5;
    let following = true;
    const notified = /* @__PURE__ */ new Set();
    const module = rendererModule(prepared.renderer);
    return {
      block,
      output: () => held2?.output ?? null,
      body: () => held2?.body ?? null,
      params: () => held2?.asked.params ?? EMPTY,
      option(id2) {
        let memo = named5;
        if (memo === void 0 || memo.for !== held2) {
          const read = optionReader(
            optionsFor(held2?.asked),
            outputOptions,
            (unnamed) => `The output family of block class "${registryId2}" names no board option "${unnamed}" in R, so the renderer "${prepared.renderer.id}" cannot read it.`
          );
          memo = { for: held2, read };
          named5 = memo;
        }
        return memo.read(id2);
      },
      loading: () => asking !== void 0,
      error: () => error,
      on(event, fn) {
        const heard2 = event;
        if (heard2 !== "output") {
          throw new Error(`The host of an output announces "output", not ${JSON.stringify(event)}.`);
        }
        const wrapped = () => {
          fn();
        };
        listeners.add(wrapped);
        return () => {
          listeners.delete(wrapped);
        };
      },
      setParams(params) {
        if (!isObject(params)) throw new TypeError("An output's parameters are an object.");
        if (following) replaceParams(params);
      },
      request(name3, args = {}, opts = {}) {
        const invalid = checkRequest(name3, args, opts);
        if (invalid !== void 0) return Promise.reject(invalid);
        if (!following || !bound()) return Promise.reject(unboundError());
        return request(prepared, name3, args, opts);
      },
      emit(name3, payload) {
        if (following && bound()) deps.emit({ block, name: name3, payload });
      },
      notify(notification) {
        const given = checkRaised(notification);
        if (!following || !bound()) return () => void 0;
        const withdraw = deps.notify(module, given);
        const mine = () => {
          notified.delete(mine);
          withdraw();
        };
        notified.add(mine);
        return mine;
      },
      log: createLog(module.package ?? null),
      announce() {
        if (following) announceTo(listeners);
      },
      drop() {
        following = false;
        listeners.clear();
        for (const withdraw of [...notified]) withdraw();
      }
    };
  }
  const offStore = store.subscribe((_change, next) => {
    if (!Object.hasOwn(next.blocks, block)) {
      end();
      return;
    }
    if (next.options === options) return;
    options = next.options;
    sync();
  });
  const offScheduler = scheduler.subscribe((changed) => {
    if (changed.includes(block)) sync();
  });
  shelf.mounts.add(sync);
  let settling;
  const stopWatching = measure.watch(el, () => {
    if (settling !== void 0) clearTimeout(settling);
    settling = setTimeout(
      () => {
        settling = void 0;
        const next = measure.info(el);
        if (jsonEqual(next, info)) return;
        info = next;
        sync();
      },
      info === null ? 0 : SETTLE_MS
    );
  });
  const offEnd = onAbort(deps.replaced, end);
  const by = `the outputs of block class "${entry.id}"`;
  const keepingOutputOptions = deps.keepOptions(outputOptions, by, lackOutputOptions);
  void keepingOutputOptions.added.then((failures) => {
    if (failures.length > 0) lackOutputOptions();
  });
  function lackOutputOptions() {
    if (ended()) return;
    const missing = outputOptions.filter((id2) => !Object.hasOwn(store.board().options, id2));
    failed2(coreFailure("board_option_failed", withoutOptions(by, missing)));
  }
  function end() {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    offScheduler();
    keepingOutputOptions.stop();
    shelf.mounts.delete(sync);
    stopWatching();
    if (settling !== void 0) clearTimeout(settling);
    asking?.leave();
    asking = void 0;
    unmountRenderer();
    el.replaceChildren();
    wake();
  }
  for (const given of entry.renderers ?? []) expect(given);
  if (shelf.last !== void 0) take(shelf.last);
  sync();
  return end;
}
function outputRequestKey(asked) {
  return JSON.stringify([asked.revision, sorted2(asked.options), sorted2(asked.info), sorted2(asked.params)]);
}
function sorted2(x) {
  if (Array.isArray(x)) return x.map(sorted2);
  if (isObject(x)) {
    const object2 = x;
    return Object.fromEntries(
      Object.keys(object2).sort().map((name3) => [name3, sorted2(object2[name3] ?? null)])
    );
  }
  return x;
}

// srcts/mount/block.ts
var CORE = "blockr.core";
var NO_STATUS = Object.freeze(unevaluated(0, false));
function createMounts(options) {
  const { store, commands, scheduler, loader } = options;
  const replaced2 = options.signal ?? new AbortController().signal;
  const answers = createAnswers(scheduler, replaced2);
  const shelves = createShelves();
  const measure = options.measure ?? domMeasure();
  const emit = options.emit ?? (() => void 0);
  const renderers = options.renderers ?? (() => ({}));
  const raise = options.raise ?? raiseNothing;
  const replied2 = options.replied ?? repliedNothing;
  const notify = options.notify ?? notifyNothing;
  const refused2 = options.refused ?? refusedNothing;
  const optionPackage = options.optionPackage ?? (() => void 0);
  store.subscribe((change) => {
    if ("opened" in change) return;
    for (const id2 of change.delta.blocks?.rm ?? []) shelves.drop(id2);
  });
  const resolving = /* @__PURE__ */ new Map();
  const reported = /* @__PURE__ */ new Set();
  function failure(option, message) {
    if (!replaced2.aborted && !reported.has(message)) {
      reported.add(message);
      log.error(message);
      const concerns = Object.hasOwn(store.options.registered(), option) ? { module: sourceModule({ option }, optionPackage(option)) } : { about: { option } };
      raise(coreNotification("board_option_failed", "error", message, concerns));
    }
    return [message];
  }
  function resolveDefault(id2, def) {
    const pending = resolving.get(id2);
    if (pending !== void 0) return pending;
    const given = def.default;
    const started = typeof given === "function" ? Promise.resolve().then(() => given(options.core())) : Promise.resolve(given);
    resolving.set(id2, started);
    const settled2 = () => {
      if (resolving.get(id2) === started) resolving.delete(id2);
    };
    started.then(settled2, settled2);
    return started;
  }
  async function addOptions(ids2, by) {
    const failures = await Promise.all(
      [...new Set(ids2)].map(async (id2) => {
        if (Object.hasOwn(store.board().options, id2)) return [];
        const def = own(store.options.registered(), id2);
        if (def === void 0) {
          return failure(id2, `The board option "${id2}", which ${by} requires, is defined by no package loaded.`);
        }
        let value2;
        try {
          value2 = await resolveDefault(id2, def);
        } catch (error) {
          return failure(id2, `The default of the board option "${id2}", which ${by} requires, failed: ${errorMessage(error)}`);
        }
        if (replaced2.aborted || Object.hasOwn(store.board().options, id2)) return [];
        try {
          await commands.run("blockr.core-set_option", { id: id2, value: value2 }, CORE);
        } catch (error) {
          return failure(
            id2,
            `The default of the board option "${id2}", which ${by} requires, did not go onto the board: ${errorMessage(error)}`
          );
        }
        return [];
      })
    );
    return failures.flat();
  }
  function keepOptions(ids2, by, failed2 = () => void 0) {
    if (ids2.length === 0) return { added: Promise.resolve([]), stop: () => void 0 };
    let stopped = false;
    const off = store.subscribe((change, board) => {
      if ("opened" in change) return;
      const removed = (change.delta.options?.unset ?? []).filter(
        (id2) => ids2.includes(id2) && !Object.hasOwn(board.options, id2)
      );
      if (removed.length === 0) return;
      void addOptions(removed, by).then((failures) => {
        if (!stopped && failures.length > 0) failed2(failures);
      });
    });
    return {
      added: addOptions(ids2, by),
      stop: () => {
        stopped = true;
        off();
      }
    };
  }
  function mountBlock(el, block) {
    if (replaced2.aborted) throw new Error(REPLACED);
    const found = own(store.board().blocks, block);
    if (found === void 0) {
      throw new Error(`The board holds no block "${block}" to mount.`);
    }
    const entry = own(store.blockRegistry(), found.registry_id);
    if (entry === void 0) {
      throw new Error(`The block registry has no class "${found.registry_id}" for block "${block}".`);
    }
    const pkg = packageFromId(entry.id);
    const module = sourceModule({ block }, pkg);
    const by = `the UI module of block class "${entry.id}"`;
    const controller = new AbortController();
    const { signal: signal5 } = controller;
    const ended = () => signal5.aborted;
    let cleanup;
    let stopKeeping;
    const end = () => {
      if (ended()) return;
      offEnd();
      host.drop();
      stopKeeping?.();
      controller.abort();
      if (cleanup !== void 0) attempt(cleanup);
      el.replaceChildren();
    };
    const host = blockHost({
      store,
      commands,
      scheduler,
      loader,
      answers,
      block,
      entry,
      module,
      replaced: replaced2,
      replied: replied2,
      notify,
      refused: refused2,
      unbound: end
    });
    const offEnd = onAbort(replaced2, end);
    void (async () => {
      let exports;
      try {
        exports = await loader.load({ module: entry.module, ...entry.css === void 0 ? {} : { css: entry.css } }, pkg);
      } catch (error) {
        fail(error);
        return;
      }
      if (ended()) return;
      let mod;
      try {
        mod = mountableModule(exports, entry.module, pkg);
      } catch (error) {
        fail(error);
        return;
      }
      const named4 = classOptions(entry);
      const kept = keepOptions(named4, by, () => {
        if (ended()) return;
        end();
        lacking(named4);
      });
      stopKeeping = kept.stop;
      const failures = await kept.added;
      if (ended()) return;
      if (failures.length > 0) {
        kept.stop();
        lacking(named4);
        return;
      }
      host.name(named4);
      try {
        const rendered = await mod.render({ el, host, signal: signal5 });
        if (typeof rendered !== "function") return;
        if (ended()) attempt(rendered);
        else cleanup = rendered;
      } catch (error) {
        fail(new ModuleError(entry.module, pkg, `failed in render(): ${errorMessage(error)}`, { cause: error }));
      }
    })();
    function fail(error) {
      if (ended()) return;
      log.error(asError(error));
      raise(coreNotification("module_failed", "error", errorMessage(error), { module }));
    }
    function lacking(ids2) {
      const missing = ids2.filter((id2) => !Object.hasOwn(store.board().options, id2));
      raise(coreNotification("module_failed", "error", withoutOptions(by, missing), { module }));
    }
    return end;
  }
  return {
    mountBlock,
    mountOutput: (el, block) => mountOutput(
      { store, scheduler, loader, answers, shelves, measure, replaced: replaced2, addOptions, keepOptions, emit, raise, replied: replied2, notify },
      el,
      block
    ),
    mountOption: (el, option) => mountOption(
      { store, commands, scheduler, loader, renderers, replaced: replaced2, keepOptions, raise, replied: replied2, notify, refused: refused2, optionPackage },
      el,
      option
    ),
    addOptions,
    keepOptions
  };
}
function classOptions(entry) {
  const read = Object.values(entry.requests).flatMap((request) => request.options);
  return [.../* @__PURE__ */ new Set([...entry.options ?? [], ...read])];
}
var contextValues = /* @__PURE__ */ new WeakMap();
function blockHost(deps) {
  const { store, commands, scheduler, block, entry } = deps;
  let seen = store.board();
  let named4 = [];
  let state2 = mountedBlock(seen)?.state ?? {};
  let context4 = blockContext(mountedBlock(seen));
  let following = true;
  let gone = false;
  const listeners = { state: /* @__PURE__ */ new Set(), context: /* @__PURE__ */ new Set() };
  const notified = /* @__PURE__ */ new Set();
  function mountedBlock(board) {
    return own(board.blocks, block);
  }
  function blockContext(now) {
    const recipe = scheduler.inputs(block)?.recipe ?? [];
    const values2 = Object.freeze(
      Object.fromEntries(
        named4.flatMap((id2) => {
          const value2 = own(seen.options, id2);
          return value2 === void 0 ? [] : [[id2, value2]];
        })
      )
    );
    const context5 = Object.freeze({
      name: now?.name ?? "",
      status: scheduler.status(block) ?? NO_STATUS,
      inputs: Object.freeze(
        recipe.flatMap(
          (input2) => input2.block === void 0 ? [] : [Object.freeze({ input: input2.input, revision: scheduler.status(input2.block)?.revision ?? 0 })]
        )
      ),
      option: optionReader(
        values2,
        named4,
        (id2) => `The block class "${entry.id}" names no board option "${id2}" in R, so its UI module cannot read it.`
      )
    });
    contextValues.set(context5, values2);
    return context5;
  }
  function bound() {
    return !gone && !deps.replaced.aborted && Object.hasOwn(store.board().blocks, block);
  }
  function announce2(event) {
    for (const fn of [...listeners[event]]) {
      try {
        fn();
      } catch (error) {
        log.error(asError(error));
      }
    }
  }
  function refresh() {
    const now = mountedBlock(seen);
    if (now === void 0) {
      unbind();
      return;
    }
    const stateChanged = now.state !== state2;
    if (stateChanged) state2 = now.state;
    const next = blockContext(now);
    const contextChanged = !sameContext(context4, next);
    if (contextChanged) context4 = next;
    if (stateChanged) announce2("state");
    if (contextChanged) announce2("context");
  }
  function unbind() {
    if (gone) return;
    gone = true;
    stop();
    deps.unbound();
  }
  const offStore = store.subscribe((_change, board) => {
    seen = board;
    refresh();
  });
  const offScheduler = scheduler.subscribe(() => {
    if (following && store.board() === seen) refresh();
  });
  function stop() {
    following = false;
    offStore();
    offScheduler();
  }
  const unboundError = () => new Error(
    deps.replaced.aborted ? `Another board opened, so the host of block "${block}" no longer changes it.` : `Block "${block}" left the board, so its host no longer changes it.`
  );
  return {
    block,
    class: entry.id,
    state: () => state2,
    context: () => context4,
    on(event, fn) {
      const heard = own(listeners, event);
      if (heard === void 0) {
        throw new Error(`A block's host announces "state" and "context", not ${JSON.stringify(event)}.`);
      }
      const wrapped = () => {
        fn();
      };
      heard.add(wrapped);
      return () => {
        heard.delete(wrapped);
      };
    },
    // The store commits what the command sets before submit() returns, as it
    // commits every command that does not prepare.
    async submit(change, opts) {
      if (!bound()) throw unboundError();
      if (!isObject(change)) throw new TypeError("A submit takes the state args it changes, as an object.");
      const notifier = givenNotifier(opts, `a submit of block "${block}"`);
      const set = Object.fromEntries(Object.entries(change).filter(([, value2]) => value2 !== void 0));
      const unset = Object.entries(change).flatMap(([arg, value2]) => value2 === void 0 ? [arg] : []);
      try {
        await commands.run("blockr.core-set_block", { id: block, state: set, ...unset.length === 0 ? {} : { unset } });
      } catch (error) {
        deps.refused(error, { block }, notifier);
        throw error;
      }
    },
    request(name3, args = {}, opts = {}) {
      if (!bound()) return Promise.reject(unboundError());
      return blockRequest({ ...deps, bound, unboundError }, block, entry, name3, args, opts);
    },
    async evaluate(params) {
      if (!bound()) throw unboundError();
      if (!isObject(params)) {
        throw new TypeError("A host's evaluate() takes the parameters that the block's block_eval() method reads, as an object.");
      }
      const statuses = await scheduler.evaluate([block], params);
      const status = own(statuses, block);
      if (status === void 0) throw unboundError();
      return status;
    },
    module(id2) {
      return deps.loader.module(id2);
    },
    notify(notification) {
      const given = checkRaised(notification);
      if (!following) return () => void 0;
      const withdraw = deps.notify(deps.module, given);
      const mine = () => {
        notified.delete(mine);
        withdraw();
      };
      notified.add(mine);
      return mine;
    },
    log: createLog(deps.module.package ?? null),
    name(options) {
      named4 = options;
      context4 = blockContext(mountedBlock(seen));
    },
    drop() {
      stop();
      for (const withdraw of [...notified]) withdraw();
    }
  };
}
function sameContext(a, b) {
  return a.name === b.name && a.status === b.status && a.inputs.length === b.inputs.length && a.inputs.every((input2, i) => input2.input === b.inputs[i]?.input && input2.revision === b.inputs[i].revision) && jsonEqual(contextValues.get(a) ?? {}, contextValues.get(b) ?? {});
}

// srcts/transport/shiny.ts
var name = "blockr";
function shinyTransport(options = {}) {
  const page = window;
  const shiny = options.shiny ?? page.Shiny;
  if (shiny === void 0) {
    throw new Error("The Shiny transport needs Shiny's JS API, window.Shiny.");
  }
  const fetcher = options.fetch ?? window.fetch.bind(window);
  const onDisconnected = options.onDisconnected ?? disconnected(page.jQuery);
  let start2 = null;
  let session = 0;
  return {
    async send(message, bodies) {
      const at = session;
      const text = JSON.stringify(message);
      refuseLarger(text, start2?.max_message_size);
      for (const [id2, body] of bodies) {
        await post(fetcher, start2, id2, body);
      }
      if (at !== session) {
        throw new Error("The session ended before its message was sent.");
      }
      shiny.setInputValue(name, text, { priority: "event" });
    },
    sessionId() {
      const token = start2?.token;
      return typeof token === "string" ? token : void 0;
    },
    listen(listener) {
      shiny.addCustomMessageHandler(name, (message) => {
        if (message instanceof ArrayBuffer) {
          for (const [item, body] of framed(message)) {
            listener.received(item, body);
          }
        } else if (Array.isArray(message)) {
          for (const item of message) {
            if (typeof item !== "object" || item === null) continue;
            const [carried, body] = received(item, fetcher);
            listener.received(carried, body);
          }
        } else if (isSessionStart(message)) {
          session += 1;
          start2 = message;
          listener.started();
        }
      });
      onDisconnected(() => {
        session += 1;
        start2 = null;
        listener.ended();
      });
    }
  };
}
function disconnected(jquery) {
  if (jquery === void 0) {
    throw new Error("The Shiny transport needs the jQuery that Shiny puts on the page.");
  }
  return (fn) => {
    jquery(document).on("shiny:disconnected", fn);
  };
}
function isSessionStart(message) {
  return typeof message === "object" && message !== null && typeof message.body_url === "string";
}
function framed(frame) {
  const length = new DataView(frame).getUint32(0, true);
  const json = new TextDecoder().decode(new Uint8Array(frame, 4, length));
  const bytes = frame.slice(4 + length);
  return JSON.parse(json).flatMap((item) => {
    if (typeof item !== "object" || item === null) return [];
    const announced = "status" in item && "body" in item ? item.body : null;
    if (typeof announced !== "object" || announced === null) {
      return [[item, null]];
    }
    const body = Promise.resolve(blob([bytes], announced));
    return [[item, body]];
  });
}
function received(item, fetcher) {
  const announced = "status" in item && "body" in item ? item.body : null;
  if (typeof announced !== "object" || announced === null) return [item, null];
  const { data, url, ...body } = announced;
  const reply = { ...item, body };
  let bytes;
  if (data !== void 0) {
    bytes = new Promise((resolve) => {
      resolve(blob([fromBase64(data)], body));
    });
  } else if (url !== void 0) {
    bytes = fetchBody(fetcher, url, body);
  } else {
    bytes = Promise.reject(
      new Error(`R sent no bytes of the body of its reply to ${reply.id}.`)
    );
  }
  bytes.catch(() => void 0);
  return [reply, bytes];
}
async function fetchBody(fetcher, url, body) {
  const response = await fetcher(url);
  if (!response.ok) {
    throw new Error(
      `R served no body at ${url}: ${String(response.status)} ${await response.text()}`
    );
  }
  const bytes = await response.blob();
  fetcher(url, { method: "DELETE" }).then((response2) => response2.text()).catch(() => void 0);
  return blob([bytes], body);
}
function refuseLarger(text, max) {
  if (max === void 0 || text.length * 3 <= max) return;
  const size = new TextEncoder().encode(text).length;
  if (size > max) {
    throw new Error(
      `The message of ${byteCount(size)} is larger than the ${byteCount(max)} that the blockr option \`max_message_size\` allows.`
    );
  }
}
function byteCount(n) {
  return `${n.toLocaleString("en-US")} bytes`;
}
async function post(fetcher, start2, id2, body) {
  if (start2 === null) {
    throw new Error("No session runs that a body could be posted to.");
  }
  const max = start2.max_request_size;
  if (max !== void 0 && body.size > max) {
    throw new Error(
      `The body of request ${id2}, of ${String(body.size)} bytes, is larger than the ${String(max)} bytes that R takes.`
    );
  }
  const url = `${start2.body_url}${start2.body_url.includes("?") ? "&" : "?"}id=${encodeURIComponent(id2)}`;
  const response = await fetcher(url, {
    method: "POST",
    body,
    headers: {
      "Content-Type": body.type === "" ? "application/octet-stream" : body.type
    }
  });
  const text = await response.text();
  if (!response.ok) {
    throw new Error(`R refused the body of request ${id2}: ${text}`);
  }
}
function blob(parts, body) {
  return body.name === void 0 ? new Blob(parts, { type: body.type }) : new File(parts, body.name, { type: body.type });
}
function fromBase64(data) {
  const text = atob(data);
  const bytes = new Uint8Array(text.length);
  for (let i = 0; i < text.length; i += 1) {
    bytes[i] = text.charCodeAt(i);
  }
  return bytes;
}

// srcts/core/blockr-options.ts
function createBlockrOptions(client) {
  const found = /* @__PURE__ */ new Map();
  let batch2;
  function ask(name3) {
    let asked = found.get(name3);
    if (asked === void 0) {
      asked = new Promise((resolve, reject) => {
        if (batch2 === void 0) {
          const sending = /* @__PURE__ */ new Map();
          batch2 = sending;
          void Promise.resolve().then(() => {
            if (batch2 === sending) batch2 = void 0;
            void send(sending);
          });
        }
        batch2.set(name3, { resolve, reject });
      });
      found.set(name3, asked);
    }
    return asked;
  }
  async function send(names) {
    const asked = [...names.keys()];
    let values2;
    try {
      values2 = await requestBlockrOptions(client, asked);
    } catch (error) {
      const failure = new Error(`R sent no values of the blockr options ${quoted(asked)}: ${errorMessage(error)}`, { cause: error });
      for (const [name3, settle2] of names) {
        found.delete(name3);
        settle2.reject(failure);
      }
      return;
    }
    for (const [name3, settle2] of names) {
      settle2.resolve(Object.hasOwn(values2, name3) ? values2[name3] ?? null : void 0);
    }
  }
  return {
    async read(name3) {
      if (typeof name3 !== "string" || name3 === "") {
        throw new TypeError("A blockr option is read by its name, a non-empty string.");
      }
      return ask(name3);
    }
  };
}
async function requestBlockrOptions(client, names) {
  for (; ; ) {
    let reply;
    try {
      ({ reply } = await client.request({ kind: "blockr_options", names }));
    } catch (error) {
      if (error instanceof SessionEndedError) continue;
      throw error;
    }
    if (reply.status !== "ok" || !("values" in reply)) {
      const said = "conditions" in reply ? reply.conditions.map((c) => c.message).join(" ") : "";
      throw new Error(`R replied ${reply.status}. ${said}`.trim());
    }
    return reply.values;
  }
}

// srcts/commands/commands.ts
import { context as otelContext, trace as trace3 } from "@opentelemetry/api";
import { computed as computed3, signal as signal4 } from "./vendor-858b8c85.js";

// srcts/store/store.ts
import { signal as signal3 } from "./vendor-858b8c85.js";

// srcts/store/words.ts
var adjectives = [
  "ablaze",
  "able",
  "acid",
  "acidic",
  "active",
  "actual",
  "added",
  "adept",
  "adored",
  "adroit",
  "aerial",
  "agile",
  "agreed",
  "airy",
  "alert",
  "alive",
  "allied",
  "almond",
  "amber",
  "ample",
  "amused",
  "apt",
  "arcane",
  "arctic",
  "ardent",
  "arid",
  "artful",
  "arty",
  "ash",
  "astral",
  "astute",
  "atomic",
  "auburn",
  "avian",
  "awake",
  "aware",
  "azure",
  "balmy",
  "basic",
  "beachy",
  "beige",
  "benign",
  "beryl",
  "best",
  "big",
  "binary",
  "bionic",
  "black",
  "blithe",
  "blocky",
  "blond",
  "blowy",
  "blue",
  "bold",
  "bonny",
  "boreal",
  "bouncy",
  "brainy",
  "brass",
  "brave",
  "breezy",
  "brick",
  "brief",
  "bright",
  "briny",
  "brisk",
  "broad",
  "bronze",
  "brown",
  "bubbly",
  "bumpy",
  "bushy",
  "busy",
  "calm",
  "candid",
  "carbon",
  "caring",
  "catchy",
  "cerise",
  "chalky",
  "chatty",
  "cheeky",
  "cheery",
  "chewy",
  "chic",
  "chief",
  "chilly",
  "choral",
  "chosen",
  "chummy",
  "chunky",
  "citric",
  "civic",
  "civil",
  "classy",
  "clay",
  "clean",
  "clever",
  "cloudy",
  "cocoa",
  "cold",
  "comfy",
  "comic",
  "common",
  "cool",
  "copper",
  "coral",
  "cosmic",
  "cotton",
  "covert",
  "coy",
  "cozy",
  "crafty",
  "craggy",
  "cream",
  "creamy",
  "crispy",
  "cubic",
  "cuddly",
  "curly",
  "curved",
  "cushy",
  "cute",
  "cyan",
  "dainty",
  "dapper",
  "daring",
  "dark",
  "dear",
  "decent",
  "deep",
  "deft",
  "demure",
  "denim",
  "dense",
  "dewy",
  "divine",
  "diving",
  "docile",
  "dogged",
  "dotted",
  "dovish",
  "downy",
  "dreamy",
  "dressy",
  "droll",
  "dry",
  "dusky",
  "dusty",
  "eager",
  "earthy",
  "easy",
  "ebony",
  "edgy",
  "edible",
  "elated",
  "elder",
  "elfin",
  "elite",
  "elvish",
  "equal",
  "even",
  "exact",
  "exotic",
  "expert",
  "extra",
  "fab",
  "fabled",
  "fair",
  "famed",
  "famous",
  "fancy",
  "far",
  "fast",
  "feisty",
  "ferny",
  "fierce",
  "fiery",
  "final",
  "fine",
  "firm",
  "first",
  "fit",
  "fixed",
  "fizzy",
  "flashy",
  "flat",
  "flax",
  "fleecy",
  "flinty",
  "floaty",
  "floppy",
  "floral",
  "fluent",
  "fluffy",
  "fluid",
  "flying",
  "foamy",
  "foggy",
  "fond",
  "forest",
  "formal",
  "frank",
  "free",
  "fresh",
  "frilly",
  "frisky",
  "frizzy",
  "frosty",
  "frothy",
  "frozen",
  "frugal",
  "fruity",
  "full",
  "fun",
  "funny",
  "furry",
  "future",
  "fuzzy",
  "garnet",
  "gauzy",
  "gem",
  "genial",
  "genius",
  "gentle",
  "giant",
  "giddy",
  "gifted",
  "giggly",
  "glad",
  "glass",
  "glassy",
  "glazed",
  "global",
  "glossy",
  "gnarly",
  "gold",
  "golden",
  "good",
  "goodly",
  "goofy",
  "gothic",
  "grainy",
  "grand",
  "grassy",
  "great",
  "green",
  "grey",
  "groovy",
  "grown",
  "gutsy",
  "handy",
  "happy",
  "hasty",
  "hazy",
  "heavy",
  "herbal",
  "herby",
  "heroic",
  "hidden",
  "high",
  "hilly",
  "hip",
  "hollow",
  "homey",
  "honest",
  "huge",
  "humane",
  "humble",
  "humid",
  "hungry",
  "iced",
  "iconic",
  "icy",
  "ideal",
  "idle",
  "immune",
  "impish",
  "indigo",
  "indoor",
  "inky",
  "inland",
  "inner",
  "intact",
  "ionic",
  "ironic",
  "ivory",
  "jade",
  "jagged",
  "jaunty",
  "jazzy",
  "jet",
  "jewel",
  "jolly",
  "jovial",
  "joyful",
  "joyous",
  "juicy",
  "jumpy",
  "junior",
  "just",
  "keen",
  "key",
  "khaki",
  "kind",
  "kindly",
  "kingly",
  "knotty",
  "known",
  "lace",
  "lacy",
  "lapis",
  "large",
  "lavish",
  "lawful",
  "leafy",
  "lean",
  "legal",
  "lemon",
  "lemony",
  "level",
  "light",
  "liked",
  "lilac",
  "lime",
  "limpid",
  "linear",
  "linen",
  "little",
  "live",
  "lively",
  "living",
  "local",
  "long",
  "loved",
  "lovely",
  "loving",
  "low",
  "lowkey",
  "loyal",
  "lucid",
  "lucky",
  "lunar",
  "lupine",
  "lush",
  "magic",
  "main",
  "maize",
  "major",
  "marble",
  "marine",
  "maroon",
  "marshy",
  "mauve",
  "medium",
  "meek",
  "mega",
  "mellow",
  "merry",
  "metal",
  "mica",
  "mid",
  "middle",
  "mighty",
  "mild",
  "milky",
  "mini",
  "minor",
  "minute",
  "misty",
  "mobile",
  "modern",
  "modest",
  "modish",
  "mossy",
  "moving",
  "mutual",
  "myrtle",
  "narrow",
  "naval",
  "navy",
  "near",
  "nearby",
  "neat",
  "neon",
  "new",
  "newsy",
  "next",
  "nice",
  "nickel",
  "nifty",
  "nimble",
  "noble",
  "noir",
  "normal",
  "novel",
  "nylon",
  "oblong",
  "ochre",
  "olive",
  "onyx",
  "opal",
  "open",
  "optic",
  "orange",
  "ornate",
  "outer",
  "oval",
  "paper",
  "papery",
  "peach",
  "peachy",
  "pearl",
  "pearly",
  "pebbly",
  "peppy",
  "perky",
  "pine",
  "pink",
  "placid",
  "plaid",
  "plain",
  "plucky",
  "plush",
  "poetic",
  "polar",
  "polite",
  "postal",
  "potent",
  "pretty",
  "prim",
  "prime",
  "proper",
  "proud",
  "public",
  "pure",
  "purple",
  "quaint",
  "quartz",
  "quick",
  "rainy",
  "random",
  "rapid",
  "rapt",
  "rare",
  "ready",
  "real",
  "recent",
  "red",
  "reedy",
  "regal",
  "rich",
  "right",
  "ripe",
  "ritzy",
  "robust",
  "rocky",
  "roomy",
  "rosy",
  "round",
  "royal",
  "rubber",
  "ruby",
  "runic",
  "rural",
  "russet",
  "rust",
  "rustic",
  "safe",
  "saline",
  "salmon",
  "salted",
  "salty",
  "sand",
  "sandy",
  "sane",
  "sassy",
  "satin",
  "satiny",
  "savvy",
  "secret",
  "secure",
  "sedate",
  "select",
  "sepia",
  "serene",
  "shaded",
  "shaggy",
  "shared",
  "sharp",
  "sheer",
  "shiny",
  "short",
  "shrewd",
  "shy",
  "sienna",
  "silk",
  "silken",
  "silky",
  "silly",
  "silver",
  "simple",
  "single",
  "slate",
  "sleepy",
  "slick",
  "slight",
  "slow",
  "sly",
  "small",
  "smart",
  "smoky",
  "smooth",
  "snappy",
  "snazzy",
  "snowy",
  "snug",
  "soapy",
  "social",
  "soft",
  "solar",
  "sole",
  "solemn",
  "solid",
  "sonic",
  "sound",
  "spare",
  "sparse",
  "speedy",
  "spicy",
  "spiky",
  "spiny",
  "sporty",
  "square",
  "stable",
  "starry",
  "static",
  "steamy",
  "steel",
  "steep",
  "still",
  "stoic",
  "stone",
  "stony",
  "stormy",
  "stout",
  "strict",
  "stripy",
  "strong",
  "suave",
  "subtle",
  "sudden",
  "sudsy",
  "suede",
  "sugar",
  "sugary",
  "sunlit",
  "sunny",
  "super",
  "superb",
  "sure",
  "sweet",
  "swift",
  "swirly",
  "tall",
  "tame",
  "tamed",
  "tan",
  "tangy",
  "tasty",
  "taupe",
  "taut",
  "teal",
  "techy",
  "tender",
  "thick",
  "thin",
  "thorny",
  "tidal",
  "tidy",
  "timely",
  "timid",
  "tin",
  "tinted",
  "tiny",
  "topaz",
  "tough",
  "toy",
  "trendy",
  "tricky",
  "tweed",
  "tweedy",
  "twill",
  "undyed",
  "unique",
  "united",
  "unlit",
  "upbeat",
  "upper",
  "urban",
  "urbane",
  "usable",
  "useful",
  "usual",
  "valid",
  "valued",
  "varied",
  "vast",
  "vellum",
  "velour",
  "velvet",
  "vernal",
  "vinyl",
  "violet",
  "vital",
  "vivid",
  "wacky",
  "warm",
  "wavy",
  "wax",
  "waxy",
  "wee",
  "well",
  "wheat",
  "white",
  "wide",
  "wild",
  "windy",
  "winged",
  "wintry",
  "wise",
  "wispy",
  "witty",
  "wood",
  "wooded",
  "wooden",
  "woodsy",
  "woody",
  "wool",
  "woolly",
  "worthy",
  "yellow",
  "young",
  "yummy",
  "zany",
  "zippy",
  "zircon"
];
var animals = [
  "addax",
  "adder",
  "agouti",
  "alpaca",
  "anole",
  "ant",
  "ape",
  "auk",
  "avocet",
  "baboon",
  "badger",
  "bass",
  "bat",
  "beagle",
  "bear",
  "beaver",
  "bee",
  "beetle",
  "bilby",
  "bird",
  "bison",
  "boa",
  "boar",
  "bobcat",
  "bongo",
  "bonobo",
  "borzoi",
  "bream",
  "bronco",
  "buck",
  "budgie",
  "bug",
  "bull",
  "bunny",
  "burro",
  "caiman",
  "calf",
  "camel",
  "canary",
  "carp",
  "cat",
  "cicada",
  "civet",
  "clam",
  "coati",
  "cobra",
  "cod",
  "collie",
  "colt",
  "conch",
  "condor",
  "coot",
  "cow",
  "coyote",
  "coypu",
  "crab",
  "crane",
  "crow",
  "cub",
  "cuckoo",
  "curlew",
  "cygnet",
  "deer",
  "degu",
  "dikdik",
  "dingo",
  "dipper",
  "dodo",
  "doe",
  "dog",
  "donkey",
  "dove",
  "drake",
  "drongo",
  "duck",
  "dugong",
  "dunlin",
  "eagle",
  "eel",
  "egret",
  "eider",
  "eland",
  "elk",
  "emu",
  "ermine",
  "ewe",
  "falcon",
  "fawn",
  "ferret",
  "finch",
  "fish",
  "fly",
  "foal",
  "fossa",
  "fowl",
  "fox",
  "frog",
  "fulmar",
  "galago",
  "galah",
  "gander",
  "gannet",
  "gaur",
  "gecko",
  "gelada",
  "genet",
  "gerbil",
  "gibbon",
  "gnu",
  "goat",
  "godwit",
  "goose",
  "gopher",
  "grebe",
  "grouse",
  "gull",
  "guppy",
  "hake",
  "hare",
  "hawk",
  "hen",
  "heron",
  "hog",
  "hoopoe",
  "hornet",
  "horse",
  "hound",
  "husky",
  "hyena",
  "ibex",
  "ibis",
  "iguana",
  "impala",
  "jackal",
  "jaguar",
  "jay",
  "jerboa",
  "joey",
  "junco",
  "kakapo",
  "kite",
  "kitten",
  "kiwi",
  "koala",
  "koi",
  "krill",
  "kudu",
  "lamb",
  "langur",
  "lark",
  "lemur",
  "liger",
  "limpet",
  "lion",
  "lizard",
  "llama",
  "loon",
  "loris",
  "lynx",
  "macaw",
  "magpie",
  "mamba",
  "manta",
  "mantis",
  "mare",
  "marlin",
  "marmot",
  "marten",
  "martin",
  "mayfly",
  "merlin",
  "mink",
  "minnow",
  "mole",
  "monkey",
  "moose",
  "moray",
  "morpho",
  "moth",
  "mouse",
  "mule",
  "mullet",
  "muskox",
  "mussel",
  "myna",
  "nene",
  "newt",
  "numbat",
  "nutria",
  "nyala",
  "ocelot",
  "okapi",
  "orca",
  "oriole",
  "oryx",
  "osprey",
  "otter",
  "owl",
  "ox",
  "oyster",
  "paca",
  "panda",
  "parrot",
  "perch",
  "phoebe",
  "pig",
  "pigeon",
  "piglet",
  "pika",
  "pike",
  "pipit",
  "plover",
  "pony",
  "poodle",
  "possum",
  "prawn",
  "puffer",
  "puffin",
  "pug",
  "puma",
  "puppy",
  "python",
  "quagga",
  "quail",
  "quokka",
  "quoll",
  "rabbit",
  "ram",
  "raven",
  "ray",
  "rhea",
  "rhino",
  "robin",
  "rook",
  "sable",
  "saiga",
  "salmon",
  "scarab",
  "seal",
  "serval",
  "shark",
  "sheep",
  "shrew",
  "shrike",
  "shrimp",
  "siskin",
  "skink",
  "skua",
  "skunk",
  "sloth",
  "snail",
  "snake",
  "snipe",
  "spider",
  "squid",
  "stag",
  "steed",
  "steer",
  "stilt",
  "stoat",
  "stork",
  "swan",
  "swift",
  "takin",
  "tapir",
  "teal",
  "tenrec",
  "tern",
  "thrush",
  "tiger",
  "toad",
  "topi",
  "toucan",
  "towhee",
  "trout",
  "tuna",
  "turaco",
  "turkey",
  "turtle",
  "urchin",
  "vervet",
  "vicuna",
  "viper",
  "vole",
  "walrus",
  "wasp",
  "weasel",
  "whale",
  "wisent",
  "wolf",
  "wombat",
  "wrasse",
  "wren",
  "yak",
  "zebra",
  "zebu"
];

// srcts/store/ids.ts
var RAND_IDS = [
  "adjective_animal",
  "adjective_adjective_animal",
  "letters_6",
  "letters_8",
  "letters_12",
  "bytes_4",
  "bytes_8",
  "uuid"
];
function isRandId(value2) {
  return typeof value2 === "string" && RAND_IDS.includes(value2);
}
function asRandId(randId) {
  if (randId === void 0) return "adjective_animal";
  if (isRandId(randId)) return randId;
  throw new RangeError(`The blockr option rand_id is ${JSON.stringify(randId)}, where it takes one of ${quoted(RAND_IDS)}.`);
}
function isId(id2) {
  return typeof id2 === "string" && id2 !== "" && !/^\d+$/.test(id2);
}
function mintId(taken, randId = "adjective_animal") {
  const draw = draws[randId];
  for (let i = 0; i < 100; i++) {
    const id3 = draw();
    if (isId(id3) && !taken(id3)) return id3;
  }
  const id2 = draw();
  let n = 2;
  while (taken(`${id2}_${String(n)}`)) n++;
  return `${id2}_${String(n)}`;
}
function sentenceCase(id2) {
  const words = id2.replace(/([A-Z])/g, " $1").replace(/[_\-.]/g, " ").trim().split(" ");
  const [first = "", ...rest] = words;
  return [
    first.charAt(0).toUpperCase() + first.slice(1).toLowerCase(),
    ...rest.map((word) => word.toLowerCase())
  ].join(" ");
}
var draws = {
  adjective_animal: () => `${pick(adjectives)}_${pick(animals)}`,
  adjective_adjective_animal: () => {
    const [first, second] = pickTwo(adjectives);
    return `${first}_${second}_${pick(animals)}`;
  },
  letters_6: () => randomLetters(6),
  letters_8: () => randomLetters(8),
  letters_12: () => randomLetters(12),
  bytes_4: () => hex2(randomBytes(4)),
  bytes_8: () => hex2(randomBytes(8)),
  uuid: randomUuid
};
function randomInt(n) {
  return Math.floor(Math.random() * n);
}
function pick(words) {
  return words[randomInt(words.length)] ?? "";
}
function pickTwo(words) {
  const i = randomInt(words.length);
  let j = randomInt(words.length - 1);
  if (j >= i) j++;
  return [words[i] ?? "", words[j] ?? ""];
}
function randomLetters(n) {
  return Array.from({ length: n }, () => String.fromCharCode(97 + randomInt(26))).join("");
}
function randomBytes(n) {
  return Array.from({ length: n }, () => randomInt(256));
}
function hex2(bytes) {
  return bytes.map((byte) => byte.toString(16).padStart(2, "0")).join("");
}
function randomUuid() {
  const bytes = randomBytes(16);
  bytes[6] = (bytes[6] ?? 0) & 15 | 64;
  bytes[8] = (bytes[8] ?? 0) & 63 | 128;
  const text = hex2(bytes);
  return [text.slice(0, 8), text.slice(8, 12), text.slice(12, 16), text.slice(16, 20), text.slice(20)].join("-");
}

// srcts/store/check.ts
var HEX_COLOR = "^#[0-9a-fA-F]{6}$";
var hexColor = new RegExp(HEX_COLOR);
var validators = /* @__PURE__ */ new WeakMap();
function stateFailures(entry, state2) {
  let validate = validators.get(entry);
  if (validate === void 0) {
    validate = compile(`the state of the block class "${entry.id}"`, entry.schema);
    validators.set(entry, validate);
  }
  return validate(state2);
}
function check(board, registry, registrations, changed) {
  const failures = [];
  if (!isId(board.id)) {
    failures.push(`The board's id "${board.id}" is empty or made of digits alone.`);
  }
  if (typeof board.name !== "string") {
    failures.push("The board's name is no string.");
  }
  checkBlocks(board, registry, registrations, changed, failures);
  checkLinks(board, registry, changed, failures);
  checkStacks(board, changed, failures);
  checkValues(board, registrations, changed, failures);
  checkPlugins(board, registrations, changed, failures);
  return failures;
}
function ids(what, record, failures) {
  for (const id2 of Object.keys(record)) {
    if (!isId(id2)) failures.push(`The ${what} id "${id2}" is empty or made of digits alone.`);
  }
}
function checkBlocks(board, registry, registrations, changed, failures) {
  ids("block", board.blocks, failures);
  for (const [id2, block] of Object.entries(board.blocks)) {
    if (changed !== void 0 && !changed.blocks.has(id2)) continue;
    const fields = shape(block, {
      registry_id: "string",
      state: "object",
      name: "string",
      locked: "boolean or null"
    });
    if (fields.length > 0) {
      failures.push(`The block "${id2}" ${fields.join(", ")}.`);
      continue;
    }
    const entry = own(registry, block.registry_id);
    if (entry === void 0) {
      failures.push(
        `The block "${id2}" is of the class "${block.registry_id}", which the block registry lacks.`
      );
      continue;
    }
    for (const failure of stateFailures(entry, block.state)) {
      failures.push(`The state of the block "${id2}" ${failure}`);
    }
  }
}
function checkLinks(board, registry, changed, failures) {
  ids("link", board.links, failures);
  const fed = /* @__PURE__ */ new Map();
  for (const [id2, link] of Object.entries(board.links)) {
    if (changed === void 0 || changed.links.has(id2)) {
      const fields = shape(link, { from: "string", to: "string", input: "string" });
      if (fields.length > 0) {
        failures.push(`The link "${id2}" ${fields.join(", ")}.`);
        continue;
      }
    }
    const from = own(board.blocks, link.from);
    const to = own(board.blocks, link.to);
    if (from === void 0) {
      failures.push(`The link "${id2}" comes from block "${link.from}", which the board lacks.`);
    }
    if (to === void 0) {
      failures.push(`The link "${id2}" goes to block "${link.to}", which the board lacks.`);
      continue;
    }
    const inputs = own(registry, to.registry_id)?.inputs;
    if (inputs === void 0 || inputs.includes("...")) continue;
    if (!inputs.includes(link.input)) {
      failures.push(
        `The link "${id2}" feeds the input "${link.input}" of block "${link.to}", whose class has no such input.`
      );
      continue;
    }
    const at = JSON.stringify([link.to, link.input]);
    fed.set(at, [...fed.get(at) ?? [], id2]);
  }
  for (const [at, links] of fed) {
    if (links.length < 2) continue;
    const [to, input2] = JSON.parse(at);
    failures.push(
      `The input "${input2}" of block "${to}" takes the links ${quoted(links)}, where it takes one.`
    );
  }
  const cycle = createGraph(board).cycle();
  if (cycle !== void 0) {
    failures.push(`The links form a cycle through the blocks ${quoted(cycle)}.`);
  }
}
function checkStacks(board, changed, failures) {
  ids("stack", board.stacks, failures);
  const holder = /* @__PURE__ */ new Map();
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (changed === void 0 || changed.stacks.has(id2)) {
      const fields = shape(stack, { name: "string", blocks: "array", color: "string or null" });
      if (fields.length > 0) {
        failures.push(`The stack "${id2}" ${fields.join(", ")}.`);
        continue;
      }
      if (stack.color !== null && !hexColor.test(stack.color)) {
        failures.push(
          `The stack "${id2}" has the colour "${stack.color}", which is no hex colour of six digits, such as "#66c2a5".`
        );
      }
    }
    for (const block of stack.blocks) {
      if (own(board.blocks, block) === void 0) {
        failures.push(`The stack "${id2}" holds block "${block}", which the board lacks.`);
        continue;
      }
      const other = holder.get(block);
      if (other === id2) {
        failures.push(`The stack "${id2}" holds block "${block}" twice.`);
      } else if (other !== void 0) {
        failures.push(`The block "${block}" sits in the stacks "${other}" and "${id2}", where it sits in one.`);
      }
      holder.set(block, id2);
    }
  }
}
function checkValues(board, registrations, changed, failures) {
  for (const [id2, value2] of Object.entries(board.options)) {
    if (changed !== void 0 && !changed.options.has(id2)) continue;
    const option = registrations.option(id2);
    if (option === void 0) {
      failures.push(`The board holds a value of the board option "${id2}", which no package registered.`);
      continue;
    }
    for (const failure of option.validate(value2)) {
      failures.push(`The value of the board option "${id2}" ${failure}`);
    }
  }
}
function checkPlugins(board, registrations, changed, failures) {
  for (const [id2, state2] of Object.entries(board.plugins)) {
    if (changed !== void 0 && !changed.plugins.has(id2)) continue;
    const cls = registrations.plugin(id2);
    if (cls === void 0) {
      failures.push(`The board holds the state of a plugin of the class "${id2}", which no package registered.`);
      continue;
    }
    if (cls.def.state === void 0) {
      failures.push(`The board holds the state of a plugin of the class "${id2}", which keeps none.`);
      continue;
    }
    for (const failure of cls.validate(state2)) {
      failures.push(`The state of the plugin "${id2}" ${failure}`);
    }
  }
}
function shape(value2, fields) {
  if (!isObject(value2)) return ["is no object"];
  const problems = [];
  for (const [field, kind] of Object.entries(fields)) {
    if (!fits(value2[field], kind)) problems.push(`has no ${kind} as its ${field}`);
  }
  for (const field of Object.keys(value2)) {
    if (!Object.hasOwn(fields, field)) problems.push(`holds "${field}", which core does not keep`);
  }
  return problems;
}
function fits(x, kind) {
  switch (kind) {
    case "array":
      return Array.isArray(x);
    case "object":
      return isObject(x);
    case "string or null":
      return x === null || typeof x === "string";
    case "boolean or null":
      return x === null || typeof x === "boolean";
    case "string":
      return typeof x === "string";
  }
}

// srcts/store/patch.ts
function segmentsFromPath(path) {
  if (!path.startsWith("/")) return void 0;
  const segments = path.slice(1).split("/");
  if (segments.some((segment) => /~(?![01])/.test(segment))) return void 0;
  return segments.map((segment) => segment.replaceAll("~1", "/").replaceAll("~0", "~"));
}
function pluginClassFromPath(path) {
  const [boardProperty, plugin] = segmentsFromPath(path) ?? [];
  return boardProperty === "plugins" ? plugin : void 0;
}
function pathFromSegments(segments) {
  return segments.map((segment) => `/${segment.replaceAll("~", "~0").replaceAll("/", "~1")}`).join("");
}
function overlap(a, b, board) {
  let at = board;
  for (const [i, segment] of a.slice(0, b.length).entries()) {
    if (Array.isArray(at) || segment !== b[i]) return false;
    at = isObject(at) ? own(at, segment) : void 0;
  }
  return true;
}
function malformed(patch2) {
  if (!isObject(patch2)) return ["is no object"];
  const problems = [];
  const { op, path, edit } = patch2;
  if (op !== "add" && op !== "remove" && op !== "replace") {
    problems.push(`has the op ${JSON.stringify(op)}, where it takes "add", "remove" or "replace"`);
  }
  if (typeof path !== "string" || segmentsFromPath(path) === void 0) {
    problems.push(`has the path ${JSON.stringify(path)}, which is no JSON Pointer below the board's root`);
  }
  if ((op === "add" || op === "replace") && patch2.value === void 0) {
    problems.push("has no value");
  }
  if (edit !== void 0 && (typeof edit !== "number" || !Number.isInteger(edit))) {
    problems.push(`has the edit id ${JSON.stringify(edit)}, which is no whole number`);
  }
  for (const field of Object.keys(patch2)) {
    if (!["op", "path", "value", "edit"].includes(field)) {
      problems.push(`holds "${field}", which a patch does not keep`);
    }
  }
  return problems;
}
function applyPatch(board, patch2, segments, place) {
  const result = step(board, segments, patch2, [], place);
  if (typeof result === "string") return { failure: result };
  return { ...result, next: result.next };
}
function step(at, segments, patch2, done, place) {
  const [segment = "", ...rest] = segments;
  if (Array.isArray(at)) {
    const items = at;
    const last = rest.length === 0;
    const index = last && segment === "-" && patch2.op === "add" ? items.length : arrayIndex(segment);
    const bound = last && patch2.op === "add" ? items.length : items.length - 1;
    if (index === void 0 || index > bound) {
      return `finds no element "${segment}" in the array at "${pathFromSegments(done)}"`;
    }
    const here2 = pathFromSegments([...done, String(index)]);
    if (!last) {
      const inner = step(items[index], rest, patch2, [...done, String(index)], place);
      if (typeof inner === "string") return inner;
      if (inner.next === items[index]) return { ...inner, next: at };
      return { ...inner, next: spliced(items, index, 1, inner.next) };
    }
    const old2 = items[index];
    switch (patch2.op) {
      case "add":
        return {
          next: spliced(items, index, 0, patch2.value),
          inverse: { op: "remove", path: here2 }
        };
      case "remove":
        return {
          next: spliced(items, index, 1),
          inverse: { op: "add", path: here2, value: old2 }
        };
      case "replace":
        if (jsonEqual(old2, patch2.value)) return { next: at, inverse: void 0 };
        return {
          next: spliced(items, index, 1, patch2.value),
          inverse: { op: "replace", path: here2, value: old2 }
        };
    }
  }
  if (!isObject(at)) {
    return `finds neither an object nor an array at "${pathFromSegments(done)}"`;
  }
  const has = Object.hasOwn(at, segment);
  const old = has ? at[segment] : void 0;
  const here = pathFromSegments([...done, segment]);
  if (rest.length > 0) {
    if (!has) return `finds no "${segment}" in "${pathFromSegments(done)}"`;
    const inner = step(old, rest, patch2, [...done, segment], place);
    if (typeof inner === "string") return inner;
    if (inner.next === old) return { ...inner, next: at };
    return { ...inner, next: { ...at, [segment]: inner.next } };
  }
  switch (patch2.op) {
    case "add":
    case "replace":
      if (!has && patch2.op === "replace") return `finds no "${segment}" in "${pathFromSegments(done)}" to replace`;
      if (!has) {
        return { next: inserted(at, segment, patch2.value, place), inverse: { op: "remove", path: here } };
      }
      if (jsonEqual(old, patch2.value)) return { next: at, inverse: void 0 };
      return {
        next: { ...at, [segment]: patch2.value },
        inverse: { op: "replace", path: here, value: old }
      };
    case "remove": {
      if (!has) return `finds no "${segment}" in "${pathFromSegments(done)}" to remove`;
      const members = Object.keys(at);
      return {
        next: without(at, [segment]),
        inverse: { op: "add", path: here, value: old },
        place: members[members.indexOf(segment) + 1] ?? null
      };
    }
  }
}
function inserted(record, key2, value2, before) {
  if (typeof before !== "string" || !Object.hasOwn(record, before)) {
    return { ...record, [key2]: value2 };
  }
  const members = Object.entries(record);
  members.splice(members.findIndex(([name3]) => name3 === before), 0, [key2, value2]);
  return Object.fromEntries(members);
}
function spliced(items, index, count, ...added) {
  const copy = [...items];
  copy.splice(index, count, ...added);
  return copy;
}
function arrayIndex(segment) {
  return /^(0|[1-9]\d*)$/.test(segment) ? Number(segment) : void 0;
}

// srcts/store/commit.ts
function target(of) {
  return "block" in of ? JSON.stringify(["block", of.block]) : JSON.stringify(["plugin", of.plugin]);
}
function commit(board, delta, places = {}) {
  const failures = [];
  const targets = /* @__PURE__ */ new Set();
  const blocks = collection(
    (id2) => `block "${id2}"`,
    board.blocks,
    delta.blocks,
    (old, mod, id2) => modifyBlock(old, mod, id2, failures),
    places.blocks,
    void 0,
    failures
  );
  for (const id2 of blocks.touched) targets.add(target({ block: id2 }));
  const links = collection(
    (id2) => `link "${id2}"`,
    board.links,
    delta.links,
    modifyFields,
    places.links,
    delta.links?.before,
    failures
  );
  const stacks = collection(
    (id2) => `stack "${id2}"`,
    board.stacks,
    delta.stacks,
    modifyFields,
    places.stacks,
    void 0,
    failures
  );
  const options = values(
    board.options,
    delta.options,
    (id2) => `the board option "${id2}"`,
    failures
  );
  let next = {
    ...board,
    name: delta.name ?? board.name,
    blocks: blocks.next,
    links: links.next,
    stacks: stacks.next,
    options: options.next
  };
  const patched = patch(next, delta.patches ?? [], places.patches ?? {}, failures);
  for (const plugin of patched.plugins) targets.add(target({ plugin }));
  next = patched.board;
  if (failures.length > 0) return { failures };
  const inverse = {
    ...empty(blocks.inverse) ? {} : { blocks: blocks.inverse },
    ...empty(links.inverse) ? {} : { links: links.inverse },
    ...empty(stacks.inverse) ? {} : { stacks: stacks.inverse },
    ...delta.name === void 0 || delta.name === board.name ? {} : { name: board.name },
    ...patched.inverse.length === 0 ? {} : { patches: patched.inverse }
  };
  return {
    board: next,
    inverse: {
      delta: inverse,
      places: {
        blocks: blocks.places,
        links: links.places,
        stacks: stacks.places,
        patches: patched.places
      }
    },
    changed: {
      blocks: new Set(blocks.changed),
      links: new Set(links.changed),
      stacks: new Set(stacks.changed),
      options: options.changed,
      plugins: patched.plugins,
      targets
    }
  };
}
function patch(board, patches2, placed, failures) {
  let next = board;
  const inverse = [];
  const places = {};
  const plugins = /* @__PURE__ */ new Set();
  for (const [i, given] of patches2.entries()) {
    const name3 = `The patch ${String(i + 1)}`;
    const problems = malformed(given);
    if (problems.length > 0) {
      failures.push(`${name3} ${problems.join(", ")}.`);
      continue;
    }
    const segments = segmentsFromPath(given.path) ?? [];
    const misplaced2 = outside(segments, given.op);
    if (misplaced2 !== void 0) {
      failures.push(`${name3}, ${given.op} at "${given.path}", ${misplaced2}.`);
      continue;
    }
    const place = given.op === "add" ? own(placed, given.path) : void 0;
    const applied = applyPatch(next, given, segments, place);
    if ("failure" in applied) {
      failures.push(`${name3}, ${given.op} at "${given.path}", ${applied.failure}.`);
      continue;
    }
    next = applied.next;
    if (applied.inverse === void 0) continue;
    inverse.unshift(applied.inverse);
    if (applied.place !== void 0) places[applied.inverse.path] = applied.place;
    plugins.add(segments[1] ?? "");
  }
  return { board: next, inverse, places, plugins };
}
function outside(segments, op) {
  const [boardProperty, plugin] = segments;
  if (boardProperty !== "plugins") return `changes "${boardProperty ?? ""}", where a patch changes the state of the board's plugins alone`;
  if (plugin === void 0) return "changes the board's plugins as a whole, where a patch changes the state of one plugin";
  if (segments.length === 2 && op !== "replace") {
    return `${op === "add" ? "adds" : "removes"} what the board holds for the plugin "${plugin}", where a patch changes a plugin's state alone, since the app sets the plugins that run`;
  }
  return void 0;
}
function collection(name3, current2, part, modify, places, before, failures) {
  if (part === void 0) {
    return { next: current2, inverse: {}, places: {}, changed: [], touched: [] };
  }
  const order = Object.keys(current2);
  const removed = /* @__PURE__ */ new Set();
  for (const id2 of part.rm ?? []) {
    if (Object.hasOwn(current2, id2)) removed.add(id2);
    else failures.push(`The board holds no ${name3(id2)} to remove.`);
  }
  const readd = {};
  const readdPlaces = {};
  for (const id2 of removed) {
    readd[id2] = current2[id2];
    const after = order.slice(order.indexOf(id2) + 1);
    readdPlaces[id2] = after.find((other) => !removed.has(other)) ?? null;
  }
  const kept = order.filter((id2) => !removed.has(id2)).map((id2) => [id2, current2[id2]]);
  const keptSuccessor = (named4) => named4 === null ? null : removed.has(named4) ? own(readdPlaces, named4) ?? null : named4;
  const moves = [];
  for (const [id2, named4] of Object.entries(before ?? {})) {
    if (Object.hasOwn(part.add ?? {}, id2)) continue;
    const at = kept.findIndex(([other]) => other === id2);
    const entry = kept[at];
    if (entry === void 0) {
      failures.push(`The board holds no ${name3(id2)} to move.`);
      continue;
    }
    const to = keptSuccessor(named4);
    const was = kept[at + 1]?.[0] ?? null;
    if (to === was || to === id2) continue;
    if (to !== null && !kept.some(([other]) => other === to)) {
      failures.push(`The board holds no ${name3(to)} for ${name3(id2)} to go before.`);
      continue;
    }
    kept.splice(at, 1);
    if (to === null) kept.push(entry);
    else kept.splice(kept.findIndex(([other]) => other === to), 0, entry);
    moves.unshift([id2, was]);
  }
  const added = [];
  for (const [id2, value2] of Object.entries(part.add ?? {})) {
    if (kept.some(([other]) => other === id2)) {
      failures.push(`The board holds ${name3(id2)} already.`);
      continue;
    }
    const named4 = own(before ?? {}, id2);
    const successor = named4 === void 0 ? own(places ?? {}, id2) ?? null : keptSuccessor(named4);
    const at = successor === null ? -1 : kept.findIndex(([other]) => other === successor);
    if (at < 0 && successor !== null && named4 !== void 0) {
      failures.push(
        `The board holds no ${name3(successor)} for ${name3(id2)} to go before.`
      );
      continue;
    }
    if (at < 0) kept.push([id2, value2]);
    else kept.splice(at, 0, [id2, value2]);
    added.push(id2);
  }
  const remod = {};
  const modded = [];
  for (const [id2, mod] of Object.entries(part.mod ?? {})) {
    const at = kept.findIndex(([other]) => other === id2);
    const entry = kept[at];
    if (entry === void 0) {
      failures.push(`The board holds no ${name3(id2)} to change.`);
      continue;
    }
    const { value: value2, inverse } = modify(entry[1], mod, id2);
    if (Object.keys(inverse).length === 0) continue;
    kept[at] = [id2, value2];
    if (!added.includes(id2)) remod[id2] = inverse;
    modded.push(id2);
  }
  return {
    next: Object.fromEntries(kept),
    inverse: {
      ...added.length === 0 ? {} : { rm: added },
      ...removed.size === 0 ? {} : { add: readd },
      ...Object.keys(remod).length === 0 ? {} : { mod: remod },
      ...moves.length === 0 ? {} : { before: Object.fromEntries(moves) }
    },
    places: readdPlaces,
    changed: [.../* @__PURE__ */ new Set([...added, ...modded])],
    touched: [.../* @__PURE__ */ new Set([...removed, ...added, ...modded])]
  };
}
function empty(part) {
  return Object.keys(part).length === 0;
}
function changes(current2, set, unset, both, failures) {
  const setting2 = Object.entries(set).filter(
    ([name3, value2]) => !jsonEqual(own(current2, name3), value2)
  );
  const restore = {};
  const remove2 = [];
  const removing2 = [];
  for (const name3 of unset) {
    if (Object.hasOwn(set, name3)) failures.push(both(name3));
    const was = own(current2, name3);
    if (was === void 0) continue;
    removing2.push(name3);
    restore[name3] = was;
  }
  for (const [name3] of setting2) {
    const was = own(current2, name3);
    if (was === void 0) remove2.push(name3);
    else restore[name3] = was;
  }
  return {
    next: setting2.length === 0 && removing2.length === 0 ? current2 : { ...without(current2, removing2), ...Object.fromEntries(setting2) },
    set: setting2.map(([name3]) => name3),
    unset: removing2,
    restore,
    remove: remove2
  };
}
function values(current2, change, what, failures) {
  if (change === void 0) {
    return { next: current2, changed: /* @__PURE__ */ new Set() };
  }
  const result = changes(
    current2,
    change.set ?? {},
    change.unset ?? [],
    (name3) => `The change both sets and unsets ${what(name3)}.`,
    failures
  );
  return {
    next: result.next,
    changed: /* @__PURE__ */ new Set([...result.set, ...result.unset])
  };
}
function modifyBlock(old, mod, id2, failures) {
  const state2 = changes(
    old.state,
    mod.state ?? {},
    mod.unset ?? [],
    (arg) => `The change to block "${id2}" both sets and unsets "${arg}".`,
    failures
  );
  const name3 = mod.name === old.name ? void 0 : mod.name;
  const locked2 = mod.locked === void 0 || mod.locked === old.locked ? void 0 : mod.locked;
  return {
    value: {
      ...old,
      state: state2.next,
      name: name3 ?? old.name,
      locked: locked2 === void 0 ? old.locked : locked2
    },
    inverse: {
      ...Object.keys(state2.restore).length === 0 ? {} : { state: state2.restore },
      ...state2.remove.length === 0 ? {} : { unset: state2.remove },
      ...name3 === void 0 ? {} : { name: old.name },
      ...locked2 === void 0 ? {} : { locked: old.locked }
    }
  };
}
function modifyFields(old, mod) {
  const inverse = {};
  const changed = {};
  for (const field of Object.keys(mod)) {
    if (jsonEqual(old[field], mod[field])) continue;
    inverse[field] = old[field];
    changed[field] = mod[field];
  }
  return { value: { ...old, ...changed }, inverse };
}

// srcts/store/delta.ts
function isDeltas(delta) {
  return Array.isArray(delta);
}

// srcts/store/error.ts
var StoreError = class extends Error {
  failures;
  constructor(message, failures) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"));
    this.name = "StoreError";
    this.failures = failures;
  }
};

// srcts/store/expand.ts
var ROUNDS = 10;
function expand(delta, board, registrations) {
  let expanded = delta;
  for (let round = 0; round < ROUNDS; round++) {
    const adding = [];
    const failures = [];
    const core = expandCore(expanded, board);
    const grew = core !== expanded;
    expanded = core;
    for (const id2 of Object.keys(board.plugins)) {
      const def = registrations.plugin(id2)?.def;
      if (def?.update === void 0) continue;
      let patches2;
      try {
        patches2 = def.update(expanded, board, id2);
      } catch (error) {
        failures.push(`The plugin class "${id2}" failed to update the delta for its plugin: ${errorMessage(error)}`);
        continue;
      }
      const wrong = misplaced(id2, patches2);
      if (wrong.length > 0) {
        failures.push(...wrong);
        continue;
      }
      const merged = merge(expanded, delta, patches2, board);
      if (merged !== expanded) adding.push(id2);
      expanded = merged;
    }
    if (failures.length > 0) return { failures };
    if (!grew && adding.length === 0) return expanded;
    if (round === ROUNDS - 1) {
      return {
        failures: [
          `The delta kept expanding after ${String(ROUNDS)} rounds, through the update of the plugin${adding.length > 1 ? "s" : ""} ${quoted(adding)}.`
        ]
      };
    }
  }
  return expanded;
}
function expandCore(delta, board) {
  const gone = new Set(delta.blocks?.rm ?? []);
  if (gone.size === 0) return delta;
  const linksRm = delta.links?.rm ?? [];
  const incident = Object.entries(board.links).filter(([id2, link]) => (gone.has(link.from) || gone.has(link.to)) && !linksRm.includes(id2)).map(([id2]) => id2);
  const stacksRm = delta.stacks?.rm ?? [];
  const stacksMod = delta.stacks?.mod ?? {};
  const placed = {};
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (stacksRm.includes(id2)) continue;
    const mod = own(stacksMod, id2);
    const blocks = mod?.blocks ?? stack.blocks;
    if (blocks.some((block) => gone.has(block))) {
      placed[id2] = { ...mod, blocks: blocks.filter((block) => !gone.has(block)) };
    }
  }
  if (incident.length === 0 && Object.keys(placed).length === 0) return delta;
  return {
    ...delta,
    ...incident.length === 0 ? {} : { links: { ...delta.links, rm: [...linksRm, ...incident] } },
    ...Object.keys(placed).length === 0 ? {} : { stacks: { ...delta.stacks, mod: { ...stacksMod, ...placed } } }
  };
}
function misplaced(id2, patches2) {
  const of = `The plugin class "${id2}" updated the delta for its plugin`;
  if (!Array.isArray(patches2)) return [`${of} with no array of patches.`];
  const failures = [];
  for (const patch2 of patches2) {
    const problems = malformed(patch2);
    if (problems.length > 0) {
      failures.push(`${of} with a patch that ${problems.join(", ")}.`);
      continue;
    }
    const { path } = patch2;
    const [boardProperty, plugin] = segmentsFromPath(path) ?? [];
    if (boardProperty !== "plugins" || plugin !== id2) {
      failures.push(`${of} with a patch at "${path}", outside that plugin's state.`);
    }
  }
  return failures;
}
function merge(delta, given, patches2, board) {
  const held2 = delta.patches ?? [];
  const fixed2 = (given.patches ?? []).map(({ path }) => segmentsFromPath(path) ?? []);
  const added = [];
  for (const patch2 of patches2) {
    const segments = segmentsFromPath(patch2.path) ?? [];
    if ([...held2, ...added].some((other) => same(other, patch2))) continue;
    if (fixed2.some((other) => overlap(other, segments, board))) continue;
    added.push(patch2);
  }
  if (added.length === 0) return delta;
  return { ...delta, patches: [...held2, ...added] };
}
function same(a, b) {
  return a.op === b.op && a.path === b.path && jsonEqual(a.value, b.value);
}

// srcts/store/lock.ts
function fixes(board, block, boardLock) {
  if (!boardLock) return false;
  const held2 = board === void 0 ? void 0 : own(board.blocks, block);
  return held2?.locked !== false;
}
function lockFailures(before, inverse) {
  const failures = [];
  const refuse = (verb, one2, several, ids2) => {
    if (ids2.length === 0) return;
    failures.push(`The board lock refuses to ${verb} ${ids2.length === 1 ? one2 : several} ${quoted(ids2)}.`);
  };
  const blocks = inverse.blocks;
  const mods = Object.entries(blocks?.mod ?? {});
  refuse("add", "block", "blocks", blocks?.rm ?? []);
  refuse("remove", "block", "blocks", Object.keys(blocks?.add ?? {}));
  refuse("rename", "block", "blocks", mods.filter(([, mod]) => mod.name !== void 0).map(([id2]) => id2));
  refuse(
    "change",
    "the lock of block",
    "the locks of the blocks",
    mods.filter(([, mod]) => mod.locked !== void 0).map(([id2]) => id2)
  );
  for (const [id2, mod] of mods) {
    if (mod.state === void 0 && mod.unset === void 0) continue;
    const locked2 = own(before.blocks, id2)?.locked;
    if (locked2 === true) {
      failures.push(`The lock of block "${id2}" fixes its state under the board lock.`);
    } else if (locked2 === null) {
      failures.push(`The board lock fixes the state of block "${id2}", whose lock follows the board lock.`);
    }
  }
  for (const [part, one2, several] of [
    [inverse.links, "link", "links"],
    [inverse.stacks, "stack", "stacks"]
  ]) {
    refuse("add", one2, several, part?.rm ?? []);
    refuse("change", one2, several, Object.keys(part?.mod ?? {}));
    refuse("remove", one2, several, Object.keys(part?.add ?? {}));
  }
  refuse("move", "link", "links", Object.keys(inverse.links?.before ?? {}));
  if (inverse.name !== void 0) failures.push("The board lock refuses to rename the board.");
  return failures;
}

// srcts/store/undo.ts
function fit(delta, board) {
  const blocks = collection2(board.blocks, delta.blocks);
  const kept = /* @__PURE__ */ new Set([
    ...Object.keys(board.blocks).filter((id2) => !(blocks.rm ?? []).includes(id2)),
    ...Object.keys(blocks.add ?? {})
  ]);
  const holds = (id2) => kept.has(id2);
  const links = collection2(
    board.links,
    delta.links,
    (link) => (link.from === void 0 || holds(link.from)) && (link.to === void 0 || holds(link.to))
  );
  const held2 = (id2) => Object.hasOwn(board.links, id2) && !(links.rm ?? []).includes(id2);
  const before = Object.fromEntries(
    Object.entries(delta.links?.before ?? {}).flatMap(([id2, named4]) => {
      if (Object.hasOwn(links.add ?? {}, id2)) return [[id2, named4]];
      if (!held2(id2)) return [];
      return [[id2, named4 !== null && Object.hasOwn(board.links, named4) ? named4 : null]];
    })
  );
  const stacks = collection2(board.stacks, delta.stacks);
  const placed = (stack) => stack.blocks === void 0 ? stack : { ...stack, blocks: stack.blocks.filter(holds) };
  return prune({
    blocks,
    links: { ...links, ...Object.keys(before).length === 0 ? {} : { before } },
    stacks: {
      ...stacks.rm === void 0 ? {} : { rm: stacks.rm },
      ...stacks.add === void 0 ? {} : { add: Object.fromEntries(Object.entries(stacks.add).map(([id2, s]) => [id2, placed(s)])) },
      ...stacks.mod === void 0 ? {} : { mod: Object.fromEntries(Object.entries(stacks.mod).map(([id2, s]) => [id2, placed(s)])) }
    },
    ...delta.options === void 0 ? {} : { options: delta.options },
    ...delta.name === void 0 ? {} : { name: delta.name },
    patches: patches(delta.patches ?? [], board)
  });
}
function patches(given, board) {
  let at = board;
  const fitted = [];
  for (const patch2 of given) {
    const segments = segmentsFromPath(patch2.path) ?? [];
    if (segments[0] !== "plugins") continue;
    const applied = applyPatch(at, patch2, segments);
    if ("failure" in applied) continue;
    at = applied.next;
    fitted.push(patch2);
  }
  return fitted;
}
function collection2(current2, part, keep = () => true) {
  if (part === void 0) return {};
  const rm = (part.rm ?? []).filter((id2) => Object.hasOwn(current2, id2));
  const there = (id2) => Object.hasOwn(current2, id2) && !rm.includes(id2);
  const add = Object.entries(part.add ?? {}).filter(([id2, entry]) => !there(id2) && keep(entry));
  const mod = Object.entries(part.mod ?? {}).filter(([id2, change]) => there(id2) && keep(change));
  return {
    ...rm.length === 0 ? {} : { rm },
    ...add.length === 0 ? {} : { add: Object.fromEntries(add) },
    ...mod.length === 0 ? {} : { mod: Object.fromEntries(mod) }
  };
}
function prune(delta) {
  const parts = Object.entries(delta);
  return Object.fromEntries(
    parts.filter(
      ([, part]) => Array.isArray(part) ? part.length > 0 : !isObject(part) || Object.keys(part).length > 0
    )
  );
}
function isEmpty(delta) {
  return Object.keys(prune(delta)).length === 0;
}

// srcts/store/plugins.ts
function inRole(plugins, registeredClass, role) {
  return Object.keys(plugins).filter((id2) => registeredClass(id2)?.[role] !== void 0);
}
function keptPlugins(board, app, registeredClass) {
  return Object.fromEntries(
    Object.entries(board).filter(
      (entry) => Object.hasOwn(app, entry[0]) && registeredClass(entry[0])?.state !== void 0
    )
  );
}
function droppedPlugins(board, app, registeredClass) {
  return Object.keys(board).filter((id2) => !Object.hasOwn(app, id2) || registeredClass(id2) === void 0);
}
function pluginsWithDefaults(kept, app, registeredClass) {
  const plugins = {};
  for (const id2 of Object.keys(app)) {
    const state2 = registeredClass(id2)?.state;
    if (state2 === void 0) continue;
    const held2 = own(kept, id2);
    plugins[id2] = held2 === void 0 ? frozenCopy(state2.default) : held2;
  }
  return plugins;
}

// srcts/store/open.ts
var FORMAT = 1;
var COLLECTIONS = ["blocks", "links", "stacks", "options", "plugins"];
var PROPERTIES = ["id", "name", ...COLLECTIONS];
var refused = "The store refused to open the board";
function parse(text) {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new StoreError(refused, [`The text holds no JSON: ${errorMessage(error)}`]);
  }
}
function checkJsonBoard(json) {
  if (!isObject(json)) throw new StoreError(refused, ["The JSON is no JSON board."]);
  if (!Object.hasOwn(json, "format")) {
    throw new StoreError(refused, [
      "The JSON board has no format, as a file of today's stack has none, which a conversion outside core turns into a JSON board first."
    ]);
  }
  if (typeof json.format !== "number" || !Number.isInteger(json.format)) {
    throw new StoreError(refused, [`The JSON board's format ${JSON.stringify(json.format)} is no whole number.`]);
  }
  if (json.format > FORMAT) {
    throw new StoreError(refused, [
      `The JSON board's format is ${String(json.format)}, while this client reads format ${String(FORMAT)} at most.`
    ]);
  }
  if (!isObject(json.packages) || !Object.values(json.packages).every((v) => typeof v === "string")) {
    throw new StoreError(refused, ["The JSON board names no packages with their versions."]);
  }
  if (!isObject(json.board)) throw new StoreError(refused, ["The JSON board holds no board."]);
  const board = json.board;
  const lacking = COLLECTIONS.filter((name3) => !isObject(board[name3]));
  if (lacking.length > 0) {
    throw new StoreError(refused, [`The board holds its ${listing(lacking)} in no object by id.`]);
  }
  checkVersions(board);
  return json;
}
function isVersion(value2) {
  return typeof value2 === "number" && Number.isInteger(value2) && value2 >= 1;
}
function checkVersions(board) {
  const failures = [];
  for (const [cls, plugin] of Object.entries(board.plugins)) {
    if (!isObject(plugin) || !Object.hasOwn(plugin, "state") || !isVersion(plugin.version)) {
      failures.push(`The plugin "${cls}" is recorded as no state with the version of its class's state.`);
    }
  }
  for (const [id2, block] of Object.entries(board.blocks)) {
    if (isObject(block) && !isVersion(block.version)) {
      failures.push(`The block "${id2}" records no version of its class's state.`);
    }
  }
  for (const [id2, option] of Object.entries(board.options)) {
    if (!isObject(option) || !Object.hasOwn(option, "value") || !isVersion(option.version)) {
      failures.push(`The board option "${id2}" is recorded as no value with the version of its shape.`);
    }
  }
  if (failures.length > 0) throw new StoreError(refused, failures);
}
function unversioned(board) {
  const record = board;
  return {
    ...Object.fromEntries(Object.entries(record).filter(([name3]) => PROPERTIES.includes(name3))),
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id2, block]) => [
        id2,
        without(block, ["version"])
      ])
    ),
    options: Object.fromEntries(Object.entries(board.options).map(([id2, option]) => [id2, option.value])),
    plugins: Object.fromEntries(Object.entries(board.plugins).map(([cls, plugin]) => [cls, plugin.state]))
  };
}
function migrate(board, json, reply, registrations) {
  const failures = /* @__PURE__ */ new Set();
  const changed = /* @__PURE__ */ new Set();
  const plugins = Object.fromEntries(
    Object.entries(board.plugins).map(([cls, state2]) => {
      const def = registrations.plugin(cls)?.def;
      if (def === void 0) return [cls, state2];
      const from = own(json.plugins, cls)?.version;
      const to = own(reply.plugins, cls)?.version ?? 1;
      if (from === void 0 || from === to) return [cls, state2];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the plugin class "${cls}", while R gives version ${String(to)}.`
        );
        return [cls, state2];
      }
      if (def.migrate === void 0) return [cls, state2];
      changed.add("plugins");
      return [
        cls,
        migratedValue(`The migration of the plugin class "${cls}"`, () => def.migrate?.(frozenCopy(state2), from))
      ];
    })
  );
  const blocks = Object.fromEntries(
    Object.entries(board.blocks).map(([id2, block]) => {
      const from = own(json.blocks, id2)?.version;
      const to = own(reply.block_registry, block.registry_id)?.version;
      if (from === void 0 || to === void 0 || from === to) return [id2, block];
      if (from > to) {
        failures.add(
          `The block "${id2}" records version ${String(from)} of the state of "${block.registry_id}", while its class is at version ${String(to)}.`
        );
        return [id2, block];
      }
      const fn = registrations.migration(block.registry_id);
      if (fn === void 0) return [id2, block];
      changed.add("blocks");
      const state2 = migrated(
        `The migration of the block class "${block.registry_id}"`,
        () => fn(frozenCopy(block.state), from)
      );
      return [id2, { ...block, state: state2 }];
    })
  );
  const options = Object.fromEntries(
    Object.entries(board.options).map(([id2, value2]) => {
      const def = registrations.option(id2)?.def;
      if (def === void 0) return [id2, value2];
      const from = own(json.options, id2)?.version;
      const to = own(reply.options, id2)?.version ?? 1;
      if (from === void 0 || from === to) return [id2, value2];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the value of the board option "${id2}", while R gives version ${String(to)}.`
        );
        return [id2, value2];
      }
      if (def.migrate === void 0) return [id2, value2];
      changed.add("options");
      return [
        id2,
        migratedValue(`The migration of the board option "${id2}"`, () => def.migrate?.(frozenCopy(value2), from))
      ];
    })
  );
  if (failures.size > 0) throw new StoreError(refused, [...failures]);
  if (changed.size === 0) return board;
  return { ...board, plugins, blocks, options };
}
function migrated(what, fn) {
  const out = migratedValue(what, fn);
  if (!isObject(out)) throw new StoreError(refused, [`${what} returned no object.`]);
  return out;
}
function migratedValue(what, fn) {
  let out;
  try {
    out = fn();
  } catch (error) {
    throw new StoreError(refused, [`${what} failed: ${errorMessage(error)}`]);
  }
  if (out === void 0) throw new StoreError(refused, [`${what} returned nothing.`]);
  return frozenCopy(out);
}
function openJsonBoard(json, reply, registrations) {
  const registeredClass = (id2) => registrations.plugin(id2)?.def;
  const read = unversioned(json.board);
  const held2 = keptPlugins(read.plugins, reply.plugins, registeredClass);
  const plugins = droppedPlugins(read.plugins, reply.plugins, registeredClass).map((id2) => ({
    plugin: id2,
    package: packageFromId(id2)
  }));
  const migrated2 = migrate({ ...read, plugins: held2 }, json.board, reply, registrations);
  const gone = Object.entries(migrated2.blocks).filter(([, block]) => own(reply.block_registry, block.registry_id) === void 0).map(([id2]) => id2);
  const blocks = gone.map((id2) => {
    const registryId2 = migrated2.blocks[id2]?.registry_id ?? "";
    return { block: id2, registry_id: registryId2, package: packageFromId(registryId2) };
  });
  const unclaimed = Object.keys(migrated2.options).filter((id2) => registrations.option(id2) === void 0);
  const options = unclaimed.map((id2) => ({ option: id2 }));
  const kept = remove({ ...migrated2, options: without(migrated2.options, unclaimed) }, gone, registrations);
  const board = { ...kept, plugins: pluginsWithDefaults(kept.plugins, reply.plugins, registeredClass) };
  const failures = check(board, reply.block_registry, registrations);
  if (failures.length > 0) throw new StoreError(refused, failures);
  const packages = Object.entries(json.packages).filter(([pkg]) => own(reply.packages, pkg) === void 0).map(([pkg, version]) => ({ package: pkg, version }));
  return { board, dropped: [...packages, ...blocks, ...plugins, ...options] };
}
function remove(board, gone, registrations) {
  if (gone.length === 0) return board;
  const delta = expand({ blocks: { rm: gone } }, board, registrations);
  if ("failures" in delta) throw new StoreError(refused, delta.failures);
  const committed = commit(board, delta);
  if ("failures" in committed) throw new StoreError(refused, committed.failures);
  return committed.board;
}

// srcts/store/registrations.ts
import { computed as computed2, signal as signal2 } from "./vendor-858b8c85.js";
function createRegistrations(migration = () => void 0) {
  const classes = signal2(/* @__PURE__ */ new Map());
  const options = signal2([]);
  const optionIndex = computed2(
    () => new Map(options.value.map((option) => [option.def.id, option]))
  );
  const classDefs = computed2(
    () => Object.fromEntries([...classes.value].map(([id2, cls]) => [id2, cls.def]))
  );
  const optionDefs = computed2(
    () => Object.fromEntries(options.value.map((option) => [option.def.id, option.def]))
  );
  function checkInitial(what, validate, initial, field) {
    if (initial === void 0) return;
    const failures = validate(initial);
    if (failures.length > 0) {
      throw new Error(`The ${field} of ${what} ${failures.join(" ")}`);
    }
  }
  function registerPlugin(def) {
    const { id: id2 } = def;
    if (typeof id2 !== "string" || !isPluginClassId(id2)) {
      throw new Error(
        `The plugin class id ${JSON.stringify(id2)} is not named as R names a plugin class, "{pkg}-{name}_plugin".`
      );
    }
    if (classes.value.has(id2)) {
      throw new Error(`The plugin class "${id2}" is registered already.`);
    }
    const given = def;
    for (const [member, article] of [["update", "an"], ["migrate", "a"], ["start", "a"]]) {
      if (given[member] !== void 0 && typeof given[member] !== "function") {
        throw new Error(`The plugin class "${id2}" has ${article} ${member} that is no function.`);
      }
    }
    for (const member of ["ui", "frontend"]) {
      const drawn2 = given[member];
      if (drawn2 !== void 0 && (!isObject(drawn2) || typeof drawn2.render !== "function")) {
        throw new Error(`The plugin class "${id2}" has a ${member} without a render() function.`);
      }
    }
    const storage = given.storage;
    if (storage !== void 0) {
      if (!isObject(storage) || typeof storage.controls !== "function") {
        throw new Error(`The plugin class "${id2}" has a storage without a controls() function.`);
      }
      if (storage.subscribe !== void 0 && typeof storage.subscribe !== "function") {
        throw new Error(`The plugin class "${id2}" has a storage whose subscribe is no function.`);
      }
      if (storage.load !== void 0 && typeof storage.load !== "function") {
        throw new Error(`The plugin class "${id2}" has a storage whose load is no function.`);
      }
    }
    if (given.commands !== void 0 && !Array.isArray(given.commands)) {
      throw new Error(`The plugin class "${id2}" gives commands that are no array.`);
    }
    const what = `the state of the plugin class "${id2}"`;
    let validate = () => [];
    const state2 = given.state;
    if (state2 !== void 0) {
      if (!isObject(state2) || !isObject(state2.schema) || !Object.hasOwn(state2, "default")) {
        throw new Error(`The plugin class "${id2}" gives a state without a schema and a default.`);
      }
      validate = compile(what, state2.schema);
      checkInitial(what, validate, state2.default, "default");
    }
    const item = { def, validate };
    classes.value = new Map([...classes.value, [id2, item]]);
    return () => {
      if (classes.value.get(id2) !== item) return;
      const rest = new Map(classes.value);
      rest.delete(id2);
      classes.value = rest;
    };
  }
  return {
    plugins: {
      register: registerPlugin,
      registered: () => classDefs.value
    },
    options: {
      register(def) {
        const what = `the board option "${def.id}"`;
        if (optionIndex.value.has(def.id)) {
          throw new Error(`The board option "${def.id}" is registered already.`);
        }
        const ui = def.ui;
        if (!isObject(ui) || typeof ui.render !== "function") {
          throw new Error(`The board option "${def.id}" has no UI module with a render() function.`);
        }
        const { onChange } = def;
        if (onChange !== void 0 && typeof onChange !== "function") {
          throw new Error(`The onChange of the board option "${def.id}" is no function.`);
        }
        if (def.migrate !== void 0 && typeof def.migrate !== "function") {
          throw new Error(`The board option "${def.id}" has a migrate that is no function.`);
        }
        const validate = compile(what, def.schema);
        if (typeof def.default !== "function") {
          checkInitial(what, validate, def.default, "default");
        }
        const item = { def, validate };
        options.value = [...options.value, item];
        return () => {
          options.value = options.value.filter((x) => x !== item);
        };
      },
      registered: () => optionDefs.value
    },
    plugin: (id2) => classes.value.get(id2),
    option: (id2) => optionIndex.value.get(id2),
    migration
  };
}
function isPluginClassId(id2) {
  const pkg = packageFromId(id2);
  return pkg !== null && /^.+_plugin$/.test(id2.slice(pkg.length + 1));
}

// srcts/store/store.ts
var ENDED = "Another board opened in a new store, so this store changes its board no more.";
function createStore(options = {}) {
  const registrations = createRegistrations(options.migration);
  const board = signal3(void 0);
  let hello = options.hello;
  const registry = signal3(options.hello?.block_registry ?? {});
  const packages = signal3(options.hello?.packages ?? {});
  const history = signal3({ undo: [], redo: [] });
  const subscribers = /* @__PURE__ */ new Set();
  let opening2 = false;
  const ended = () => options.signal?.aborted === true;
  options.signal?.addEventListener(
    "abort",
    () => {
      subscribers.clear();
    },
    { once: true }
  );
  const edits = /* @__PURE__ */ new Map();
  let lastStep = 0;
  let busy = false;
  function guard2() {
    if (busy) {
      throw new Error(
        "The store is applying or announcing a change, which a subscriber, a plugin class's update or a migration may not change, since what follows from a delta is a plugin class's update."
      );
    }
  }
  function current2(refused2) {
    if (ended()) throw new StoreError(refused2, [ENDED]);
    if (board.value !== void 0) return board.value;
    throw new StoreError(refused2, ["The store holds no board until it opens one."]);
  }
  function announce2(change, next) {
    for (const fn of [...subscribers]) {
      try {
        fn(change, next);
      } catch (error) {
        log.error(asError(error));
      }
    }
  }
  function run(start2, deltas, how, refused2) {
    const applied = [];
    const inverses = [];
    let current3 = start2;
    for (const [i, given] of deltas.entries()) {
      const prefix = deltas.length > 1 ? `Delta ${String(i + 1)}: ` : "";
      const fail = (failures2) => new StoreError(refused2, failures2.map((failure) => prefix + failure));
      const delta = how.fit === true ? fit(given, current3) : given;
      if (how.fit === true && isEmpty(delta)) continue;
      const expanded = expand(delta, current3, registrations);
      if ("failures" in expanded) throw fail(expanded.failures);
      const committed = commit(current3, expanded, how.places?.[i]);
      if ("failures" in committed) throw fail(committed.failures);
      if (hello?.lock.board === true) {
        const locked2 = lockFailures(current3, committed.inverse.delta);
        if (locked2.length > 0) throw fail(locked2);
      }
      const inverse = isEmpty(committed.inverse.delta) ? void 0 : committed.inverse;
      if (inverse === void 0 && committed.changed.options.size === 0) continue;
      const failures = check(
        committed.board,
        registry.value,
        registrations,
        committed.changed
      );
      if (failures.length > 0) throw fail(failures);
      applied.push({ delta: expanded, board: committed.board, changed: committed.changed });
      if (inverse !== void 0 && given.undo !== false) inverses.unshift(inverse);
      current3 = committed.board;
    }
    return { applied, inverses };
  }
  function exists(next, key2) {
    const [kind, id2] = JSON.parse(key2);
    return Object.hasOwn(kind === "block" ? next.blocks : next.plugins, id2);
  }
  function describe2(key2) {
    const [kind, id2] = JSON.parse(key2);
    return kind === "block" ? `block "${id2}"` : `the plugin "${id2}"`;
  }
  function named4(deltas) {
    const failures = [];
    const compare = (key2, edit) => {
      if (edit === void 0) return;
      const id2 = edits.get(key2);
      if (id2 !== edit) {
        failures.push(
          id2 === void 0 ? `The delta was built on edit ${String(edit)} of ${describe2(key2)}, which the board lacks.` : `The delta was built on edit ${String(edit)} of ${describe2(key2)}, which another edit has replaced with edit ${String(id2)}.`
        );
      }
    };
    for (const delta of deltas) {
      for (const [id2, mod] of Object.entries(delta.blocks?.mod ?? {})) {
        compare(target({ block: id2 }), mod.edit);
      }
      for (const patch2 of delta.patches ?? []) {
        const plugin = pluginClassFromPath(patch2.path);
        if (plugin !== void 0) compare(target({ plugin }), patch2.edit);
      }
    }
    return [...new Set(failures)];
  }
  function number(applied, last) {
    const step2 = ++lastStep;
    for (const key2 of new Set(applied.flatMap(({ changed }) => [...changed.targets]))) {
      if (exists(last, key2)) edits.set(key2, step2);
      else edits.delete(key2);
    }
  }
  function runOnBoard(deltas) {
    const refused2 = "The store rejected the delta";
    const start2 = current2(refused2);
    const failures = named4(deltas);
    if (failures.length > 0) throw new StoreError(refused2, failures);
    return { start: start2, ...run(start2, deltas, {}, refused2) };
  }
  function apply(delta, origin = "user", opts = {}) {
    guard2();
    const given = isDeltas(delta) ? delta : [delta];
    if (given.length === 0) return false;
    const deltas = opts.undo === false ? given.map((each) => ({ ...each, undo: false })) : given;
    busy = true;
    try {
      const { start: start2, applied, inverses } = runOnBoard(deltas);
      if (applied.length === 0) return false;
      number(applied, applied.at(-1)?.board ?? start2);
      const undoStep = inverses.length > 0;
      if (undoStep) {
        history.value = { undo: [...history.value.undo, { origin, inverses }], redo: [] };
      }
      for (const { delta: delta2, board: next } of applied) {
        board.value = next;
        announce2({ delta: delta2, origin }, next);
      }
      return undoStep;
    } finally {
      busy = false;
    }
  }
  function dryRun(delta) {
    const deltas = isDeltas(delta) ? delta : [delta];
    const was = busy;
    busy = true;
    try {
      const { start: start2, applied } = runOnBoard(deltas);
      return applied.at(-1)?.board ?? start2;
    } finally {
      busy = was;
    }
  }
  function revert(back, origin) {
    guard2();
    const { undo, redo } = history.value;
    const at = back ? lastStepIndex(undo, origin) : redo.length - 1;
    const step2 = (back ? undo : redo)[at];
    if (step2 === void 0) return;
    busy = true;
    try {
      const refused2 = back ? "The store refused to undo the step" : "The store refused to redo the step";
      const start2 = current2(refused2);
      const { applied, inverses } = run(
        start2,
        step2.inverses.map(({ delta }) => delta),
        { fit: true, places: step2.inverses.map(({ places }) => places) },
        refused2
      );
      number(applied, applied.at(-1)?.board ?? start2);
      const kept = { origin: step2.origin, inverses };
      history.value = back ? { undo: [...undo.slice(0, at), ...undo.slice(at + 1)], redo: [...redo, kept] } : { undo: [...undo, kept], redo: redo.slice(0, -1) };
      const asking = origin ?? "user";
      for (const { delta, board: next } of applied) {
        board.value = next;
        announce2({ delta, origin: asking }, next);
      }
    } finally {
      busy = false;
    }
  }
  const refusedOpen = "The store refused to open the board";
  function prepareOpen(given) {
    if (typeof given !== "string") {
      hello = given;
      registry.value = given.block_registry;
      packages.value = given.packages;
      return { json: checkJsonBoard(given.board), reply: given };
    }
    if (hello === void 0) {
      throw new StoreError(refusedOpen, ["The store opens a JSON board only once R has answered `hello`."]);
    }
    if (hello.lock.load) {
      throw new StoreError(refusedOpen, [
        "The load lock fixes the app to its opening board, so the store opens no other board."
      ]);
    }
    return { json: checkJsonBoard(parse(given)), reply: hello };
  }
  function build(json, reply) {
    guard2();
    busy = true;
    try {
      return openJsonBoard(json, reply, registrations);
    } finally {
      busy = false;
    }
  }
  function stillOpen() {
    if (ended()) throw new StoreError(refusedOpen, [ENDED]);
  }
  function open(given) {
    return settled(() => {
      stillOpen();
      if (opening2) {
        throw new StoreError(refusedOpen, [
          "The store holds a board or opens one already: a store opens one board, and core opens the next in a new store as it starts again."
        ]);
      }
      opening2 = true;
      try {
        const { json, reply } = prepareOpen(given);
        const opened = build(json, reply);
        guard2();
        busy = true;
        try {
          board.value = opened.board;
          const step2 = ++lastStep;
          for (const key2 of everyTarget(opened.board)) edits.set(key2, step2);
          announce2({ opened: opened.board, dropped: opened.dropped }, opened.board);
          return opened.dropped;
        } finally {
          busy = false;
        }
      } catch (error) {
        opening2 = board.value !== void 0;
        throw error;
      }
    });
  }
  function checkText(text) {
    return settled(() => {
      stillOpen();
      const { json, reply } = prepareOpen(text);
      return build(json, reply).dropped;
    });
  }
  function jsonBoard() {
    const next = board.value;
    if (next === void 0) {
      throw new Error("The store holds no board until it opens one.");
    }
    const classes = Object.keys(next.plugins);
    const owned = /* @__PURE__ */ new Set([
      ...classes.flatMap((cls) => packageFromId(cls) ?? []),
      ...Object.values(next.blocks).flatMap(({ registry_id }) => packageFromId(registry_id) ?? [])
    ]);
    const recorded = Object.fromEntries(
      [...owned].sort().flatMap((pkg) => {
        const version = own(packages.value, pkg);
        return version === void 0 ? [] : [[pkg, version]];
      })
    );
    const blocks = Object.fromEntries(
      Object.entries(next.blocks).map(([id3, block]) => [
        id3,
        {
          registry_id: block.registry_id,
          version: own(registry.value, block.registry_id)?.version ?? 1,
          state: block.state,
          name: block.name,
          locked: block.locked
        }
      ])
    );
    const options2 = Object.fromEntries(
      Object.entries(next.options).map(([id3, value2]) => [
        id3,
        { value: value2, version: own(hello?.options ?? {}, id3)?.version ?? 1 }
      ])
    );
    const plugins = Object.fromEntries(
      Object.entries(next.plugins).map(([cls, state2]) => [
        cls,
        { state: state2, version: own(hello?.plugins ?? {}, cls)?.version ?? 1 }
      ])
    );
    const { id: id2, name: name3, links, stacks } = next;
    return {
      format: FORMAT,
      packages: recorded,
      board: { id: id2, name: name3, blocks, links, stacks, options: options2, plugins }
    };
  }
  function save() {
    return `${JSON.stringify(jsonBoard(), null, 2)}
`;
  }
  function everyTarget(next) {
    const targets = [
      ...Object.keys(next.blocks).map((id2) => ({ block: id2 })),
      ...Object.keys(next.plugins).map((id2) => ({ plugin: id2 }))
    ];
    return targets.map(target);
  }
  return {
    board() {
      if (board.value === void 0) {
        throw new Error("The store holds no board until it opens one.");
      }
      return board.value;
    },
    hasBoard: () => board.value !== void 0,
    locked: (block) => fixes(board.value, block, hello?.lock.board === true),
    subscribe(fn) {
      if (ended()) return () => void 0;
      const wrapped = (change, next) => {
        fn(change, next);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },
    apply,
    dryRun,
    open,
    check: checkText,
    undo: (origin) => {
      revert(true, origin);
    },
    redo: () => {
      revert(false);
    },
    canUndo: (origin) => lastStepIndex(history.value.undo, origin) >= 0,
    canRedo: () => history.value.redo.length > 0,
    plugins: registrations.plugins,
    options: registrations.options,
    jsonBoard,
    save,
    blockRegistry: () => registry.value,
    packages: () => packages.value,
    editId: (of) => edits.get(target(of))
  };
}
function lastStepIndex(steps2, origin) {
  return origin === void 0 ? steps2.length - 1 : steps2.map((step2) => step2.origin).lastIndexOf(origin);
}
function settled(fn) {
  try {
    return Promise.resolve(fn());
  } catch (error) {
    return Promise.reject(asError(error));
  }
}

// srcts/commands/schemas.ts
var id = { type: "string", pattern: "\\D" };
var blockId = {
  ...id,
  description: "The id of a block, unique among the board's blocks."
};
var linkId = {
  ...id,
  description: "The id of a link, unique among the board's links."
};
var stackId = {
  ...id,
  description: "The id of a stack, unique among the board's stacks."
};
var registryId = {
  type: "string",
  minLength: 1,
  description: `A block class's id in the block registry, such as "blockr.core-head_block".`
};
var name2 = {
  type: "string",
  description: "A name that the user sees and may change, which nothing refers to."
};
var input = {
  type: "string",
  description: 'An input of the block a link goes to, such as "data", or empty for a variadic block, which takes such links in their order.'
};
var state = {
  type: "object",
  description: "Values of a block's state args, which the schema of its class types."
};
var stateArgs = {
  type: "array",
  items: { type: "string" },
  uniqueItems: true,
  description: "Names of a block's state args."
};
var blockIds = {
  type: "array",
  items: blockId,
  uniqueItems: true,
  description: "Ids of blocks, in order."
};
var locked = {
  type: ["boolean", "null"],
  description: "A block's lock, which fixes its state where the board lock is on: true locks the block, false leaves it open, and null follows the board lock."
};
var color = {
  type: ["string", "null"],
  pattern: HEX_COLOR,
  description: 'A hex colour, such as "#66c2a5", which frontends draw a stack in, or null for none. A frontend draws a stack without a colour in a default of its own.'
};
var optionId = {
  type: "string",
  minLength: 1,
  description: 'The id of a board option, such as "n_rows".'
};
var value = {
  description: "A JSON value, which the schema of what it is set on types."
};
var pluginClassId = {
  type: "string",
  minLength: 1,
  description: `The id of a plugin's class, such as "blockr.dag-dag_plugin", which identifies the plugin, since an app runs at most one plugin of each class.`
};
var statePatches = {
  type: "array",
  items: {
    type: "object",
    properties: {
      op: { enum: ["add", "remove", "replace"] },
      path: {
        type: "string",
        pattern: "^(/.*)?$",
        description: `A JSON Pointer within the plugin's state, such as "/positions/a", or "" for the whole state.`
      },
      value: { description: "What an add or a replace puts at the path." }
    },
    required: ["op", "path"],
    additionalProperties: false
  },
  description: "Changes of a plugin's state, as JSON Patch operations, which apply in their order and which the schema of the plugin's class types."
};

// srcts/commands/core.ts
var newBlock = {
  registry_id: { ...registryId, description: "The class of the new block." },
  id: {
    ...blockId,
    description: "The new block's id, which is minted where it is left out."
  },
  name: {
    ...name2,
    description: "The new block's name, which its class gives where it is left out."
  },
  state: {
    ...state,
    description: "Values of the new block's state args, which the schema of its class types, in place of those its class starts a block with."
  }
};
function coreCommands(store) {
  function registryEntry(command2, registryId2) {
    const entry = own(store.blockRegistry(), registryId2);
    if (entry === void 0) {
      throw refusal(command2, [`The block registry has no block class "${registryId2}".`]);
    }
    return entry;
  }
  function added(command2, board2, args, mintId2) {
    const entry = registryEntry(command2, args.registry_id);
    const state2 = { ...entry.state, ...args.state };
    const failures = stateFailures(entry, state2);
    if (failures.length > 0) {
      throw refusal(command2, failures.map((failure) => `The state of the new block ${failure}`));
    }
    const id2 = args.id ?? mintId2((id3) => Object.hasOwn(board2.blocks, id3));
    return [id2, { registry_id: entry.id, state: state2, name: args.name ?? defaultBlockName(entry.id), locked: null }];
  }
  function inputFailures(board2, to, given) {
    const registry = store.blockRegistry();
    if (!named(given)) {
      const free = freeInputs(board2, registry, to) ?? [];
      return free.length > 0 ? [] : [noFreeInput(board2, to)];
    }
    const other = linkFeeding(board2, registry, to, given);
    return other === void 0 ? [] : [`The link "${other}" feeds the input "${given}" of block "${to}" already.`];
  }
  function noFreeInput(board2, to) {
    const block = own(board2.blocks, to);
    if (block === void 0) return `The board holds no block "${to}".`;
    const entry = own(store.blockRegistry(), block.registry_id);
    if (entry === void 0) return `The block registry has no block class "${block.registry_id}".`;
    if (entry.inputs.length === 0) {
      return `The block "${to}" takes no input, since its class "${entry.id}" has none.`;
    }
    return `Every input of the block "${to}" takes a link already.`;
  }
  function linkInput(command2, board2, to, given, except) {
    if (named(given)) return given;
    const free = freeInputs(board2, store.blockRegistry(), to, except) ?? [];
    return one(
      command2,
      free,
      noFreeInput(board2, to),
      `The block "${to}" has the free inputs ${quoted(free)}, so the args have to name the one to use.`
    );
  }
  const blocks = [
    command({
      id: "blockr.core-add_block",
      label: "Add block",
      description: "Adds a block of a class from the block registry, with the state its class starts a block with, changed by the state args given. To add a block that reads from another, give the new block an id, and link it with blockr.core-add_link in the same batch, after this command.",
      args: object(newBlock, ["registry_id"]),
      run(board2, args, { mintId: mintId2 }) {
        const [id2, block] = added(this.id, board2, args, mintId2);
        return { blocks: { add: { [id2]: block } } };
      }
    }),
    command({
      id: "blockr.core-remove_block",
      label: "Remove block",
      description: "Removes a block, with its links and its place in its stack.",
      args: object({ id: blockId }, ["id"]),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run: (_board, { id: id2 }) => ({ blocks: { rm: [id2] } })
    }),
    command({
      id: "blockr.core-rename_block",
      label: "Rename block",
      description: "Gives a block a new name.",
      args: object({ id: blockId, name: name2 }, ["id", "name"]),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run: (_board, { id: id2, name: name3 }) => ({ blocks: { mod: { [id2]: { name: name3 } } } })
    }),
    command({
      id: "blockr.core-set_block_lock",
      label: "Set block lock",
      description: "Sets a block's lock, which fixes its state where the board lock is on: true locks the block, false leaves it open to readers, and null has it follow the board lock.",
      args: object({ id: blockId, locked }, ["id", "locked"]),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run: (_board, { id: id2, locked: locked2 }) => ({ blocks: { mod: { [id2]: { locked: locked2 } } } })
    }),
    command({
      id: "blockr.core-set_block",
      label: "Set block state",
      description: "Sets values of a block's state args, which the schema of its class types, and unsets the state args named, as a block's UI submits an edit.",
      args: object(
        {
          id: blockId,
          state: { ...state, description: "Values of the block's state args to set." },
          unset: { ...stateArgs, description: "Names of the block's state args to unset." }
        },
        ["id"]
      ),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run(board2, { id: id2, state: state2, unset }) {
        const block = own(board2.blocks, id2);
        if (block === void 0) return [];
        const entry = registryEntry(this.id, block.registry_id);
        const failures = stateFailures(entry, { ...without(block.state, unset ?? []), ...state2 });
        if (failures.length > 0) {
          throw refusal(this.id, failures.map((failure) => `The state of block "${id2}" ${failure}`));
        }
        return {
          blocks: {
            mod: {
              [id2]: { ...state2 === void 0 ? {} : { state: state2 }, ...unset === void 0 ? {} : { unset } }
            }
          }
        };
      }
    })
  ];
  const links = [
    command({
      id: "blockr.core-add_link",
      label: "Add link",
      description: "Adds a link, through which a block takes the result of another as one of its inputs.",
      args: object(
        {
          from: { ...blockId, description: "The block whose result the link carries." },
          to: { ...blockId, description: "The block that takes the result." },
          input: {
            ...input,
            description: "The input of the block that takes the result, which may be left out where only one of its inputs is free or it is variadic."
          },
          id: {
            ...linkId,
            description: "The new link's id, which is minted where it is left out."
          }
        },
        ["from", "to"]
      ),
      preflight: (board2, { from, to, input: input2 }, { refusal: refusal2 }) => Object.hasOwn(board2.blocks, from) && Object.hasOwn(board2.blocks, to) && passes([...cycleFailures(board2, from, to), ...inputFailures(board2, to, input2)], refusal2),
      run(board2, { from, to, input: input2, id: id2 }, { mintId: mintId2 }) {
        const link = id2 ?? mintId2((id3) => Object.hasOwn(board2.links, id3));
        const resolved = linkInput(this.id, board2, to, input2);
        return { links: { add: { [link]: { from, to, input: resolved } } } };
      }
    }),
    command({
      id: "blockr.core-change_link",
      label: "Change link",
      description: "Changes the block a link comes from, the block it goes to or the input it feeds, keeping its id. Where the link keeps its block and another link feeds the input given, that other link takes the link's old input, so that the two links swap their inputs, as the links into a merge block's x and y.",
      args: object(
        {
          id: linkId,
          from: { ...blockId, description: "The block whose result the link carries." },
          to: { ...blockId, description: "The block that takes the result." },
          input: {
            ...input,
            description: "The input of the block that takes the result, which the link keeps where it is left out, unless the link goes to another block, where it may be left out only if one input of that block is free or the block is variadic. Where the link keeps its block, an input that another link feeds swaps the inputs of the two links."
          }
        },
        ["id"]
      ),
      preflight(board2, args, { refusal: refusal2 }) {
        const link = own(board2.links, args.id);
        if (link === void 0) return false;
        const from = args.from ?? link.from;
        const to = args.to ?? link.to;
        if (!Object.hasOwn(board2.blocks, from) || !Object.hasOwn(board2.blocks, to)) return false;
        const inputs = to === link.to ? [] : inputFailures(board2, to, args.input);
        return passes([...cycleFailures(board2, from, to, args.id), ...inputs], refusal2);
      },
      run(board2, args) {
        const link = own(board2.links, args.id);
        if (link === void 0) return [];
        const to = args.to ?? link.to;
        const input2 = args.input === void 0 && to === link.to ? link.input : linkInput(this.id, board2, to, args.input, args.id);
        const changed = { [args.id]: { from: args.from ?? link.from, to, input: input2 } };
        const other = to === link.to ? linkFeeding(board2, store.blockRegistry(), to, input2, args.id) : void 0;
        return { links: { mod: other === void 0 ? changed : { ...changed, [other]: { input: link.input } } } };
      }
    }),
    command({
      id: "blockr.core-move_link",
      label: "Move link",
      description: "Moves a link among the links into the block that it goes to, keeping its id and its ends. A variadic block takes its links in their order on the board, so an rbind block binds first the rows of the block that its first link comes from. A block that is not variadic takes each input by its name, so the command refuses a link into one.",
      args: object(
        {
          id: linkId,
          before: {
            ...linkId,
            description: "The link into the same block that the link goes before. Where it is left out, the link goes last among the links into its block."
          }
        },
        ["id"]
      ),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.links, id2),
      run(board2, { id: id2, before }) {
        const link = own(board2.links, id2);
        if (link === void 0) return [];
        const block = own(board2.blocks, link.to);
        if (block !== void 0 && !registryEntry(this.id, block.registry_id).inputs.includes("...")) {
          throw refusal(this.id, [
            `The block "${link.to}" is not variadic and takes each input by its name, so the order of its links changes nothing that it computes. The command "blockr.core-change_link" changes the input that a link feeds.`
          ]);
        }
        if (before === void 0) {
          const place = lastPlace(board2, link.to, id2);
          return place === void 0 ? [] : { links: { before: { [id2]: place } } };
        }
        if (before === id2) {
          throw refusal(this.id, [`The link "${id2}" cannot go before itself.`]);
        }
        const next = own(board2.links, before);
        if (next !== void 0 && next.to !== link.to) {
          throw refusal(this.id, [
            `The link "${id2}" goes into block "${link.to}" and the link "${before}" into block "${next.to}", so the args have to name a link into block "${link.to}".`
          ]);
        }
        return { links: { before: { [id2]: before } } };
      }
    }),
    command({
      id: "blockr.core-remove_link",
      label: "Remove link",
      description: "Removes a link.",
      args: object({ id: linkId }, ["id"]),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.links, id2),
      run: (_board, { id: id2 }) => ({ links: { rm: [id2] } })
    })
  ];
  const stacks = [
    command({
      id: "blockr.core-add_stack",
      label: "Add stack",
      description: "Adds a stack of blocks, taking each out of the stack it is in, since a block sits in one stack at most.",
      args: object({
        id: {
          ...stackId,
          description: "The new stack's id, which is minted where it is left out."
        },
        name: { ...name2, description: `The new stack's name, "Stack" where it is left out.` },
        blocks: { ...blockIds, description: "The blocks of the new stack, in order." },
        color: {
          ...color,
          description: "The new stack's colour, which frontends draw it in, or null for none. The stack gets null where this is left out, and a frontend draws a stack without a colour in a default of its own."
        }
      }),
      run(board2, { id: id2, name: name3 = "Stack", blocks: blocks2 = [], color: color2 = null }, { mintId: mintId2 }) {
        const stack = id2 ?? mintId2((id3) => Object.hasOwn(board2.stacks, id3));
        const left2 = leave(board2, stack, blocks2);
        return {
          stacks: {
            add: { [stack]: { name: name3, blocks: blocks2, color: color2 } },
            ...Object.keys(left2).length === 0 ? {} : { mod: left2 }
          }
        };
      }
    }),
    command({
      id: "blockr.core-change_stack",
      label: "Change stack",
      description: "Changes the blocks of a stack or its colour, taking each block out of the stack it is in, since a block sits in one stack at most.",
      args: object(
        {
          id: stackId,
          blocks: {
            ...blockIds,
            description: "The blocks of the stack, in order, which it keeps where they are left out."
          },
          color: {
            ...color,
            description: "The stack's colour, which frontends draw it in, or null to clear it. A frontend draws a stack without a colour in a default of its own. The stack keeps its colour where this is left out."
          }
        },
        ["id"]
      ),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (board2, { id: id2, blocks: blocks2, color: color2 }) => ({
        stacks: {
          mod: {
            ...blocks2 === void 0 ? {} : leave(board2, id2, blocks2),
            [id2]: {
              ...blocks2 === void 0 ? {} : { blocks: blocks2 },
              ...color2 === void 0 ? {} : { color: color2 }
            }
          }
        }
      })
    }),
    command({
      id: "blockr.core-rename_stack",
      label: "Rename stack",
      description: "Gives a stack a new name.",
      args: object({ id: stackId, name: name2 }, ["id", "name"]),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (_board, { id: id2, name: name3 }) => ({ stacks: { mod: { [id2]: { name: name3 } } } })
    }),
    command({
      id: "blockr.core-remove_stack",
      label: "Remove stack",
      description: "Removes a stack, leaving its blocks on the board.",
      args: object({ id: stackId }, ["id"]),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (_board, { id: id2 }) => ({ stacks: { rm: [id2] } })
    })
  ];
  const board = [
    command({
      id: "blockr.core-rename_board",
      label: "Rename board",
      description: "Gives the board a new name.",
      args: object({ name: name2 }, ["name"]),
      run: (_board, { name: name3 }) => ({ name: name3 })
    }),
    command({
      id: "blockr.core-set_option",
      label: "Set board option",
      description: "Sets the value of a board option, which the board option's schema types.",
      args: object({ id: optionId, value }, ["id", "value"]),
      preflight: (_board, { id: id2 }) => own(store.options.registered(), id2) !== void 0,
      run: (_board, { id: id2, value: value2 }) => ({ options: { set: { [id2]: value2 } } })
    })
  ];
  const plugins = [
    command({
      id: "blockr.core-set_plugin_state",
      label: "Set plugin state",
      description: "Changes a plugin's state by patches at paths within that state, as JSON Patch has them, which the schema of the plugin's class types. It fails its preflight on a locked board, where only the commands of a plugin's class know what the plugin locks.",
      args: object(
        {
          plugin: {
            ...pluginClassId,
            description: "The id of the class of the plugin whose state the patches change."
          },
          patches: statePatches
        },
        ["plugin", "patches"]
      ),
      preflight: (board2, { plugin }, { lock }) => !lock.board && Object.hasOwn(board2.plugins, plugin),
      run: (_board, { plugin, patches: patches2 }) => ({
        patches: patches2.map((patch2) => ({ ...patch2, path: `${pathFromSegments(["plugins", plugin])}${patch2.path}` }))
      })
    })
  ];
  return [...blocks, ...links, ...stacks, ...board, ...plugins];
}
function freeInputs(board, registry, to, except) {
  const block = own(board.blocks, to);
  if (block === void 0) return void 0;
  const inputs = own(registry, block.registry_id)?.inputs;
  if (inputs === void 0) return void 0;
  if (inputs.includes("...")) return [""];
  const fed = new Set(
    Object.entries(board.links).filter(([id2, link]) => id2 !== except && link.to === to).map(([, link]) => link.input)
  );
  return inputs.filter((input2) => !fed.has(input2));
}
function linkFeeding(board, registry, to, input2, except) {
  const block = own(board.blocks, to);
  const inputs = block === void 0 ? void 0 : own(registry, block.registry_id)?.inputs;
  if (inputs === void 0 || inputs.includes("...")) return void 0;
  return Object.entries(board.links).find(([id2, link]) => id2 !== except && link.to === to && link.input === input2)?.[0];
}
function defaultBlockName(registryId2) {
  const pkg = packageFromId(registryId2);
  const name3 = pkg === null ? registryId2 : registryId2.slice(pkg.length + 1);
  return sentenceCase(name3.replace(/_block$/, ""));
}
function cycleFailures(board, from, to, except) {
  if (from === to) return [`The link would join block "${to}" to itself.`];
  const links = except === void 0 ? board.links : without(board.links, [except]);
  const cycle = createGraph({ blocks: board.blocks, links }).path(to, from);
  return cycle === void 0 ? [] : [`The link would close a cycle through the blocks ${quoted(cycle)}.`];
}
function passes(failures, refusal2) {
  if (failures.length > 0) throw refusal2(failures);
  return true;
}
function lastPlace(board, to, id2) {
  const others = Object.keys(board.links).filter((other) => other !== id2);
  const last = others.filter((other) => own(board.links, other)?.to === to).at(-1);
  if (last === void 0) return void 0;
  return others[others.indexOf(last) + 1] ?? null;
}
function leave(board, id2, blocks) {
  const moving = new Set(blocks);
  return Object.fromEntries(
    Object.entries(board.stacks).filter(([other, stack]) => other !== id2 && stack.blocks.some((block) => moving.has(block))).map(([other, stack]) => [
      other,
      { blocks: stack.blocks.filter((block) => !moving.has(block)) }
    ])
  );
}
function object(properties, required = []) {
  return { type: "object", properties, required, additionalProperties: false };
}
function named(input2) {
  return input2 !== void 0 && input2 !== "";
}
function one(command2, inputs, none, several) {
  const [input2, ...rest] = inputs;
  if (input2 === void 0) throw refusal(command2, [none]);
  if (rest.length > 0) throw refusal(command2, [several]);
  return input2;
}
function command(def) {
  return {
    ...def,
    run: def.run.bind(def),
    ...def.preflight === void 0 ? {} : { preflight: def.preflight.bind(def) }
  };
}

// srcts/commands/commands.ts
var CommandError = class extends Error {
  failures;
  // The command's place in the list that run() or plan() was given, counted
  // from 0, where the command refused as one of a list.
  index;
  // False where the command failed its preflight, its preflight() answering
  // false, and left out otherwise, as a command's `undo` is false only where it
  // takes no undo step. The assistant's edit tool reads it to tell the model
  // what to check. A preflight() that throws the refusal() of its context
  // leaves it out, since that refusal says why.
  preflight;
  constructor(message, failures, options) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"), options);
    this.name = "CommandError";
    this.failures = failures;
    if (options?.index !== void 0) this.index = options.index;
    if (options?.preflight === false) this.preflight = false;
  }
};
function refusal(id2, failures, options) {
  return new CommandError(`The command "${id2}" refused to run`, failures, options);
}
var DROPPED = "A board opened while it prepared, which drops it.";
function createCommands(store, options = {}) {
  const commands = signal4(/* @__PURE__ */ new Map());
  const registered = computed3(
    () => Object.fromEntries([...commands.value].map(([id2, { command: command2 }]) => [id2, command2]))
  );
  const ended = () => options.signal?.aborted === true;
  const randId = asRandId(options.randId);
  const shared = {
    lock: options.lock ?? { board: false, load: false },
    mintId: (taken) => mintId(taken, randId),
    blockrOption: options.blockrOption ?? (() => Promise.resolve(void 0))
  };
  function register(command2) {
    const { id: id2 } = command2;
    if (typeof id2 !== "string" || !isCommandId(id2)) {
      throw new Error(
        `The command id ${JSON.stringify(id2)} is not named as a registry id is, "{pkg}-{name}".`
      );
    }
    if (typeof command2.label !== "string" || command2.label === "") {
      throw new Error(`The command "${id2}" has no label.`);
    }
    if (typeof command2.run !== "function") {
      throw new Error(`The command "${id2}" has no run().`);
    }
    if (commands.value.has(id2)) {
      throw new Error(`The command "${id2}" is registered already.`);
    }
    const validate = command2.args === void 0 ? void 0 : compile(`the args of the command "${id2}"`, command2.args, "fail their schema");
    const item = {
      command: command2,
      validate,
      context: { ...shared, refusal: (failures) => refusal(id2, failures) }
    };
    commands.value = new Map([...commands.value, [id2, item]]);
    return () => {
      if (commands.value.get(id2) !== item) return;
      const rest = new Map(commands.value);
      rest.delete(id2);
      commands.value = rest;
    };
  }
  function lookup(id2, given) {
    const item = commands.value.get(id2);
    if (item === void 0) {
      throw refusal(id2, [`No command "${id2}" is registered.`]);
    }
    const args = given ?? {};
    const failures = item.validate?.(args) ?? [];
    if (failures.length > 0) {
      throw refusal(id2, failures.map((failure) => `The args ${failure}`));
    }
    return { item, args };
  }
  function current2(id2) {
    if (ended()) {
      throw refusal(id2, ["Another board opened in a new store, so this command registry runs no command."]);
    }
    if (!store.hasBoard()) {
      throw refusal(id2, ["The store holds no board until it opens one."]);
    }
    return store.board();
  }
  function allow({ command: command2, context: context4 }, board, args) {
    const passes2 = attempt2(command2, () => command2.preflight?.(board, args, context4) ?? true);
    if (!passes2) {
      throw refusal(
        command2.id,
        [`It fails its preflight on the board as it is, with the args ${JSON.stringify(args)}.`],
        { preflight: false }
      );
    }
  }
  function build({ command: command2, context: context4 }, board, args) {
    const returned = attempt2(command2, () => command2.run(board, args, context4));
    const deltas2 = isDeltas(returned) ? returned : [returned];
    return deltas2.map((delta) => withEdits(command2.undo === false ? { ...delta, undo: false } : delta));
  }
  function deltas(item, args) {
    const board = current2(item.command.id);
    allow(item, board, args);
    if (item.command.prepare === void 0) return build(item, board, args);
    return prepared(item, board, args);
  }
  async function prepared(item, board, args) {
    const { command: command2 } = item;
    const input2 = await prepare(item, board, args);
    if (ended()) throw refusal(command2.id, [DROPPED]);
    const now = current2(command2.id);
    allow(item, now, args);
    return build(item, now, input2);
  }
  async function prepare({ command: command2, context: context4 }, board, args) {
    try {
      return await command2.prepare?.(board, args, context4);
    } catch (error) {
      throw failed(command2, error);
    }
  }
  async function planned(list, then) {
    const within = keepContext();
    const checked3 = list.map(({ id: id2, args }, index) => {
      try {
        return lookup(id2, args);
      } catch (error) {
        throw inList(index, id2, error);
      }
    });
    const [first] = checked3;
    if (first === void 0) return then([]);
    const inputs = /* @__PURE__ */ new Map();
    let built = [];
    let again = true;
    while (again) {
      again = false;
      built = [];
      let start2;
      try {
        start2 = current2(first.item.command.id);
      } catch (error) {
        throw inList(0, first.item.command.id, error);
      }
      let board = start2;
      for (const [index, { item, args }] of checked3.entries()) {
        const { command: command2 } = item;
        try {
          allow(item, board, args);
          if (command2.prepare !== void 0 && !inputs.has(index)) {
            inputs.set(index, await within(() => prepare(item, board, args)));
            if (ended()) throw refusal(command2.id, [DROPPED]);
            if (store.board() !== start2) {
              again = true;
              break;
            }
          }
          built.push(...build(item, board, inputs.has(index) ? inputs.get(index) : args));
          board = store.dryRun(built);
        } catch (error) {
          throw inList(index, command2.id, error);
        }
      }
    }
    return within(() => then(built));
  }
  function runList(list, origin) {
    const ids2 = list.map(({ id: id2 }) => id2);
    return traced(
      "command list",
      { "blockr.command.ids": ids2, "blockr.delta.origin": origin },
      () => planned(list, (built) => store.apply(built, origin))
    );
  }
  async function traced(name3, attributes, run) {
    const span = tracer().startSpan(name3, { attributes });
    try {
      return await otelContext.with(trace3.setSpan(otelContext.active(), span), run);
    } catch (error) {
      failSpan(span, error);
      throw error;
    } finally {
      span.end();
    }
  }
  function withEdits(delta) {
    const blocks = delta.blocks?.mod;
    const patches2 = delta.patches;
    if (blocks === void 0 && patches2 === void 0) return delta;
    const edit = (change, id2) => change.edit !== void 0 || id2 === void 0 ? change : { ...change, edit: id2 };
    return {
      ...delta,
      ...blocks === void 0 ? {} : {
        blocks: {
          ...delta.blocks,
          mod: Object.fromEntries(
            Object.entries(blocks).map(([id2, mod]) => [
              id2,
              edit(mod, store.editId({ block: id2 }))
            ])
          )
        }
      },
      ...patches2 === void 0 ? {} : {
        patches: patches2.map((patch2) => {
          const plugin = pluginClassFromPath(patch2.path);
          return plugin === void 0 ? patch2 : edit(patch2, store.editId({ plugin }));
        })
      }
    };
  }
  const registry = {
    register,
    registered: () => registered.value,
    preflight(id2, given) {
      try {
        const { item, args } = lookup(id2, given);
        allow(item, current2(id2), args);
        return true;
      } catch {
        return false;
      }
    },
    // A list comes with its origin second, in place of the args, and its
    // options third, in place of the origin. The command registry ignores the
    // notifier in the options, which CoreApi acts on (../core/core.ts).
    async run(target2, given, third) {
      if (typeof target2 !== "string") return runList(target2, typeof given === "string" ? given : "user");
      const origin = typeof third === "string" ? third : "user";
      return traced(`command ${target2}`, { "blockr.command.id": target2, "blockr.delta.origin": origin }, async () => {
        const within = keepContext();
        const { item, args } = lookup(target2, given);
        const step2 = deltas(item, args);
        const ready = Array.isArray(step2) ? step2 : await step2;
        return within(() => store.apply(ready, origin));
      });
    },
    async plan(target2, given) {
      if (typeof target2 !== "string") return planned(target2, (built) => built);
      const { item, args } = lookup(target2, given);
      return deltas(item, args);
    }
  };
  for (const command2 of coreCommands(store)) {
    registry.register(command2);
  }
  return registry;
}
function isCommandId(id2) {
  const pkg = packageFromId(id2);
  return pkg !== null && id2.length > pkg.length + 1;
}
var loggedErrors = /* @__PURE__ */ new WeakSet();
function isLogged(error) {
  return error instanceof CommandError && loggedErrors.has(error);
}
function inList(index, id2, error) {
  const named4 = error instanceof CommandError || error instanceof StoreError;
  const refused2 = new CommandError(
    `The list of commands was refused at command ${String(index + 1)}, "${id2}"`,
    named4 ? error.failures : [errorMessage(error)],
    { cause: error, index, preflight: error instanceof CommandError ? error.preflight : void 0 }
  );
  if (isLogged(error)) loggedErrors.add(refused2);
  return refused2;
}
function failed(command2, error) {
  if (error instanceof CommandError) return error;
  const failure = new CommandError(`The command "${command2.id}" failed`, [errorMessage(error)], {
    cause: error
  });
  log.error(failure);
  loggedErrors.add(failure);
  return failure;
}
function attempt2(command2, fn) {
  try {
    return fn();
  } catch (error) {
    throw failed(command2, error);
  }
}

// srcts/actions/clipboard.ts
async function copyText(doc, text) {
  const { clipboard } = doc.defaultView?.navigator ?? {};
  if (clipboard === void 0) return copyCommand(doc, text);
  try {
    await clipboard.writeText(text);
    return true;
  } catch {
    return copyCommand(doc, text);
  }
}
function copyCommand(doc, text) {
  const before = doc.activeElement;
  const field = doc.createElement("textarea");
  field.value = text;
  field.setAttribute("readonly", "");
  field.style.cssText = "position: fixed; inset-block-start: 0; opacity: 0;";
  doc.body.append(field);
  field.select();
  try {
    return doc.execCommand("copy");
  } catch {
    return false;
  } finally {
    field.remove();
    if (before instanceof HTMLElement) before.focus({ preventScroll: true });
  }
}

// srcts/actions/core.ts
function coreBoardActions(raise) {
  return [
    {
      id: "blockr.core-rename_action",
      label: "Rename",
      on: ["block", "stack"],
      category: "edit",
      offers: (board, target2, lock) => !lock.board && named2(board, target2) !== void 0,
      quick(board, target2) {
        const object2 = named2(board, target2);
        if (object2 === void 0) return void 0;
        return {
          rows: [
            {
              kind: "text",
              label: "Name",
              value: object2.name,
              check: (value2) => value2.trim() === "" ? "A name cannot be empty." : void 0,
              submit(value2) {
                const name3 = value2.trim();
                return name3 === object2.name ? [] : [{ id: object2.command, args: { id: object2.id, name: name3 } }];
              }
            }
          ]
        };
      }
    },
    {
      id: "blockr.core-copy_block_id_action",
      label: "Copy block ID",
      detail: (_board, target2) => blockOf(target2),
      on: ["block"],
      category: "clipboard",
      offers: (board, target2) => heldBlock(board, target2) !== void 0,
      async run(_board, target2) {
        const block = blockOf(target2);
        if (block === void 0) return void 0;
        if (!await copyText(document, block)) {
          raise(
            coreNotification("copy_failed", "error", `The browser did not let core copy the block ID "${block}".`, {
              about: { block }
            })
          );
        }
        return void 0;
      }
    },
    {
      id: "blockr.core-remove_block_action",
      label: "Remove block",
      icon: TRASH,
      on: ["block"],
      category: "remove",
      offers: (board, target2, lock) => !lock.board && heldBlock(board, target2) !== void 0,
      run: (_board, target2) => removing("blockr.core-remove_block", blockOf(target2))
    },
    {
      id: "blockr.core-remove_link_action",
      label: "Remove link",
      icon: TRASH,
      on: ["link"],
      category: "remove",
      offers: (board, target2, lock) => !lock.board && "link" in target2 && Object.hasOwn(board.links, target2.link),
      run: (_board, target2) => removing("blockr.core-remove_link", "link" in target2 ? target2.link : void 0)
    },
    {
      id: "blockr.core-remove_stack_action",
      label: "Remove stack",
      icon: TRASH,
      on: ["stack"],
      category: "remove",
      offers: (board, target2, lock) => !lock.board && "stack" in target2 && Object.hasOwn(board.stacks, target2.stack),
      run: (_board, target2) => removing("blockr.core-remove_stack", "stack" in target2 ? target2.stack : void 0)
    },
    {
      id: "blockr.core-remove_selected_action",
      label: "Remove",
      icon: TRASH,
      on: ["selection"],
      category: "remove",
      offers: (board, target2, lock) => !lock.board && removals(board, target2).length > 0,
      run: (board, target2) => removals(board, target2)
    }
  ];
}
function named2(board, target2) {
  if ("block" in target2) {
    const block = own(board.blocks, target2.block);
    return block === void 0 ? void 0 : { id: target2.block, name: block.name, command: "blockr.core-rename_block" };
  }
  if ("stack" in target2) {
    const stack = own(board.stacks, target2.stack);
    return stack === void 0 ? void 0 : { id: target2.stack, name: stack.name, command: "blockr.core-rename_stack" };
  }
  return void 0;
}
function blockOf(target2) {
  return "block" in target2 ? target2.block : void 0;
}
function heldBlock(board, target2) {
  const block = blockOf(target2);
  return block !== void 0 && Object.hasOwn(board.blocks, block) ? block : void 0;
}
function removing(command2, id2) {
  return id2 === void 0 ? void 0 : [{ id: command2, args: { id: id2 } }];
}
function removals(board, target2) {
  if (!("selection" in target2)) return [];
  const links = [];
  const blocks = [];
  const stacks = [];
  for (const object2 of target2.selection) {
    if ("link" in object2 && Object.hasOwn(board.links, object2.link)) {
      links.push({ id: "blockr.core-remove_link", args: { id: object2.link } });
    } else if ("block" in object2 && Object.hasOwn(board.blocks, object2.block)) {
      blocks.push({ id: "blockr.core-remove_block", args: { id: object2.block } });
    } else if ("stack" in object2 && Object.hasOwn(board.stacks, object2.stack)) {
      stacks.push({ id: "blockr.core-remove_stack", args: { id: object2.stack } });
    }
  }
  return [...links, ...blocks, ...stacks];
}
var TRASH = '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.25" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M2.5 4.5h11M6.5 4.5V3h3v1.5M4 4.5l.7 9h6.6l.7-9"/></svg>';

// srcts/actions/quick.ts
var SUBJECTS = {
  quick: "The quick action of the board action",
  pick: "A row of the quick action of the board action",
  submit: "A row of the quick action of the board action"
};
var GAVE = {
  quick: 'It gave no level of rows, as { rows: [{ kind: "item", label: "Head", pick: () => [] }] }.',
  pick: "It gave neither a batch of commands nor a level of rows.",
  submit: 'It gave no batch of commands, as [{ id: "blockr.core-rename_block", args: { id: "a", name: "Head" } }].'
};
var NO_BATCH = 'It gave no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].';
var MENU = "Board actions";
var FIRST = ["add", "edit", "clipboard"];
var LAST = "remove";
function inMenuOrder(actions) {
  const seen = [...new Set(actions.map(({ category }) => category))].filter(
    (category) => !FIRST.includes(category) && category !== LAST
  );
  const order = [...FIRST, ...seen, LAST];
  return [...actions].sort((a, b) => order.indexOf(a.category) - order.indexOf(b.category));
}
function openMenu(start2, parts) {
  const stack = [];
  const listeners = /* @__PURE__ */ new Set();
  let ended = false;
  let levels = 0;
  let view;
  function changed() {
    view = void 0;
    for (const fn of [...listeners]) fn();
  }
  function now() {
    view ??= shownNow();
    return view;
  }
  function entry(action2, state2, actions) {
    return {
      id: ++levels,
      action: action2,
      ...actions === void 0 ? {} : { actions },
      state: state2,
      filter: "",
      values: /* @__PURE__ */ new Map(),
      tried: /* @__PURE__ */ new Set(),
      ticks: /* @__PURE__ */ new Map()
    };
  }
  function place(state2, action2, replacing) {
    const next = entry(action2, state2);
    if (replacing === void 0) stack.push(next);
    else stack.splice(stack.indexOf(replacing), 1, next);
    changed();
    return next;
  }
  function end(ran) {
    if (ended) return;
    ended = true;
    stack.length = 0;
    changed();
    listeners.clear();
    parts.closed(ran);
  }
  function finish(batch2, action2) {
    if (batch2.length === 0) {
      end();
      return Promise.resolve();
    }
    return closing(parts.run(batch2, action2));
  }
  function closing(ran) {
    ran.catch(() => void 0);
    end(ran);
    return ran;
  }
  function settle2(given, action2, what, replacing) {
    let value2;
    try {
      value2 = given();
    } catch (error) {
      place(failure(action2, what, error), action2, replacing);
      return Promise.resolve();
    }
    if (!isThenable(value2)) return outcome(value2, action2, what, replacing);
    const loading = place({ kind: "loading" }, action2, replacing);
    const current2 = () => !ended && stack.at(-1) === loading;
    return Promise.resolve(value2).then(
      (resolved) => current2() ? outcome(resolved, action2, what, loading) : void 0,
      (error) => {
        if (current2()) place(failure(action2, what, error), action2, loading);
      }
    );
  }
  function outcome(value2, action2, what, replacing) {
    if (what === "quick" && value2 === void 0) {
      place(
        {
          kind: "error",
          message: `The board action "${action2.id}" gives no quick action for the target, and core does not open advanced actions yet.`
        },
        action2,
        replacing
      );
      return Promise.resolve();
    }
    if (what !== "quick" && isBatch(value2)) return finish(value2, action2);
    if ((what === "quick" || what === "pick") && isLevel(value2)) {
      place({ kind: "level", level: value2 }, action2, replacing);
      return Promise.resolve();
    }
    place(failure(action2, what, new TypeError(GAVE[what])), action2, replacing);
    return Promise.resolve();
  }
  function failure(action2, what, error) {
    log.error(new Error(`${SUBJECTS[what]} "${action2.id}" failed: ${errorMessage(error)}`, { cause: error }));
    return { kind: "error", message: errorMessage(error) };
  }
  function shownNow() {
    const current2 = stack.at(-1);
    const back = stack.length > 1;
    if (current2 === void 0) return { level: { id: 0, label: MENU, back: false, loading: false, rows: [] }, rows: [] };
    const { id: id2, state: state2 } = current2;
    const label = current2.action?.label ?? MENU;
    if (state2.kind === "loading") return { level: { id: id2, label, back, loading: true, rows: [] }, rows: [] };
    if (state2.kind === "error") return { level: { id: id2, label, back, loading: false, error: state2.message, rows: [] }, rows: [] };
    const { title, filter } = state2.level;
    const rows = filtered(state2.level, filter === true ? folded(current2.filter.trim()) : "");
    return {
      level: {
        id: id2,
        label: title ?? label,
        ...title === void 0 ? {} : { title },
        ...filter === true ? { filter: current2.filter } : {},
        back,
        loading: false,
        rows: rows.map((showing) => shownRow(current2, showing))
      },
      rows
    };
  }
  function shownRow(current2, { row, index, choices }) {
    if (row.kind === "item") {
      const { label, heading, icon, detail, category } = row;
      return {
        kind: "item",
        label,
        ...heading === void 0 ? {} : { heading },
        ...icon === void 0 ? {} : { icon },
        ...detail === void 0 ? {} : { detail },
        ...category === void 0 ? {} : { category }
      };
    }
    if (row.kind === "multi") {
      const ticked = ticks(current2, index, row);
      return {
        kind: "multi",
        label: row.label,
        choices: (choices ?? row.choices).map(({ id: id2, label }) => ({ id: id2, label, picked: ticked.has(id2) }))
      };
    }
    const value2 = current2.values.get(index) ?? row.value ?? "";
    const refusal2 = current2.tried.has(index) ? refused2(current2, row, value2) : void 0;
    return { kind: "text", label: row.label, value: value2, ...refusal2 === void 0 ? {} : { refusal: refusal2 } };
  }
  function refused2(current2, row, value2) {
    if (row.check === void 0) return void 0;
    try {
      const why = row.check(value2);
      return typeof why === "string" && why !== "" ? why : void 0;
    } catch (error) {
      if (current2.action !== void 0) failure(current2.action, "submit", error);
      return errorMessage(error);
    }
  }
  function at(index) {
    const current2 = stack.at(-1);
    if (ended || current2?.state.kind !== "level") return void 0;
    const found = index === void 0 ? void 0 : now().rows[index];
    return found === void 0 ? { current: current2 } : { current: current2, found };
  }
  function enter(action2) {
    if (action2.run !== void 0) return closing(parts.trigger(action2));
    if (action2.quick !== void 0) return settle2(() => action2.quick?.(parts.board(), parts.target, parts.core()), action2, "quick");
    place(
      { kind: "error", message: `The board action "${action2.id}" asks the user through its advanced action, which core does not open yet.` },
      action2
    );
    return Promise.resolve();
  }
  function submit(current2, row, index, action2) {
    const value2 = current2.values.get(index) ?? row.value ?? "";
    current2.tried.add(index);
    if (refused2(current2, row, value2) !== void 0) {
      changed();
      return Promise.resolve();
    }
    return settle2(() => row.submit(value2), action2, "submit");
  }
  function applied(current2) {
    const { action: action2, state: state2 } = current2;
    if (action2 === void 0 || state2.kind !== "level") return [];
    return state2.level.rows.flatMap((row, index) => {
      if (row.kind !== "multi" || !current2.ticks.has(index)) return [];
      const ticked = ticks(current2, index, row);
      const before = row.choices.filter((choice) => choice.picked).map(({ id: id2 }) => id2);
      const picked = row.choices.filter((choice) => ticked.has(choice.id)).map(({ id: id2 }) => id2);
      if (picked.length === before.length && picked.every((id2, i) => id2 === before[i])) return [];
      try {
        const batch2 = row.apply(picked);
        if (!isBatch(batch2)) throw new TypeError(NO_BATCH);
        return batch2;
      } catch (error) {
        parts.failed(action2, new Error(`${SUBJECTS.submit} "${action2.id}" failed: ${errorMessage(error)}`, { cause: error }));
        return [];
      }
    });
  }
  function pick2(index) {
    const shown2 = at(index);
    if (shown2 === void 0) return Promise.resolve();
    const { current: current2, found } = shown2;
    if (index === void 0) {
      const rows = now().rows;
      const first = rows.findIndex(({ row: row3 }) => row3.kind !== "text");
      const row2 = rows[first];
      if (row2?.row.kind === "multi") {
        const choice = (row2.choices ?? row2.row.choices)[0];
        if (choice !== void 0) tick(first, choice.id);
        return Promise.resolve();
      }
      return row2 === void 0 ? Promise.resolve() : pick2(first);
    }
    if (found === void 0) return Promise.resolve();
    const action2 = current2.actions?.[found.index];
    if (action2 !== void 0) {
      if (parts.applies(action2)) return enter(action2);
      end();
      return Promise.resolve();
    }
    const { row } = found;
    if (current2.action === void 0) return Promise.resolve();
    if (row.kind === "item") return settle2(() => row.pick(), current2.action, "pick");
    if (row.kind === "text") return submit(current2, row, found.index, current2.action);
    return Promise.resolve();
  }
  function tick(index, choice) {
    const shown2 = at(index);
    const row = shown2?.found?.row;
    if (shown2?.found === void 0 || row?.kind !== "multi" || !row.choices.some(({ id: id2 }) => id2 === choice)) return;
    const { current: current2, found } = shown2;
    const ticked = new Set(ticks(current2, found.index, row));
    if (ticked.has(choice)) ticked.delete(choice);
    else ticked.add(choice);
    current2.ticks.set(found.index, ticked);
    changed();
  }
  if ("actions" in start2) {
    const board = parts.board();
    const actions = inMenuOrder(start2.actions);
    stack.push(
      entry(void 0, { kind: "level", level: { rows: actions.map((action2) => actionRow(action2, board, parts.target)) } }, actions)
    );
  } else {
    void enter(start2.action);
  }
  return {
    get: () => now().level,
    subscribe(fn) {
      if (ended) return () => void 0;
      const wrapped = () => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    closed: () => ended,
    pick: pick2,
    tick,
    type(value2, index) {
      const shown2 = at(index);
      if (shown2 === void 0) return;
      const { current: current2, found } = shown2;
      if (index === void 0) {
        if (current2.state.kind !== "level" || current2.state.level.filter !== true || current2.filter === value2) return;
        current2.filter = value2;
        changed();
        return;
      }
      if (found?.row.kind !== "text") return;
      current2.values.set(found.index, value2);
      changed();
    },
    escape() {
      if (ended) return;
      if (stack.length > 1) {
        stack.pop();
        changed();
        return;
      }
      end();
    },
    close() {
      const current2 = stack.at(-1);
      if (ended || current2 === void 0) return Promise.resolve();
      const batch2 = applied(current2);
      if (current2.action !== void 0) return finish(batch2, current2.action);
      end();
      return Promise.resolve();
    },
    drop() {
      end();
    }
  };
}
function ticks(current2, index, row) {
  return current2.ticks.get(index) ?? new Set(row.choices.filter((choice) => choice.picked).map(({ id: id2 }) => id2));
}
function filtered(level, query) {
  return level.rows.flatMap((row, index) => {
    if (query === "" || row.kind === "text") return [{ row, index }];
    if (row.kind === "item") return [row.label, row.detail ?? ""].some((text) => folded(text).includes(query)) ? [{ row, index }] : [];
    if (folded(row.label).includes(query)) return [{ row, index }];
    const choices = row.choices.filter((choice) => folded(choice.label).includes(query));
    return choices.length === 0 ? [] : [{ row, index, choices }];
  });
}
function actionRow(action2, board, target2) {
  let detail;
  try {
    const given = action2.detail?.(board, target2);
    if (typeof given === "string" && given !== "") detail = given;
  } catch (error) {
    log.error(new Error(`The detail of the board action "${action2.id}" failed: ${errorMessage(error)}`, { cause: error }));
  }
  return {
    kind: "item",
    label: action2.label,
    ...action2.icon === void 0 ? {} : { icon: action2.icon },
    ...detail === void 0 ? {} : { detail },
    category: action2.category,
    pick: () => []
  };
}
function isBatch(value2) {
  return Array.isArray(value2) && value2.every((call) => isObject(call) && typeof call.id === "string");
}
function isLevel(value2) {
  if (!isObject(value2) || !Array.isArray(value2.rows)) return false;
  if (!optional(value2.title, "string") || !optional(value2.filter, "boolean")) return false;
  return value2.rows.every(isRow);
}
function isRow(row) {
  if (!isObject(row) || typeof row.label !== "string" || row.label === "") return false;
  switch (row.kind) {
    case "item":
      return typeof row.pick === "function" && [row.heading, row.icon, row.detail, row.category].every((member) => optional(member, "string"));
    case "multi":
      return typeof row.apply === "function" && Array.isArray(row.choices) && row.choices.every(
        (choice) => isObject(choice) && typeof choice.id === "string" && typeof choice.label === "string" && typeof choice.picked === "boolean"
      );
    case "text":
      return typeof row.submit === "function" && optional(row.value, "string") && optional(row.check, "function");
    default:
      return false;
  }
}
function optional(member, type) {
  return member === void 0 || typeof member === type;
}
function isThenable(value2) {
  return (typeof value2 === "object" || typeof value2 === "function") && value2 !== null && typeof value2.then === "function";
}
function folded(text) {
  return text.normalize("NFD").replace(/\p{Diacritic}/gu, "").toLocaleLowerCase();
}

// srcts/actions/actions.ts
var KINDS2 = {
  board: true,
  block: true,
  link: true,
  stack: true,
  option: true,
  plugin: true,
  selection: true
};
function createBoardActions(raise) {
  let registered = coreBoardActions(raise);
  const classes = /* @__PURE__ */ new Map();
  return {
    register(plugin, given) {
      if (given === void 0) return () => void 0;
      const actions = checked2(plugin, given, new Set(registered.map(({ id: id2 }) => id2)));
      registered = [...registered, ...actions];
      for (const { id: id2 } of actions) classes.set(id2, plugin);
      return () => {
        registered = registered.filter((action2) => !actions.includes(action2));
        for (const { id: id2 } of actions) classes.delete(id2);
      };
    },
    registered: () => registered,
    classOf: (id2) => classes.get(id2)
  };
}
function checked2(plugin, given, taken) {
  const of = `The plugin class "${plugin}"`;
  if (!Array.isArray(given)) {
    throw new Error(`${of} gives board actions that are no array.`);
  }
  const ids2 = new Set(taken);
  return given.map((action2) => {
    if (!isObject(action2)) {
      throw new Error(`${of} gives a board action that is no object.`);
    }
    const { id: id2 } = action2;
    if (typeof id2 !== "string" || !isCommandId(id2)) {
      throw new Error(`${of} gives a board action whose id ${JSON.stringify(id2)} is not named as a command's id is, "{pkg}-{name}".`);
    }
    const named4 = `${of} gives the board action "${id2}"`;
    if (ids2.has(id2)) {
      throw new Error(`${named4}, which has the id of another board action.`);
    }
    ids2.add(id2);
    if (typeof action2.label !== "string" || action2.label === "") {
      throw new Error(`${named4} without a label.`);
    }
    if (action2.icon !== void 0 && typeof action2.icon !== "string") {
      throw new Error(`${named4}, whose icon is no string of SVG markup.`);
    }
    const on = action2.on;
    if (!Array.isArray(on)) {
      throw new Error(`${named4}, whose on is no array of the kinds of target that it applies to.`);
    }
    if (on.length === 0) {
      throw new Error(`${named4}, whose on is empty, so that no menu would offer it.`);
    }
    const other = on.find((kind) => typeof kind !== "string" || !Object.hasOwn(KINDS2, kind));
    if (other !== void 0) {
      throw new Error(`${named4}, whose on names ${JSON.stringify(other)}, which is none of the kinds of target, ${quoted(Object.keys(KINDS2))}.`);
    }
    if (typeof action2.category !== "string" || action2.category === "") {
      throw new Error(`${named4} without a category.`);
    }
    for (const member of ["detail", "offers", "run", "quick"]) {
      if (action2[member] !== void 0 && typeof action2[member] !== "function") {
        throw new Error(`${named4}, whose ${member} is no function.`);
      }
    }
    const asks = ["quick", "advanced"].filter((member) => action2[member] !== void 0);
    if (action2.run !== void 0 && asks.length > 0) {
      throw new Error(`${named4}, which has run beside ${asks.join(" and ")}, while run is for a board action that asks the user nothing.`);
    }
    if (action2.run === void 0 && asks.length === 0) {
      throw new Error(`${named4}, which has none of run, quick and advanced.`);
    }
    return action2;
  });
}
function boardActionsApi(actions, parts) {
  const { store, lock, refused: refused2, raiseAgain, signal: signal5 } = parts;
  let shown2;
  signal5?.addEventListener(
    "abort",
    () => {
      shown2?.drop();
    },
    { once: true }
  );
  function applies(action2, board, target2) {
    if (!action2.on.includes(kindOf(target2))) return false;
    if (action2.offers === void 0) return true;
    try {
      return action2.offers(board, target2, lock);
    } catch (error) {
      log.error(new Error(`The offers of the board action "${action2.id}" failed: ${errorMessage(error)}`, { cause: error }));
      return false;
    }
  }
  function on(target2) {
    if (!store.hasBoard()) return [];
    const board = store.board();
    return actions.registered().filter((action2) => applies(action2, board, target2));
  }
  async function batchOf(action2, target2, notifier) {
    let batch2;
    try {
      batch2 = await action2.run?.(store.board(), target2, parts.core());
    } catch (error) {
      throw failed2(action2, new Error(`The board action "${action2.id}" failed: ${errorMessage(error)}`, { cause: error }), notifier);
    }
    if (batch2 !== void 0 && !isBatch(batch2)) {
      throw failed2(
        action2,
        new TypeError(`The run of the board action "${action2.id}" returned no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].`),
        notifier
      );
    }
    return batch2;
  }
  function failed2(action2, failure, notifier) {
    log.error(failure);
    const plugin = actions.classOf(action2.id);
    raiseAgain(
      coreNotification(
        "board_action_failed",
        "error",
        failure.message,
        plugin === void 0 ? {} : { module: sourceModule({ plugin }, packageFromId(plugin)) }
      ),
      notifier
    );
    return failure;
  }
  function notifierOf(given, action2) {
    const { notifier } = given;
    return notifier === void 0 ? void 0 : (notifications) => {
      notifier(notifications, action2);
    };
  }
  async function runBatch(batch2, action2, given) {
    if (batch2.length === 0) return;
    const core = parts.core();
    const notifier = notifierOf(given, action2);
    let added = [];
    if (given.extend !== void 0) {
      let deltas;
      try {
        deltas = await core.commands.plan(batch2);
      } catch (error) {
        refused2(error, void 0, notifier);
        throw error;
      }
      const extra = given.extend(deltas);
      if (!isBatch(extra)) {
        throw new TypeError(`The \`extend\` of ${given.call} returned no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].`);
      }
      added = extra;
    }
    await core.commands.run([...batch2, ...added], "user", notifier === void 0 ? void 0 : { notifier });
  }
  async function runOf(action2, target2, given) {
    const batch2 = await batchOf(action2, target2, notifierOf(given, action2));
    if (batch2 !== void 0) await runBatch(batch2, action2, given);
  }
  function open(start2, target2, given, closed) {
    void shown2?.close().catch(() => void 0);
    let hide;
    const menu = openMenu(start2, {
      board: () => store.board(),
      target: target2,
      core: parts.core,
      applies: (action2) => store.hasBoard() && applies(action2, store.board(), target2),
      run: (batch2, action2) => runBatch(batch2, action2, given),
      trigger: (action2) => runOf(action2, target2, given),
      failed(action2, failure) {
        failed2(action2, failure, notifierOf(given, action2));
      },
      closed(ran) {
        if (shown2 === menu) shown2 = void 0;
        hide?.();
        hide = void 0;
        closed?.(ran);
      }
    });
    shown2 = menu;
    hide = parts.show(menu, { el: given.el, anchor: given.anchor ?? given.el.getBoundingClientRect() });
  }
  return {
    on: (target2) => on(targetGiven(target2)),
    menu(target2, opts) {
      const checked3 = targetGiven(target2);
      const given = optionsGiven(opts, "menu()");
      if (signal5?.aborted === true) return;
      const offered = on(checked3);
      if (offered.length > 0) open({ actions: offered }, checked3, given);
    },
    async trigger(id2, target2, opts) {
      const checked3 = targetGiven(target2);
      const given = optionsGiven(opts, "trigger()");
      const action2 = actions.registered().find((registered) => registered.id === id2);
      if (action2 === void 0) {
        throw new Error(`No board action "${id2}" is registered.`);
      }
      if (action2.run === void 0 && action2.quick === void 0) {
        throw new Error(`The board action "${id2}" asks the user through its advanced action alone, which core does not open yet.`);
      }
      if (signal5?.aborted === true || !store.hasBoard() || !applies(action2, store.board(), checked3)) return;
      if (action2.run !== void 0) {
        await runOf(action2, checked3, given);
        return;
      }
      await new Promise((resolve, reject) => {
        open({ action: action2 }, checked3, given, (ran) => {
          if (ran === void 0) resolve();
          else ran.then(resolve, reject);
        });
      });
    }
  };
}
function optionsGiven(opts, call) {
  const given = isObject(opts) ? opts : {};
  const { el, anchor, extend, notifier } = given;
  if (!isElement(el)) {
    throw new TypeError(`The \`el\` of ${call} is the element that the user triggered from, to which the keyboard's focus returns.`);
  }
  if (anchor === void 0 ? call === "menu()" : !isRect(anchor)) {
    throw new TypeError(`The \`anchor\` of ${call} is the box at which the menu opens, a DOMRect.`);
  }
  if (extend !== void 0 && typeof extend !== "function") {
    throw new TypeError(`The \`extend\` of ${call} is a function.`);
  }
  if (notifier !== void 0 && typeof notifier !== "function") {
    throw new TypeError(`The \`notifier\` of ${call} is a function.`);
  }
  return {
    call,
    el,
    anchor,
    // Each is called as a method of the options, as a caller may write it.
    extend: extend === void 0 ? void 0 : (deltas) => extend.call(given, deltas),
    notifier: notifier === void 0 ? void 0 : (notifications, action2) => {
      notifier.call(given, notifications, action2);
    }
  };
}
function isElement(value2) {
  return isObject(value2) && typeof value2.focus === "function" && typeof value2.getBoundingClientRect === "function";
}
function isRect(value2) {
  return isObject(value2) && ["x", "y", "width", "height"].every((key2) => typeof value2[key2] === "number");
}
function targetGiven(given) {
  if (isObject(given) && Object.keys(given).length === 1) {
    if (given.board === true) return Object.freeze({ board: true });
    const { selection } = given;
    if (Array.isArray(selection) && selection.every(isBoardObject)) {
      return Object.freeze({ selection: Object.freeze(selection.map((object2) => Object.freeze({ ...object2 }))) });
    }
    if (isBoardObject(given)) return Object.freeze({ ...given });
  }
  throw new TypeError(
    `A board action acts on the board, as { board: true }, on a board object, as { block: "a" }, or on a selection of them, as { selection: [{ block: "a" }, { link: "ab" }] }, not ${JSON.stringify(given)}.`
  );
}
function kindOf(target2) {
  if ("board" in target2) return "board";
  if ("selection" in target2) return "selection";
  if ("block" in target2) return "block";
  if ("link" in target2) return "link";
  if ("stack" in target2) return "stack";
  if ("option" in target2) return "option";
  return "plugin";
}

// srcts/actions/menu.tsx
import { useEffect, useRef, useSyncExternalStore } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Autocomplete, Input, Separator, TextField as AriaTextField } from "./vendor-858b8c85.js";
import { TextField } from "@blockr/core/react";
import { Menu, MenuItem, MenuSection, Popover } from "@blockr/core/react";
import { Fragment, jsx, jsxs } from "react/jsx-runtime";
var showMenu = (menu, at) => {
  const doc = at.el.ownerDocument;
  const host = doc.createElement("div");
  doc.body.append(host);
  const root = createRoot(host);
  const insideRef = { current: null };
  const tab = (event) => {
    if (event.key !== "Tab" || event.isComposing || !(event.target instanceof Node) || insideRef.current?.contains(event.target) !== true) return;
    event.preventDefault();
    event.stopPropagation();
    void menu.close().catch(() => void 0);
  };
  doc.defaultView?.addEventListener("keydown", tab, true);
  flushSync(() => {
    root.render(/* @__PURE__ */ jsx(CoreMenu, { menu, at, insideRef }));
  });
  return () => {
    doc.defaultView?.removeEventListener("keydown", tab, true);
    const active = doc.activeElement;
    const lost = active === null || active === doc.body || (insideRef.current?.contains(active) ?? false);
    root.unmount();
    host.remove();
    if (lost && at.el.isConnected) at.el.focus({ preventScroll: true });
  };
};
function CoreMenu({ menu, at, insideRef }) {
  const level = useSyncExternalStore(menu.subscribe, menu.get);
  const anchor = useRef(null);
  const { x, y, width, height } = at.anchor;
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx("div", { ref: anchor, "aria-hidden": "true", style: { position: "fixed", left: x, top: y, width, height, pointerEvents: "none" } }),
    /* @__PURE__ */ jsx(
      Popover,
      {
        triggerRef: anchor,
        isOpen: true,
        placement: "bottom start",
        isKeyboardDismissDisabled: true,
        onOpenChange: (open) => {
          if (!open) void menu.close().catch(() => void 0);
        },
        className: "bc:max-w-80 bc:min-w-45",
        children: /* @__PURE__ */ jsx(
          "div",
          {
            ref: (el) => {
              insideRef.current = el;
            },
            role: "dialog",
            "aria-label": level.title ?? level.label,
            tabIndex: -1,
            className: "bc:grid bc:gap-1 bc:outline-none",
            onKeyDownCapture: (event) => {
              if (event.nativeEvent.isComposing) return;
              if (event.key === "Escape") {
                event.preventDefault();
                event.stopPropagation();
                menu.escape();
              } else if (event.key === "Enter" && isFilter(event.target) && !event.target.hasAttribute("aria-activedescendant")) {
                event.preventDefault();
                event.stopPropagation();
                void menu.pick().catch(() => void 0);
              }
            },
            children: /* @__PURE__ */ jsx(Level, { menu, level, insideRef }, level.id)
          }
        )
      }
    )
  ] });
}
function Level({ menu, level, insideRef }) {
  const texts = level.rows.flatMap((row, i) => row.kind === "text" ? [{ row, i }] : []);
  const listed = level.rows.flatMap((row, i) => row.kind === "text" ? [] : [{ row, i }]);
  const rows = level.rows.length > 0;
  useEffect(() => {
    let frames = 0;
    let frame;
    const place = () => {
      const el = insideRef.current;
      if (el === null) return;
      const field = el.querySelector("input:not([data-filter])") ?? el.querySelector("input[data-filter]");
      const first = field ?? el.querySelector("[role=menuitem][data-key], [role=menuitemcheckbox][data-key]");
      if (first === null && rows && frames++ < 10) {
        frame = requestAnimationFrame(place);
        return;
      }
      (first ?? el).focus({ preventScroll: true });
      if (field !== null && !field.hasAttribute("data-filter")) field.select();
    };
    place();
    return () => {
      if (frame !== void 0) cancelAnimationFrame(frame);
    };
  }, [insideRef, rows]);
  const pick2 = (i) => {
    void menu.pick(i).catch(() => void 0);
  };
  const grouped = groups(listed);
  const list = /* @__PURE__ */ jsx(Menu, { "aria-label": level.label, renderEmptyState: () => /* @__PURE__ */ jsx("p", { className: NOTE, children: "No matches" }), children: grouped.map((group, g) => {
    const divider = g > 0 && group.category !== grouped[g - 1]?.category;
    const section = group.kind === "multi" ? /* @__PURE__ */ jsx(
      MenuSection,
      {
        id: `multi-${String(group.i)}`,
        title: group.row.label,
        selectionMode: "multiple",
        selectedKeys: group.row.choices.filter((choice) => choice.picked).map(({ id: id2 }) => choiceKey(group.i, id2)),
        onSelectionChange: (keys) => {
          for (const choice of group.row.choices) {
            const ticked = keys === "all" || keys.has(choiceKey(group.i, choice.id));
            if (ticked !== choice.picked) menu.tick(group.i, choice.id);
          }
        },
        children: group.row.choices.map((choice) => /* @__PURE__ */ jsx(MenuItem, { id: choiceKey(group.i, choice.id), textValue: choice.label, children: choice.label }, choice.id))
      },
      `multi-${String(group.i)}`
    ) : /* @__PURE__ */ jsx(MenuSection, { id: `items-${String(group.items[0]?.i ?? g)}`, title: group.heading, children: group.items.map(({ row, i }) => /* @__PURE__ */ jsx(
      Item,
      {
        row,
        onPick: () => {
          pick2(i);
        }
      },
      i
    )) }, `items-${String(group.items[0]?.i ?? g)}`);
    return divider ? [/* @__PURE__ */ jsx(Separator, { className: DIVIDER }, `divider-${String(g)}`), section] : [section];
  }) });
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    level.title === void 0 ? null : /* @__PURE__ */ jsx("p", { className: "bc:m-0 bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted", children: level.title }),
    level.loading ? /* @__PURE__ */ jsx("p", { role: "status", className: `${NOTE} bc:animate-[bc-appear_300ms_ease-out_300ms_both]`, children: "Loading\u2026" }) : level.error === void 0 ? /* @__PURE__ */ jsxs(Fragment, { children: [
      texts.map(({ row, i }) => /* @__PURE__ */ jsx(
        TextRow,
        {
          row,
          onType: (value2) => {
            menu.type(value2, i);
          },
          onSubmit: () => {
            pick2(i);
          }
        },
        i
      )),
      level.filter === void 0 ? listed.length === 0 ? null : list : /* @__PURE__ */ jsxs(
        Autocomplete,
        {
          inputValue: level.filter,
          onInputChange: (value2) => {
            menu.type(value2);
          },
          children: [
            /* @__PURE__ */ jsx(AriaTextField, { "aria-label": "Filter", className: "bc:px-1 bc:pt-1", children: /* @__PURE__ */ jsx(Input, { "data-filter": "", placeholder: "Filter", spellCheck: "false", className: FILTER }) }),
            list
          ]
        }
      )
    ] }) : /* @__PURE__ */ jsx("p", { role: "alert", className: "bc:m-0 bc:px-2 bc:py-1.5 bc:text-sm bc:break-words bc:text-text-danger", children: level.error })
  ] });
}
function groups(listed) {
  const all = [];
  for (const { row, i } of listed) {
    if (row.kind === "text") continue;
    if (row.kind === "multi") {
      all.push({ kind: "multi", row, i });
      continue;
    }
    const last = all.at(-1);
    if (last?.kind === "items" && last.heading === row.heading && last.category === row.category) {
      last.items.push({ row, i });
    } else {
      all.push({
        kind: "items",
        ...row.heading === void 0 ? {} : { heading: row.heading },
        ...row.category === void 0 ? {} : { category: row.category },
        items: [{ row, i }]
      });
    }
  }
  return all;
}
function Item({ row, onPick }) {
  const { label, icon, detail } = row;
  const removes = row.category === "remove";
  return /* @__PURE__ */ jsxs(
    MenuItem,
    {
      textValue: label,
      ...detail === void 0 ? {} : { "aria-label": `${label}, ${detail}` },
      onAction: onPick,
      className: removes ? REMOVES : void 0,
      children: [
        /* @__PURE__ */ jsxs(
          "span",
          {
            className: `bc:flex bc:min-w-0 bc:items-center bc:gap-2 ${removes ? "bc:group-data-focused:text-text-danger bc:group-data-hovered:text-text-danger" : ""}`,
            children: [
              icon === void 0 ? null : /* @__PURE__ */ jsx(
                "span",
                {
                  "aria-hidden": "true",
                  className: "bc:flex bc:size-4 bc:shrink-0 bc:[&>svg]:size-full",
                  dangerouslySetInnerHTML: { __html: icon }
                }
              ),
              /* @__PURE__ */ jsx("span", { className: "bc:min-w-0 bc:truncate", children: label })
            ]
          }
        ),
        detail === void 0 ? null : /* @__PURE__ */ jsx("span", { className: "bc:text-xs bc:text-text-muted", children: detail })
      ]
    }
  );
}
function TextRow({
  row,
  onType,
  onSubmit
}) {
  return /* @__PURE__ */ jsx(
    TextField,
    {
      label: row.label,
      value: row.value,
      spellCheck: "false",
      isInvalid: row.refusal !== void 0,
      ...row.refusal === void 0 ? {} : { errorMessage: row.refusal },
      className: "bc:px-1 bc:pt-1",
      onChange: onType,
      onKeyDown: (event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) {
          event.preventDefault();
          onSubmit();
        } else event.continuePropagation();
      }
    }
  );
}
function choiceKey(row, choice) {
  return `${String(row)}:${choice}`;
}
function isFilter(target2) {
  return target2 instanceof HTMLInputElement && target2.hasAttribute("data-filter");
}
var NOTE = "bc:m-0 bc:px-2 bc:py-1.5 bc:text-center bc:text-sm bc:text-text-muted bc:italic";
var FILTER = "bc:box-border bc:w-full bc:rounded bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-xs bc:text-text-default bc:outline-none";
var DIVIDER = "bc:mx-0 bc:my-1 bc:h-px bc:border-0 bc:bg-border-default";
var REMOVES = "bc:group bc:data-hovered:bg-bg-danger! bc:data-focused:bg-bg-danger!";

// srcts/core/plugins.ts
var NONE = Object.freeze({});
var NOTHING_FAILED = Promise.resolve([]);
function runPlugins(deps) {
  const { store, keepOptions } = deps;
  const replaced2 = deps.signal ?? new AbortController().signal;
  const registeredClass = (id2) => own(store.plugins.registered(), id2);
  const added = /* @__PURE__ */ new Map();
  const stops = [];
  function keep(ids2, by) {
    if (ids2.length === 0) return NOTHING_FAILED;
    const kept = keepOptions(ids2, by);
    stops.push(kept.stop);
    return kept.added;
  }
  const optionsOf = (id2) => own(deps.plugins, id2)?.options ?? [];
  function opened() {
    for (const id2 of Object.keys(appPlugins())) {
      added.set(id2, keep(optionsOf(id2), `the plugin class "${id2}"`));
    }
  }
  const offStore = store.subscribe((change) => {
    if ("opened" in change) opened();
  });
  onAbort(replaced2, () => {
    offStore();
    for (const stop of stops.splice(0)) stop();
  });
  let memo;
  function appPlugins() {
    if (!store.hasBoard()) return NONE;
    const classes = store.plugins.registered();
    if (memo?.classes !== classes) {
      const registered = Object.entries(deps.plugins).flatMap(
        ([id2, { name: name3 }]) => Object.hasOwn(classes, id2) ? [[id2, Object.freeze({ name: name3 })]] : []
      );
      memo = { classes, plugins: Object.freeze(Object.fromEntries(registered)) };
    }
    return memo.plugins;
  }
  function pluginClass(plugin) {
    const def = Object.hasOwn(appPlugins(), plugin) ? registeredClass(plugin) : void 0;
    if (def === void 0) throw new Error(`The app runs no plugin "${plugin}" on the open board.`);
    return def;
  }
  function storage() {
    const [id2] = inRole(appPlugins(), registeredClass, "storage");
    return id2 === void 0 ? void 0 : pluginClass(id2).storage;
  }
  function frontend() {
    const [id2] = inRole(appPlugins(), registeredClass, "frontend");
    const drawn2 = id2 === void 0 ? void 0 : pluginClass(id2).frontend;
    if (id2 === void 0 || drawn2 === void 0) return void 0;
    return { frontend: drawn2, plugin: id2, added: added.get(id2) ?? NOTHING_FAILED };
  }
  let started = false;
  async function start2() {
    if (started) return;
    started = true;
    const starting = Object.keys(appPlugins()).flatMap((id2) => {
      const def = registeredClass(id2);
      return def?.start === void 0 ? [] : [{ id: id2, def }];
    });
    const failures = await Promise.all(starting.map(({ id: id2 }) => added.get(id2) ?? NOTHING_FAILED));
    starting.forEach(({ id: id2, def }, i) => {
      if (replaced2.aborted) return;
      if ((failures[i] ?? []).length > 0) {
        log.error(`The plugin "${id2}" does not start, since board options that its class requires did not go onto the board.`);
        return;
      }
      launch(id2, def);
    });
  }
  function launch(id2, def) {
    const controller = new AbortController();
    let cleanup;
    onAbort(replaced2, () => {
      controller.abort();
      if (cleanup !== void 0) attempt(cleanup);
    });
    let returned;
    try {
      returned = def.start?.({ core: deps.core(), plugin: id2, signal: controller.signal });
    } catch (error) {
      const why = new Error(`The start of the plugin class "${id2}" failed: ${errorMessage(error)}`, { cause: error });
      log.error(why);
      const module = sourceModule({ plugin: id2 }, packageFromId(id2));
      deps.raise(coreNotification("plugin_start_failed", "error", why.message, { module }));
      return;
    }
    if (typeof returned !== "function") return;
    const given = returned;
    if (controller.signal.aborted) attempt(given);
    else cleanup = given;
  }
  function mountPlugin(el, plugin) {
    if (replaced2.aborted) throw new Error(REPLACED);
    const def = pluginClass(plugin);
    const { ui } = def;
    if (ui === void 0) {
      throw new Error(`The class of the plugin "${plugin}" has no ui to mount.`);
    }
    const controller = new AbortController();
    const { signal: signal5 } = controller;
    const ended = () => signal5.aborted;
    let cleanup;
    let stopKeeping;
    const end = () => {
      if (ended()) return;
      offEnd();
      stopKeeping?.();
      controller.abort();
      if (cleanup !== void 0) attempt(cleanup);
      el.replaceChildren();
    };
    const offEnd = onAbort(replaced2, end);
    function fail(error) {
      if (ended()) return;
      log.error(asError(error));
      cannotDraw(errorMessage(error));
    }
    function cannotDraw(message) {
      deps.raise(coreNotification("module_failed", "error", message, { module: sourceModule({ plugin }, packageFromId(plugin)) }));
    }
    function lacking(ids2) {
      const missing = ids2.filter((id2) => !Object.hasOwn(store.board().options, id2));
      cannotDraw(withoutOptions(`the ui of the plugin class "${plugin}"`, missing));
    }
    void (async () => {
      const named4 = optionsOf(plugin);
      const kept = keepOptions(named4, `the plugin class "${plugin}"`, () => {
        if (ended()) return;
        end();
        lacking(named4);
      });
      stopKeeping = kept.stop;
      const failures = await kept.added;
      if (ended()) return;
      if (failures.length > 0) {
        kept.stop();
        lacking(named4);
        return;
      }
      try {
        const rendered = await ui.render({ el, core: deps.core(), plugin, signal: signal5 });
        if (typeof rendered !== "function") return;
        if (ended()) attempt(rendered);
        else cleanup = rendered;
      } catch (error) {
        fail(new Error(`The ui of the plugin class "${plugin}" failed in render(): ${errorMessage(error)}`, { cause: error }));
      }
    })();
    return end;
  }
  return { appPlugins, pluginClass, storage, mountPlugin, frontend, start: start2 };
}

// srcts/core/requests.ts
var ATTEMPTS3 = 3;
async function pluginRequest(deps, plugin, name3, args, opts) {
  if (typeof plugin !== "string" || plugin === "") {
    throw new TypeError("A plugin's request names the plugin by the id of its class, a non-empty string.");
  }
  if (!Object.hasOwn(deps.plugins.appPlugins(), plugin)) {
    throw new Error(`The app runs no plugin "${plugin}" on the open board, so its request did not go to R.`);
  }
  if (typeof name3 !== "string" || name3 === "") {
    throw new TypeError(`A request of the plugin "${plugin}" is named by a non-empty string.`);
  }
  if (!isObject(args)) {
    throw new TypeError(`The args of the request "${name3}" of the plugin "${plugin}" are an object.`);
  }
  const reads = opts.reads ?? [];
  if (!Array.isArray(reads) || !reads.every((block) => typeof block === "string")) {
    throw new TypeError(`The blocks that the request "${name3}" of the plugin "${plugin}" reads are an array of their ids.`);
  }
  const notifier = givenNotifier(opts, `the request "${name3}" of the plugin "${plugin}"`);
  const what = `The request "${name3}" of the plugin "${plugin}"`;
  const module = sourceModule({ plugin }, packageFromId(plugin));
  const inCaller = keepContext();
  const handOn = (conditions) => {
    deps.replied(module, { type: "plugin", name: name3 }, conditions, notifier);
  };
  for (let attempt3 = 1; ; attempt3++) {
    const revisions = await evaluateReads2(deps, what, reads, opts.signal);
    const { reply, body } = await inCaller(
      () => deps.scheduler.request(
        { kind: "request", type: "plugin", plugin, name: name3, args: { ...args }, reads: revisions },
        {
          ...opts.file === void 0 ? {} : { body: opts.file },
          ...opts.signal === void 0 ? {} : { signal: opts.signal },
          ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
        }
      )
    );
    const conditions = "conditions" in reply ? reply.conditions : [];
    switch (reply.status) {
      case "ok":
        handOn(conditions);
        if (body !== null) return body;
        return "value" in reply && reply.value !== void 0 ? reply.value : null;
      case "missing":
        if (attempt3 < ATTEMPTS3) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, { status: "missing" });
      case "cancelled":
        throw new RequestError(`R cancelled the request "${name3}" of the plugin "${plugin}".`, { status: "cancelled" });
      default:
        handOn(conditions);
        throw new RequestError(`R replied ${reply.status} to the request "${name3}" of the plugin "${plugin}".`, {
          status: reply.status,
          conditions
        });
    }
  }
}
async function evaluateReads2(deps, what, blocks, signal5) {
  if (blocks.length === 0) return [];
  const absent = blocks.filter((block) => deps.scheduler.status(block) === void 0);
  if (absent.length > 0) {
    throw new Error(`${what} reads ${absent.length === 1 ? "the block" : "the blocks"} ${quoted(absent)}, which the board lacks.`);
  }
  const unique = [...new Set(blocks)];
  const statuses = await deps.scheduler.evaluate(unique, void 0, signal5 === void 0 ? {} : { signal: signal5 });
  const unready = unique.flatMap((block) => {
    const status = own(statuses, block);
    return status === void 0 || status.status === "ready" ? [] : [{ block, status }];
  });
  if (unready.length > 0) {
    const listed = unready.map(({ block, status }) => `"${block}" reads ${status.status}`);
    throw new RequestError(`${what} did not go to R, since a block it reads is not ready: ${listed.join(", ")}.`, {
      blocks: unready
    });
  }
  const left2 = unique.filter((block) => own(statuses, block) === void 0);
  if (left2.length > 0) {
    throw new Error(`${what} did not go to R, since ${left2.length === 1 ? "the block" : "the blocks"} ${quoted(left2)} left the board.`);
  }
  return unique.map((block) => ({ block, revision: own(statuses, block)?.revision ?? 0 }));
}

// srcts/core/core.ts
function createCore(parts) {
  const { store, commands, scheduler, loader, blockrOptions, notifications } = parts;
  const randId = asRandId(parts.randId);
  const { raise } = notifications;
  notifications.follow(scheduler);
  const optionPackage = (id2) => own(parts.options, id2)?.package;
  const listeners = /* @__PURE__ */ new Set();
  parts.signal?.addEventListener(
    "abort",
    () => {
      listeners.clear();
    },
    { once: true }
  );
  const outputEvents = {
    subscribe(fn) {
      const wrapped = (event) => {
        fn(event);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    }
  };
  const refused2 = (error, about, notifier) => {
    const why = errorMessage(error);
    if (!isLogged(error)) log.debug(why);
    notifications.raiseAgain(coreNotification("board_edit_failed", "error", why, about === void 0 ? {} : { about }), notifier);
  };
  const mounts = createMounts({
    store,
    commands,
    scheduler,
    loader,
    core: () => api,
    renderers: parts.renderers,
    raise,
    replied: notifications.replied,
    notify: notifications.notify,
    refused: refused2,
    optionPackage,
    ...parts.measure === void 0 ? {} : { measure: parts.measure },
    ...parts.signal === void 0 ? {} : { signal: parts.signal },
    emit(event) {
      for (const fn of [...listeners]) {
        try {
          fn(event);
        } catch (error) {
          log.error(asError(error));
        }
      }
    }
  });
  const plugins = runPlugins({
    store,
    keepOptions: (ids2, by, failed2) => mounts.keepOptions(ids2, by, failed2),
    core: () => api,
    plugins: parts.plugins,
    raise,
    ...parts.signal === void 0 ? {} : { signal: parts.signal }
  });
  const focus = createFocus(store, parts.signal);
  const selection = createSelection(store, plugins, parts.signal);
  const edits = refusing(store, commands, refused2);
  const api = {
    store: edits.store,
    commands: edits.commands,
    boardActions: boardActionsApi(parts.boardActions, {
      store,
      lock: parts.lock,
      core: () => api,
      refused: refused2,
      raiseAgain: notifications.raiseAgain,
      show: parts.showMenu,
      ...parts.signal === void 0 ? {} : { signal: parts.signal }
    }),
    scheduler: {
      demand: (owner, blocks, options) => scheduler.demand(owner, blocks, options),
      evaluate: (blocks, params, options) => scheduler.evaluate(blocks, params, options),
      invalidate: (id2) => {
        scheduler.invalidate(id2);
      },
      status: (id2) => scheduler.status(id2),
      subscribe: (fn) => scheduler.subscribe(fn)
    },
    blockRegistry: () => store.blockRegistry(),
    options: {
      register: (def) => store.options.register(def),
      registered: () => store.options.registered(),
      value: (id2) => store.hasBoard() ? own(store.board().options, id2) : void 0
    },
    plugins: {
      appPlugins: () => plugins.appPlugins(),
      pluginClass: (plugin) => plugins.pluginClass(plugin),
      storage: () => plugins.storage()
    },
    lock: parts.lock,
    outputEvents,
    notifications: notifications.api,
    log(pkg) {
      const given = pkg;
      if (!isPackageName(given)) {
        throw new TypeError(`A log names the package whose lines it writes, as core.log("blockr.dock"), not ${JSON.stringify(given)}.`);
      }
      return createLog(given);
    },
    focus,
    selection,
    onScreen: createOnScreen(parts.signal),
    show(object2) {
      if (parts.signal?.aborted === true) return Promise.resolve();
      const shown2 = shownObject(object2);
      if ("block" in shown2) {
        if (!store.hasBoard() || !Object.hasOwn(store.board().blocks, shown2.block)) {
          throw new Error(`The board holds no block ${JSON.stringify(shown2.block)} to show.`);
        }
        focus.set(shown2.block);
      } else if (plugins.pluginClass(shown2.plugin).ui === void 0) {
        throw new Error(`The class of the plugin "${shown2.plugin}" has no ui to show.`);
      }
      return parts.show(shown2);
    },
    request: (plugin, name3, args = {}, opts = {}) => pluginRequest({ plugins, scheduler, replied: notifications.replied }, plugin, name3, args, opts),
    module: (id2) => loader.module(id2),
    blockrOption: (name3) => blockrOptions.read(name3),
    mintId: (taken) => mintId(taken, randId),
    mountBlock: (el, block) => mounts.mountBlock(el, block),
    mountOutput: (el, block) => mounts.mountOutput(el, block),
    mountOption: (el, option) => mounts.mountOption(el, option),
    mountPlugin: (el, plugin) => plugins.mountPlugin(el, plugin)
  };
  let heard = {};
  store.subscribe((change, board) => {
    const opened = "opened" in change;
    if (board.options !== heard || opened) {
      const before = opened ? {} : heard;
      heard = board.options;
      for (const [id2, value2] of Object.entries(board.options)) {
        if (!jsonEqual(own(before, id2), value2)) changed(id2, value2);
      }
    }
    if (opened || (change.delta.options?.unset?.length ?? 0) > 0) {
      const lacking = Object.values(store.options.registered()).filter((def) => def.onChange !== void 0 && !Object.hasOwn(board.options, def.id)).map((def) => def.id);
      if (lacking.length > 0) void mounts.addOptions(lacking, "its onChange");
    }
  });
  const failing = /* @__PURE__ */ new Map();
  const raisedBy = /* @__PURE__ */ new Map();
  function changed(id2, value2) {
    const def = own(store.options.registered(), id2);
    if (def?.onChange === void 0) return;
    const raisedNow = [];
    const module = sourceModule({ option: id2 }, optionPackage(id2));
    const notify = (notification) => {
      const withdraw = notifications.notify(module, notification);
      raisedNow.push(withdraw);
      return withdraw;
    };
    let failure;
    try {
      def.onChange(value2, api, notify);
    } catch (error) {
      log.error(asError(error));
      failure = raise(
        coreNotification("on_change_failed", "error", `The onChange of the board option "${id2}" failed: ${errorMessage(error)}`, {
          module
        })
      );
    }
    failing.get(id2)?.();
    if (failure === void 0) failing.delete(id2);
    else failing.set(id2, failure);
    for (const withdraw of raisedBy.get(id2) ?? []) withdraw();
    raisedBy.set(id2, raisedNow);
  }
  return { api, plugins };
}
function refusing(store, commands, refused2) {
  function edit(opts, call, fn) {
    const notifier = givenNotifier(opts, call);
    try {
      return fn();
    } catch (error) {
      refused2(error, void 0, notifier);
      throw error;
    }
  }
  function ran(opts, run) {
    let notifier;
    try {
      notifier = givenNotifier(opts, "run()");
    } catch (error) {
      return Promise.reject(asError(error));
    }
    return run().catch((error) => {
      refused2(error, void 0, notifier);
      throw error;
    });
  }
  return {
    store: {
      ...store,
      apply(delta, origin, opts) {
        return edit(opts, "apply()", () => store.apply(delta, origin, opts?.undo === false ? { undo: false } : void 0));
      },
      undo(origin, opts) {
        edit(opts, "undo()", () => {
          store.undo(origin);
        });
      },
      redo(opts) {
        edit(opts, "redo()", () => {
          store.redo();
        });
      }
    },
    commands: {
      ...commands,
      // A list comes with its origin second, in place of the args, and its
      // options third, as the command registry takes it.
      run(target2, given, third, fourth) {
        if (typeof target2 === "string") {
          const origin = typeof third === "string" ? third : void 0;
          return ran(fourth, () => commands.run(target2, given, origin));
        }
        const listed = typeof given === "string" ? given : void 0;
        return ran(typeof third === "string" ? void 0 : third, () => commands.run(target2, listed));
      }
    }
  };
}
function shownObject(given) {
  if (typeof given === "object" && given !== null && !Array.isArray(given)) {
    const entries = Object.entries(given);
    const [entry] = entries;
    if (entries.length === 1 && entry !== void 0 && typeof entry[1] === "string") {
      if (entry[0] === "block") return Object.freeze({ block: entry[1] });
      if (entry[0] === "plugin") return Object.freeze({ plugin: entry[1] });
    }
  }
  throw new TypeError(`Core shows a block or a plugin, as { block: "a" } or { plugin: "blockr.md-md_plugin" }, not ${JSON.stringify(given)}.`);
}
function createOnScreen(signal5) {
  let seen = null;
  const listeners = /* @__PURE__ */ new Set();
  signal5?.addEventListener(
    "abort",
    () => {
      listeners.clear();
    },
    { once: true }
  );
  return {
    get: () => seen,
    set(shown2) {
      const next = seenGiven(shown2);
      if (seen !== null && sameIds(seen.blocks, next.blocks) && sameIds(seen.plugins, next.plugins)) return;
      seen = next;
      announceTo(listeners, seen);
    },
    subscribe(fn) {
      const wrapped = (shown2) => {
        fn(shown2);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    }
  };
}
function seenGiven(given) {
  if (typeof given === "object" && given !== null) {
    const { blocks, plugins } = given;
    if (isIds(blocks) && isIds(plugins)) {
      return Object.freeze({ blocks: Object.freeze([...blocks]), plugins: Object.freeze([...plugins]) });
    }
  }
  throw new TypeError(
    `A frontend says what is on screen as { blocks: ["a"], plugins: ["blockr.md-md_plugin"] }, not ${JSON.stringify(given)}.`
  );
}
function isIds(given) {
  return Array.isArray(given) && given.every((id2) => typeof id2 === "string");
}
function sameIds(a, b) {
  const inB = new Set(b);
  return new Set(a).size === inB.size && a.every((id2) => inB.has(id2));
}
function createFocus(store, signal5) {
  let focused = null;
  const listeners = /* @__PURE__ */ new Set();
  const offStore = store.subscribe((_change, board) => {
    if (focused === null || Object.hasOwn(board.blocks, focused)) return;
    focused = null;
    announceTo(listeners, focused);
  });
  signal5?.addEventListener(
    "abort",
    () => {
      offStore();
      listeners.clear();
    },
    { once: true }
  );
  return {
    get: () => focused,
    set(block) {
      const given = block;
      if (given !== null && (typeof given !== "string" || !store.hasBoard() || !Object.hasOwn(store.board().blocks, given))) {
        throw new Error(`The board holds no block ${JSON.stringify(given)} to give focus to.`);
      }
      if (given === focused) return;
      focused = given;
      announceTo(listeners, focused);
    },
    subscribe(fn) {
      const wrapped = (block) => {
        fn(block);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    }
  };
}
var NOTHING_SELECTED = Object.freeze([]);
function createSelection(store, plugins, signal5) {
  let selected = NOTHING_SELECTED;
  const listeners = /* @__PURE__ */ new Set();
  function holds(object2, board) {
    if ("block" in object2) return Object.hasOwn(board.blocks, object2.block);
    if ("link" in object2) return Object.hasOwn(board.links, object2.link);
    if ("stack" in object2) return Object.hasOwn(board.stacks, object2.stack);
    if ("option" in object2) return Object.hasOwn(board.options, object2.option);
    return Object.hasOwn(plugins.appPlugins(), object2.plugin);
  }
  const offStore = store.subscribe((_change, board) => {
    const kept = selected.filter((object2) => holds(object2, board));
    if (kept.length === selected.length) return;
    selected = Object.freeze(kept);
    announceTo(listeners, selected);
  });
  signal5?.addEventListener(
    "abort",
    () => {
      offStore();
      listeners.clear();
    },
    { once: true }
  );
  return {
    get: () => selected,
    set(objects) {
      const given = selectionGiven(objects);
      const lacking = given.find((object2) => !store.hasBoard() || !holds(object2, store.board()));
      if (lacking !== void 0) throw new Error(unheld(lacking));
      if (sameIds(selected.map(objectKey), given.map(objectKey))) return;
      selected = given;
      announceTo(listeners, selected);
    },
    subscribe(fn) {
      const wrapped = (objects) => {
        fn(objects);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    }
  };
}
function selectionGiven(given) {
  if (!Array.isArray(given) || !given.every(isBoardObject)) {
    throw new TypeError(`A selection is an array of board objects, as [{ block: "a" }, { link: "ab" }], not ${JSON.stringify(given)}.`);
  }
  const copies = /* @__PURE__ */ new Map();
  for (const object2 of given) {
    const copy = Object.freeze({ ...object2 });
    const key2 = objectKey(copy);
    if (!copies.has(key2)) copies.set(key2, copy);
  }
  return Object.freeze([...copies.values()]);
}
function objectKey(object2) {
  return JSON.stringify(object2);
}
function unheld(object2) {
  if ("block" in object2) return `The board holds no block ${JSON.stringify(object2.block)} to select.`;
  if ("link" in object2) return `The board holds no link ${JSON.stringify(object2.link)} to select.`;
  if ("stack" in object2) return `The board holds no stack ${JSON.stringify(object2.stack)} to select.`;
  if ("option" in object2) return `The board holds no board option ${JSON.stringify(object2.option)} to select.`;
  return `The app runs no plugin ${JSON.stringify(object2.plugin)} to select.`;
}

// srcts/core/frontends.ts
function loadingNote() {
  const box = document.createElement("div");
  box.setAttribute("role", "status");
  box.className = "bc:loading-note";
  const spinner = document.createElement("progress");
  spinner.className = "bc:spinner";
  spinner.setAttribute("aria-label", "Loading the board");
  const line = document.createElement("span");
  line.setAttribute("aria-hidden", "true");
  line.textContent = "Loading the board\u2026";
  box.append(spinner, line);
  return box;
}
function pageFrontEnds(el) {
  let drawn2;
  let next;
  function end() {
    const current2 = drawn2;
    if (current2 === void 0) return;
    drawn2 = void 0;
    current2.off();
    current2.controller.abort();
    if (current2.cleanup !== void 0) attempt(current2.cleanup);
    el.replaceChildren(loadingNote());
  }
  return {
    prepare(front, signal5) {
      if (signal5.aborted) return;
      end();
      next = front;
    },
    async render(core, signal5, failures) {
      if (signal5.aborted) return;
      const prepared = next;
      next = void 0;
      if (prepared === void 0) return;
      if ("none" in prepared) {
        showError(el, prepared.none);
        log.error(prepared.none);
        return;
      }
      if (failures.length > 0) {
        showError(el, failures.join("\n"));
        return;
      }
      const controller = new AbortController();
      const ending = () => {
        if (drawn2 === current2) end();
      };
      signal5.addEventListener("abort", ending, { once: true });
      const current2 = {
        frontend: prepared.frontend,
        plugin: prepared.plugin,
        core,
        board: signal5,
        controller,
        off: () => {
          signal5.removeEventListener("abort", ending);
        },
        cleanup: void 0
      };
      drawn2 = current2;
      const note = [...el.childNodes];
      try {
        const rendered = await prepared.frontend.render({
          el,
          core,
          plugin: prepared.plugin,
          signal: controller.signal,
          args: prepared.args
        });
        for (const node of note) if (node.parentNode === el) node.remove();
        if (typeof rendered !== "function") return;
        if (controller.signal.aborted) attempt(rendered);
        else current2.cleanup = rendered;
      } catch (error) {
        if (controller.signal.aborted) return;
        const why = new Error(`The frontend of the plugin class "${prepared.plugin}" failed in render(): ${errorMessage(error)}`, {
          cause: error
        });
        showError(el, why.message);
        log.error(why);
      }
    },
    async show(object2, signal5) {
      const current2 = drawn2;
      if (current2?.board !== signal5 || current2.frontend.show === void 0) return;
      try {
        await current2.frontend.show({ object: object2, el, core: current2.core });
      } catch (error) {
        if (current2.controller.signal.aborted) return;
        throw new Error(`The frontend of the plugin class "${current2.plugin}" failed in show(): ${errorMessage(error)}`, { cause: error });
      }
    }
  };
}

// srcts/core/migrations.ts
function createMigrations(loader) {
  const known = /* @__PURE__ */ new Map();
  async function load(registryId2, module) {
    const pkg = packageFromId(registryId2);
    let migration;
    try {
      migration = migrationFromExports(await loader.load({ module }, pkg), module, pkg);
    } catch (error) {
      log.error(asError(error));
    }
    known.set(registryId2, { module, migration });
  }
  return {
    get: (registryId2) => known.get(registryId2)?.migration,
    async prepare(board, registry) {
      const needed = /* @__PURE__ */ new Map();
      for (const block of olderBlocks(board, registry)) {
        const entry = own(registry, block);
        if (entry === void 0 || known.get(block)?.module === entry.module) continue;
        needed.set(block, entry.module);
      }
      await Promise.all([...needed].map(([registryId2, module]) => load(registryId2, module)));
    }
  };
}
function olderBlocks(given, registry) {
  let json = given;
  if (typeof given === "string") {
    try {
      json = JSON.parse(given);
    } catch {
      return /* @__PURE__ */ new Set();
    }
  }
  const blocks = isObject(json) && isObject(json.board) ? json.board.blocks : void 0;
  const older = /* @__PURE__ */ new Set();
  if (!isObject(blocks)) return older;
  for (const block of Object.values(blocks)) {
    if (!isObject(block) || typeof block.registry_id !== "string" || typeof block.version !== "number") continue;
    const version = own(registry, block.registry_id)?.version;
    if (version !== void 0 && block.version < version) older.add(block.registry_id);
  }
  return older;
}
function migrationFromExports(exports, module, pkg) {
  const { migrate: migrate2 } = moduleExports(exports, module, pkg);
  if (migrate2 === void 0) return void 0;
  if (typeof migrate2 !== "function") {
    throw new ModuleError(module, pkg, "exports a migrate that is no function.");
  }
  return migrate2;
}

// srcts/core/notifications.ts
var EVALUATE_PHASES = {
  expr: true,
  inputs: true,
  block_eval: true,
  result_summary: true
};
var NONE2 = Object.freeze([]);
var nothing = () => void 0;
function createNotifications(store, options = {}) {
  const { signal: signal5 } = options;
  const plugins = options.plugins ?? {};
  const held2 = /* @__PURE__ */ new Map();
  const raised = /* @__PURE__ */ new Map();
  const again = /* @__PURE__ */ new Map();
  const replacing = /* @__PURE__ */ new Map();
  const left2 = /* @__PURE__ */ new Set();
  const listeners = /* @__PURE__ */ new Set();
  let all = NONE2;
  let count = 0;
  let ended = false;
  const stops = [];
  function publish() {
    all = Object.freeze([...held2.values()]);
    announceTo(listeners, all);
  }
  function present(object2) {
    if (!("block" in object2 || "link" in object2 || "stack" in object2)) return true;
    if (!store.hasBoard()) return false;
    const board = store.board();
    if ("block" in object2) return Object.hasOwn(board.blocks, object2.block);
    if ("link" in object2) return Object.hasOwn(board.links, object2.link);
    return Object.hasOwn(board.stacks, object2.stack);
  }
  function known(object2) {
    if ("option" in object2) {
      return Object.hasOwn(store.options.registered(), object2.option) || store.hasBoard() && Object.hasOwn(store.board().options, object2.option);
    }
    if ("plugin" in object2) return Object.hasOwn(plugins, object2.plugin);
    return present(object2);
  }
  const offStore = store.subscribe((change) => {
    if ("opened" in change) return;
    const gone = /* @__PURE__ */ new Set([
      ...(change.delta.blocks?.rm ?? []).map((block) => key({ block })),
      ...(change.delta.links?.rm ?? []).map((link) => key({ link })),
      ...(change.delta.stacks?.rm ?? []).map((stack) => key({ stack }))
    ]);
    if (gone.size === 0) return;
    for (const object2 of gone) left2.add(object2);
    const goes = (object2) => object2 !== void 0 && gone.has(key(object2));
    let moved = false;
    for (const [id2, notification] of held2) {
      if (!goes(notification.about) && !goes(notification.source.module?.object)) continue;
      held2.delete(id2);
      raised.delete(id2);
      moved = true;
    }
    for (const [what, holding] of replacing) {
      if (goes(holding.object)) replacing.delete(what);
    }
    if (moved) publish();
  });
  stops.push(offStore);
  function end() {
    if (ended) return;
    ended = true;
    for (const stop of stops) stop();
    const had = held2.size > 0;
    held2.clear();
    raised.clear();
    again.clear();
    replacing.clear();
    left2.clear();
    if (had) publish();
    listeners.clear();
  }
  if (signal5?.aborted === true) end();
  else if (signal5 !== void 0) onAbort(signal5, end);
  function replace(what, object2, next) {
    let moved = false;
    for (const id2 of replacing.get(what)?.ids ?? []) {
      if (!next.has(id2) && held2.delete(id2)) moved = true;
    }
    for (const [id2, notification] of next) {
      const had = held2.get(id2);
      if (had !== void 0 && jsonEqual(had, notification)) continue;
      held2.set(id2, notification);
      moved = true;
    }
    if (next.size === 0) replacing.delete(what);
    else replacing.set(what, { object: object2, ids: [...next.keys()] });
    return moved;
  }
  function fromStatus(block, status) {
    const about = { block };
    const next = /* @__PURE__ */ new Map();
    const add = (given) => {
      const id2 = JSON.stringify(["status", block, given.severity, given.source, given.message]);
      if (!next.has(id2)) next.set(id2, frozen(id2, { ...given, about }));
    };
    if (status !== void 0 && present(about)) {
      for (const condition of status.conditions) {
        add({ severity: condition.severity, message: condition.message, source: statusSource(condition) });
      }
      if (status.unsent !== null) {
        add({ severity: "error", message: status.unsent, source: { kind: "core", code: "evaluation_unsent" } });
      }
    }
    return replace(JSON.stringify(["status", block]), about, next);
  }
  const replied2 = (module, request, conditions, notifier) => {
    const about = module.object;
    const by = moduleCopy(module);
    const next = /* @__PURE__ */ new Map();
    for (const condition of conditions) {
      const source = { kind: "r", class: condition.class, request, module: by };
      const id2 = JSON.stringify(["request", condition.severity, source, condition.message]);
      if (!next.has(id2)) next.set(id2, frozen(id2, { severity: condition.severity, message: condition.message, source, about }));
    }
    if (notifier !== void 0) {
      handTo(notifier, [...next.values()]);
      return;
    }
    if (ended) return;
    if (!present(about)) {
      if (next.size > 0) {
        log.debug(`Core drops the notifications of R's reply to ${requested(request, module)} about ${named3(about)}, which the board does not hold.`);
      }
      next.clear();
    }
    if (replace(JSON.stringify(["request", by, request]), about, next)) publish();
  };
  const raise = (given) => {
    if (ended) return nothing;
    if (given.about !== void 0 && !present(given.about)) {
      log.debug(`Core drops a notification that core raised about ${named3(given.about)}, which the board does not hold.`);
      return nothing;
    }
    const id2 = raisedId(given);
    const token = {};
    const tokens = raised.get(id2) ?? /* @__PURE__ */ new Set();
    tokens.add(token);
    raised.set(id2, tokens);
    if (!held2.has(id2)) {
      held2.set(id2, frozen(id2, given));
      publish();
    }
    return () => {
      const holding = raised.get(id2);
      if (holding?.delete(token) !== true || holding.size > 0) return;
      raised.delete(id2);
      if (held2.delete(id2)) publish();
    };
  };
  const raiseAgain = (given, notifier) => {
    const id2 = raisedId(given);
    if (notifier !== void 0) {
      handTo(notifier, [frozen(id2, given)]);
      return;
    }
    again.get(id2)?.();
    const withdraw = raise(given);
    if (!ended) again.set(id2, withdraw);
  };
  const notify = (module, notification) => {
    const given = checkRaised(notification);
    if (ended) return nothing;
    const about = given.about === void 0 ? module.object : given.about ?? void 0;
    if (about !== void 0 && !known(about)) {
      if (left2.has(key(about))) {
        log.debug(`Core drops a notification that ${raiser(module.object)} raised about ${named3(about)}, which has left the board.`);
      } else {
        log.error(`Core drops a notification that ${raiser(module.object)} raised about ${named3(about)}, ${unknown(about)}.`);
      }
      return nothing;
    }
    const id2 = `module-${String(++count)}`;
    held2.set(
      id2,
      frozen(id2, {
        severity: given.severity,
        message: given.message,
        source: { kind: "module", module, code: given.code },
        ...about === void 0 ? {} : { about }
      })
    );
    publish();
    let withdrawn = false;
    return () => {
      if (withdrawn) return;
      withdrawn = true;
      if (held2.delete(id2)) publish();
    };
  };
  return {
    api: {
      notify(plugin, notification) {
        if (typeof plugin !== "string" || plugin === "") {
          throw new TypeError(
            `A plugin's notification names the plugin by the id of its class, a non-empty string, not ${JSON.stringify(plugin)}.`
          );
        }
        const given = checkRaised(notification);
        if (ended) return nothing;
        if (!Object.hasOwn(plugins, plugin)) {
          log.error(`Core drops a notification that the plugin "${plugin}" raised, which the app does not run.`);
          return nothing;
        }
        return notify(sourceModule({ plugin }, packageFromId(plugin)), given);
      },
      get: () => all,
      subscribe(fn) {
        const wrapped = (notifications) => {
          fn(notifications);
        };
        listeners.add(wrapped);
        return () => {
          listeners.delete(wrapped);
        };
      }
    },
    raise,
    raiseAgain,
    replied: replied2,
    notify,
    follow(scheduler) {
      if (ended) return;
      const heard = (blocks) => {
        let moved = false;
        for (const block of blocks) {
          if (fromStatus(block, scheduler.status(block))) moved = true;
        }
        if (moved) publish();
      };
      stops.push(scheduler.subscribe(heard));
      if (store.hasBoard()) heard(Object.keys(store.board().blocks));
    }
  };
}
function handTo(notifier, notifications) {
  try {
    notifier(Object.freeze(notifications));
  } catch (error) {
    log.error(asError(error));
  }
}
function raisedId(given) {
  return JSON.stringify([
    "raised",
    given.severity,
    given.message,
    sourceCopy(given.source),
    given.about === void 0 ? null : objectCopy(given.about)
  ]);
}
function statusSource(condition) {
  const { phase } = condition;
  return {
    kind: "r",
    class: condition.class,
    request: { type: "evaluate", ...isEvaluatePhase(phase) ? { phase } : {} }
  };
}
function isEvaluatePhase(phase) {
  return phase !== void 0 && Object.hasOwn(EVALUATE_PHASES, phase);
}
function key(object2) {
  return JSON.stringify(object2);
}
function named3(object2) {
  if ("block" in object2) return `the block "${object2.block}"`;
  if ("link" in object2) return `the link "${object2.link}"`;
  if ("stack" in object2) return `the stack "${object2.stack}"`;
  if ("option" in object2) return `the board option "${object2.option}"`;
  return `the plugin "${object2.plugin}"`;
}
function requested(request, module) {
  if (request.type === "evaluate") return "an evaluation";
  if (request.type === "output") return "an output";
  const renderer = request.type === "renderer" ? module.renderer : void 0;
  return renderer === void 0 ? `the request "${request.name}"` : `the request "${request.name}" of the renderer "${renderer}"`;
}
function raiser(object2) {
  return "plugin" in object2 ? named3(object2) : `a module of ${named3(object2)}`;
}
function unknown(object2) {
  if ("option" in object2) return "which no loaded package defines";
  if ("plugin" in object2) return "which the app does not run";
  return "which the board has never held";
}
function frozen(id2, given) {
  return deepFrozen({
    id: id2,
    severity: given.severity,
    message: given.message,
    source: sourceCopy(given.source),
    ...given.about === void 0 ? {} : { about: objectCopy(given.about) }
  });
}
function sourceCopy(source) {
  switch (source.kind) {
    case "r":
      return {
        kind: "r",
        class: [...source.class],
        request: { ...source.request },
        ...source.module === void 0 ? {} : { module: moduleCopy(source.module) }
      };
    case "core":
      return { kind: "core", code: source.code, ...source.module === void 0 ? {} : { module: moduleCopy(source.module) } };
    case "module":
      return { kind: "module", module: moduleCopy(source.module), code: source.code };
  }
}
function moduleCopy(module) {
  return sourceModule(moduleObjectCopy(module.object), module.package, module.renderer);
}
function objectCopy(object2) {
  if ("link" in object2) return { link: object2.link };
  if ("stack" in object2) return { stack: object2.stack };
  return moduleObjectCopy(object2);
}
function moduleObjectCopy(object2) {
  if ("block" in object2) return { block: object2.block };
  if ("option" in object2) return { option: object2.option };
  return { plugin: object2.plugin };
}
function deepFrozen(value2) {
  if (typeof value2 === "object" && value2 !== null) {
    for (const member of Object.values(value2)) deepFrozen(member);
    Object.freeze(value2);
  }
  return value2;
}

// srcts/core/start.ts
var CAPABILITIES = ["cancel", "interrupt", "progress"];
function start(options) {
  const page = new AbortController();
  const alone = options.alone === true;
  const starting = steps("start");
  let helloSent;
  let helloAnswered;
  let tracing = Promise.resolve();
  const fail = (error) => {
    fallBack();
    if (options.fail === void 0) log.error(error);
    else options.fail(error);
  };
  let logging2 = false;
  let first;
  let latest;
  const loaded = /* @__PURE__ */ new Map();
  let changed = [];
  let reloading;
  let running2;
  const holdsBoard = () => running2?.store.hasBoard() === true;
  let restarting;
  const nextRevision = counter();
  let settle2;
  const ready = new Promise((resolve, reject) => {
    settle2 = { resolve, reject };
  });
  ready.catch(() => void 0);
  const client = createClient(options.transport, {
    hello: () => {
      helloSent = performance.now();
      return { protocol: PROTOCOL_VERSION, capabilities: CAPABILITIES };
    },
    session(reply) {
      heard(reply);
    }
  });
  const blockrOptions = createBlockrOptions(client);
  const loader = createLoader({
    ...options.loader,
    modules: () => latest?.modules ?? {},
    changed: (pkg) => changed.find((c) => c.package === pkg)
  });
  const migrations = createMigrations(loader);
  function heard(reply) {
    helloAnswered = performance.now();
    if (page.signal.aborted) return;
    const error = helloError(reply);
    if (error !== void 0) {
      if (first === void 0) {
        settle2?.reject(error);
        fail(error);
      } else if (restarting !== void 0) {
        const waiting2 = restarting;
        restarting = void 0;
        waiting2.reject(error);
      } else {
        log.error(error);
      }
      return;
    }
    const hello = reply;
    if (!sameMajor(hello.protocol, PROTOCOL_VERSION)) {
      halt(
        new Error(
          `R speaks version ${hello.protocol} of the wire protocol, where this page speaks ${PROTOCOL_VERSION}. Reload the page, which then loads the client that R serves.`
        )
      );
      return;
    }
    if (first === void 0) {
      first = hello;
      adopt(hello);
      noteHello(starting);
      boot(hello, void 0, starting).then(
        ({ core }) => {
          starting.record(tracing);
          settle2?.resolve(core);
        },
        (failure) => {
          starting.record(tracing, failure);
          settle2?.reject(failure);
          if (!page.signal.aborted) fail(asError(failure));
        }
      );
      return;
    }
    changed = changedPackages(loaded, hello);
    askReload(running2?.notifications.raise);
    if (changed.length === 0) adopt(hello);
    if (restarting !== void 0) {
      const waiting2 = restarting;
      restarting = void 0;
      waiting2.resolve(latest ?? first);
      return;
    }
    running2?.scheduler.session();
  }
  function noteHello(opening2) {
    const end2 = helloAnswered ?? performance.now();
    opening2.noted("hello", helloSent ?? end2, end2);
  }
  function adopt(reply) {
    latest = reply;
    for (const [pkg, version] of Object.entries(reply.packages)) {
      loaded.set(pkg, version);
    }
  }
  function end(why) {
    page.abort();
    const waiting2 = restarting;
    restarting = void 0;
    waiting2?.reject(why);
  }
  function askReload(raise) {
    const before = reloading;
    reloading = changed.length > 0 && raise !== void 0 ? raise(reloadNotification(changed)) : void 0;
    before?.();
  }
  function halt(error) {
    end(error);
    settle2?.reject(error);
    fail(error);
  }
  async function boot(reply, later, opening2) {
    const controller = new AbortController();
    const { signal: signal5 } = controller;
    const ending = () => {
      controller.abort();
    };
    if (page.signal.aborted) ending();
    page.signal.addEventListener("abort", ending, { once: true });
    signal5.addEventListener(
      "abort",
      () => {
        page.signal.removeEventListener("abort", ending);
      },
      { once: true }
    );
    const [evaluationQueue, backgroundEvaluationDelay, gateVisibility, randId, logLevel2, logger, otelClientEndpoint, otelClientServiceName] = await Promise.all([
      blockrOptions.read("evaluation_queue"),
      blockrOptions.read("background_evaluation_delay"),
      blockrOptions.read("gate_visibility"),
      blockrOptions.read("rand_id"),
      blockrOptions.read("log_level"),
      blockrOptions.read("logger"),
      blockrOptions.read("otel_client_endpoint"),
      blockrOptions.read("otel_client_service_name")
    ]);
    if (!logging2) {
      const base = typeof document === "undefined" ? void 0 : document.baseURI;
      const settings = logSettings({ logLevel: logLevel2, logger, otelClientEndpoint }, base);
      const service = serviceName(otelClientServiceName);
      const sessionId = () => options.transport.sessionId?.();
      logging2 = true;
      setUpLogging(settings, service, sessionId, options.otel ?? noOtel);
      tracing = setUpTracing(tracesUrl(otelClientEndpoint, base), service, sessionId, options.otel ?? noOtel);
    }
    const store = createStore({ hello: reply, signal: signal5, migration: (id2) => migrations.get(id2) });
    const notifications = createNotifications(store, { signal: signal5, plugins: reply.plugins });
    const { raise } = notifications;
    askReload(raise);
    const commands = createCommands(store, {
      signal: signal5,
      lock: reply.lock,
      randId,
      blockrOption: (name3) => blockrOptions.read(name3)
    });
    const boardActions = createBoardActions(raise);
    await opening2.step("modules", () => register(reply, store, commands, boardActions, signal5, raise));
    const bound = later === void 0 ? reply.bound : void 0;
    const scheduler = createScheduler({
      store,
      client,
      evaluationQueue,
      backgroundEvaluationDelay,
      gateVisibility,
      nextRevision,
      signal: signal5,
      ...bound === void 0 ? {} : { bound }
    });
    const { api: core, plugins } = createCore({
      store: {
        ...store,
        open: (given, opts) => reopen(given, controller, opts)
      },
      commands,
      boardActions,
      showMenu: options.showMenu ?? showMenu,
      scheduler,
      loader,
      blockrOptions,
      randId,
      lock: (first ?? reply).lock,
      renderers: () => latest?.renderers ?? {},
      plugins: reply.plugins,
      options: reply.options,
      notifications,
      signal: signal5,
      show: (object2) => options.frontEnds?.show(object2, signal5) ?? Promise.resolve(),
      ...options.measure === void 0 ? {} : { measure: options.measure }
    });
    running2 = { store, scheduler, core, notifications, controller };
    let dropped = [];
    let failure;
    if (later === void 0) {
      const instead = alone ? void 0 : await named4(reply, store, core, signal5, raise, opening2);
      await migrations.prepare(instead?.text ?? reply.board, reply.block_registry);
      if (instead === void 0) {
        dropped = await opening2.step("store.open", () => store.open(reply));
      } else {
        try {
          dropped = await opening2.step("store.open", () => store.open(instead.text));
        } catch (error) {
          log.error(asError(error));
          raise(
            coreNotification(
              "board_open_failed",
              "error",
              `The board that the storage plugin "${instead.plugin}" named did not open, so core opens the board that R sent: ${errorMessage(error)}`,
              { module: sourceModule({ plugin: instead.plugin }, packageFromId(instead.plugin)) }
            )
          );
          await migrations.prepare(reply.board, reply.block_registry);
          dropped = await opening2.step("store.open", () => store.open(reply));
        }
      }
    } else {
      try {
        await migrations.prepare(later.text, reply.block_registry);
        dropped = await opening2.step("store.open", () => store.open(later.text));
      } catch (error) {
        failure = asError(error);
        try {
          await opening2.step("store.open", () => store.open(later.before));
        } catch (again) {
          log.error(asError(again));
          throw failure;
        }
        const why = `The board did not open, so core opened the board before it again: ${failure.message}`;
        log.debug(why);
        raise(coreNotification("board_open_failed", "error", why));
      }
    }
    if (dropped.length > 0) raise(droppedNotification(dropped));
    if (alone) {
      try {
        options.frontEnds?.card?.(core, signal5);
      } finally {
        scheduler.start();
      }
    } else {
      const front = plugins.frontend();
      options.frontEnds?.prepare(
        front === void 0 ? { none: noFrontend(reply, core) } : { frontend: front.frontend, plugin: front.plugin, args: reply.frontend },
        signal5
      );
      const failures = await (front?.added ?? []);
      try {
        await opening2.step("render", async () => options.frontEnds?.render(core, signal5, failures));
      } finally {
        scheduler.start();
      }
    }
    await plugins.start();
    if (failure !== void 0) throw failure;
    return { core, dropped };
  }
  async function register(reply, store, commands, boardActions, signal5, raise) {
    const failed2 = (error, concerns) => {
      log.error(asError(error));
      if (!signal5.aborted) raise(coreNotification("module_failed", "error", errorMessage(error), concerns));
    };
    const [defs, opts] = await Promise.all([
      Promise.all(
        Object.entries(reply.plugins).map(
          ([id2, entry]) => listed(entry, packageFromId(id2), (error) => {
            failed2(error, { module: sourceModule({ plugin: id2 }, packageFromId(id2)) });
          })
        )
      ),
      Promise.all(
        Object.entries(reply.options).map(
          ([id2, entry]) => listed(entry, entry.package, (error) => {
            failed2(error, { module: sourceModule({ option: id2 }, entry.package) });
          })
        )
      )
    ]);
    if (signal5.aborted) return;
    Object.entries(reply.options).forEach(([id2, entry], i) => {
      const def = opts[i];
      if (def === void 0) return;
      try {
        store.options.register({ ...def, id: id2 });
      } catch (error) {
        failed2(error, { module: sourceModule({ option: id2 }, entry.package) });
      }
    });
    Object.keys(reply.plugins).forEach((id2, i) => {
      const def = defs[i];
      if (def === void 0) return;
      let cls;
      try {
        cls = { ...def, id: id2 };
        const unregister = store.plugins.register(cls);
        try {
          boardActions.register(id2, cls.boardActions);
        } catch (error) {
          unregister();
          throw error;
        }
      } catch (error) {
        failed2(error, { module: sourceModule({ plugin: id2 }, packageFromId(id2)) });
        return;
      }
      for (const command2 of cls.commands ?? []) {
        try {
          commands.register(command2);
        } catch (error) {
          log.error(new Error(`A command of the plugin class "${id2}" is refused: ${errorMessage(error)}`, { cause: error }));
        }
      }
    });
  }
  async function listed(source, pkg, failed2) {
    try {
      const at = source.css === void 0 ? { module: source.module } : { module: source.module, css: source.css };
      return listedDefault(await loader.load(at, pkg), source.module, pkg);
    } catch (error) {
      failed2(error);
      return void 0;
    }
  }
  async function named4(reply, store, core, signal5, raise, opening2) {
    if (reply.lock.load) return void 0;
    const classes = store.plugins.registered();
    const plugin = Object.keys(reply.plugins).find((id2) => own(classes, id2)?.storage !== void 0);
    const storage = plugin === void 0 ? void 0 : own(classes, plugin)?.storage;
    if (plugin === void 0 || storage?.load === void 0) return void 0;
    const refuse = (error) => {
      log.error(error);
      raise(coreNotification("board_open_failed", "error", error.message, { module: sourceModule({ plugin }, packageFromId(plugin)) }));
    };
    let text;
    try {
      text = await opening2.step("storage.load", async () => storage.load?.({ core, plugin, signal: signal5 }));
    } catch (error) {
      refuse(
        new Error(`The load() of the storage plugin "${plugin}" failed, so core opens the board that R sent: ${errorMessage(error)}`, {
          cause: error
        })
      );
      return void 0;
    }
    if (text === void 0) return void 0;
    if (typeof text !== "string") {
      refuse(
        new Error(
          `The load() of the storage plugin "${plugin}" resolved with something other than the text of a JSON board, so core opens the board that R sent.`
        )
      );
      return void 0;
    }
    try {
      JSON.parse(text);
    } catch (error) {
      refuse(
        new Error(`The storage plugin "${plugin}" named a board that holds no JSON, so core opens the board that R sent: ${errorMessage(error)}`)
      );
      return void 0;
    }
    return { text, plugin };
  }
  let queue = Promise.resolve();
  function reopen(given, of, opts) {
    let notifier;
    try {
      notifier = givenNotifier(opts, "open()");
    } catch (error) {
      return Promise.reject(asError(error));
    }
    const opening2 = steps("open");
    const run = queue.then(() => reopening(given, of, opening2, notifier));
    queue = run.catch(() => void 0);
    return run;
  }
  async function reopening(given, of, opening2, notifier) {
    const refused2 = "The store refused to open the board";
    const current2 = running2;
    if (current2?.controller !== of || of.signal.aborted) {
      const error = new StoreError(refused2, ["Another board opened since, so this store opens no board."]);
      opening2.record(tracing, error);
      throw error;
    }
    if (!current2.store.hasBoard()) {
      const error = new StoreError(refused2, ["The store holds no board yet, so it opens none in place of it."]);
      opening2.record(tracing, error);
      throw error;
    }
    const text = typeof given === "string" ? given : JSON.stringify(given.board);
    let before;
    try {
      await migrations.prepare(text, current2.store.blockRegistry());
      try {
        await current2.store.check(text);
      } catch (error) {
        const why = errorMessage(error);
        log.debug(why);
        current2.notifications.raiseAgain(coreNotification("board_open_failed", "error", why), notifier);
        throw error;
      }
      before = current2.store.save();
    } catch (error) {
      opening2.record(tracing, error);
      throw error;
    }
    of.abort();
    running2 = void 0;
    try {
      const reply = await afresh();
      noteHello(opening2);
      const { dropped } = await boot(reply, { text, before }, opening2);
      opening2.record(tracing);
      return dropped;
    } catch (error) {
      opening2.record(tracing, error);
      if (!page.signal.aborted && !holdsBoard()) fail(asError(error));
      throw error;
    }
  }
  function afresh() {
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
    core: () => running2?.core,
    stop() {
      end(new Error("Core has stopped."));
    }
  };
}
function helloError(reply) {
  if (reply.status !== "ok" || !("block_registry" in reply)) {
    const said = "conditions" in reply ? reply.conditions.map((c) => c.message).join(" ") : "";
    return new Error(`R replied ${reply.status} to the client's hello. ${said}`.trim());
  }
  return void 0;
}
function noFrontend(reply, core) {
  const missing = Object.keys(reply.plugins).filter((id2) => !Object.hasOwn(core.plugins.appPlugins(), id2));
  const why = missing.length === 0 ? "none of the app's plugins is a frontend" : `core registered no class of the app's ${missing.length === 1 ? "plugin" : "plugins"} ${quoted(missing)}`;
  return `The app runs no frontend that draws the board, since ${why}.`;
}
function changedPackages(loaded, reply) {
  return Object.entries(reply.packages).flatMap(([pkg, now]) => {
    const before = loaded.get(pkg);
    return before === void 0 || before === now ? [] : [{ package: pkg, loaded: before, now }];
  });
}
function reloadNotification(changed) {
  const versions = changed.map((c) => `${c.package} ${c.loaded} to ${c.now}`).join(", ");
  return coreNotification(
    "page_reload_needed",
    "warning",
    `R now runs other versions of packages than this page loaded (${versions}). Save the board, then reload the page.`
  );
}
function droppedNotification(dropped) {
  const lines = dropped.map((item) => {
    if ("option" in item) return `- board option "${item.option}"`;
    if ("block" in item) return `- block "${item.block}" (${item.registry_id})`;
    if ("plugin" in item) return `- plugin "${item.plugin}"`;
    return `- package ${item.package} ${item.version}`;
  });
  return coreNotification("board_objects_dropped", "warning", ["The board opened without:", ...lines].join("\n"));
}
function listedDefault(exports, module, pkg) {
  const def = moduleExports(exports, module, pkg).default;
  if (!isObject(def)) {
    throw new ModuleError(module, pkg, "has no default export, an object.");
  }
  return def;
}
function noOtel() {
  return Promise.reject(new Error("Core was given no OpenTelemetry module to send log records or spans with."));
}

// srcts/page/card.tsx
import { useCallback as useCallback2, useSyncExternalStore as useSyncExternalStore2 } from "react";
import { flushSync as flushSync2 } from "react-dom";
import { createRoot as createRoot2 } from "react-dom/client";

// srcts/card/footer.tsx
import { useCallback, useId, useState } from "react";
import { Fragment as Fragment2, jsx as jsx2, jsxs as jsxs2 } from "react/jsx-runtime";
var SEVERITIES2 = ["error", "warning", "message"];
function useFooter(status, notifications) {
  const shown2 = useShown(status);
  const opened = useOpened(OPEN_AT_START);
  return { status, shown: shown2, settled: isSettled(status, shown2), notifications, ...opened };
}
function useOpened(atStart) {
  const [open, setOpen] = useState(atStart);
  const panel = useId();
  const toggle = useCallback((severity) => {
    setOpen((was) => {
      const next = new Set(was);
      if (!next.delete(severity)) next.add(severity);
      return next;
    });
  }, []);
  return { open, toggle, panel };
}
function Footer({ status, shown: shown2, settled: settled2 }) {
  return /* @__PURE__ */ jsx2(
    "footer",
    {
      "aria-busy": status?.running === true,
      "data-status": shown2?.status,
      className: `bc:min-h-4 bc:shrink-0 bc:px-5 bc:pb-1 bc:text-center bc:font-mono bc:text-xs bc:text-text-muted bc:transition-opacity ${settled2 ? "" : "bc:opacity-50"}`,
      children: shown2?.status === "ready" ? shown2.summary : null
    }
  );
}
function Pills({
  status,
  shown: shown2,
  settled: settled2,
  notifications,
  open,
  toggle,
  panel,
  className = "bc:justify-center bc:px-5"
}) {
  const pills = SEVERITIES2.flatMap((severity) => {
    const n = counted(shown2, notifications, severity);
    return n === 0 ? [] : [{ severity, n }];
  });
  if (pills.length === 0) return null;
  return /* @__PURE__ */ jsx2(
    "div",
    {
      role: "group",
      "aria-label": "Status",
      "aria-busy": status?.running === true,
      className: `bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:font-mono bc:text-xs bc:transition-opacity ${className} ${settled2 ? "" : "bc:opacity-50"}`,
      children: pills.map(({ severity, n }) => {
        const mark = MARKS[severity];
        return /* @__PURE__ */ jsxs2(
          "button",
          {
            type: "button",
            "aria-expanded": open.has(severity),
            "aria-controls": panel,
            onClick: () => {
              toggle(severity);
            },
            className: `bc:inline-flex bc:cursor-pointer bc:items-center bc:gap-1 bc:rounded-full bc:border-0 bc:px-1.5 bc:py-0.5 bc:font-mono bc:text-xs bc:hover:underline bc:aria-expanded:ring-1 bc:aria-expanded:ring-current ${mark.tint} ${mark.text}`,
            children: [
              /* @__PURE__ */ jsx2(SeverityIcon, { severity }),
              severity === "error" ? mark.noun : `${String(n)} ${mark.noun}${n === 1 ? "" : "s"}`
            ]
          },
          severity
        );
      })
    }
  );
}
function Banners({ shown: shown2, notifications, open, panel }) {
  const listed = notifications.filter(
    (notification) => open.has(notification.severity)
  );
  const reason = shown2?.reason ?? null;
  if (reason !== null && open.has("message")) listed.push({ severity: "message", message: reason });
  if (listed.length === 0) return null;
  return /* @__PURE__ */ jsx2("ul", { id: panel, "aria-label": "Conditions", className: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0", children: listed.map(({ severity, message }, i) => /* @__PURE__ */ jsx2(Banner, { severity, message }, i)) });
}
function isSettled(status, shown2) {
  return status !== void 0 && status === shown2 && !status.running;
}
function counted(status, notifications, severity) {
  const n = notifications.filter((notification) => notification.severity === severity).length;
  return severity === "message" && (status?.reason ?? null) !== null ? n + 1 : n;
}
var MARKS = {
  error: {
    label: "Error",
    noun: "error",
    text: "bc:text-text-danger",
    tint: "bc:bg-red-500/10",
    border: "bc:border-border-danger"
  },
  warning: {
    label: "Warning",
    noun: "warning",
    text: "bc:text-text-warning",
    tint: "bc:bg-amber-500/10",
    border: "bc:border-amber-500"
  },
  message: {
    label: "Message",
    noun: "message",
    text: "bc:text-blue-700 bc:dark:text-blue-400",
    tint: "bc:bg-blue-500/10",
    border: "bc:border-border-accent"
  }
};
function Banner({ severity, message }) {
  const mark = MARKS[severity];
  return /* @__PURE__ */ jsxs2(
    "li",
    {
      "data-severity": severity,
      className: `bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`,
      children: [
        /* @__PURE__ */ jsx2("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx2(SeverityIcon, { severity }) }),
        /* @__PURE__ */ jsxs2("span", { className: "bc:grid bc:min-w-0 bc:gap-0.5", children: [
          /* @__PURE__ */ jsx2("span", { className: `bc:font-sans bc:text-xs bc:font-semibold ${mark.text}`, children: mark.label }),
          /* @__PURE__ */ jsx2("span", { className: "bc:font-mono bc:text-xs bc:whitespace-pre-wrap", children: message })
        ] })
      ]
    }
  );
}
function SeverityIcon({ severity }) {
  return /* @__PURE__ */ jsx2(
    "svg",
    {
      "aria-hidden": "true",
      viewBox: "0 0 16 16",
      className: "bc:size-3 bc:shrink-0",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: 1.6,
      strokeLinecap: "round",
      strokeLinejoin: "round",
      children: severity === "warning" ? /* @__PURE__ */ jsxs2(Fragment2, { children: [
        /* @__PURE__ */ jsx2("path", { d: "M8 1.8 14.6 13.6H1.4Z" }),
        /* @__PURE__ */ jsx2("path", { d: "M8 6.2v3.2M8 11.6v.1" })
      ] }) : /* @__PURE__ */ jsxs2(Fragment2, { children: [
        /* @__PURE__ */ jsx2("circle", { cx: "8", cy: "8", r: "6.4" }),
        /* @__PURE__ */ jsx2("path", { d: severity === "error" ? "M5.7 5.7l4.6 4.6M10.3 5.7l-4.6 4.6" : "M8 7.4v3.6M8 4.9v.1" })
      ] })
    }
  );
}
function useShown(status) {
  const [kept, setKept] = useState(void 0);
  const now = status !== void 0 && status.status !== "stale" && status.status !== "unevaluated" ? status : void 0;
  if (now !== void 0 && now !== kept) setKept(now);
  return now ?? kept;
}
var OPEN_AT_START = /* @__PURE__ */ new Set(["error"]);

// srcts/card/mounted.tsx
import { useLayoutEffect, useRef as useRef2 } from "react";
import { jsx as jsx3 } from "react/jsx-runtime";
function Mounted({
  mount,
  label,
  className,
  data
}) {
  const ref = useRef2(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const cleanup = mount(el);
    return () => {
      queueMicrotask(cleanup);
    };
  }, [mount]);
  return /* @__PURE__ */ jsx3("div", { ref, role: label === void 0 ? void 0 : "region", "aria-label": label, className, ...data });
}

// srcts/card/status.ts
import { useState as useState2 } from "react";
function useSized(status) {
  const ready = status?.status === "ready";
  const [sized, setSized] = useState2(ready);
  if (ready && !sized) setSized(true);
  return sized || ready;
}

// srcts/page/card.tsx
import { useNotified, useStatus } from "@blockr/core/react";
import { jsx as jsx4, jsxs as jsxs3 } from "react/jsx-runtime";
var OWNER = "blockr.core";
function drawCard(el, core, signal5) {
  if (signal5.aborted) return;
  const ids2 = Object.keys(core.store.board().blocks);
  const [id2] = ids2;
  if (id2 === void 0 || ids2.length > 1) {
    showError(el, `A block that serve() serves alone comes as a board of one block, and this board holds ${String(ids2.length)}.`);
    return;
  }
  const demand = core.scheduler.demand(OWNER, [id2]);
  el.replaceChildren();
  const root = createRoot2(el);
  flushSync2(() => {
    root.render(/* @__PURE__ */ jsx4(AloneCard, { core, id: id2 }));
  });
  signal5.addEventListener(
    "abort",
    () => {
      demand.release();
      root.unmount();
      el.replaceChildren(loadingNote());
    },
    { once: true }
  );
}
function AloneCard({ core, id: id2 }) {
  const name3 = useBlockName(core, id2);
  const status = useStatus(core, id2);
  const notified = useNotified(core, null);
  const footer = useFooter(status, notified);
  const sized = useSized(status);
  const mountUi = useCallback2((el) => core.mountBlock(el, id2), [core, id2]);
  const mountOutput2 = useCallback2((el) => core.mountOutput(el, id2), [core, id2]);
  const inputs = !core.store.locked(id2);
  const current2 = status?.status === "ready" && !status.running;
  return /* @__PURE__ */ jsxs3(
    "article",
    {
      "aria-label": name3,
      className: "bc:mx-auto bc:my-4 bc:grid bc:max-w-5xl bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-subtle bc:p-3 bc:font-sans bc:text-gray-900 bc:dark:text-gray-100",
      children: [
        /* @__PURE__ */ jsx4("header", { children: /* @__PURE__ */ jsx4("h1", { className: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold", children: name3 }) }),
        inputs ? /* @__PURE__ */ jsx4(Mounted, { mount: mountUi, label: "Inputs", className: "bc:min-w-0" }) : null,
        /* @__PURE__ */ jsxs3(
          "div",
          {
            className: `bc:min-w-0 ${sized ? "bc:flex bc:h-104 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`,
            children: [
              /* @__PURE__ */ jsx4(
                Mounted,
                {
                  mount: mountOutput2,
                  label: "Output",
                  data: { "data-current": String(current2) },
                  className: `bc:min-w-0 bc:transition-opacity ${current2 ? "" : "bc:opacity-50 bc:grayscale"} ${sized ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"}`
                }
              ),
              sized ? /* @__PURE__ */ jsx4(Footer, { ...footer }) : null
            ]
          }
        ),
        /* @__PURE__ */ jsx4(Pills, { ...footer }),
        /* @__PURE__ */ jsx4(Banners, { ...footer })
      ]
    }
  );
}
function useBlockName(core, id2) {
  const subscribe = useCallback2(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
  return useSyncExternalStore2(subscribe, () => own(core.store.board().blocks, id2)?.name ?? id2);
}

// srcts/page/page.ts
function startPage(options) {
  const { el } = options;
  if (el.firstElementChild === null) el.replaceChildren(loadingNote());
  return start({
    transport: options.transport ?? shinyTransport(),
    ...options.loader === void 0 ? {} : { loader: options.loader },
    ...options.measure === void 0 ? {} : { measure: options.measure },
    frontEnds: {
      ...pageFrontEnds(el),
      card(core, signal5) {
        drawCard(el, core, signal5);
      }
    },
    ...options.alone === true ? { alone: true } : {},
    // The build leaves this import an import, which the page's import map
    // resolves, so that a page loads the module only where it imports it.
    otel: () => import("@blockr/core/otel"),
    fail(error) {
      showError(el, error.message);
      log.error(error);
    }
  });
}
export {
  SessionEndedError,
  createClient,
  shinyTransport,
  startPage
};

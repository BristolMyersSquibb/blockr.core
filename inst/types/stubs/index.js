// srcts/testing/dom.ts
for (const name2 of ["clientWidth", "clientHeight"]) {
  if (typeof HTMLElement === "undefined") break;
  const getter = Object.getOwnPropertyDescriptor(HTMLElement.prototype, name2);
  if (getter === void 0) continue;
  Reflect.deleteProperty(HTMLElement.prototype, name2);
  if (!Object.hasOwn(Element.prototype, name2)) Object.defineProperty(Element.prototype, name2, getter);
}
for (const name2 of ["min", "max", "step"]) {
  if (typeof HTMLInputElement === "undefined") break;
  const property = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, name2);
  if (property?.set === void 0) continue;
  Object.defineProperty(HTMLInputElement.prototype, name2, {
    ...property,
    set(value2) {
      if (value2 === "") this.removeAttribute(name2);
      else property.set?.call(this, value2);
    }
  });
}
if (typeof navigator !== "undefined" && navigator.userAgent.includes("HappyDOM")) {
  globalThis.IntersectionObserver = class InView {
    root = null;
    rootMargin = "0px";
    scrollMargin = "0px";
    thresholds = [0];
    #callback;
    #observed = /* @__PURE__ */ new Set();
    constructor(callback) {
      this.#callback = callback;
    }
    observe(target2) {
      this.#observed.add(target2);
      queueMicrotask(() => {
        if (!this.#observed.has(target2)) return;
        const rect = target2.getBoundingClientRect();
        const entry = {
          target: target2,
          isIntersecting: true,
          intersectionRatio: 1,
          boundingClientRect: rect,
          intersectionRect: rect,
          rootBounds: null,
          time: performance.now()
        };
        this.#callback([entry], this);
      });
    }
    unobserve(target2) {
      this.#observed.delete(target2);
    }
    disconnect() {
      this.#observed.clear();
    }
    takeRecords() {
      return [];
    }
  };
}
if (typeof document !== "undefined") {
  const view = document.defaultView;
  if (view !== null && !(document instanceof view.Document)) {
    const own2 = Object.getPrototypeOf(view.Document);
    Object.defineProperty(view.Document, Symbol.hasInstance, { value: (value2) => value2 instanceof own2 });
  }
}

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
function isPackageName(name2) {
  return typeof name2 === "string" && /^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$/.test(name2);
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
function holdsObject(board, object2, plugins) {
  if ("block" in object2) return Object.hasOwn(board.blocks, object2.block);
  if ("link" in object2) return Object.hasOwn(board.links, object2.link);
  if ("stack" in object2) return Object.hasOwn(board.stacks, object2.stack);
  if ("option" in object2) return Object.hasOwn(board.options, object2.option);
  return Object.hasOwn(plugins, object2.plugin);
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

// srcts/util/shortcuts.ts
function matchesShortcut(text, event) {
  const shortcut = readShortcut(text);
  const key2 = event.key;
  if (shortcut === void 0 || typeof key2 !== "string" || key2.toLowerCase() !== shortcut.key.toLowerCase()) {
    return false;
  }
  if (event.altKey !== shortcut.alt || event.shiftKey !== shortcut.shift) return false;
  if (shortcut.mod) return event.ctrlKey || event.metaKey;
  return event.ctrlKey === shortcut.ctrl && event.metaKey === shortcut.meta;
}
function ariaShortcuts(text) {
  const shortcut = text === void 0 ? void 0 : readShortcut(text);
  if (shortcut === void 0) return void 0;
  const rest = [...shortcut.alt ? ["Alt"] : [], ...shortcut.shift ? ["Shift"] : [], shortcut.key];
  const firsts = shortcut.mod ? [["Control"], ["Meta"]] : [[...shortcut.ctrl ? ["Control"] : [], ...shortcut.meta ? ["Meta"] : []]];
  return firsts.map((first) => [...first, ...rest].join("+")).join(" ");
}
function shortcutLabel(text, mac) {
  const shortcut = text === void 0 ? void 0 : readShortcut(text);
  if (shortcut === void 0) return void 0;
  const key2 = shortcut.key.length === 1 ? shortcut.key.toUpperCase() : shortcut.key;
  if (mac) {
    const glyphs = [
      ...shortcut.ctrl ? ["\u2303"] : [],
      ...shortcut.alt ? ["\u2325"] : [],
      ...shortcut.mod || shortcut.meta ? ["\u2318"] : [],
      ...shortcut.shift ? ["\u21E7"] : []
    ];
    return [...glyphs, key2].join("");
  }
  const words = [
    ...shortcut.mod || shortcut.ctrl ? ["Ctrl"] : [],
    ...shortcut.alt ? ["Alt"] : [],
    ...shortcut.shift ? ["Shift"] : [],
    ...shortcut.meta ? ["Meta"] : []
  ];
  return [...words, key2].join("+");
}
function onMac(win) {
  const nav = win.navigator;
  return /mac|iphone|ipad/i.test(nav.userAgentData?.platform ?? nav.platform);
}
var MODIFIERS = ["mod", "ctrl", "alt", "shift", "meta"];
function readShortcut(text) {
  const at = text.length < 2 ? -1 : text.lastIndexOf("+", text.length - 2);
  const key2 = text.slice(at + 1);
  const names = at < 0 ? [] : text.slice(0, at).toLowerCase().split("+");
  if (key2 === "" || new Set(names).size < names.length || !names.every((name2) => MODIFIERS.includes(name2))) {
    return void 0;
  }
  const has = (name2) => names.includes(name2);
  if (has("mod") && (has("ctrl") || has("meta"))) return void 0;
  return { key: key2, mod: has("mod"), ctrl: has("ctrl"), alt: has("alt"), shift: has("shift"), meta: has("meta") };
}

// srcts/util/validate.ts
import { Validator } from "@cfworker/json-schema";
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
function replaceLogger(logger, passOn = false) {
  const entry = { logger, passOn };
  replaced.push(entry);
  return () => {
    const at = replaced.lastIndexOf(entry);
    if (at >= 0) replaced.splice(at, 1);
  };
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
  const named5 = typeof value2 === "string" && value2 !== "" ? THRESHOLDS.filter((level2) => level2.startsWith(value2)) : [];
  const [level] = named5;
  if (named5.length === 1 && level !== void 0) return level;
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

// srcts/stubs/util.ts
var READY = Object.freeze({
  status: "ready",
  revision: 1,
  evaluated: 1,
  running: false,
  conditions: Object.freeze([]),
  stdout: "",
  stderr: "",
  summary: null,
  reason: null,
  unsent: null
});
function stubBlockContext(given = {}) {
  const values2 = given.options ?? {};
  return Object.freeze({
    name: given.name ?? "Block",
    status: given.status ?? READY,
    inputs: given.inputs ?? [],
    option: (id2) => optionValue(values2, id2, "context")
  });
}
function optionValue(values2, id2, where) {
  if (!Object.hasOwn(values2, id2)) {
    throw new Error(`The stub's ${where} holds no board option "${id2}", whose value a test gives in its \`options\`.`);
  }
  return values2[id2];
}
function announcer(events) {
  const listeners = new Map(events.map((event) => [event, /* @__PURE__ */ new Set()]));
  return {
    listen: (event, fn) => {
      const heard = listeners.get(event);
      if (heard === void 0) {
        throw new Error(`The host announces ${events.map((name2) => `"${name2}"`).join(" and ")}, not ${JSON.stringify(event)}.`);
      }
      const wrapped = () => {
        fn();
      };
      heard.add(wrapped);
      return () => {
        heard.delete(wrapped);
      };
    },
    announce: (event) => {
      for (const fn of [...listeners.get(event) ?? []]) fn();
    }
  };
}
function raisedHolder() {
  const notified = [];
  return {
    notified,
    notify(notification) {
      const given = checkRaised(notification);
      notified.push(given);
      let withdrawn = false;
      return () => {
        if (withdrawn) return;
        withdrawn = true;
        notified.splice(notified.indexOf(given), 1);
      };
    }
  };
}
function lineHolder(pkg = null) {
  const logged = [];
  return {
    logged,
    log: writingLog(pkg, (line) => {
      logged.push(line);
    })
  };
}

// srcts/stubs/block.ts
function stubBlockHost(fixture, options = {}) {
  let state2 = options.state ?? fixture.states[0] ?? {};
  let contextGiven = options.context ?? {};
  let context4 = stubBlockContext(contextGiven);
  const submitted = [];
  const requested2 = [];
  const evaluated = [];
  const on = announcer(["state", "context"]);
  const { notified, notify } = raisedHolder();
  const { logged, log: log2 } = lineHolder(packageFromId(options.class ?? "stub"));
  return {
    block: options.block ?? "block",
    class: options.class ?? "stub",
    state: () => state2,
    context: () => context4,
    on: on.listen,
    submitted,
    requested: requested2,
    evaluated,
    notified,
    notify,
    logged,
    log: log2,
    submit(change) {
      if (!isObject(change)) {
        return Promise.reject(new TypeError("A submit takes the state args it changes, as an object."));
      }
      const unset = Object.entries(change).flatMap(([arg, value2]) => value2 === void 0 ? [arg] : []);
      const next = without({ ...state2, ...defined(change) }, unset);
      submitted.push(next);
      if (!fixture.states.some((listed) => jsonEqual(listed, next))) {
        return Promise.reject(
          new Error(`The module submitted the state ${JSON.stringify(next)}, which the fixture does not list.`)
        );
      }
      state2 = next;
      on.announce("state");
      return Promise.resolve();
    },
    request(name2, args = {}, opts = {}) {
      const given = defined(args);
      requested2.push({ name: name2, args: given, ...opts.file === void 0 ? {} : { file: opts.file } });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      const found = fixture.requests.find(
        (call) => call.name === name2 && Object.keys(given).every((arg) => Object.hasOwn(call.args, arg)) && // The host fills in a state arg from the block's state
        // where the module passes none.
        Object.entries(call.args).every(
          ([arg, value2]) => jsonEqual(Object.hasOwn(given, arg) ? given[arg] : own(state2, arg), value2)
        ) && (call.file === void 0 ? opts.file === void 0 : opts.file !== void 0 && sameFile(call.file, opts.file)) && Object.entries(call.options).every(([id2, value2]) => jsonEqual(own(contextGiven.options ?? {}, id2), value2))
      );
      if (found === void 0) {
        return Promise.reject(
          new Error(
            `The fixture records no answer of the request "${name2}" with the args ${JSON.stringify(given)}, on the state ${JSON.stringify(state2)} and the board options ${JSON.stringify(contextGiven.options ?? {})}.`
          )
        );
      }
      return Promise.resolve(found.answer);
    },
    async evaluate(params) {
      if (!isObject(params)) {
        throw new TypeError("A host's evaluate() takes the parameters that the block's block_eval() method reads, as an object.");
      }
      evaluated.push(params);
      return options.evaluate === void 0 ? context4.status : await options.evaluate(params);
    },
    module(id2) {
      const modules = options.modules ?? {};
      return Object.hasOwn(modules, id2) ? Promise.resolve(modules[id2]) : Promise.reject(new Error(`No package offers a module "${id2}".`));
    },
    setState(next) {
      state2 = next;
      on.announce("state");
    },
    setContext(changes2) {
      contextGiven = { ...contextGiven, ...changes2 };
      context4 = stubBlockContext(contextGiven);
      on.announce("context");
    }
  };
}
function sameFile(recorded, file) {
  return isObject(recorded) && recorded.name === file.name && recorded.type === file.type && recorded.size === file.size;
}
function defined(x) {
  return Object.fromEntries(Object.entries(x).filter(([, value2]) => value2 !== void 0));
}

// srcts/stubs/option.ts
function stubOptionHost(options) {
  let value2 = options.value;
  const submitted = [];
  const requested2 = [];
  const on = announcer(["value"]);
  const { notified, notify } = raisedHolder();
  const { logged, log: log2 } = lineHolder();
  return {
    option: options.option,
    value: () => value2,
    on: on.listen,
    submitted,
    requested: requested2,
    notified,
    notify,
    logged,
    log: log2,
    submit(next) {
      submitted.push(next);
      value2 = next;
      on.announce("value");
      return Promise.resolve();
    },
    request(name2, args = {}, opts = {}) {
      requested2.push({ name: name2, args });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      return answer(options.requests ?? [], name2, args, `the board option "${options.option}"`);
    },
    setValue(next) {
      value2 = next;
      on.announce("value");
    }
  };
}
function answer(answers, name2, args, of) {
  if (!isObject(args)) {
    return Promise.reject(new TypeError(`The args of the request "${name2}" are an object.`));
  }
  const found = answers.find((call) => call.name === name2 && jsonEqual(call.args ?? {}, args));
  return found === void 0 ? Promise.reject(new Error(`No answer is given for the request "${name2}" of ${of} with the args ${JSON.stringify(args)}.`)) : Promise.resolve(found.answer);
}

// srcts/stubs/output.ts
function stubOutputHost(options = {}) {
  const first = options.fixtures?.outputs[0];
  let output = options.output ?? first?.output ?? null;
  let body = options.body ?? null;
  let params = options.params ?? (options.output === void 0 ? first?.params ?? {} : {});
  const values2 = options.options ?? (options.output === void 0 ? first?.options ?? {} : {});
  let loading = false;
  let error = null;
  const emitted = [];
  const asked = [];
  const requested2 = [];
  const on = announcer(["output"]);
  const { notified, notify } = raisedHolder();
  const { logged, log: log2 } = lineHolder();
  return {
    block: options.block ?? "block",
    output: () => output,
    body: () => body,
    params: () => params,
    option: (id2) => optionValue(values2, id2, "output host"),
    loading: () => loading,
    error: () => error,
    on: on.listen,
    emitted,
    asked,
    requested: requested2,
    notified,
    notify,
    logged,
    log: log2,
    setParams(next) {
      asked.push(next);
      const found = options.fixtures?.outputs.find(
        (fixture) => jsonEqual(fixture.params, next) && Object.entries(fixture.options).every(([id2, value2]) => jsonEqual(values2[id2], value2))
      );
      if (found === void 0) {
        error = {
          severity: "error",
          class: ["blockr_error", "error", "condition"],
          message: `The fixtures hold no output asked with the parameters ${JSON.stringify(next)}.`
        };
      } else {
        output = found.output;
        params = next;
        error = null;
      }
      on.announce("output");
    },
    request(name2, args = {}, opts = {}) {
      const scope = opts.scope ?? "class";
      requested2.push({ name: name2, args, scope });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      const answers = (options.requests ?? []).filter((call) => (call.scope ?? "class") === scope);
      return answer(answers, name2, args, scope === "class" ? "the output class" : "the renderer");
    },
    emit(name2, payload) {
      emitted.push({ name: name2, payload });
    },
    setOutput(next, nextBody = null, nextParams) {
      output = next;
      body = nextBody;
      if (nextParams !== void 0) params = nextParams;
      loading = false;
      error = null;
      on.announce("output");
    },
    setLoading(next) {
      loading = next;
      on.announce("output");
    },
    setError(next) {
      error = next;
      on.announce("output");
    }
  };
}

// srcts/scheduler/scheduler.ts
import { ROOT_CONTEXT } from "@opentelemetry/api";
import { action, batch, computed, effect, signal, untracked } from "@preact/signals-core";

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
    withUpstream: (ids3) => closure(ids3, upstream),
    withDownstream: (ids3) => closure(ids3, downstream),
    cycle() {
      cycle ??= { found: findCycle(board, targets) };
      return cycle.found;
    },
    path: (from, to) => findPath(from, to, downstream)
  };
}
function once(ends2) {
  return new Map([...ends2].map(([id2, blocks]) => [id2, [...new Set(blocks)]]));
}
function closure(ids3, steps2) {
  const reached = /* @__PURE__ */ new Set();
  const todo = [...ids3].filter((id2) => steps2.has(id2));
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
      const count2 = (left2.get(next) ?? 0) - 1;
      left2.set(next, count2);
      if (count2 === 0) order.push(next);
    }
  }
  const placed2 = new Set(order);
  return [...order, ...blocks.filter((id2) => !placed2.has(id2))];
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
function steps(name2, start3 = performance.now(), parent = context2.active()) {
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
        const whole = tracer().startSpan(name2, { startTime: start3 }, parent);
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
  let held4 = [];
  const sending = /* @__PURE__ */ new Set();
  let ends2 = 0;
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
    const index = held4.indexOf(entry);
    if (index >= 0) {
      held4.splice(index, 1);
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
      held4.push(entry);
    }
  }
  function end() {
    state2 = "none";
    shared = /* @__PURE__ */ new Set();
    ends2 += 1;
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
    const lost = held4;
    held4 = [];
    for (const { call } of lost) {
      call?.settle({ error: stopped });
    }
  }
  function sendHello() {
    const named5 = options.hello();
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
          if (sameMajor(reply.protocol, named5.protocol)) {
            const theirs = new Set(reply.capabilities);
            shared = new Set(named5.capabilities.filter((c) => theirs.has(c)));
            state2 = "ready";
            const outbound = held4;
            held4 = [];
            dispatch(outbound);
          } else {
            stop(reply.protocol, named5.protocol);
          }
        }
        guard(() => {
          options.session(reply);
        });
      }
    };
    transmit([{ message: { ...named5, id: id2, kind: "hello" }, call }], /* @__PURE__ */ new Map());
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
          (blob) => {
            call.settle({ reply: item, body: blob });
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
      const lost = held4;
      held4 = [];
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
      const at = ends2;
      void Promise.allSettled([...sending]).then(() => {
        if (ends2 === at) sendHello();
      });
    }
  };
}
function requestSpan(request, given = {}) {
  const named5 = spanName(request);
  if (named5 === void 0) return void 0;
  return tracer().startSpan(
    named5,
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
      const named5 = { "blockr.request.type": request.type, "blockr.request.name": request.name };
      switch (request.type) {
        case "block_class":
          return { ...named5, "blockr.block.id": request.block, "blockr.block.registry_id": request.registry_id };
        case "option":
          return { ...named5, "blockr.option.id": request.option };
        case "plugin":
          return { ...named5, "blockr.plugin.class_id": request.plugin };
        case "output_class":
          return { ...named5, "blockr.block.id": request.output.block, "blockr.block.revision": request.output.revision };
        case "renderer":
          return {
            ...named5,
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
    const named5 = absent.length === 1 ? "block" : "blocks";
    return new Error(`The board holds no ${named5} ${quoted(absent)} to evaluate.`);
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
function without2(oneTimes, ids3) {
  if (ids3.length === 0) return oneTimes;
  const kept = new Map(oneTimes);
  for (const id2 of ids3) kept.delete(id2);
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
  for (const name2 of declared) {
    const link = links.find((candidate) => candidate.input === name2);
    if (link !== void 0) recipe.push({ input: name2, block: link.from });
    else if (bound.has(name2)) recipe.push({ input: name2 });
    else unlinked.push(name2);
  }
  return { recipe, unlinked };
}
function variadic(declared, links, bound) {
  const recipe = links.map((link) => ({ input: link.input, block: link.from }));
  for (const name2 of bound) {
    if (!links.some((link) => link.input === name2)) recipe.push({ input: name2 });
  }
  const unlinked = recipe.length === 0 ? ["..."] : [];
  for (const name2 of declared) {
    if (name2 === "...") continue;
    if (!links.some((link) => link.input === name2) && !bound.has(name2)) unlinked.push(name2);
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
  let ids3 = 0;
  const count2 = options.nextRevision ?? counter();
  let stopped;
  const ended = () => options.signal?.aborted === true;
  const endedError = () => new Error("Another board opened.");
  let idleSince;
  let planned;
  let passTimer;
  let wakeTimer;
  function revision() {
    try {
      return count2();
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
  const lacking2 = action((missing) => {
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
    const id2 = ++ids3;
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
    if (reply.status === "missing") lacking2(reply.missing);
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
    demanding(++ids3, "blockr.core", (all) => Object.keys(all.blocks), 0);
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
      const id2 = ++ids3;
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
        const id2 = ++ids3;
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
        const id2 = ++ids3;
        asked.value = new Map(asked.peek()).set(id2, { id: id2, at: Date.now() });
        try {
          const result = await inCaller(() => client.request(request, { ...requestOptions, span }));
          if (result.reply.status === "missing") lacking2(result.reply.missing);
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
function setting(value2, fallback, name2, counts = false) {
  if (value2 === void 0) return fallback;
  const number = typeof value2 === "string" ? asNumber(value2) : value2;
  if (number === -1) return Infinity;
  if (typeof number === "number" && Number.isFinite(number) && number >= 0 && (!counts || Number.isInteger(number))) {
    return number;
  }
  throw new RangeError(
    `The blockr option ${name2} is ${shown(value2)}, where it takes ${counts ? "a whole number" : "a number"} of 0 or more, or -1.`
  );
}
function flag(value2, fallback, name2) {
  if (value2 === void 0) return fallback;
  const logical = typeof value2 === "string" ? asLogical(value2) : value2;
  if (typeof logical === "boolean") return logical;
  throw new TypeError(`The blockr option ${name2} is ${shown(value2)}, where it takes TRUE or FALSE.`);
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
  function start3(ask, key2) {
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
    }).then((answer2) => {
      if (key2 !== void 0 && ask.share === "keep" && answer2.reply.status === "ok" && current2(ask.request)) {
        kept.set(key2, { answer: answer2, request: ask.request });
      }
      return answer2;
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
        (answer2) => {
          if (left2) return;
          leave2();
          resolve(answer2);
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
      const answer2 = key2 === void 0 ? void 0 : kept.get(key2)?.answer;
      if (answer2 !== void 0) return Promise.resolve(answer2);
      const flight = (key2 === void 0 ? void 0 : flights.get(key2)) ?? start3(ask, key2);
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
      Object.keys(object2).sort().map((name2) => [name2, sorted(object2[name2] ?? null)])
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
    const name2 = module.replace(/[?#].*$/, "");
    super(`The module ${name2} ${pkg === null ? "from no package" : `of ${pkg}`} ${what}`, options);
    this.name = "ModuleError";
    this.module = name2;
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
async function blockRequest(deps, block, entry, name2, given, opts) {
  const spec = own(entry.requests, name2);
  if (spec === void 0) {
    throw new Error(`The block class "${entry.id}" has no request "${name2}".`);
  }
  if (!isObject(given)) {
    throw new TypeError(`The args of the request "${name2}" are an object of state args and parameters.`);
  }
  if (opts.file !== void 0 && spec.file !== true) {
    throw new TypeError(
      `The request "${name2}" of the block class "${entry.id}" takes no file, since its new_block_request() names no argument for one.`
    );
  }
  const notifier = givenNotifier(opts, `the request "${name2}" of block "${block}"`);
  const share = spec.action === true || opts.file !== void 0 ? "once" : spec.volatile === true ? "merge" : "keep";
  const what = `The request "${name2}" of block "${block}"`;
  const module = sourceModule({ block }, packageFromId(entry.id));
  const inCaller = keepContext();
  for (let attempt3 = 1; ; attempt3++) {
    const reads = await evaluateReads(deps, block, entry, name2, spec.inputs, opts.signal);
    if (!deps.bound()) throw deps.unboundError();
    const board = deps.store.board();
    const state2 = own(board.blocks, block)?.state;
    if (state2 === void 0) throw new Error(`Block "${block}" left the board, so its request "${name2}" did not go to R.`);
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
      name: name2,
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
        deps.replied(module, { type: "block_class", name: name2 }, conditions, notifier);
        if (body !== null) return body;
        return "value" in reply && reply.value !== void 0 ? reply.value : null;
      case "missing":
        if (attempt3 < ATTEMPTS) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, {
          status: "missing"
        });
      case "cancelled":
        throw new RequestError(`R cancelled the request "${name2}" of block "${block}".`, { status: "cancelled" });
      default:
        deps.replied(module, { type: "block_class", name: name2 }, conditions, notifier);
        throw new RequestError(`R replied ${reply.status} to the request "${name2}" of block "${block}".`, {
          status: reply.status,
          conditions
        });
    }
  }
}
async function evaluateReads(deps, block, entry, name2, names, signal5) {
  const what = `The request "${name2}" of block "${block}"`;
  if (names.length === 0) return [];
  const inputs = deps.scheduler.inputs(block);
  if (inputs === void 0) throw new Error(`Block "${block}" left the board, so its request "${name2}" did not go to R.`);
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
function optionReader(values2, named5, unnamed) {
  return (id2) => {
    if (!named5.includes(id2)) throw new Error(unnamed(id2));
    return own(values2, id2);
  };
}
function withoutOptions(module, ids3) {
  return `Without the board option${ids3.length === 1 ? "" : "s"} ${quoted(ids3)}, ${module} cannot draw.`;
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
    request(name2, args = {}, opts = {}) {
      if (typeof name2 !== "string" || name2 === "") {
        return Promise.reject(new TypeError("A request of a board option is named by a non-empty string."));
      }
      if (!isObject(args)) return Promise.reject(new TypeError(`The args of the request "${name2}" are an object.`));
      if (!bound()) return Promise.reject(unboundError());
      return optionRequest(deps, module, option, name2, args, opts);
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
async function optionRequest(deps, module, option, name2, args, opts) {
  const notifier = givenNotifier(opts, `the request "${name2}" of the board option "${option}"`);
  const { reply, body } = await deps.scheduler.request(
    { kind: "request", type: "option", option, name: name2, args: { ...args } },
    {
      ...opts.file === void 0 ? {} : { body: opts.file },
      ...opts.signal === void 0 ? {} : { signal: opts.signal },
      ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
    }
  );
  const conditions = "conditions" in reply ? reply.conditions : [];
  switch (reply.status) {
    case "ok":
      deps.replied(module, { type: "option", name: name2 }, conditions, notifier);
      if (body !== null) return body;
      return "value" in reply && reply.value !== void 0 ? reply.value : null;
    case "cancelled":
      throw new RequestError(`R cancelled the request "${name2}" of the board option "${option}".`, { status: "cancelled" });
    default:
      deps.replied(module, { type: "option", name: name2 }, conditions, notifier);
      throw new RequestError(`R replied ${reply.status} to the request "${name2}" of the board option "${option}".`, {
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
  let held4;
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
  function named5() {
    if ([...expected.keys()].some((id2) => !ready.has(id2))) return void 0;
    return outputOptions;
  }
  function values2(ids3) {
    const options2 = store.board().options;
    return Object.fromEntries(
      ids3.flatMap((id2) => {
        const value2 = own(options2, id2);
        return value2 === void 0 ? [] : [[id2, value2]];
      })
    );
  }
  function onScreen() {
    return info !== null && deps.onScreen.holds(el, block);
  }
  function wanted() {
    if (info === null) return void 0;
    if (waiting2.size === 0 && !onScreen()) return void 0;
    const ids3 = named5();
    if (ids3 === void 0) return void 0;
    if (ids3.some((id2) => !Object.hasOwn(store.board().options, id2))) return void 0;
    const status = scheduler.status(block);
    if (status?.status !== "ready") return void 0;
    return { asked: { revision: status.revision, options: values2(ids3), info, params: shelf.params }, ids: ids3 };
  }
  function fits2(asked, want, ids3) {
    if (asked.revision !== want.revision || !jsonEqual(asked.info, want.info)) return false;
    if (!jsonEqual(asked.params, want.params)) return false;
    return ids3.every((id2) => jsonEqual(own(asked.options, id2), own(want.options, id2)));
  }
  function sync() {
    if (ended()) return;
    const want = wanted();
    if (want !== void 0) {
      if (held4 !== void 0 && fits2(held4.asked, want.asked, want.ids)) {
        stopAsking();
      } else if (shelf.last !== void 0 && shelf.last !== held4 && fits2(shelf.last.asked, want.asked, want.ids)) {
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
    draw();
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
      ).then((answer2) => {
        if (answer2.reply.status === "ok" && "output" in answer2.reply && !deps.replaced.aborted) {
          shelf.last = { asked, output: answer2.reply.output, body: answer2.body };
        }
        heard(answer2.reply);
        return answer2;
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
      (answer2) => {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (asking === ask) asking = void 0;
        received(asked, answer2);
      },
      (failure) => {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (asking === ask) asking = void 0;
        if (!ended()) failed3(coreFailure("output_unanswered", errorMessage(failure)));
      }
    );
    return ask;
  }
  function received(asked, { reply, body }) {
    if (ended()) return;
    switch (reply.status) {
      case "ok":
        take({ asked, output: reply.output, body });
        return;
      case "error":
        failed3(whyFailed(reply, block));
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
  function failed3(why) {
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
    held4 = next;
    error = null;
    const named6 = next.output.renderer;
    expect(named6);
    if (drawing !== void 0 && drawing.prepared.renderer.id !== named6.id) unmountRenderer();
    draw();
    announce2();
    wake();
  }
  function draw() {
    if (held4 === void 0 || drawing !== void 0 || !onScreen()) return;
    const named6 = held4.output.renderer;
    prepare(named6).then(
      (prepared) => {
        if (ended() || drawing !== void 0 || held4?.output.renderer.id !== prepared.renderer.id || !onScreen()) return;
        mountRenderer(prepared);
      },
      (failure) => {
        fail(failure, named6);
      }
    );
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
  function current2(name2, signal5) {
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
              `The request "${name2}" of the output of block "${block}" did not go to R, since the block reads ${status.status}.`,
              { blocks: [{ block, status }] }
            )
          );
          return true;
        }
        if (status.status === "ready" && held4?.asked.revision === status.revision) {
          resolve(held4);
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
      sync();
    });
  }
  async function request(prepared, name2, args, opts) {
    const what = `The request "${name2}" of the output of block "${block}"`;
    const notifier = givenNotifier(opts, `the request "${name2}" of the output of block "${block}"`);
    const module = rendererModule(prepared.renderer);
    const requested2 = { type: opts.scope === "renderer" ? "renderer" : "output_class", name: name2 };
    const inCaller = keepContext();
    for (let attempt3 = 1; ; attempt3++) {
      const target2 = await current2(name2, opts.signal);
      const output = { block, revision: target2.asked.revision };
      const fields = { output, name: name2, args: { ...args }, options: target2.asked.options, params: target2.asked.params };
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
  function checkRequest(name2, args, opts) {
    if (typeof name2 !== "string" || name2 === "") return new TypeError("A request of an output is named by a non-empty string.");
    if (!isObject(args)) return new TypeError(`The args of the request "${name2}" are an object.`);
    if (!isObject(opts)) return new TypeError(`The options of the request "${name2}" are an object.`);
    const scope = opts.scope;
    if (scope !== void 0 && scope !== "class" && scope !== "renderer") {
      return new TypeError(`A request of an output has the scope "class" or "renderer", not ${JSON.stringify(scope)}.`);
    }
    return void 0;
  }
  function outputHost(prepared) {
    const listeners = /* @__PURE__ */ new Set();
    let named6;
    let following = true;
    const notified = /* @__PURE__ */ new Set();
    const module = rendererModule(prepared.renderer);
    return {
      block,
      output: () => held4?.output ?? null,
      body: () => held4?.body ?? null,
      params: () => held4?.asked.params ?? EMPTY,
      option(id2) {
        let memo2 = named6;
        if (memo2 === void 0 || memo2.for !== held4) {
          const read = optionReader(
            optionsFor(held4?.asked),
            outputOptions,
            (unnamed) => `The output family of block class "${registryId2}" names no board option "${unnamed}" in R, so the renderer "${prepared.renderer.id}" cannot read it.`
          );
          memo2 = { for: held4, read };
          named6 = memo2;
        }
        return memo2.read(id2);
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
      request(name2, args = {}, opts = {}) {
        const invalid = checkRequest(name2, args, opts);
        if (invalid !== void 0) return Promise.reject(invalid);
        if (!following || !bound()) return Promise.reject(unboundError());
        return request(prepared, name2, args, opts);
      },
      emit(name2, payload) {
        if (following && bound()) deps.emit({ block, name: name2, payload });
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
  const offOnScreen = deps.onScreen.subscribe(sync);
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
    failed3(coreFailure("board_option_failed", withoutOptions(by, missing)));
  }
  function end() {
    if (ended()) return;
    controller.abort();
    offEnd();
    offOnScreen();
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
      Object.keys(object2).sort().map((name2) => [name2, sorted2(object2[name2] ?? null)])
    );
  }
  return x;
}

// srcts/mount/block.ts
var CORE = "blockr.core";
var NO_STATUS = Object.freeze(unevaluated(0, false));
var EVERY_OUTPUT_ON_SCREEN = { holds: () => true, subscribe: () => () => void 0 };
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
  const onScreen = options.onScreen ?? EVERY_OUTPUT_ON_SCREEN;
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
  async function addOptions(ids3, by) {
    const failures = await Promise.all(
      [...new Set(ids3)].map(async (id2) => {
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
  function keepOptions(ids3, by, failed3 = () => void 0) {
    if (ids3.length === 0) return { added: Promise.resolve([]), stop: () => void 0 };
    let stopped = false;
    const off = store.subscribe((change, board) => {
      if ("opened" in change) return;
      const removed = (change.delta.options?.unset ?? []).filter(
        (id2) => ids3.includes(id2) && !Object.hasOwn(board.options, id2)
      );
      if (removed.length === 0) return;
      void addOptions(removed, by).then((failures) => {
        if (!stopped && failures.length > 0) failed3(failures);
      });
    });
    return {
      added: addOptions(ids3, by),
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
      const named5 = classOptions(entry);
      const kept = keepOptions(named5, by, () => {
        if (ended()) return;
        end();
        lacking2(named5);
      });
      stopKeeping = kept.stop;
      const failures = await kept.added;
      if (ended()) return;
      if (failures.length > 0) {
        kept.stop();
        lacking2(named5);
        return;
      }
      host.name(named5);
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
    function lacking2(ids3) {
      const missing = ids3.filter((id2) => !Object.hasOwn(store.board().options, id2));
      raise(coreNotification("module_failed", "error", withoutOptions(by, missing), { module }));
    }
    return end;
  }
  return {
    mountBlock,
    mountOutput: (el, block) => mountOutput(
      { store, scheduler, loader, answers, shelves, measure, replaced: replaced2, addOptions, keepOptions, emit, raise, replied: replied2, notify, onScreen },
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
  let named5 = [];
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
        named5.flatMap((id2) => {
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
        named5,
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
    request(name2, args = {}, opts = {}) {
      if (!bound()) return Promise.reject(unboundError());
      return blockRequest({ ...deps, bound, unboundError }, block, entry, name2, args, opts);
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
      named5 = options;
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

// srcts/stubs/mount.ts
async function mount(exports, host, el = document.createElement("div")) {
  if (!isObject(exports) || exports.api !== API) {
    throw new Error(
      `The module declares ${isObject(exports) && typeof exports.api === "number" ? `major ${String(exports.api)}` : "no major"} of core's JS API, where core's is ${String(API)}, as \`export const api = ${String(API)}\`.`
    );
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.render !== "function") {
    throw new Error("The module has no default export with a render() function.");
  }
  const render2 = mod.render;
  const controller = new AbortController();
  const cleanup = await render2.call(mod, { el, host, signal: controller.signal });
  return {
    el,
    signal: controller.signal,
    unmount() {
      if (controller.signal.aborted) return;
      controller.abort();
      if (typeof cleanup === "function") cleanup();
    }
  };
}

// srcts/core/blockr-options.ts
function createBlockrOptions(client) {
  const found = /* @__PURE__ */ new Map();
  let batch2;
  function ask(name2) {
    let asked = found.get(name2);
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
        batch2.set(name2, { resolve, reject });
      });
      found.set(name2, asked);
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
      for (const [name2, settle2] of names) {
        found.delete(name2);
        settle2.reject(failure);
      }
      return;
    }
    for (const [name2, settle2] of names) {
      settle2.resolve(Object.hasOwn(values2, name2) ? values2[name2] ?? null : void 0);
    }
  }
  return {
    async read(name2) {
      if (typeof name2 !== "string" || name2 === "") {
        throw new TypeError("A blockr option is read by its name, a non-empty string.");
      }
      return ask(name2);
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
import { computed as computed3, signal as signal4 } from "@preact/signals-core";

// srcts/store/store.ts
import { signal as signal3 } from "@preact/signals-core";

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
  members.splice(members.findIndex(([name2]) => name2 === before), 0, [key2, value2]);
  return Object.fromEntries(members);
}
function spliced(items, index, count2, ...added) {
  const copy = [...items];
  copy.splice(index, count2, ...added);
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
function patch(board, patches2, placed2, failures) {
  let next = board;
  const inverse = [];
  const places = {};
  const plugins = /* @__PURE__ */ new Set();
  for (const [i, given] of patches2.entries()) {
    const name2 = `The patch ${String(i + 1)}`;
    const problems = malformed(given);
    if (problems.length > 0) {
      failures.push(`${name2} ${problems.join(", ")}.`);
      continue;
    }
    const segments = segmentsFromPath(given.path) ?? [];
    const misplaced2 = outside(segments, given.op);
    if (misplaced2 !== void 0) {
      failures.push(`${name2}, ${given.op} at "${given.path}", ${misplaced2}.`);
      continue;
    }
    const place = given.op === "add" ? own(placed2, given.path) : void 0;
    const applied = applyPatch(next, given, segments, place);
    if ("failure" in applied) {
      failures.push(`${name2}, ${given.op} at "${given.path}", ${applied.failure}.`);
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
function collection(name2, current2, part, modify, places, before, failures) {
  if (part === void 0) {
    return { next: current2, inverse: {}, places: {}, changed: [], touched: [] };
  }
  const order = Object.keys(current2);
  const removed = /* @__PURE__ */ new Set();
  for (const id2 of part.rm ?? []) {
    if (Object.hasOwn(current2, id2)) removed.add(id2);
    else failures.push(`The board holds no ${name2(id2)} to remove.`);
  }
  const readd = {};
  const readdPlaces = {};
  for (const id2 of removed) {
    readd[id2] = current2[id2];
    const after = order.slice(order.indexOf(id2) + 1);
    readdPlaces[id2] = after.find((other) => !removed.has(other)) ?? null;
  }
  const kept = order.filter((id2) => !removed.has(id2)).map((id2) => [id2, current2[id2]]);
  const keptSuccessor = (named5) => named5 === null ? null : removed.has(named5) ? own(readdPlaces, named5) ?? null : named5;
  const moves = [];
  for (const [id2, named5] of Object.entries(before ?? {})) {
    if (Object.hasOwn(part.add ?? {}, id2)) continue;
    const at = kept.findIndex(([other]) => other === id2);
    const entry = kept[at];
    if (entry === void 0) {
      failures.push(`The board holds no ${name2(id2)} to move.`);
      continue;
    }
    const to = keptSuccessor(named5);
    const was = kept[at + 1]?.[0] ?? null;
    if (to === was || to === id2) continue;
    if (to !== null && !kept.some(([other]) => other === to)) {
      failures.push(`The board holds no ${name2(to)} for ${name2(id2)} to go before.`);
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
      failures.push(`The board holds ${name2(id2)} already.`);
      continue;
    }
    const named5 = own(before ?? {}, id2);
    const successor = named5 === void 0 ? own(places ?? {}, id2) ?? null : keptSuccessor(named5);
    const at = successor === null ? -1 : kept.findIndex(([other]) => other === successor);
    if (at < 0 && successor !== null && named5 !== void 0) {
      failures.push(
        `The board holds no ${name2(successor)} for ${name2(id2)} to go before.`
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
      failures.push(`The board holds no ${name2(id2)} to change.`);
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
    ([name2, value2]) => !jsonEqual(own(current2, name2), value2)
  );
  const restore = {};
  const remove2 = [];
  const removing2 = [];
  for (const name2 of unset) {
    if (Object.hasOwn(set, name2)) failures.push(both(name2));
    const was = own(current2, name2);
    if (was === void 0) continue;
    removing2.push(name2);
    restore[name2] = was;
  }
  for (const [name2] of setting2) {
    const was = own(current2, name2);
    if (was === void 0) remove2.push(name2);
    else restore[name2] = was;
  }
  return {
    next: setting2.length === 0 && removing2.length === 0 ? current2 : { ...without(current2, removing2), ...Object.fromEntries(setting2) },
    set: setting2.map(([name2]) => name2),
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
    (name2) => `The change both sets and unsets ${what(name2)}.`,
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
  const name2 = mod.name === old.name ? void 0 : mod.name;
  const locked2 = mod.locked === void 0 || mod.locked === old.locked ? void 0 : mod.locked;
  return {
    value: {
      ...old,
      state: state2.next,
      name: name2 ?? old.name,
      locked: locked2 === void 0 ? old.locked : locked2
    },
    inverse: {
      ...Object.keys(state2.restore).length === 0 ? {} : { state: state2.restore },
      ...state2.remove.length === 0 ? {} : { unset: state2.remove },
      ...name2 === void 0 ? {} : { name: old.name },
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
      const merged2 = merge(expanded, delta, patches2, board);
      if (merged2 !== expanded) adding.push(id2);
      expanded = merged2;
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
  const placed2 = {};
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (stacksRm.includes(id2)) continue;
    const mod = own(stacksMod, id2);
    const blocks = mod?.blocks ?? stack.blocks;
    if (blocks.some((block) => gone.has(block))) {
      placed2[id2] = { ...mod, blocks: blocks.filter((block) => !gone.has(block)) };
    }
  }
  if (incident.length === 0 && Object.keys(placed2).length === 0) return delta;
  return {
    ...delta,
    ...incident.length === 0 ? {} : { links: { ...delta.links, rm: [...linksRm, ...incident] } },
    ...Object.keys(placed2).length === 0 ? {} : { stacks: { ...delta.stacks, mod: { ...stacksMod, ...placed2 } } }
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
  const held4 = delta.patches ?? [];
  const fixed2 = (given.patches ?? []).map(({ path }) => segmentsFromPath(path) ?? []);
  const added = [];
  for (const patch2 of patches2) {
    const segments = segmentsFromPath(patch2.path) ?? [];
    if ([...held4, ...added].some((other) => same(other, patch2))) continue;
    if (fixed2.some((other) => overlap(other, segments, board))) continue;
    added.push(patch2);
  }
  if (added.length === 0) return delta;
  return { ...delta, patches: [...held4, ...added] };
}
function same(a, b) {
  return a.op === b.op && a.path === b.path && jsonEqual(a.value, b.value);
}

// srcts/store/lock.ts
function fixes(board, block, boardLock) {
  if (!boardLock) return false;
  const held4 = board === void 0 ? void 0 : own(board.blocks, block);
  return held4?.locked !== false;
}
function lockFailures(before, inverse) {
  const failures = [];
  const refuse = (verb, one2, several, ids3) => {
    if (ids3.length === 0) return;
    failures.push(`The board lock refuses to ${verb} ${ids3.length === 1 ? one2 : several} ${quoted(ids3)}.`);
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
  const holds2 = (id2) => kept.has(id2);
  const links = collection2(
    board.links,
    delta.links,
    (link) => (link.from === void 0 || holds2(link.from)) && (link.to === void 0 || holds2(link.to))
  );
  const held4 = (id2) => Object.hasOwn(board.links, id2) && !(links.rm ?? []).includes(id2);
  const before = Object.fromEntries(
    Object.entries(delta.links?.before ?? {}).flatMap(([id2, named5]) => {
      if (Object.hasOwn(links.add ?? {}, id2)) return [[id2, named5]];
      if (!held4(id2)) return [];
      return [[id2, named5 !== null && Object.hasOwn(board.links, named5) ? named5 : null]];
    })
  );
  const stacks = collection2(board.stacks, delta.stacks);
  const placed2 = (stack) => stack.blocks === void 0 ? stack : { ...stack, blocks: stack.blocks.filter(holds2) };
  return prune({
    blocks,
    links: { ...links, ...Object.keys(before).length === 0 ? {} : { before } },
    stacks: {
      ...stacks.rm === void 0 ? {} : { rm: stacks.rm },
      ...stacks.add === void 0 ? {} : { add: Object.fromEntries(Object.entries(stacks.add).map(([id2, s]) => [id2, placed2(s)])) },
      ...stacks.mod === void 0 ? {} : { mod: Object.fromEntries(Object.entries(stacks.mod).map(([id2, s]) => [id2, placed2(s)])) }
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
    const held4 = own(kept, id2);
    plugins[id2] = held4 === void 0 ? frozenCopy(state2.default) : held4;
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
  const lacking2 = COLLECTIONS.filter((name2) => !isObject(board[name2]));
  if (lacking2.length > 0) {
    throw new StoreError(refused, [`The board holds its ${listing(lacking2)} in no object by id.`]);
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
    ...Object.fromEntries(Object.entries(record).filter(([name2]) => PROPERTIES.includes(name2))),
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
  const held4 = keptPlugins(read.plugins, reply.plugins, registeredClass);
  const plugins = droppedPlugins(read.plugins, reply.plugins, registeredClass).map((id2) => ({
    plugin: id2,
    package: packageFromId(id2)
  }));
  const migrated2 = migrate({ ...read, plugins: held4 }, json.board, reply, registrations);
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
import { computed as computed2, signal as signal2 } from "@preact/signals-core";
function createRegistrations(migration = () => void 0) {
  const classes2 = signal2(/* @__PURE__ */ new Map());
  const options = signal2([]);
  const optionIndex = computed2(
    () => new Map(options.value.map((option) => [option.def.id, option]))
  );
  const classDefs = computed2(
    () => Object.fromEntries([...classes2.value].map(([id2, cls]) => [id2, cls.def]))
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
    if (classes2.value.has(id2)) {
      throw new Error(`The plugin class "${id2}" is registered already.`);
    }
    const given = def;
    for (const [member, article] of [["update", "an"], ["migrate", "a"], ["start", "a"]]) {
      if (given[member] !== void 0 && typeof given[member] !== "function") {
        throw new Error(`The plugin class "${id2}" has ${article} ${member} that is no function.`);
      }
    }
    for (const member of ["ui", "frontend"]) {
      const drawn3 = given[member];
      if (drawn3 !== void 0 && (!isObject(drawn3) || typeof drawn3.render !== "function")) {
        throw new Error(`The plugin class "${id2}" has a ${member} without a render() function.`);
      }
    }
    const frontend = given.frontend;
    if (isObject(frontend) && frontend.advanced !== void 0 && typeof frontend.advanced !== "function") {
      throw new Error(`The plugin class "${id2}" has a frontend whose advanced is no function.`);
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
    classes2.value = new Map([...classes2.value, [id2, item]]);
    return () => {
      if (classes2.value.get(id2) !== item) return;
      const rest = new Map(classes2.value);
      rest.delete(id2);
      classes2.value = rest;
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
    plugin: (id2) => classes2.value.get(id2),
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
  function run(start3, deltas, how, refused2) {
    const applied = [];
    const inverses = [];
    let current3 = start3;
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
  function named5(deltas) {
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
    const start3 = current2(refused2);
    const failures = named5(deltas);
    if (failures.length > 0) throw new StoreError(refused2, failures);
    return { start: start3, ...run(start3, deltas, {}, refused2) };
  }
  function apply(delta, origin = "user", opts = {}) {
    guard2();
    const given = isDeltas(delta) ? delta : [delta];
    if (given.length === 0) return false;
    const deltas = opts.undo === false ? given.map((each) => ({ ...each, undo: false })) : given;
    busy = true;
    try {
      const { start: start3, applied, inverses } = runOnBoard(deltas);
      if (applied.length === 0) return false;
      number(applied, applied.at(-1)?.board ?? start3);
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
      const { start: start3, applied } = runOnBoard(deltas);
      return applied.at(-1)?.board ?? start3;
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
      const start3 = current2(refused2);
      const { applied, inverses } = run(
        start3,
        step2.inverses.map(({ delta }) => delta),
        { fit: true, places: step2.inverses.map(({ places }) => places) },
        refused2
      );
      number(applied, applied.at(-1)?.board ?? start3);
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
    const classes2 = Object.keys(next.plugins);
    const owned = /* @__PURE__ */ new Set([
      ...classes2.flatMap((cls) => packageFromId(cls) ?? []),
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
    const { id: id2, name: name2, links, stacks } = next;
    return {
      format: FORMAT,
      packages: recorded,
      board: { id: id2, name: name2, blocks, links, stacks, options: options2, plugins }
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
var name = {
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
    ...name,
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
      args: object({ id: blockId, name }, ["id", "name"]),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run: (_board, { id: id2, name: name2 }) => ({ blocks: { mod: { [id2]: { name: name2 } } } })
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
        name: { ...name, description: `The new stack's name, "Stack" where it is left out.` },
        blocks: { ...blockIds, description: "The blocks of the new stack, in order." },
        color: {
          ...color,
          description: "The new stack's colour, which frontends draw it in, or null for none. The stack gets null where this is left out, and a frontend draws a stack without a colour in a default of its own."
        }
      }),
      run(board2, { id: id2, name: name2 = "Stack", blocks: blocks2 = [], color: color2 = null }, { mintId: mintId2 }) {
        const stack = id2 ?? mintId2((id3) => Object.hasOwn(board2.stacks, id3));
        const left2 = leave(board2, stack, blocks2);
        return {
          stacks: {
            add: { [stack]: { name: name2, blocks: blocks2, color: color2 } },
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
      args: object({ id: stackId, name }, ["id", "name"]),
      preflight: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (_board, { id: id2, name: name2 }) => ({ stacks: { mod: { [id2]: { name: name2 } } } })
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
      args: object({ name }, ["name"]),
      run: (_board, { name: name2 }) => ({ name: name2 })
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
function takesLink(board, registry, to) {
  return (freeInputs(board, registry, to)?.length ?? 0) > 0;
}
function linkFeeding(board, registry, to, input2, except) {
  const block = own(board.blocks, to);
  const inputs = block === void 0 ? void 0 : own(registry, block.registry_id)?.inputs;
  if (inputs === void 0 || inputs.includes("...")) return void 0;
  return Object.entries(board.links).find(([id2, link]) => id2 !== except && link.to === to && link.input === input2)?.[0];
}
function defaultBlockName(registryId2) {
  const pkg = packageFromId(registryId2);
  const name2 = pkg === null ? registryId2 : registryId2.slice(pkg.length + 1);
  return sentenceCase(name2.replace(/_block$/, ""));
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
    const checked4 = list.map(({ id: id2, args }, index) => {
      try {
        return lookup(id2, args);
      } catch (error) {
        throw inList(index, id2, error);
      }
    });
    const [first] = checked4;
    if (first === void 0) return then([]);
    const inputs = /* @__PURE__ */ new Map();
    let built = [];
    let again = true;
    while (again) {
      again = false;
      built = [];
      let start3;
      try {
        start3 = current2(first.item.command.id);
      } catch (error) {
        throw inList(0, first.item.command.id, error);
      }
      let board = start3;
      for (const [index, { item, args }] of checked4.entries()) {
        const { command: command2 } = item;
        try {
          allow(item, board, args);
          if (command2.prepare !== void 0 && !inputs.has(index)) {
            inputs.set(index, await within(() => prepare(item, board, args)));
            if (ended()) throw refusal(command2.id, [DROPPED]);
            if (store.board() !== start3) {
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
    const ids3 = list.map(({ id: id2 }) => id2);
    return traced(
      "command list",
      { "blockr.command.ids": ids3, "blockr.delta.origin": origin },
      () => planned(list, (built) => store.apply(built, origin))
    );
  }
  async function traced(name2, attributes, run) {
    const span = tracer().startSpan(name2, { attributes });
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
  const named5 = error instanceof CommandError || error instanceof StoreError;
  const refused2 = new CommandError(
    `The list of commands was refused at command ${String(index + 1)}, "${id2}"`,
    named5 ? error.failures : [errorMessage(error)],
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

// srcts/actions/given.ts
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
function isElement(value2) {
  return isObject(value2) && typeof value2.focus === "function" && typeof value2.getBoundingClientRect === "function";
}

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
var NO_LEVEL = "It gave no level for the target, and the board action has no advanced action to open in its place.";
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
function openMenu(start3, parts) {
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
      if (action2.advanced !== void 0) return advanced(action2, {});
      place(failure(action2, what, new TypeError(NO_LEVEL)), action2, replacing);
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
    if (current2 === void 0) return { level: { id: 0, label: MENU, advanced: false, back: false, loading: false, rows: [] }, rows: [] };
    const { id: id2, state: state2 } = current2;
    const label = current2.action?.label ?? MENU;
    if (state2.kind === "loading") return { level: { id: id2, label, advanced: false, back, loading: true, rows: [] }, rows: [] };
    if (state2.kind === "error") {
      return { level: { id: id2, label, advanced: false, back, loading: false, error: state2.message, rows: [] }, rows: [] };
    }
    const { title, filter } = state2.level;
    const rows = filtered(state2.level, filter === true ? folded(current2.filter.trim()) : "");
    return {
      level: {
        id: id2,
        label: title ?? label,
        ...title === void 0 ? {} : { title },
        ...filter === true ? { filter: current2.filter } : {},
        advanced: current2.action?.advanced !== void 0,
        back,
        loading: false,
        rows: rows.map((showing) => shownRow(current2, showing))
      },
      rows
    };
  }
  function shownRow(current2, { row, index, choices }) {
    if (row.kind === "item") {
      const { label, heading, icon, detail, category, checked: checked4 } = row;
      return {
        kind: "item",
        label,
        ...heading === void 0 ? {} : { heading },
        ...icon === void 0 ? {} : { icon },
        ...detail === void 0 ? {} : { detail },
        ...category === void 0 ? {} : { category },
        ...checked4 === void 0 ? {} : { checked: checked4 },
        ...row.advanced !== void 0 && current2.action?.advanced !== void 0 ? { advanced: true } : {}
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
    return advanced(action2, {});
  }
  function advanced(action2, opened) {
    let resolve = () => void 0;
    let reject = () => void 0;
    const ran = closing(
      new Promise((resolved, rejected) => {
        resolve = resolved;
        reject = rejected;
      })
    );
    parts.advanced(action2, opened).then(resolve, reject);
    return ran;
  }
  function openAdvanced(index) {
    const shown2 = at(index);
    if (shown2 === void 0) return Promise.resolve();
    const { current: current2, found } = shown2;
    const { action: action2, state: state2 } = current2;
    if (action2?.advanced === void 0 || state2.kind !== "level") return Promise.resolve();
    let picked = state2.level.picked;
    if (index !== void 0) {
      const row = found?.row;
      if (row?.kind !== "item" || row.advanced === void 0) return Promise.resolve();
      picked = row.advanced;
    }
    if (!parts.applies(action2)) {
      end();
      return Promise.resolve();
    }
    return advanced(action2, {
      ...picked === void 0 ? {} : { picked },
      ...state2.level.filter === true ? { filter: current2.filter } : {}
    });
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
  if ("actions" in start3) {
    const board = parts.board();
    const actions = inMenuOrder(start3.actions);
    stack.push(
      entry(void 0, { kind: "level", level: { rows: actions.map((action2) => actionRow(action2, board, parts.target)) } }, actions)
    );
  } else {
    void enter(start3.action);
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
    openAdvanced,
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
function levelItem(label, level) {
  return { kind: "item", label, pick: () => level };
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
      return typeof row.pick === "function" && [row.heading, row.icon, row.detail, row.category].every((member) => optional(member, "string")) && optional(row.checked, "boolean");
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

// srcts/actions/advanced.ts
var DIALOG = /* @__PURE__ */ Symbol("core's dialog");
var FRONTEND = /* @__PURE__ */ Symbol("the frontend's place");
function createAdvanced(parts) {
  const { store } = parts;
  const places = /* @__PURE__ */ new Map();
  if (parts.signal !== void 0) {
    onAbort(parts.signal, () => {
      for (const shown2 of [...places.values()]) close(shown2);
    });
  }
  function render2(el, action2, target2, given, finish, left2) {
    const module = action2.advanced;
    const controller = new AbortController();
    const { signal: signal5 } = controller;
    let cleanup;
    let pending = false;
    const gone = () => !signal5.aborted && !pending && !held4(target2);
    const offStore = store.subscribe(() => {
      if (gone()) left2();
    });
    const end2 = () => {
      if (signal5.aborted) return;
      controller.abort();
      offStore();
      offBoard();
      if (cleanup !== void 0) attempt(cleanup);
      el.replaceChildren();
    };
    const offBoard = parts.signal === void 0 ? () => void 0 : onAbort(parts.signal, end2);
    const done = (calls, opts) => {
      if (signal5.aborted) {
        return Promise.reject(new Error(`The advanced action of the board action "${action2.id}" has ended, so its done() runs nothing.`));
      }
      if (pending) {
        return Promise.reject(new Error(`The advanced action of the board action "${action2.id}" runs a done() already.`));
      }
      let notifier;
      try {
        notifier = givenNotifier(opts, "done()");
      } catch (error) {
        return Promise.reject(asError(error));
      }
      const batch2 = calls;
      if (batch2 !== void 0 && !isBatch(batch2)) {
        return Promise.reject(
          new TypeError(
            `The done() of the advanced action of the board action "${action2.id}" takes a batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }], or none.`
          )
        );
      }
      pending = true;
      return finish(batch2, notifier).finally(() => {
        pending = false;
        if (gone()) left2();
      });
    };
    const took = (rendered) => {
      if (typeof rendered !== "function") return;
      if (signal5.aborted) attempt(rendered);
      else cleanup = rendered;
    };
    if (module !== void 0) {
      let rendered;
      try {
        rendered = module.render({ el, core: parts.core(), target: target2, ...given, signal: signal5, done });
      } catch (error) {
        failed3(action2, error);
        return end2;
      }
      if (isThenable2(rendered)) {
        Promise.resolve(rendered).then(took, (error) => {
          if (!signal5.aborted) failed3(action2, error);
        });
      } else {
        took(rendered);
      }
    }
    return end2;
  }
  function failed3(action2, error) {
    const why = new Error(`The advanced action of the board action "${action2.id}" failed in render(): ${errorMessage(error)}`, { cause: error });
    log.error(why);
    const plugin = parts.classOf(action2.id);
    parts.raise(
      coreNotification("module_failed", "error", why.message, plugin === void 0 ? {} : { module: sourceModule({ plugin }, packageFromId(plugin)) })
    );
  }
  function renders(action2, target2) {
    return parts.signal?.aborted !== true && held4(target2) && parts.applies(action2, target2);
  }
  function held4(target2) {
    if (!store.hasBoard()) return false;
    if ("board" in target2) return true;
    const board = store.board();
    const plugins = parts.core().plugins.appPlugins();
    const objects = "selection" in target2 ? target2.selection : [target2];
    return objects.every((object2) => holdsObject(board, object2, plugins));
  }
  function mountIn(shown2, el, other) {
    if (!isElement(el)) {
      throw new TypeError("The `el` of mount() is the element in which core renders the advanced action.");
    }
    const target2 = other === void 0 ? shown2.target : targetGiven(other);
    if (shown2.closed || !renders(shown2.action, target2)) return () => void 0;
    const own2 = sameTarget(target2, shown2.target);
    const { picked, filter } = shown2.opening;
    const given = shown2.first && own2 ? { ...picked === void 0 ? {} : { picked }, ...filter === void 0 ? {} : { filter } } : {};
    const unmount = render2(
      el,
      shown2.action,
      target2,
      given,
      (calls, notifier) => finishShown(shown2, own2, calls, notifier),
      () => {
        end(shown2);
      }
    );
    shown2.mounts.add(unmount);
    return () => {
      shown2.mounts.delete(unmount);
      unmount();
    };
  }
  async function finishShown(shown2, own2, calls, notifier) {
    if (calls === void 0 || calls.length === 0) {
      end(shown2);
      return;
    }
    const { opening: opening2 } = shown2;
    const triggered = shown2.first && own2;
    const deltas = await parts.run(calls, {
      ...opening2.call === void 0 ? {} : { call: opening2.call },
      extend: triggered ? opening2.extend : void 0,
      notifier: notifier ?? (triggered ? opening2.notifier : void 0)
    });
    end(shown2, deltas);
  }
  function end(shown2, deltas) {
    if (shown2.closed) return;
    unmountAll(shown2);
    if (shown2.first) {
      shown2.first = false;
      shown2.settle();
    }
    const { ended } = shown2;
    if (ended === void 0) {
      shown2.early = { deltas };
      return;
    }
    attempt(() => {
      if (deltas === void 0) ended();
      else ended(deltas);
    });
  }
  function close(shown2) {
    if (shown2.closed) return;
    shown2.closed = true;
    unmountAll(shown2);
    if (shown2.first) {
      shown2.first = false;
      shown2.settle();
    }
    if (places.get(shown2.key) === shown2) places.delete(shown2.key);
  }
  function unmountAll(shown2) {
    const mounts = [...shown2.mounts];
    shown2.mounts.clear();
    for (const unmount of mounts) unmount();
  }
  function showIn(shown2, args) {
    const { place } = shown2;
    const at = { el: shown2.opening.el, ...parts.signal === void 0 ? {} : { signal: parts.signal } };
    if (place === DIALOG) return parts.dialog(args, at);
    let returned;
    try {
      returned = place(args);
    } catch (error) {
      log.error(
        new Error(`The place that was to show the advanced action of the board action "${args.id}" failed, so core's dialog shows it: ${errorMessage(error)}`, {
          cause: error
        })
      );
      if (places.get(shown2.key) === shown2) places.delete(shown2.key);
      shown2.place = DIALOG;
      shown2.key = DIALOG;
      replace(shown2);
      return parts.dialog(args, at);
    }
    if (typeof returned === "function") return returned;
    log.error(`The place that shows the advanced action of the board action "${args.id}" returned no function, which core calls once done() ends it.`);
    return () => void 0;
  }
  function replace(shown2) {
    const before = places.get(shown2.key);
    if (before !== void 0) {
      const { ended } = before;
      if (before.place === DIALOG && ended !== void 0) {
        attempt(() => {
          ended();
        });
      }
      close(before);
    }
    places.set(shown2.key, shown2);
  }
  return {
    open(action2, target2, opening2) {
      if (parts.signal?.aborted === true || action2.advanced === void 0) return Promise.resolve();
      return new Promise((resolve) => {
        const caller = opening2.place;
        const frontend = caller === void 0 ? parts.frontend() : void 0;
        const place = caller?.show ?? frontend ?? DIALOG;
        const key2 = caller !== void 0 ? caller.key : frontend !== void 0 ? FRONTEND : DIALOG;
        const shown2 = {
          action: action2,
          target: target2,
          opening: opening2,
          place,
          key: key2,
          mounts: /* @__PURE__ */ new Set(),
          first: true,
          closed: false,
          ended: void 0,
          early: void 0,
          settle: resolve
        };
        replace(shown2);
        const args = Object.freeze({
          id: action2.id,
          title: action2.label,
          target: target2,
          mount: (el, other) => mountIn(shown2, el, other),
          close: () => {
            close(shown2);
          }
        });
        shown2.ended = showIn(shown2, args);
        const { early } = shown2;
        if (early !== void 0 && !shown2.closed) {
          shown2.early = void 0;
          const { ended } = shown2;
          attempt(() => {
            if (early.deltas === void 0) ended();
            else ended(early.deltas);
          });
        }
      });
    },
    mount(el, action2, target2, picked) {
      if (!renders(action2, target2)) return () => void 0;
      const given = picked === void 0 ? {} : { picked };
      let stopped = false;
      let current2;
      const stop = () => {
        stopped = true;
        current2?.();
        current2 = void 0;
      };
      const again = () => {
        current2?.();
        current2 = void 0;
        if (stopped) return;
        if (!held4(target2)) {
          stop();
          return;
        }
        current2 = render2(el, action2, target2, given, finish, stop);
      };
      const finish = async (calls, notifier) => {
        if (calls !== void 0 && calls.length > 0) {
          await parts.run(calls, notifier === void 0 ? {} : { notifier });
        }
        again();
      };
      again();
      return stop;
    }
  };
}
function sameTarget(a, b) {
  return JSON.stringify(a) === JSON.stringify(b);
}
function isThenable2(value2) {
  return (typeof value2 === "object" || typeof value2 === "function") && value2 !== null && typeof value2.then === "function";
}

// srcts/react/hooks.ts
import { useCallback, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";

// srcts/binding/binding.ts
var PAUSE = 500;
function bind(host, key2, next, last, given) {
  const [change, show2, options] = typeof key2 === "function" ? [next, last, given ?? {}] : [void 0, next, last ?? {}];
  const source = createSource(host);
  const held4 = source.value(key2, change);
  const submitting = options.notifier === void 0 ? {} : { notifier: options.notifier };
  let shown2;
  let typed;
  let timer;
  let released = false;
  const display = (again = false) => {
    if (released || timer !== void 0) return;
    const value2 = held4.get();
    if (!again && shown2 !== void 0 && jsonEqual(shown2.value, value2)) return;
    shown2 = { value: value2 };
    show2(value2);
  };
  let unbuilt;
  const unbuild = (error) => {
    host.log.error(asError(error));
    unbuilt?.();
    unbuilt = options.notifier === void 0 ? host.notify({
      severity: "error",
      code: "change_failed",
      message: `The control failed to build the change it submits: ${errorMessage(error)}`
    }) : void 0;
  };
  const submit = async (value2) => {
    let submitted;
    try {
      submitted = held4.submit(value2, submitting);
    } catch (error) {
      unbuild(error);
      options.settled?.(error);
      return false;
    }
    unbuilt?.();
    unbuilt = void 0;
    try {
      if (submitted !== null) await submitted;
    } catch (error) {
      options.settled?.(error);
      return false;
    }
    options.settled?.(null);
    return true;
  };
  const send = () => {
    const value2 = typed;
    typed = void 0;
    if (value2 !== void 0) void submit(value2);
  };
  const flush = () => {
    if (timer === void 0) return;
    clearTimeout(timer);
    timer = void 0;
    send();
  };
  const off = source.onValues(() => {
    display();
  });
  display(true);
  return {
    get: () => held4.get(),
    async commit(value2) {
      const took = await submit(value2);
      if (!took) display(true);
      return took;
    },
    async unset() {
      const took = await submit(void 0);
      if (!took) display(true);
      return took;
    },
    type(value2) {
      if (released) return;
      typed = value2;
      clearTimeout(timer);
      timer = setTimeout(() => {
        timer = void 0;
        send();
      }, PAUSE);
    },
    flush,
    release() {
      if (released) return;
      released = true;
      off();
      flush();
    }
  };
}
function createSource(host) {
  if ("option" in host) {
    return {
      value(key2) {
        if (typeof key2 === "function") {
          throw new TypeError(`The value of the board option "${host.option}" is bound by its id, not through functions.`);
        }
        if (key2 !== host.option) {
          throw new Error(`The host of the board option "${host.option}" holds no value "${key2}".`);
        }
        return {
          get: () => host.value(),
          submit(value2, opts) {
            if (jsonEqual(host.value(), value2)) return null;
            if (value2 === void 0) {
              return Promise.reject(new Error(`The board option "${host.option}" always holds a value, so none of it is unset.`));
            }
            return host.submit(value2, opts);
          }
        };
      },
      onValues: (fn) => host.on("value", fn),
      onContext: () => () => void 0,
      inputs: () => null,
      request: (name2, args, opts) => host.request(name2, args, opts)
    };
  }
  return {
    value(key2, change) {
      const read = typeof key2 === "function" ? key2 : (state2) => own(state2, key2);
      const build = typeof key2 === "function" ? change : (value2) => ({ [key2]: value2 });
      if (typeof build !== "function") {
        throw new TypeError("A binding made with a function that reads its value takes a second, which builds the change it submits.");
      }
      return {
        get: () => read(host.state()),
        submit(value2, opts) {
          const state2 = host.state();
          const next = build(value2, state2);
          return holds(state2, next) ? null : host.submit(next, opts);
        }
      };
    },
    onValues: (fn) => host.on("state", fn),
    onContext: (fn) => host.on("context", fn),
    inputs: () => host.context().inputs.map((input2) => ({ input: input2.input, revision: input2.revision })),
    request: (name2, args, opts) => host.request(name2, args, opts)
  };
}
function holds(state2, change) {
  return Object.entries(change).every(([arg, value2]) => jsonEqual(own(state2, arg), value2));
}
function strings(value2) {
  return Array.isArray(value2) ? value2.filter((x) => typeof x === "string") : [];
}

// srcts/react/hooks.ts
function useValue(host, key2, change) {
  const read = typeof key2 === "function" ? key2 : null;
  const name2 = typeof key2 === "function" ? null : key2 ?? ("option" in host ? host.option : "");
  const given = read === null || change === void 0 ? null : { read, change };
  const [value2, setValue] = useState(() => held2(host, name2, { current: given }));
  const [error, setError] = useState(null);
  const binding = useRef(null);
  const functions = useRef(given);
  useLayoutEffect(() => {
    binding.current = start(host, name2, functions, setValue, setError);
    return () => {
      const bound = binding.current;
      binding.current = null;
      bound?.release();
    };
  }, [host, name2]);
  useLayoutEffect(() => {
    if (read === null || change === void 0) return;
    const last = functions.current;
    if (last !== null && last.read === read && last.change === change) return;
    const moved = last === null || !readAlike(last.read, read, host.state());
    if (moved) binding.current?.release();
    functions.current = { read, change };
    if (moved) binding.current = start(host, name2, functions, setValue, setError);
  }, [read, change, host, name2]);
  return useMemo(
    () => ({
      value: value2,
      error,
      get: () => binding.current?.get() ?? held2(host, name2, functions),
      commit: (next) => binding.current?.commit(next) ?? Promise.resolve(false),
      unset: () => binding.current?.unset() ?? Promise.resolve(false),
      type: (next) => {
        binding.current?.type(next);
      },
      flush: () => {
        binding.current?.flush();
      }
    }),
    [value2, error, host, name2]
  );
}
function start(host, name2, functions, show2, refused2) {
  const options = {
    settled: (error) => {
      refused2(error === null ? null : errorMessage(error));
    },
    notifier: () => void 0
  };
  if (name2 !== null) return bind(host, name2, show2, options);
  return bind(
    host,
    (state2) => functions.current?.read(state2),
    (value2, state2) => functions.current?.change(value2, state2) ?? {},
    show2,
    options
  );
}
function held2(host, name2, functions) {
  if ("option" in host) return host.value();
  return name2 === null ? functions.current?.read(host.state()) : own(host.state(), name2);
}
function readAlike(a, b, state2) {
  try {
    return jsonEqual(a(state2), b(state2));
  } catch {
    return false;
  }
}
function useBoard(core) {
  const subscribe = useCallback(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
  return useSyncExternalStore(subscribe, () => core.store.board());
}
function useStatus(core, block) {
  const subscribe = useCallback(
    (notify) => core.scheduler.subscribe((changed) => {
      if (changed.includes(block)) notify();
    }),
    [core, block]
  );
  return useSyncExternalStore(subscribe, () => core.scheduler.status(block));
}
function useNotified(core, about) {
  const subscribe = useCallback(
    (notify) => core.notifications.subscribe(() => {
      notify();
    }),
    [core]
  );
  const all = useSyncExternalStore(subscribe, () => core.notifications.get());
  const [kind, id2] = about === null ? [] : Object.entries(about)[0] ?? [];
  return useMemo(
    () => kind === void 0 ? all : all.filter((notification) => notification.about !== void 0 && own(notification.about, kind) === id2),
    [all, kind, id2]
  );
}

// srcts/react/module.tsx
import { Component } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { jsx } from "react/jsx-runtime";
function reactModule(Draw) {
  return {
    render({ el, host, signal: signal5 }) {
      const root = createRoot(el);
      flushSync(() => {
        root.render(/* @__PURE__ */ jsx(Draw, { host, signal: signal5 }));
      });
      return () => {
        root.unmount();
      };
    }
  };
}

// srcts/aria/index.ts
import {
  Autocomplete,
  Breadcrumb,
  Breadcrumbs,
  Button,
  Calendar,
  CalendarCell,
  CalendarGrid,
  CalendarGridBody,
  CalendarGridHeader,
  CalendarHeaderCell,
  CalendarHeading,
  Cell,
  CheckboxButton,
  CheckboxField,
  CheckboxGroup,
  Collection,
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatch,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ColorThumb,
  ColorWheel,
  ColorWheelTrack,
  Column,
  ComboBox,
  ComboBoxStateContext,
  composeRenderProps,
  DateField,
  DateInput,
  DatePicker,
  DateRangePicker,
  DateSegment,
  Dialog,
  DialogTrigger,
  Disclosure,
  DisclosureGroup,
  DisclosurePanel,
  DisclosureStateContext,
  DropIndicator,
  DropZone,
  FieldError,
  FileTrigger,
  Focusable,
  Form,
  GridList,
  GridListHeader,
  GridListItem,
  GridListSection,
  Group,
  Header,
  Heading,
  Input,
  Keyboard,
  Label,
  Link,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  ListLayout,
  Menu,
  MenuItem,
  MenuSection,
  MenuTrigger,
  Meter,
  Modal,
  ModalOverlay,
  NavigationTree,
  NavigationTreeHeader,
  NavigationTreeItem,
  NavigationTreeItemContent,
  NavigationTreeSection,
  NumberField,
  OverlayArrow,
  parseColor,
  Popover,
  ProgressBar,
  RadioButton,
  RadioField,
  RadioGroup,
  RangeCalendar,
  Row,
  SearchField,
  Select,
  SelectionIndicator,
  SelectStateContext,
  SelectValue,
  Separator,
  Sheet,
  SheetBackdrop,
  SheetContent,
  SheetOverlay,
  Slider,
  SliderFill,
  SliderOutput,
  SliderStateContext,
  SliderThumb,
  SliderTrack,
  SubmenuTrigger,
  SwitchButton,
  SwitchField,
  Tab,
  Table,
  TableBody,
  TableHeader,
  TableStateContext,
  TabList,
  TabPanel,
  Tabs,
  Tag,
  TagGroup,
  TagList,
  Text,
  TextArea,
  TextField,
  TimeField,
  ToggleButton,
  ToggleButtonGroup,
  ToggleButtonGroupContext,
  Token,
  TokenField,
  TokenInput,
  Toolbar,
  Tooltip,
  TooltipTrigger,
  TooltipTriggerStateContext,
  Tree,
  TreeItem,
  TreeItemContent,
  UNSTABLE_Toast,
  UNSTABLE_ToastContent,
  UNSTABLE_ToastQueue,
  UNSTABLE_ToastRegion,
  useDragAndDrop,
  useFilter,
  useLocale,
  useTableOptions,
  Virtualizer
} from "react-aria-components";

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:font-sans bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent bc:aria-invalid:accent-border-danger",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};
var buttons = {
  primary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-accent bc:bg-bg-accent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-on-accent bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50",
  secondary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50",
  destructive: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-red-700 bc:bg-red-700 bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-white bc:cursor-pointer bc:hover:bg-red-800 bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle"
};

// srcts/inputs/button.tsx
import { jsx as jsx2 } from "react/jsx-runtime";
function Button2({ variant = "primary", className, "aria-keyshortcuts": shortcuts, ...props }) {
  return /* @__PURE__ */ jsx2(
    Button,
    {
      ...props,
      render: shortcuts === void 0 || props.render !== void 0 ? props.render : (dom) => /* @__PURE__ */ jsx2("button", { ...dom, "aria-keyshortcuts": shortcuts }),
      className: composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`)
    }
  );
}
function FieldButton({ className, ...props }) {
  return /* @__PURE__ */ jsx2(Button, { ...props, className: composeRenderProps(className, (given) => `${FIELD_BUTTON} ${given ?? ""}`) });
}
var FIELD_BUTTON = "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/bounddate.ts
import { useRef as useRef2, useState as useState2 } from "react";

// srcts/inputs/inputs.tsx
import { useState as useState3 } from "react";
import { jsx as jsx3, jsxs } from "react/jsx-runtime";
function NotAvailable({
  values: values2,
  onRemove
}) {
  if (values2.length === 0) return null;
  const named5 = values2.join(", ");
  return /* @__PURE__ */ jsxs("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-x-2", children: [
    /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: `Not available: ${named5}` }),
    onRemove === void 0 ? null : /* @__PURE__ */ jsx3(Button2, { variant: "quiet", "aria-label": `Remove ${named5}`, onPress: onRemove, children: "Remove" })
  ] });
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: error });
}
function useTyped(bound) {
  const [text, setText] = useState3(null);
  const [seen, setSeen] = useState3(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}

// srcts/inputs/calendar.tsx
import { Fragment, jsx as jsx4, jsxs as jsxs2 } from "react/jsx-runtime";

// srcts/inputs/checkbox.tsx
import { useRef as useRef4 } from "react";

// srcts/inputs/icons.tsx
import { jsx as jsx5, jsxs as jsxs3 } from "react/jsx-runtime";
function Chevron() {
  return /* @__PURE__ */ jsx5(
    "svg",
    {
      width: "12",
      height: "12",
      viewBox: "0 0 12 12",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      className: "bc:shrink-0 bc:text-text-muted bc:transition-transform bc:group-data-open:rotate-180",
      children: /* @__PURE__ */ jsx5("polyline", { points: "3 4.5 6 7.5 9 4.5" })
    }
  );
}
function ChevronRight({ size = 12 }) {
  return /* @__PURE__ */ jsx5(
    "svg",
    {
      width: size,
      height: size,
      viewBox: "0 0 12 12",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      className: "bc:shrink-0 bc:text-text-muted",
      children: /* @__PURE__ */ jsx5("polyline", { points: "4.5 3 7.5 6 4.5 9" })
    }
  );
}
function Tick({ size = 14 }) {
  return /* @__PURE__ */ jsx5(
    "svg",
    {
      width: size,
      height: size,
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      children: /* @__PURE__ */ jsx5("path", { d: "M3.5 8.5l3 3 6-7" })
    }
  );
}
function Dash() {
  return /* @__PURE__ */ jsx5("svg", { width: "12", height: "12", viewBox: "0 0 16 16", fill: "none", stroke: "currentColor", strokeWidth: "1.5", strokeLinecap: "round", "aria-hidden": "true", children: /* @__PURE__ */ jsx5("path", { d: "M4 8h8" }) });
}
function Cross() {
  return /* @__PURE__ */ jsx5(
    "svg",
    {
      width: "10",
      height: "10",
      viewBox: "0 0 10 10",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      "aria-hidden": "true",
      children: /* @__PURE__ */ jsx5("path", { d: "M2.5 2.5l5 5M7.5 2.5l-5 5" })
    }
  );
}
function Magnifier() {
  return /* @__PURE__ */ jsxs3(
    "svg",
    {
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      "aria-hidden": "true",
      className: "bc:shrink-0 bc:text-text-muted",
      children: [
        /* @__PURE__ */ jsx5("circle", { cx: "7", cy: "7", r: "4.5" }),
        /* @__PURE__ */ jsx5("path", { d: "M10.5 10.5L14 14" })
      ]
    }
  );
}

// srcts/inputs/items.ts
import { createContext, useCallback as useCallback2, useContext, useLayoutEffect as useLayoutEffect2, useMemo as useMemo2, useRef as useRef3, useState as useState4 } from "react";
var ItemsContext = createContext(null);
function useItems() {
  const entries2 = useRef3(/* @__PURE__ */ new Map());
  const [known2, setKnown] = useState4(null);
  const sync = useCallback2(() => {
    const next = inPageOrder([...entries2.current.values()]);
    setKnown((last) => last !== null && sameValues(last, next) ? last : next);
  }, []);
  const items = useMemo2(
    () => ({
      add(value2, element) {
        const key2 = Symbol(value2);
        entries2.current.set(key2, { value: value2, element });
        sync();
        return () => {
          entries2.current.delete(key2);
          sync();
        };
      }
    }),
    [sync]
  );
  useLayoutEffect2(() => {
    sync();
  });
  return { known: known2, items };
}
function useItem(value2, ref) {
  const items = useContext(ItemsContext);
  useLayoutEffect2(() => {
    if (items === null || value2 === void 0) return void 0;
    return items.add(value2, () => ref.current);
  }, [items, value2, ref]);
}
function inPageOrder(entries2) {
  const placed2 = entries2.map((entry) => ({ value: entry.value, element: entry.element() }));
  placed2.sort((a, b) => {
    if (a.element === null || b.element === null) return a.element === null ? b.element === null ? 0 : 1 : -1;
    return a.element.compareDocumentPosition(b.element) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1;
  });
  return [...new Set(placed2.map((entry) => entry.value))];
}
function sameValues(a, b) {
  return a.length === b.length && a.every((value2, i) => value2 === b[i]);
}
function inOrder(values2, order) {
  return [...order.filter((id2) => values2.includes(id2)), ...values2.filter((value2) => !order.includes(value2))];
}
function merged(now, before, picks) {
  const added = picks.filter((value2) => !before.includes(value2));
  const removed = before.filter((value2) => !picks.includes(value2));
  if (added.length === 0 && removed.length === 0) return null;
  return [...now.filter((x) => !removed.includes(x)), ...added.filter((x) => !now.includes(x))];
}
function lacking(values2, known2) {
  return known2 === null ? [] : values2.filter((value2) => !known2.includes(value2));
}
function held3(values2, known2) {
  return known2 === null ? [...values2] : values2.filter((value2) => known2.includes(value2));
}

// srcts/inputs/checkbox.tsx
import { Fragment as Fragment2, jsx as jsx6, jsxs as jsxs4 } from "react/jsx-runtime";
function Checkbox({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx6(DrawnCheckbox, { ...props }) : /* @__PURE__ */ jsx6(BoundCheckbox, { ...props, bound });
}
function BoundCheckbox({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs4(Fragment2, { children: [
    /* @__PURE__ */ jsx6(
      DrawnCheckbox,
      {
        ...props,
        isSelected: bound.value === true,
        onChange: (selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }
      }
    ),
    /* @__PURE__ */ jsx6(Refusal, { error: bound.error })
  ] });
}
function DrawnCheckbox({ children, description, errorMessage: errorMessage2, className, ...props }) {
  const ref = useRef4(null);
  useItem(props.value, ref);
  return /* @__PURE__ */ jsxs4(CheckboxField, { ...props, ref, className: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsx6(CheckboxButton, { className: composeRenderProps(className, (given) => `bc:group ${CHECK} ${given ?? ""}`), children: composeRenderProps(children, (children2, { isSelected, isIndeterminate }) => /* @__PURE__ */ jsxs4(Fragment2, { children: [
      /* @__PURE__ */ jsx6("span", { className: BOX, children: isIndeterminate ? /* @__PURE__ */ jsx6(Dash, {}) : isSelected ? /* @__PURE__ */ jsx6(Tick, { size: 12 }) : null }),
      children2
    ] })) }),
    /* @__PURE__ */ jsx6(Text, { slot: "description", className: `${styles.hint} bc:ml-6 bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx6(FieldError, { className: `${styles.alert} bc:ml-6`, children: errorMessage2 })
  ] });
}
var CHECK = "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var BOX = "bc:box-border bc:flex bc:size-4 bc:shrink-0 bc:items-center bc:justify-center bc:rounded-sm bc:border bc:border-border-default bc:bg-transparent bc:outline-offset-2 bc:outline-border-accent bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-border-accent bc:group-data-selected:bg-bg-accent bc:group-data-selected:text-text-on-accent bc:group-data-indeterminate:border-border-accent bc:group-data-indeterminate:bg-bg-accent bc:group-data-indeterminate:text-text-on-accent bc:group-data-invalid:border-border-danger bc:group-data-selected:group-data-invalid:bg-border-danger";

// srcts/inputs/colorfield.tsx
import { useContext as useContext2, useState as useState6 } from "react";

// srcts/inputs/colors.ts
import { createContext as createContext2, useState as useState5 } from "react";
var FALLBACK2 = "#000000";
function hex3(color2) {
  return color2.getChannelValue("alpha") < 1 ? color2.toString("hexa") : color2.toString("hex");
}
function valueColor(value2) {
  if (typeof value2 !== "string") return void 0;
  try {
    parseColor(value2);
  } catch {
    return void 0;
  }
  return value2;
}
var Ended = createContext2(null);
function useDragged(bound) {
  const value2 = valueColor(bound.value);
  const [kept, setKept] = useState5(null);
  const [seen, setSeen] = useState5(value2);
  if (seen !== value2) {
    setSeen(value2);
    if (kept !== null && !kept.dragging && (value2 === void 0 || hex3(parseColor(value2)) !== hex3(kept.color))) setKept(null);
  }
  return {
    shown: kept?.color ?? value2 ?? FALLBACK2,
    change: (color2) => {
      setKept({ color: color2, dragging: true });
    },
    end: (color2) => {
      setKept({ color: color2, dragging: false });
      void bound.commit(hex3(color2)).then((took) => {
        if (!took) setKept((now) => now?.dragging === true ? now : null);
      });
    }
  };
}

// srcts/inputs/colorfield.tsx
import { Fragment as Fragment3, jsx as jsx7, jsxs as jsxs5 } from "react/jsx-runtime";
function ColorField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Drawn, { ...props }) : /* @__PURE__ */ jsx7(BoundField, { ...props, bound });
}
function BoundField({ bound, onChange, onBlur, ...props }) {
  const value2 = valueColor(bound.value);
  const [typing, setTyping] = useState6(null);
  const shown2 = typing === null ? value2 : typing.value;
  return /* @__PURE__ */ jsxs5(Fragment3, { children: [
    /* @__PURE__ */ jsx7(
      Drawn,
      {
        ...props,
        value: shown2 ?? null,
        onInput: () => {
          setTyping((now) => now ?? { value: shown2 });
        },
        onChange: (color2) => {
          setTyping(null);
          if (color2 !== null) {
            const alpha = shown2 === void 0 ? 1 : parseColor(shown2).getChannelValue("alpha");
            void bound.commit(hex3(color2.withChannelValue("alpha", alpha)));
          }
          onChange?.(color2);
        },
        onBlur: (event) => {
          setTyping(null);
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx7(Refusal, { error: bound.error })
  ] });
}
function Drawn({ label, description, errorMessage: errorMessage2, className, onChange, onInput, ...props }) {
  const ended = useContext2(Ended);
  return /* @__PURE__ */ jsxs5(
    ColorField,
    {
      ...props,
      onChange: (color2) => {
        onChange?.(color2);
        if (color2 !== null) ended?.(color2);
      },
      className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx7(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx7(Input, { onInput, className: styles.box }),
        /* @__PURE__ */ jsx7(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx7(FieldError, { className: styles.alert, children: errorMessage2 })
      ]
    }
  );
}

// srcts/inputs/colorparts.tsx
import { useContext as useContext3 } from "react";
import { Fragment as Fragment4, jsx as jsx8, jsxs as jsxs6 } from "react/jsx-runtime";
function ColorThumb2({ className, style, ...props }) {
  return /* @__PURE__ */ jsx8(
    ColorThumb,
    {
      ...props,
      className: composeRenderProps(className, (given) => `${THUMB} ${given ?? ""}`),
      style: composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        backgroundColor: isDisabled ? void 0 : defaultStyle.backgroundColor,
        boxShadow: "0 0 0 1px black, inset 0 0 0 1px black"
      }))
    }
  );
}
function ColorArea2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx8(Area, { ...props }) : /* @__PURE__ */ jsx8(BoundPart, { Part: Area, props, bound });
}
function ColorSlider2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx8(Slider2, { ...props }) : /* @__PURE__ */ jsx8(BoundPart, { Part: Slider2, props, bound });
}
function BoundPart({
  Part: Part2,
  props,
  bound
}) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs6(Fragment4, { children: [
    /* @__PURE__ */ jsx8(
      Part2,
      {
        ...props,
        value: dragged.shown,
        onChange: (color2) => {
          dragged.change(color2);
          props.onChange?.(color2);
        },
        onChangeEnd: (color2) => {
          dragged.end(color2);
          props.onChangeEnd?.(color2);
        }
      }
    ),
    /* @__PURE__ */ jsx8(Refusal, { error: bound.error })
  ] });
}
function useEnd(onChangeEnd) {
  const ended = useContext3(Ended);
  return (color2) => {
    onChangeEnd?.(color2);
    ended?.(color2);
  };
}
function Area({ className, style, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsx8(
    ColorArea,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps(className, (given) => `${AREA} ${given ?? ""}`),
      style: composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        background: isDisabled ? void 0 : defaultStyle.background
      })),
      children: /* @__PURE__ */ jsx8(ColorThumb2, {})
    }
  );
}
function Slider2({ label, className, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs6(
    ColorSlider,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps(className, (given) => `${SLIDER} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx8(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx8(SliderOutput, { className: OUTPUT }),
        /* @__PURE__ */ jsx8(
          SliderTrack,
          {
            className: ({ orientation, isDisabled }) => `${TRACK} ${orientation === "vertical" ? "bc:h-50 bc:w-6" : "bc:h-6 bc:w-full"} ${isDisabled ? DISABLED : ""}`,
            style: ({ defaultStyle, isDisabled }) => ({
              ...defaultStyle,
              background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
            }),
            children: /* @__PURE__ */ jsx8(ColorThumb2, {})
          }
        )
      ]
    }
  );
}
var THUMB = "bc:box-border bc:size-4.5 bc:top-[50%] bc:left-[50%] bc:rounded-full bc:border-2 bc:border-white bc:data-focus-visible:size-8 bc:data-dragging:bg-gray-700 bc:dark:data-dragging:bg-gray-300 bc:forced-colors:data-dragging:bg-[ButtonBorder] bc:data-disabled:border-gray-300 bc:data-disabled:bg-gray-300 bc:dark:data-disabled:border-gray-700 bc:dark:data-disabled:bg-gray-800 bc:forced-colors:data-disabled:border-[GrayText] bc:forced-colors:data-disabled:bg-[GrayText]";
var AREA = "bc:aspect-square bc:w-full bc:max-w-56 bc:rounded-lg bc:bg-gray-300 bc:dark:bg-gray-800 bc:forced-colors:bg-[GrayText]";
var SLIDER = "bc:grid-cols-[1fr_auto] bc:flex-col bc:items-center bc:gap-2 bc:font-sans bc:text-text-default bc:data-[orientation=horizontal]:grid bc:data-[orientation=horizontal]:w-56 bc:data-[orientation=vertical]:flex";
var OUTPUT = "bc:text-sm bc:font-medium bc:text-text-muted bc:data-[orientation=vertical]:hidden";
var TRACK = "bc:group bc:col-span-2 bc:rounded-md";
var DISABLED = "bc:bg-gray-300 bc:dark:bg-gray-800 bc:forced-colors:bg-[GrayText]";
var CHECKERBOARD = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";

// srcts/overlays/dialog.tsx
import { Fragment as Fragment5, jsx as jsx9, jsxs as jsxs7 } from "react/jsx-runtime";
function Modal2({ className, ...props }) {
  return /* @__PURE__ */ jsx9(ModalOverlay, { ...props, className: OVERLAY, children: /* @__PURE__ */ jsx9("div", { className: CENTER, children: /* @__PURE__ */ jsx9(Modal, { ...props, className: composeRenderProps(className, (given) => `${MODAL} ${given ?? ""}`) }) }) });
}
function Dialog2({ className, ...props }) {
  return /* @__PURE__ */ jsx9(Dialog, { ...props, className: `${DIALOG2} ${className ?? ""}` });
}
var OVERLAY = "bc:absolute bc:top-0 bc:left-0 bc:isolate bc:z-overlay bc:h-(--page-height) bc:w-full bc:bg-black/40";
var CENTER = "bc:sticky bc:top-0 bc:left-0 bc:box-border bc:flex bc:h-(--visual-viewport-height) bc:w-full bc:items-center bc:justify-center bc:p-4";
var MODAL = "bc:box-border bc:max-h-[calc(var(--visual-viewport-height)*.9)] bc:w-full bc:max-w-xl bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-gray-900 bc:shadow-xl bc:outline-none bc:dark:text-gray-100";
var DIALOG2 = "bc:relative bc:box-border bc:max-h-[inherit] bc:p-4 bc:outline-none";

// srcts/overlays/menu.tsx
import { Children } from "react";

// srcts/overlays/stacking.ts
var OVERLAY_Z_INDEX = "var(--bc-z-index-overlay)";
function overlayStyle(style) {
  return composeRenderProps(style, (given) => ({ ...given, zIndex: OVERLAY_Z_INDEX }));
}

// srcts/overlays/popover.tsx
import { jsx as jsx10 } from "react/jsx-runtime";
function Popover2({ className, style, offset = 4, ...props }) {
  return /* @__PURE__ */ jsx10(
    Popover,
    {
      ...props,
      offset,
      style: overlayStyle(style),
      className: composeRenderProps(className, (given) => `${POPOVER} ${given ?? ""}`)
    }
  );
}
var POPOVER = "bc:overflow-auto bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:dark:text-gray-100";

// srcts/overlays/menu.tsx
import { Fragment as Fragment6, jsx as jsx11, jsxs as jsxs8 } from "react/jsx-runtime";
function MenuTrigger2({ placement, children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs8(MenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx11(Popover2, { placement, className: "bc:min-w-44", children: menu })
  ] });
}
function SubmenuTrigger2({ children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs8(SubmenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx11(Popover2, { offset: -2, crossOffset: -4, className: "bc:min-w-44", children: menu })
  ] });
}
function Menu2({ className, ...props }) {
  return /* @__PURE__ */ jsx11(Menu, { ...props, className: composeRenderProps(className, (given) => `${MENU2} ${given ?? ""}`) });
}
function MenuItem2(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx11(MenuItem, { ...props, textValue, className: composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`), children: composeRenderProps(props.children, (children, { selectionMode, isSelected, hasSubmenu }) => /* @__PURE__ */ jsxs8(Fragment6, { children: [
    selectionMode === "none" ? null : /* @__PURE__ */ jsx11("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx11(Tick, {}) : null }),
    /* @__PURE__ */ jsx11("span", { className: CONTENT, children }),
    hasSubmenu ? /* @__PURE__ */ jsx11(ChevronRight, { size: 14 }) : null
  ] })) });
}
function MenuSection2({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs8(MenuSection, { ...props, children: [
    title === void 0 ? null : /* @__PURE__ */ jsx11(Header, { className: HEADING, children: title }),
    /* @__PURE__ */ jsx11(Collection, { items, children })
  ] });
}
var MENU2 = "bc:grid bc:max-h-[inherit] bc:gap-0.5 bc:overflow-auto bc:p-1 bc:outline-none";
var ITEM = "bc:box-border bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var HEADING = "bc:block bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted";
var CONTENT = "bc:grid bc:min-w-0 bc:flex-1 bc:grid-cols-[minmax(0,1fr)_auto] bc:items-center bc:gap-x-4 bc:[&>*]:truncate bc:[&>[slot=description]]:col-start-1 bc:[&>[slot=description]]:text-xs bc:[&>[slot=description]]:text-text-muted bc:[&>kbd]:col-start-2 bc:[&>kbd]:row-start-1 bc:[&>kbd]:font-sans bc:[&>kbd]:text-xs bc:[&>kbd]:text-text-muted";

// srcts/overlays/palette.tsx
import { jsx as jsx12, jsxs as jsxs9 } from "react/jsx-runtime";

// srcts/overlays/sheet.tsx
import { jsx as jsx13, jsxs as jsxs10 } from "react/jsx-runtime";

// srcts/overlays/toast.tsx
import { jsx as jsx14 } from "react/jsx-runtime";
var FADES_FOR = 300;
function ToastRegion({ className, ...props }) {
  return /* @__PURE__ */ jsx14(UNSTABLE_ToastRegion, { ...props, className: composeRenderProps(className, (given) => `${REGION} ${given ?? ""}`) });
}
function Toast({ className, style, ...props }) {
  const { timeout } = props.toast;
  const fade = timeout === void 0 ? void 0 : { "--bc-toast-fade-at": `${String(Math.max(0, timeout - FADES_FOR))}ms` };
  return /* @__PURE__ */ jsx14(
    UNSTABLE_Toast,
    {
      ...props,
      style: composeRenderProps(style, (given) => ({ ...fade, ...given })),
      className: composeRenderProps(className, (given) => `${TOAST} ${timeout === void 0 ? "" : FADE} ${given ?? ""}`)
    }
  );
}
var REGION = "bc:group/toasts bc:pointer-events-none bc:fixed bc:right-2 bc:bottom-2 bc:z-[calc(var(--bc-z-index-overlay)+1)] bc:m-0 bc:box-border bc:flex bc:max-h-[calc(100dvh-1rem)] bc:w-[21rem] bc:max-w-[calc(100vw-1rem)] bc:flex-col-reverse bc:gap-2 bc:overflow-y-auto bc:p-2 bc:font-sans bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
var TOAST = "bc:pointer-events-auto bc:box-border bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:px-3 bc:py-2 bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:dark:text-gray-100";
var FADE = "bc:toast-fade bc:group-data-hovered/toasts:[animation-play-state:paused] bc:group-focus-within/toasts:[animation-play-state:paused]";

// srcts/overlays/tooltip.tsx
import { jsx as jsx15, jsxs as jsxs11 } from "react/jsx-runtime";
function Tooltip2({ children, className, style, offset = 8, ...props }) {
  return /* @__PURE__ */ jsxs11(
    Tooltip,
    {
      ...props,
      offset,
      style: overlayStyle(style),
      className: composeRenderProps(className, (given) => `${TOOLTIP} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx15(OverlayArrow, { children: /* @__PURE__ */ jsx15("svg", { width: 8, height: 8, viewBox: "0 0 8 8", className: ARROW, "aria-hidden": "true", children: /* @__PURE__ */ jsx15("path", { d: "M0 0 L4 4 L8 0" }) }) }),
        children
      ]
    }
  );
}
var TOOLTIP = "bc:group bc:box-border bc:max-w-xs bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:px-2 bc:py-1 bc:font-sans bc:text-xs bc:text-gray-900 bc:shadow-md bc:outline-none bc:will-change-transform bc:dark:text-gray-100";
var ARROW = "bc:block bc:fill-bg-raised bc:stroke-border-default bc:forced-colors:fill-[Canvas] bc:forced-colors:stroke-[ButtonBorder] bc:group-data-[placement=bottom]:rotate-180 bc:group-data-[placement=left]:-rotate-90 bc:group-data-[placement=right]:rotate-90";

// srcts/inputs/colorswatches.tsx
import { useContext as useContext4 } from "react";
import { Fragment as Fragment7, jsx as jsx16, jsxs as jsxs12 } from "react/jsx-runtime";
function ColorSwatch2({ className, style, ...props }) {
  return /* @__PURE__ */ jsx16(
    ColorSwatch,
    {
      ...props,
      className: composeRenderProps(className, (given) => `${SWATCH} ${given ?? ""}`),
      style: composeRenderProps(style, (given, { color: color2 }) => ({
        ...given,
        background: `linear-gradient(${color2.toString()}, ${color2.toString()}), ${CHECKERBOARD2}`
      }))
    }
  );
}
function ColorSwatchPicker2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx16(Drawn2, { ...props }) : /* @__PURE__ */ jsx16(BoundSwatches, { ...props, bound });
}
function BoundSwatches({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs12(Fragment7, { children: [
    /* @__PURE__ */ jsx16(
      Drawn2,
      {
        ...props,
        value: valueColor(bound.value) ?? FALLBACK2,
        onChange: (color2) => {
          void bound.commit(hex3(color2));
          onChange?.(color2);
        }
      }
    ),
    /* @__PURE__ */ jsx16(Refusal, { error: bound.error })
  ] });
}
function Drawn2({ className, onChange, ...props }) {
  const ended = useContext4(Ended);
  return /* @__PURE__ */ jsx16(
    ColorSwatchPicker,
    {
      ...props,
      onChange: (color2) => {
        onChange?.(color2);
        ended?.(color2);
      },
      className: composeRenderProps(className, (given, { layout }) => `${PICKER} ${layout === "stack" ? "bc:flex-col" : "bc:flex-wrap"} ${given ?? ""}`)
    }
  );
}
function ColorSwatchPickerItem2({ className, ...props }) {
  return /* @__PURE__ */ jsx16(ColorSwatchPickerItem, { ...props, className: composeRenderProps(className, (given) => `${ITEM2} ${given ?? ""}`), children: ({ isSelected }) => /* @__PURE__ */ jsxs12(Fragment7, { children: [
    /* @__PURE__ */ jsx16(ColorSwatch2, {}),
    isSelected ? /* @__PURE__ */ jsx16("div", { className: SELECTED }) : null
  ] }) });
}
var SWATCH = "bc:box-border bc:size-8 bc:shrink-0 bc:rounded-md bc:border bc:border-border-default";
var CHECKERBOARD2 = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
var PICKER = "bc:flex bc:gap-1";
var ITEM2 = "bc:relative bc:cursor-pointer bc:rounded-xs bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var SELECTED = "bc:absolute bc:top-0 bc:left-0 bc:box-border bc:size-full bc:rounded-md bc:border-2 bc:border-black bc:outline-2 bc:-outline-offset-4 bc:outline-white bc:dark:border-white bc:dark:outline-black bc:forced-color-adjust-none";

// srcts/inputs/colorpicker.tsx
import { Fragment as Fragment8, jsx as jsx17, jsxs as jsxs13 } from "react/jsx-runtime";
function ColorPicker2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx17(Drawn3, { ...props }) : /* @__PURE__ */ jsx17(BoundPicker, { ...props, bound });
}
function BoundPicker({ bound, onChange, ...props }) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs13(Fragment8, { children: [
    /* @__PURE__ */ jsx17(Ended, { value: dragged.end, children: /* @__PURE__ */ jsx17(
      Drawn3,
      {
        ...props,
        value: dragged.shown,
        onChange: (color2) => {
          dragged.change(color2);
          onChange?.(color2);
        }
      }
    ) }),
    /* @__PURE__ */ jsx17(Refusal, { error: bound.error })
  ] });
}
function Drawn3({ label, children, isDisabled, ...props }) {
  return /* @__PURE__ */ jsx17(ColorPicker, { ...props, children: /* @__PURE__ */ jsxs13(DialogTrigger, { children: [
    /* @__PURE__ */ jsxs13(Button, { isDisabled, className: BUTTON, children: [
      /* @__PURE__ */ jsx17(ColorSwatch2, {}),
      label === void 0 ? null : /* @__PURE__ */ jsx17("span", { children: label })
    ] }),
    /* @__PURE__ */ jsx17(Popover2, { placement: "bottom start", children: /* @__PURE__ */ jsx17(Dialog2, { className: "bc:flex bc:flex-col bc:gap-2", children: children ?? /* @__PURE__ */ jsxs13(Fragment8, { children: [
      /* @__PURE__ */ jsx17(ColorArea2, { colorSpace: "hsb", xChannel: "saturation", yChannel: "brightness" }),
      /* @__PURE__ */ jsx17(ColorSlider2, { colorSpace: "hsb", channel: "hue" }),
      /* @__PURE__ */ jsx17(ColorField2, { label: "Hex" })
    ] }) }) })
  ] }) });
}
var BUTTON = "bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded-xs bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/combobox.tsx
import { useContext as useContext5, useEffect, useEffectEvent, useRef as useRef5, useState as useState7 } from "react";
import { Fragment as Fragment9, jsx as jsx18, jsxs as jsxs14 } from "react/jsx-runtime";
function ComboBox2(props) {
  const { bound, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx18(Drawn4, { ...rest });
  return props.selectionMode === "multiple" ? /* @__PURE__ */ jsx18(BoundSeveral, { ...rest, bound, ordered }) : /* @__PURE__ */ jsx18(BoundOne, { ...rest, bound });
}
function BoundOne({ bound, outdated = false, ...props }) {
  const current2 = typeof bound.value === "string" ? bound.value : null;
  return /* @__PURE__ */ jsxs14(Fragment9, { children: [
    /* @__PURE__ */ jsx18(
      Drawn4,
      {
        ...props,
        selectionMode: "single",
        outdated,
        marking: !outdated && known(props),
        onClear: () => {
          void bound.unset();
        },
        value: current2,
        onChange: (key2) => {
          if (key2 !== null && !Array.isArray(key2)) void bound.commit(String(key2));
        }
      }
    ),
    /* @__PURE__ */ jsx18(Refusal, { error: bound.error })
  ] });
}
function BoundSeveral({
  bound,
  outdated = false,
  ordered,
  onOpenChange,
  ...props
}) {
  const items = props.items ?? props.defaultItems;
  const held4 = strings(bound.value);
  const [picks, setPicks] = useState7(null);
  const pending = useRef5(null);
  const submit = (next, opened) => {
    const value2 = merged(strings(bound.get()), opened, next);
    if (value2 === null) return;
    void bound.commit(ordered && items !== void 0 ? inOrder(value2, ids2(items)) : value2);
  };
  return /* @__PURE__ */ jsxs14(Fragment9, { children: [
    /* @__PURE__ */ jsx18(
      Drawn4,
      {
        ...props,
        selectionMode: "multiple",
        outdated,
        marking: !outdated && known(props),
        value: picks ?? held4,
        onOpenChange: (isOpen, trigger) => {
          if (isOpen) {
            pending.current ??= { picks: held4, opened: held4 };
            setPicks(pending.current.picks);
          } else {
            const left2 = pending.current;
            pending.current = null;
            setPicks(null);
            if (left2 !== null) submit(left2.picks, left2.opened);
          }
          onOpenChange?.(isOpen, trigger);
        },
        onChange: (keys) => {
          if (!Array.isArray(keys)) return;
          const next = keys.map(String);
          if (pending.current === null) {
            submit(next, held4);
            return;
          }
          pending.current = { ...pending.current, picks: next };
          setPicks(next);
        }
      }
    ),
    /* @__PURE__ */ jsx18(Refusal, { error: bound.error })
  ] });
}
function known(props) {
  return (props.items ?? props.defaultItems) !== void 0 || typeof props.children !== "function";
}
function ids2(items) {
  return [...items].flatMap((item) => {
    const id2 = item.id;
    return typeof id2 === "string" || typeof id2 === "number" ? [String(id2)] : [];
  });
}
function Drawn4({
  label,
  description,
  errorMessage: errorMessage2,
  placeholder,
  children,
  renderEmptyState,
  outdated = false,
  marking = false,
  onClear,
  className,
  allowsEmptyCollection = true,
  ...props
}) {
  const multiple = props.selectionMode === "multiple";
  const isDisabled = props.isDisabled === true;
  const editable2 = !isDisabled && props.isReadOnly !== true;
  const given = props.items ?? props.defaultItems;
  const unknown2 = given === void 0 && typeof children === "function";
  const none = given === void 0 ? typeof children === "function" || !hasChildren(children) : [...given].length === 0;
  const empty2 = none && (outdated || unknown2) ? "Loading\u2026" : none ? renderEmptyState?.() ?? "No options" : "No matches";
  const value2 = useSameValue(props.value);
  return /* @__PURE__ */ jsxs14(
    ComboBox,
    {
      ...props,
      value: value2,
      defaultItems: unknown2 ? [] : props.defaultItems,
      allowsEmptyCollection,
      className: composeRenderProps(className, (given2) => `bc:group ${styles.field} ${given2 ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx18(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx18(
          Field,
          {
            placeholder,
            multiple,
            marking,
            isDisabled,
            editable: editable2,
            clearable: props.isRequired !== true && editable2,
            onClear,
            label
          }
        ),
        /* @__PURE__ */ jsx18(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx18(FieldError, { className: styles.alert, children: errorMessage2 }),
        /* @__PURE__ */ jsx18(Lacking, { multiple, marking }),
        /* @__PURE__ */ jsx18(Popover, { offset: 4, className: LIST, style: { zIndex: OVERLAY_Z_INDEX }, children: /* @__PURE__ */ jsx18(Virtualizer, { layout: ListLayout, layoutOptions: LAYOUT, children: /* @__PURE__ */ jsx18(ListBox, { renderEmptyState: () => /* @__PURE__ */ jsx18("p", { className: NOTE, children: empty2 }), className: LISTBOX, children }) }) })
      ]
    }
  );
}
function useSameValue(value2) {
  const [drawn3, setDrawn] = useState7(value2);
  const same2 = drawn3 === value2 || Array.isArray(drawn3) && Array.isArray(value2) && sameKeys(drawn3, value2);
  if (!same2) setDrawn(value2);
  return same2 ? drawn3 : value2;
}
function sameKeys(a, b) {
  return a.length === b.length && a.every((key2, i) => key2 === b[i]);
}
function hasChildren(children) {
  return Array.isArray(children) ? children.some((child) => hasChildren(child)) : children !== null && children !== void 0 && children !== false;
}
function Field({
  placeholder,
  multiple,
  marking,
  isDisabled,
  editable: editable2,
  clearable,
  onClear,
  label
}) {
  const { state: state2, keys, one: one2, known: known2, lacked } = usePicks(multiple);
  const item = one2 === null ? void 0 : known2.get(one2);
  const invalid = marking && lacked.length > 0;
  const remove2 = (gone) => {
    state2?.setValue(keys.filter((key2) => !gone.has(key2)));
  };
  const chips = keys.map((key2) => ({ key: key2, text: known2.get(key2) ?? String(key2), gone: marking && !known2.has(key2) }));
  const picks = label === void 0 ? "Picks" : `Picks of ${label}`;
  return /* @__PURE__ */ jsxs14(Group, { isInvalid: invalid, className: FIELD, children: [
    /* @__PURE__ */ jsxs14("div", { className: VALUES, children: [
      keys.length === 0 ? null : isDisabled ? (
        // The chips of a disabled select take no focus, as its text field
        // takes none.
        // React Aria's TagGroup keeps a tab stop on its list even where
        // every tag is disabled, so a disabled select draws its chips as
        // a plain list.
        /* @__PURE__ */ jsx18("div", { role: "list", "aria-label": picks, className: "bc:flex bc:min-w-0 bc:max-w-full bc:flex-wrap bc:gap-1", children: chips.map(({ key: key2, text, gone }) => /* @__PURE__ */ jsx18(PlainChip, { text, gone }, key2)) })
      ) : /* @__PURE__ */ jsx18(TagGroup, { "aria-label": picks, onRemove: editable2 ? remove2 : void 0, className: "bc:flex bc:min-w-0 bc:max-w-full", children: /* @__PURE__ */ jsx18(TagList, { className: "bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1", children: chips.map(({ key: key2, text, gone }) => /* @__PURE__ */ jsx18(Tag, { id: key2, textValue: text, "data-missing": gone || void 0, className: CHIP, children: ({ isFocusVisible }) => /* @__PURE__ */ jsxs14(Fragment9, { children: [
        /* @__PURE__ */ jsx18(ChipText, { text, focused: isFocusVisible }),
        editable2 ? /* @__PURE__ */ jsx18(Button, { slot: "remove", "aria-label": `Remove ${text}`, className: CHIP_REMOVE, children: /* @__PURE__ */ jsx18(Cross, {}) }) : null
      ] }) }, key2)) }) }),
      /* @__PURE__ */ jsx18(
        Input,
        {
          value: multiple || state2 === null || state2.isFocused || state2.isOpen ? void 0 : item ?? (one2 === null ? "" : String(one2)),
          placeholder: one2 !== null && item === void 0 ? String(one2) : keys.length > 0 ? void 0 : placeholder,
          "aria-invalid": invalid || void 0,
          className: INPUT,
          onClick: () => {
            if (editable2 && state2 !== null && !state2.isOpen) state2.open(null, "manual");
          },
          onKeyDown: (event) => {
            if (event.key === "Escape" && state2?.isOpen === true) event.preventDefault();
            if (!editable2 || event.key !== "Backspace" || event.currentTarget.value !== "" || keys.length === 0) return;
            state2?.setValue(keys.slice(0, -1));
          }
        }
      )
    ] }),
    clearable && (keys.length > 0 || one2 !== null) ? (
      // A FieldButton whose slot is null, which React Aria's ComboBox then
      // leaves alone, rather than take it for the button that opens the
      // list. It is outside the order of Tab, and keeps the focus in the
      // text field, as a press in the list does.
      // It is the trigger of its tooltip, as a button of React Aria is.
      /* @__PURE__ */ jsxs14(TooltipTrigger, { children: [
        /* @__PURE__ */ jsx18(
          FieldButton,
          {
            slot: null,
            excludeFromTabOrder: true,
            preventFocusOnPress: true,
            "aria-label": label === void 0 ? "Clear" : `Clear ${label}`,
            onPress: () => {
              if (onClear !== void 0) onClear();
              else state2?.setValue(multiple ? [] : null);
            },
            children: /* @__PURE__ */ jsx18(Cross, {})
          }
        ),
        /* @__PURE__ */ jsx18(Tooltip2, { children: multiple ? "Clear every pick" : "Clear the pick" })
      ] })
    ) : null,
    /* @__PURE__ */ jsx18(Button, { className: OPENER, children: /* @__PURE__ */ jsx18(Chevron, {}) })
  ] });
}
function PlainChip({ text, gone }) {
  return /* @__PURE__ */ jsx18("div", { role: "listitem", "data-missing": gone || void 0, className: CHIP, children: /* @__PURE__ */ jsx18(ChipText, { text, focused: false }) });
}
function ChipText({ text, focused }) {
  return /* @__PURE__ */ jsx18(TooltipTrigger, { children: /* @__PURE__ */ jsx18(CutText, { text, focused }) });
}
function CutText({ text, focused }) {
  const tip = useContext5(TooltipTriggerStateContext);
  const ref = useRef5(null);
  const [pointed, setPointed] = useState7(false);
  const shown2 = useRef5({ pointed: false, focused: false });
  const follow2 = useEffectEvent((now) => {
    const before = shown2.current;
    shown2.current = now;
    const el = ref.current;
    const cut = el !== null && el.scrollWidth > el.clientWidth;
    if (tip === null) return;
    if (cut && now.focused) tip.open(true);
    else if (cut && now.pointed) tip.open();
    else if (before.pointed || before.focused) tip.close(before.focused);
  });
  useEffect(() => {
    follow2({ pointed, focused });
  }, [pointed, focused]);
  return /* @__PURE__ */ jsxs14(Fragment9, { children: [
    /* @__PURE__ */ jsx18(
      "span",
      {
        ref,
        className: "bc:truncate",
        onPointerEnter: (event) => {
          if (event.pointerType !== "touch") setPointed(true);
        },
        onPointerLeave: () => {
          setPointed(false);
        },
        children: text
      }
    ),
    /* @__PURE__ */ jsx18(Tooltip2, { triggerRef: ref, children: text })
  ] });
}
function usePicks(multiple) {
  const state2 = useContext5(ComboBoxStateContext);
  const value2 = state2?.value ?? null;
  const keys = multiple && Array.isArray(value2) ? value2 : [];
  const one2 = !multiple && value2 !== null && !Array.isArray(value2) ? value2 : null;
  const known2 = new Map((state2?.selectedItems ?? []).map((item) => [item.key, item.textValue]));
  const lacked = (one2 === null ? keys : [one2]).filter((key2) => !known2.has(key2));
  return { state: state2, keys, one: one2, known: known2, lacked };
}
function Lacking({ multiple, marking }) {
  const { lacked } = usePicks(multiple);
  return marking ? /* @__PURE__ */ jsx18(NotAvailable, { values: lacked.map(String) }) : null;
}
function ComboBoxItem(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx18(ListBoxItem, { ...props, textValue, className: ROW, children: composeRenderProps(props.children, (children, { isSelected, selectionMode }) => /* @__PURE__ */ jsxs14(Fragment9, { children: [
    selectionMode === "multiple" ? /* @__PURE__ */ jsx18("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx18(Tick, {}) : null }) : null,
    /* @__PURE__ */ jsx18(
      "span",
      {
        className: `bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2 bc:truncate ${selectionMode === "single" && isSelected ? "bc:font-semibold" : ""}`,
        children
      }
    )
  ] })) });
}
function ComboBoxSection({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs14(ListBoxSection, { ...props, children: [
    /* @__PURE__ */ jsx18(Header, { className: HEADING2, children: title }),
    /* @__PURE__ */ jsx18(Collection, { items, children })
  ] });
}
var FIELD = "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var VALUES = "bc:flex bc:min-w-0 bc:flex-1 bc:flex-wrap bc:items-center bc:gap-1";
var INPUT = "bc:min-w-16 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:disabled:cursor-default";
var OPENER = "bc:ml-auto bc:flex bc:size-6 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:data-disabled:cursor-default";
var CHIP = "bc:inline-flex bc:max-w-full bc:min-w-0 bc:items-center bc:gap-0.5 bc:rounded bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0 bc:pr-1.5 bc:pl-1.5 bc:data-allows-removing:pr-0.5 bc:text-xs bc:leading-5 bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-missing:border-border-danger bc:data-missing:text-text-danger";
var CHIP_REMOVE = "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default";
var LAYOUT = { rowHeight: 32, headingHeight: 28, padding: 4 };
var LIST = "bc:box-border bc:w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
var LISTBOX = "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:outline-none";
var HEADING2 = "bc:box-border bc:h-[28px] bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
var ROW = "bc:box-border bc:flex bc:h-[32px] bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-focus-visible:outline bc:data-focus-visible:outline-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:[&_[slot=description]]:min-w-0 bc:[&_[slot=description]]:flex-1 bc:[&_[slot=description]]:truncate bc:[&_[slot=description]]:text-[13px] bc:[&_[slot=description]]:text-text-muted";
var NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/inputs/datefield.tsx
import { Fragment as Fragment10, jsx as jsx19, jsxs as jsxs15 } from "react/jsx-runtime";

// srcts/inputs/datepicker.tsx
import { useRef as useRef6 } from "react";
import { Fragment as Fragment11, jsx as jsx20, jsxs as jsxs16 } from "react/jsx-runtime";

// srcts/inputs/dropzone.tsx
import { jsx as jsx21 } from "react/jsx-runtime";

// srcts/inputs/form.tsx
import { jsx as jsx22 } from "react/jsx-runtime";
function Form2({ className, ...props }) {
  return /* @__PURE__ */ jsx22(Form, { ...props, className: `bc:grid bc:gap-3 ${className ?? ""}` });
}

// srcts/inputs/listbox.tsx
import { jsx as jsx23 } from "react/jsx-runtime";

// srcts/inputs/numberfield.tsx
import { useState as useState8 } from "react";
import { Fragment as Fragment12, jsx as jsx24, jsxs as jsxs17 } from "react/jsx-runtime";
function NumberField2({ bound, outdated = false, maxValue, ...props }) {
  const largest = outdated ? void 0 : maxValue;
  return bound === void 0 ? /* @__PURE__ */ jsx24(Drawn5, { ...props, maxValue: largest }) : /* @__PURE__ */ jsx24(BoundNumber, { ...props, maxValue: largest, bound });
}
function BoundNumber({
  bound,
  onChange,
  onInput,
  onBlur,
  commitBehavior = "validate",
  validationBehavior = "aria",
  ...props
}) {
  const held4 = typeof bound.value === "number" ? bound.value : NaN;
  const [kept, setKept] = useState8(null);
  const [seen, setSeen] = useState8(held4);
  if (!Object.is(seen, held4)) {
    setSeen(held4);
    if (kept !== null && !kept.typing) setKept(null);
  }
  const fits2 = (value2) => inRange(value2, props.minValue, props.maxValue, props.step);
  return /* @__PURE__ */ jsxs17(Fragment12, { children: [
    /* @__PURE__ */ jsx24(
      Drawn5,
      {
        ...props,
        commitBehavior,
        validationBehavior,
        value: kept?.value ?? held4,
        onInput: (event) => {
          setKept((now) => now?.typing === true ? now : { value: now?.value ?? held4, typing: true });
          onInput?.(event);
        },
        onChange: (value2) => {
          if (!fits2(value2)) {
            setKept(null);
          } else if (kept?.typing === true) {
            bound.type(value2);
            bound.flush();
            setKept(Object.is(value2, held4) ? null : { value: value2, typing: false });
          } else {
            setKept({ value: value2, typing: false });
            void bound.commit(value2).then((took) => {
              if (!took) setKept(null);
            });
          }
          onChange?.(value2);
        },
        onBlur: (event) => {
          setKept((now) => now?.typing === true ? null : now);
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx24(Refusal, { error: bound.error })
  ] });
}
function inRange(value2, min, max, step2) {
  if (Number.isNaN(value2)) return false;
  if (min !== void 0 && value2 < min) return false;
  if (max !== void 0 && value2 > max) return false;
  if (step2 === void 0 || !(step2 > 0)) return true;
  const steps2 = (value2 - (min ?? 0)) / step2;
  return Math.abs(steps2 - Math.round(steps2)) < 1e-9;
}
function Drawn5({ label, description, errorMessage: errorMessage2, placeholder, className, onInput, ...props }) {
  return /* @__PURE__ */ jsxs17(NumberField, { ...props, className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx24(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsxs17(Group, { className: FIELD2, children: [
      /* @__PURE__ */ jsx24(Input, { placeholder, onInput, className: INPUT2 }),
      /* @__PURE__ */ jsxs17("div", { className: STEPPERS, children: [
        /* @__PURE__ */ jsx24(Button, { slot: "increment", className: STEPPER, children: /* @__PURE__ */ jsx24(Chevron2, { up: true }) }),
        /* @__PURE__ */ jsx24(Button, { slot: "decrement", className: STEPPER, children: /* @__PURE__ */ jsx24(Chevron2, { up: false }) })
      ] })
    ] }),
    /* @__PURE__ */ jsx24(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx24(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}
function Chevron2({ up }) {
  return /* @__PURE__ */ jsx24(
    "svg",
    {
      width: "10",
      height: "10",
      viewBox: "0 0 12 12",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      children: /* @__PURE__ */ jsx24("polyline", { points: up ? "3 7.5 6 4.5 9 7.5" : "3 4.5 6 7.5 9 4.5" })
    }
  );
}
var FIELD2 = "bc:flex bc:min-w-0 bc:items-stretch bc:justify-self-stretch bc:overflow-hidden bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var INPUT2 = "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted";
var STEPPERS = "bc:flex bc:flex-col bc:border-l bc:border-border-default bc:group-data-invalid:border-border-danger";
var STEPPER = "bc:flex bc:flex-1 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:px-1 bc:py-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-pressed:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/radio.tsx
import { useRef as useRef7 } from "react";
import { Fragment as Fragment13, jsx as jsx25, jsxs as jsxs18 } from "react/jsx-runtime";

// srcts/inputs/searchfield.tsx
import { Fragment as Fragment14, jsx as jsx26, jsxs as jsxs19 } from "react/jsx-runtime";
function SearchField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx26(Drawn6, { ...props }) : /* @__PURE__ */ jsx26(BoundSearch, { ...props, bound });
}
function BoundSearch({ bound, onChange, onKeyDown, onBlur, onClear, ...props }) {
  const [text, typed] = useTyped(bound);
  return /* @__PURE__ */ jsxs19(Fragment14, { children: [
    /* @__PURE__ */ jsx26(
      Drawn6,
      {
        ...props,
        value: text ?? (typeof bound.value === "string" ? bound.value : ""),
        onChange: (value2) => {
          typed(value2);
          bound.type(value2);
          onChange?.(value2);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing) bound.flush();
          event.continuePropagation();
          onKeyDown?.(event);
        },
        onBlur: (event) => {
          bound.flush();
          onBlur?.(event);
        },
        onClear: () => {
          bound.flush();
          onClear?.();
        }
      }
    ),
    /* @__PURE__ */ jsx26(Refusal, { error: bound.error })
  ] });
}
function Drawn6({ label, description, errorMessage: errorMessage2, placeholder, className, ...props }) {
  return /* @__PURE__ */ jsxs19(SearchField, { ...props, className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx26(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsxs19(Group, { className: FIELD3, children: [
      /* @__PURE__ */ jsx26(Magnifier, {}),
      /* @__PURE__ */ jsx26(Input, { placeholder, className: INPUT3 }),
      /* @__PURE__ */ jsx26(FieldButton, { className: "bc:group-data-empty:invisible", children: /* @__PURE__ */ jsx26(Cross, {}) })
    ] }),
    /* @__PURE__ */ jsx26(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx26(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}
var FIELD3 = "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1.5 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-1 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:group-data-invalid:border-border-danger bc:data-disabled:opacity-50";
var INPUT3 = "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:[&::-webkit-search-cancel-button]:hidden";

// srcts/inputs/select.tsx
import { useContext as useContext6, useLayoutEffect as useLayoutEffect3, useMemo as useMemo3, useRef as useRef8, useState as useState9 } from "react";
import { Fragment as Fragment15, jsx as jsx27, jsxs as jsxs20 } from "react/jsx-runtime";
function Select2(props) {
  const { bound, outdated = false, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx27(Drawn7, { ...rest });
  return /* @__PURE__ */ jsx27(BoundSelect, { ...rest, bound, outdated, ordered });
}
function SelectItem(props) {
  return /* @__PURE__ */ jsx27(ComboBoxItem, { ...props });
}
function BoundSelect({
  bound,
  outdated,
  ordered,
  onChange,
  onOpenChange,
  ...props
}) {
  const several = props.selectionMode === "multiple";
  const values2 = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const [known2, setKnown] = useState9(null);
  const unknown2 = props.items === void 0 && typeof props.children === "function";
  const ids3 = unknown2 ? null : known2;
  const [picks, setPicks] = useState9(null);
  const pending = useRef8(null);
  const shown2 = picks ?? values2;
  const asIs = ids3 === null ? values2 : values2.filter((value2) => !ids3.includes(value2));
  const marking = !outdated && ids3 !== null && asIs.length > 0;
  const submit = (next, opened) => {
    const value2 = merged(strings(bound.get()), opened, next);
    if (value2 === null) return;
    void bound.commit(ordered && ids3 !== null ? inOrder(value2, ids3) : value2);
  };
  const common = {
    ...props,
    isInvalid: props.isInvalid === true || marking,
    asIs,
    onKnown: (next) => {
      setKnown((last) => last !== null && sameIds(last, next) ? last : next);
    }
  };
  const line = marking ? /* @__PURE__ */ jsx27(
    NotAvailable,
    {
      values: asIs,
      onRemove: several ? () => {
        void bound.commit(held3(strings(bound.get()), ids3));
      } : void 0
    }
  ) : null;
  if (!several) {
    return /* @__PURE__ */ jsxs20(Fragment15, { children: [
      /* @__PURE__ */ jsx27(
        Drawn7,
        {
          ...common,
          selectionMode: "single",
          value: values2[0] ?? null,
          onChange: (key2) => {
            if (key2 !== null && !Array.isArray(key2)) void bound.commit(String(key2));
            onChange?.(key2);
          },
          onOpenChange
        }
      ),
      line,
      /* @__PURE__ */ jsx27(Refusal, { error: bound.error })
    ] });
  }
  return /* @__PURE__ */ jsxs20(Fragment15, { children: [
    /* @__PURE__ */ jsx27(
      Drawn7,
      {
        ...common,
        selectionMode: "multiple",
        value: [...shown2],
        onOpenChange: (isOpen) => {
          if (isOpen) {
            pending.current ??= { picks: values2, opened: values2 };
            setPicks(pending.current.picks);
          } else {
            const left2 = pending.current;
            pending.current = null;
            setPicks(null);
            if (left2 !== null) submit(left2.picks, left2.opened);
          }
          onOpenChange?.(isOpen);
        },
        onChange: (keys) => {
          if (Array.isArray(keys)) {
            const next = keys.map(String);
            if (pending.current === null) {
              submit(next, values2);
            } else {
              pending.current = { ...pending.current, picks: next };
              setPicks(next);
            }
          }
          onChange?.(keys);
        }
      }
    ),
    line,
    /* @__PURE__ */ jsx27(Refusal, { error: bound.error })
  ] });
}
function sameIds(a, b) {
  return a.length === b.length && a.every((id2, i) => id2 === b[i]);
}
function Drawn7({ label, description, errorMessage: errorMessage2, items, children, asIs = [], onKnown, className, ...props }) {
  const value2 = useSameValue(props.value);
  const { locale } = useLocale();
  const list = useMemo3(() => new Intl.ListFormat(locale), [locale]);
  return /* @__PURE__ */ jsxs20(
    Select,
    {
      ...props,
      value: value2,
      className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx27(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs20(Button, { className: TRIGGER, children: [
          /* @__PURE__ */ jsx27(SelectValue, { className: ({ isPlaceholder }) => isPlaceholder && asIs.length === 0 ? `${VALUE} ${PLACEHOLDER}` : VALUE, children: ({ state: state2, selectedText, defaultChildren, isPlaceholder }) => asIs.length > 0 ? list.format([...state2.selectedItems.map((item) => item.textValue), ...asIs]) : isPlaceholder ? defaultChildren : selectedText || defaultChildren }),
          /* @__PURE__ */ jsx27(Chevron, {})
        ] }),
        /* @__PURE__ */ jsx27(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx27(FieldError, { className: styles.alert, children: errorMessage2 }),
        /* @__PURE__ */ jsx27(Popover, { offset: 4, className: LIST2, children: /* @__PURE__ */ jsx27(ListBox, { renderEmptyState: () => /* @__PURE__ */ jsx27("p", { className: NOTE2, children: "No options" }), className: LISTBOX2, children: items === void 0 ? typeof children === "function" ? null : children : /* @__PURE__ */ jsx27(Collection, { items, children }) }) }),
        onKnown === void 0 ? null : /* @__PURE__ */ jsx27(Known, { onKnown })
      ]
    }
  );
}
function Known({ onKnown }) {
  const state2 = useContext6(SelectStateContext);
  useLayoutEffect3(() => {
    if (state2 === null) return;
    const { collection: collection3 } = state2;
    const ids3 = [...collection3.getKeys()].flatMap(
      (key2) => collection3.getItem(key2)?.type === "item" ? [String(key2)] : []
    );
    onKnown(ids3);
  });
  return null;
}
var TRIGGER = "bc:flex bc:min-h-[30px] bc:w-full bc:min-w-0 bc:cursor-pointer bc:items-center bc:gap-2 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-1 bc:pr-1.5 bc:pl-2 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:group-data-invalid:border-border-danger bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var VALUE = "bc:min-w-0 bc:flex-1 bc:truncate bc:group-data-invalid:text-text-danger";
var PLACEHOLDER = "bc:text-text-muted";
var LIST2 = "bc:box-border bc:min-w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
var LISTBOX2 = "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:p-1 bc:outline-none";
var NOTE2 = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/inputs/slider.tsx
import { useContext as useContext7, useLayoutEffect as useLayoutEffect4, useRef as useRef9, useState as useState10 } from "react";
import { Fragment as Fragment16, jsx as jsx28, jsxs as jsxs21 } from "react/jsx-runtime";

// srcts/inputs/switch.tsx
import { Fragment as Fragment17, jsx as jsx29, jsxs as jsxs22 } from "react/jsx-runtime";

// srcts/inputs/tags.tsx
import { createContext as createContext3, useContext as useContext8 } from "react";
import { Fragment as Fragment18, jsx as jsx30, jsxs as jsxs23 } from "react/jsx-runtime";
var ColorContext = createContext3("gray");

// srcts/inputs/textfield.tsx
import { Fragment as Fragment19, jsx as jsx31, jsxs as jsxs24 } from "react/jsx-runtime";
function TextField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx31(Drawn8, { ...props }) : /* @__PURE__ */ jsx31(BoundText, { ...props, bound });
}
function BoundText({ bound, onChange, onKeyDown, onBlur, ...props }) {
  const [text, typed] = useTyped(bound);
  const lines = props.lines ?? 1;
  return /* @__PURE__ */ jsxs24(Fragment19, { children: [
    /* @__PURE__ */ jsx31(
      Drawn8,
      {
        ...props,
        value: text ?? (typeof bound.value === "string" ? bound.value : ""),
        onChange: (value2) => {
          typed(value2);
          bound.type(value2);
          onChange?.(value2);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing && (lines <= 1 || event.ctrlKey || event.metaKey)) {
            bound.flush();
          }
          event.continuePropagation();
          onKeyDown?.(event);
        },
        onBlur: (event) => {
          bound.flush();
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx31(Refusal, { error: bound.error })
  ] });
}
function Drawn8({ label, description, errorMessage: errorMessage2, placeholder, lines = 1, className, ...props }) {
  return /* @__PURE__ */ jsxs24(TextField, { ...props, className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx31(Label, { className: styles.label, children: label }),
    lines > 1 ? /* @__PURE__ */ jsx31(TextArea, { rows: lines, placeholder, className: styles.box }) : /* @__PURE__ */ jsx31(Input, { placeholder, className: styles.box }),
    /* @__PURE__ */ jsx31(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx31(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}

// srcts/inputs/toggle.tsx
import { createContext as createContext4, useContext as useContext9, useId, useRef as useRef10 } from "react";
import { Fragment as Fragment20, jsx as jsx32, jsxs as jsxs25 } from "react/jsx-runtime";
function ToggleButton2({ bound, onChange, ...props }) {
  if (bound === void 0) return /* @__PURE__ */ jsx32(DrawnButton, { ...props, onChange });
  return /* @__PURE__ */ jsxs25(Fragment20, { children: [
    /* @__PURE__ */ jsx32(
      DrawnButton,
      {
        ...props,
        isSelected: bound.value === true,
        onChange: (selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }
      }
    ),
    /* @__PURE__ */ jsx32(Refusal, { error: bound.error })
  ] });
}
var InGroup = createContext4(false);
function DrawnButton({ className, ...props }) {
  const ref = useRef10(null);
  const inGroup = useContext9(InGroup);
  useItem(props.id === void 0 ? void 0 : String(props.id), ref);
  return /* @__PURE__ */ jsx32(
    ToggleButton,
    {
      ...props,
      ref,
      className: composeRenderProps(className, (given) => `${TOGGLE} ${inGroup ? "" : ALONE} ${given ?? ""}`)
    }
  );
}
function ToggleButtonGroup2({ bound, outdated = false, ordered = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx32(DrawnGroup, { ...props }) : /* @__PURE__ */ jsx32(BoundGroup, { ...props, bound, outdated, ordered });
}
function BoundGroup({
  bound,
  outdated,
  ordered,
  onSelectionChange,
  children,
  ...props
}) {
  const { known: known2, items } = useItems();
  const several = props.selectionMode === "multiple";
  const shown2 = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const gone = lacking(shown2, known2);
  const marking = !outdated && gone.length > 0;
  return /* @__PURE__ */ jsxs25(Fragment20, { children: [
    /* @__PURE__ */ jsx32(
      DrawnGroup,
      {
        ...props,
        selectedKeys: shown2,
        invalid: marking,
        onSelectionChange: (keys) => {
          const picks = [...keys].map(String);
          if (several) {
            const value2 = merged(strings(bound.get()), shown2, picks);
            if (value2 !== null) void bound.commit(ordered && known2 !== null ? inOrder(value2, known2) : value2);
          } else {
            const [pick2] = picks;
            void (pick2 === void 0 ? bound.unset() : bound.commit(pick2));
          }
          onSelectionChange?.(keys);
        },
        children: /* @__PURE__ */ jsx32(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx32(
      NotAvailable,
      {
        values: gone,
        onRemove: several ? () => {
          void bound.commit(held3(strings(bound.get()), known2));
        } : void 0
      }
    ) : null,
    /* @__PURE__ */ jsx32(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup({ label, description, invalid = false, children, className, ...props }) {
  const labelId = useId();
  const descriptionId = useId();
  const described = description !== void 0 && description !== "";
  const group = /* @__PURE__ */ jsx32(
    ToggleButtonGroup,
    {
      ...props,
      "aria-labelledby": props["aria-labelledby"] ?? (label === void 0 ? void 0 : labelId),
      "aria-describedby": described ? descriptionId : void 0,
      "data-invalid": invalid || void 0,
      className: composeRenderProps(className, (given) => `${GROUP} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx32(InGroup.Provider, { value: true, children })
    }
  );
  if (label === void 0 && !described) return group;
  return /* @__PURE__ */ jsxs25("div", { className: styles.field, children: [
    label === void 0 ? null : /* @__PURE__ */ jsx32("span", { id: labelId, className: styles.label, children: label }),
    group,
    described ? /* @__PURE__ */ jsx32("p", { id: descriptionId, className: styles.hint, children: description }) : null
  ] });
}
var GROUP = "bc:flex bc:w-fit bc:max-w-full bc:flex-wrap bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:p-0.5 bc:font-sans bc:data-[orientation=vertical]:flex-col bc:data-invalid:border-border-danger";
var TOGGLE = "bc:inline-flex bc:min-h-6 bc:cursor-pointer bc:items-center bc:justify-center bc:gap-1 bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-0.5 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var ALONE = "bc:rounded-md bc:border bc:border-solid bc:border-border-default bc:data-selected:border-border-accent";

// srcts/inputs/tokenfield.tsx
import { jsx as jsx33, jsxs as jsxs26 } from "react/jsx-runtime";

// srcts/layout/link.tsx
import { jsx as jsx34 } from "react/jsx-runtime";

// srcts/layout/breadcrumbs.tsx
import { Fragment as Fragment21, jsx as jsx35, jsxs as jsxs27 } from "react/jsx-runtime";

// srcts/layout/disclosure.tsx
import { useContext as useContext10 } from "react";
import { jsx as jsx36, jsxs as jsxs28 } from "react/jsx-runtime";
function Disclosure2({ className, ...props }) {
  return /* @__PURE__ */ jsx36(Disclosure, { ...props, className: composeRenderProps(className, (given) => `${DISCLOSURE} ${given ?? ""}`) });
}
function DisclosureHeader({ children, level = 3, className }) {
  const expanded = useContext10(DisclosureStateContext)?.isExpanded ?? false;
  return /* @__PURE__ */ jsx36(Heading, { level, className: `${HEADING3} ${className ?? FACE}`, children: /* @__PURE__ */ jsxs28(Button, { slot: "trigger", className: TRIGGER2, children: [
    /* @__PURE__ */ jsx36(Marker, { expanded }),
    children
  ] }) });
}
function DisclosurePanel2({ className, ...props }) {
  return /* @__PURE__ */ jsx36(DisclosurePanel, { ...props, className: composeRenderProps(className, (given) => `${PANEL} ${given ?? ""}`) });
}
function Marker({ expanded }) {
  return /* @__PURE__ */ jsx36(
    "svg",
    {
      "aria-hidden": "true",
      viewBox: "0 0 16 16",
      fill: "currentColor",
      className: `bc:size-3 bc:shrink-0 bc:transition-transform bc:motion-reduce:transition-none ${expanded ? "bc:rotate-90" : ""}`,
      children: /* @__PURE__ */ jsx36("path", { d: "M5.5 3.5 11 8l-5.5 4.5Z" })
    }
  );
}
var DISCLOSURE = "bc:font-sans bc:text-text-default";
var HEADING3 = "bc:m-0 bc:font-sans";
var FACE = "bc:text-base bc:font-semibold";
var TRIGGER2 = "bc:flex bc:w-full bc:cursor-pointer bc:items-center bc:gap-1.5 bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-left bc:text-inherit bc:[font:inherit] bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var PANEL = "bc:h-(--disclosure-panel-height) bc:overflow-clip bc:motion-safe:transition-[height]";

// srcts/layout/meter.tsx
import { Fragment as Fragment22, jsx as jsx37, jsxs as jsxs29 } from "react/jsx-runtime";

// srcts/layout/tabs.tsx
import { Fragment as Fragment23, jsx as jsx38, jsxs as jsxs30 } from "react/jsx-runtime";

// srcts/layout/toolbar.tsx
import { jsx as jsx39 } from "react/jsx-runtime";
function Toolbar2({ className, ...props }) {
  return /* @__PURE__ */ jsx39(ToggleButtonGroupContext, { value: { orientation: props.orientation }, children: /* @__PURE__ */ jsx39(
    Toolbar,
    {
      ...props,
      className: composeRenderProps(className, (given, { orientation }) => `${TOOLBAR} ${ORIENTED[orientation]} ${given ?? ""}`)
    }
  ) });
}
function Separator2({ className, ...props }) {
  return /* @__PURE__ */ jsx39(Separator, { ...props, className: `${SEPARATOR} ${ACROSS[props.orientation ?? "horizontal"]} ${className ?? ""}` });
}
var TOOLBAR = "bc:flex bc:flex-wrap bc:gap-2";
var ORIENTED = { horizontal: "bc:flex-row bc:items-center", vertical: "bc:flex-col bc:items-start" };
var SEPARATOR = "bc:m-0 bc:border-none bc:bg-border-default bc:forced-colors:bg-[ButtonBorder]";
var ACROSS = { horizontal: "bc:h-px bc:w-full", vertical: "bc:w-px bc:self-stretch" };

// srcts/react/notifications.tsx
import { jsx as jsx40 } from "react/jsx-runtime";

// srcts/dnd/dnd.tsx
import { jsx as jsx41, jsxs as jsxs31 } from "react/jsx-runtime";
function useDragAndDrop2(options) {
  return useDragAndDrop({ renderDropIndicator: (target2) => /* @__PURE__ */ jsx41(DropIndicator2, { target: target2 }), ...options });
}
function DropIndicator2(props) {
  return /* @__PURE__ */ jsx41(DropIndicator, { ...props, className: composeRenderProps(props.className, (given) => `${INDICATOR} ${given ?? ""}`) });
}
function DragButton() {
  return /* @__PURE__ */ jsx41(Button2, { slot: "drag", variant: "quiet", children: /* @__PURE__ */ jsx41(Grip, {}) });
}
function Grip() {
  return /* @__PURE__ */ jsxs31("svg", { width: "12", height: "12", viewBox: "0 0 12 12", fill: "currentColor", "aria-hidden": "true", children: [
    /* @__PURE__ */ jsx41("circle", { cx: "4", cy: "2.5", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "8", cy: "2.5", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "4", cy: "6", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "8", cy: "6", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "4", cy: "9.5", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "8", cy: "9.5", r: "1" })
  ] });
}
var INDICATOR = "bc:outline-none bc:data-drop-target:outline-1 bc:data-drop-target:outline-bg-accent bc:forced-colors:data-drop-target:outline-[Highlight]";

// srcts/gridlist/gridlist.tsx
import { Fragment as Fragment24, jsx as jsx42, jsxs as jsxs32 } from "react/jsx-runtime";
function GridList2(props) {
  return /* @__PURE__ */ jsx42(GridList, { ...props, className: composeRenderProps(props.className, (given) => `${LIST3} ${given ?? ""}`) });
}
function GridListItem2({ children, ...props }) {
  const textValue = props.textValue ?? (typeof children === "string" ? children : void 0);
  return /* @__PURE__ */ jsx42(GridListItem, { ...props, textValue, className: composeRenderProps(props.className, (given) => `${ITEM3} ${given ?? ""}`), children: composeRenderProps(children, (children2, { selectionMode, selectionBehavior, allowsDragging }) => /* @__PURE__ */ jsxs32(Fragment24, { children: [
    allowsDragging && /* @__PURE__ */ jsx42(DragButton, {}),
    selectionMode !== "none" && selectionBehavior === "toggle" && /* @__PURE__ */ jsx42(Checkbox, { slot: "selection" }),
    children2
  ] })) });
}
var LIST3 = "bc:grid bc:content-start bc:gap-0.5 bc:overflow-auto bc:font-sans bc:text-text-default bc:outline-none bc:data-[orientation=horizontal]:flex bc:data-[orientation=horizontal]:flex-row bc:data-[orientation=horizontal]:flex-nowrap bc:data-empty:px-2 bc:data-empty:py-0.5 bc:data-empty:text-sm bc:data-empty:text-text-muted bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
var ITEM3 = "bc:flex bc:shrink-0 bc:cursor-pointer bc:select-none bc:items-center bc:gap-2 bc:rounded bc:px-2 bc:py-0.5 bc:text-sm bc:text-text-default bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent/10 bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/table/table.tsx
import { jsx as jsx43, jsxs as jsxs33 } from "react/jsx-runtime";
function Table2({ className, ...props }) {
  return /* @__PURE__ */ jsx43("div", { onScroll: props.onScroll, className: `${BOX2} ${className ?? ""}`, children: /* @__PURE__ */ jsx43(Table, { ...props, className: TABLE }) });
}
function Column2({ align = "start", ...props }) {
  return /* @__PURE__ */ jsx43(
    Column,
    {
      ...props,
      className: composeRenderProps(props.className, (given) => `${COLUMN} ${align === "end" ? "bc:text-end" : "bc:text-start"} ${given ?? ""}`),
      children: composeRenderProps(props.children, (children, { sortDirection }) => /* @__PURE__ */ jsxs33("span", { className: `${HEADING4} ${align === "end" ? "bc:flex-row-reverse" : ""}`, children: [
        children,
        sortDirection === void 0 ? null : /* @__PURE__ */ jsx43(Arrow, { down: sortDirection === "descending" })
      ] }))
    }
  );
}
function TableHeader2({ columns, children, ...props }) {
  const { selectionBehavior, selectionMode, allowsDragging } = useTableOptions();
  return /* @__PURE__ */ jsxs33(TableHeader, { ...props, children: [
    allowsDragging && /* @__PURE__ */ jsx43(Column, { className: PART }),
    selectionBehavior === "toggle" && /* @__PURE__ */ jsx43(Column, { className: PART, children: selectionMode === "multiple" && /* @__PURE__ */ jsx43(Checkbox, { slot: "selection" }) }),
    /* @__PURE__ */ jsx43(Collection, { items: columns, children })
  ] });
}
function TableBody2(props) {
  return /* @__PURE__ */ jsx43(TableBody, { ...props, className: composeRenderProps(props.className, (given) => `${BODY} ${given ?? ""}`) });
}
function Row2({ id: id2, columns, children, ...props }) {
  const { selectionBehavior, allowsDragging } = useTableOptions();
  return /* @__PURE__ */ jsxs33(Row, { id: id2, ...props, className: composeRenderProps(props.className, (given) => `${ROW2} ${given ?? ""}`), children: [
    allowsDragging && /* @__PURE__ */ jsx43(Cell2, { className: "bc:w-px", children: /* @__PURE__ */ jsx43(DragButton, {}) }),
    selectionBehavior === "toggle" && /* @__PURE__ */ jsx43(Cell2, { className: "bc:w-px", children: /* @__PURE__ */ jsx43(Checkbox, { slot: "selection" }) }),
    /* @__PURE__ */ jsx43(Collection, { items: columns, children })
  ] });
}
function Cell2(props) {
  return /* @__PURE__ */ jsx43(Cell, { ...props, className: composeRenderProps(props.className, (given) => `${CELL} ${given ?? ""}`) });
}
function Arrow({ down }) {
  return /* @__PURE__ */ jsx43(
    "svg",
    {
      width: "12",
      height: "12",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      className: `bc:mb-0.5 bc:shrink-0 bc:forced-colors:text-[ButtonText] ${down ? "bc:rotate-180" : ""}`,
      children: /* @__PURE__ */ jsx43("path", { d: "M8 13V3M4 7l4-4 4 4" })
    }
  );
}
var BOX2 = "bc:min-w-0 bc:overflow-auto bc:font-sans bc:text-text-default";
var TABLE = "bc:w-full bc:border-collapse bc:text-left bc:text-sm";
var COLUMN = "bc:border-b bc:border-border-default bc:px-2 bc:py-1 bc:align-bottom bc:font-semibold bc:whitespace-nowrap bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-allows-sorting:cursor-pointer";
var HEADING4 = "bc:flex bc:items-end bc:gap-1";
var PART = "bc:w-px bc:border-b bc:border-border-default bc:px-2 bc:py-1";
var BODY = "bc:data-empty:text-center bc:data-empty:text-sm bc:data-empty:italic bc:data-empty:text-text-muted";
var ROW2 = "bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent/10 bc:data-disabled:opacity-50 bc:data-selection-mode:select-none";
var CELL = "bc:border-b bc:border-border-default/40 bc:px-2 bc:py-1 bc:whitespace-nowrap bc:outline-none bc:-outline-offset-2 bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";

// srcts/tree/navigation-tree.tsx
import { createContext as createContext5, useContext as useContext11 } from "react";

// srcts/tree/tree.tsx
import { jsx as jsx44, jsxs as jsxs34 } from "react/jsx-runtime";

// srcts/tree/navigation-tree.tsx
import { Fragment as Fragment25, jsx as jsx45, jsxs as jsxs35 } from "react/jsx-runtime";
var IndicatorContext = createContext5(void 0);
var RenderLinkContext = createContext5(void 0);

// srcts/ui/blocks.tsx
import { jsx as jsx46, jsxs as jsxs36 } from "react/jsx-runtime";
function blockItem(board, registry, id2) {
  const block = own(board.blocks, id2);
  const cls = block === void 0 ? void 0 : own(registry, block.registry_id)?.name ?? block.registry_id;
  return { id: id2, name: block?.name ?? id2, detail: cls === void 0 ? id2 : `${id2} \xB7 ${cls}` };
}
function blockTexts(item) {
  return [item.name, item.detail, item.id];
}
function blockRow(item) {
  return /* @__PURE__ */ jsxs36(ComboBoxItem, { id: item.id, textValue: item.name, children: [
    /* @__PURE__ */ jsx46(Text, { slot: "label", className: "bc:shrink-0 bc:truncate", children: item.name }),
    /* @__PURE__ */ jsx46(Text, { slot: "description", children: item.detail })
  ] });
}
function blockName(board, block) {
  return own(board.blocks, block)?.name ?? block;
}
function classInputs(board, registry, block) {
  const held4 = own(board.blocks, block);
  return (held4 === void 0 ? void 0 : own(registry, held4.registry_id)?.inputs) ?? [];
}
function isVariadicBlock(board, registry, block) {
  return classInputs(board, registry, block).includes("...");
}

// srcts/ui/edits.ts
import { useMemo as useMemo4, useState as useState11 } from "react";
function runCommand(core, target2, args) {
  const running2 = typeof target2 === "string" ? core.commands.run(target2, args) : core.commands.run(target2);
  return running2.then(
    () => true,
    () => false
  );
}
function useEdits(core) {
  const [refusal2, setRefusal] = useState11(null);
  return useMemo4(() => {
    const opts = { notifier: () => void 0 };
    function run(title, target2, given) {
      setRefusal(null);
      const running2 = typeof target2 === "string" ? core.commands.run(target2, given, "user", opts) : typeof given === "function" ? given(target2, opts) : core.commands.run(target2, "user", opts);
      return running2.then(
        () => true,
        (error) => {
          setRefusal({ title, reasons: reasons(error) });
          return false;
        }
      );
    }
    return {
      refusal: refusal2,
      run,
      clear: () => {
        setRefusal(null);
      }
    };
  }, [core, refusal2]);
}
function reasons(error) {
  const failures = error instanceof Error && "failures" in error ? error.failures : void 0;
  return isSentences(failures) ? failures : [errorMessage(error)];
}
function notifiedReasons(notifications) {
  return notifications.flatMap(({ message }) => {
    const [, ...lines] = message.split("\n");
    const failures = lines.filter((line) => line.startsWith("- "));
    return failures.length > 0 && failures.length === lines.length ? failures.map((line) => line.slice(2)) : [message];
  });
}
function isSentences(x) {
  return Array.isArray(x) && x.length > 0 && x.every((item) => typeof item === "string");
}

// srcts/ui/hooks.ts
import { useCallback as useCallback3, useSyncExternalStore as useSyncExternalStore2 } from "react";
function useHistory(core) {
  const subscribe = useCallback3(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
  const canUndo = useSyncExternalStore2(subscribe, () => core.store.canUndo());
  const canRedo = useSyncExternalStore2(subscribe, () => core.store.canRedo());
  return { canUndo, canRedo };
}

// srcts/ui/search.ts
import { useState as useState12 } from "react";
function useSearch(all, texts, none = "No options", start3 = "") {
  const { contains } = useFilter({ sensitivity: "base" });
  const [query, setQuery] = useState12(start3);
  const [started, setStarted] = useState12(start3 !== "");
  return {
    items: query === "" ? all : all.filter((item) => texts(item).some((text) => contains(text, query))),
    onInputChange: (text) => {
      setStarted(false);
      setQuery(text);
    },
    onOpenChange: (isOpen, trigger) => {
      if (isOpen && trigger === "manual" && !started) setQuery("");
    },
    renderEmptyState: () => all.length === 0 ? none : "No matches"
  };
}

// srcts/ui/table.tsx
import { useState as useState16 } from "react";

// srcts/ui/ui.tsx
import { useRef as useRef12, useState as useState15 } from "react";
import { flushSync as flushSync2 } from "react-dom";
import { createRoot as createRoot2 } from "react-dom/client";

// srcts/card/footer.tsx
import { useCallback as useCallback4, useId as useId2, useState as useState13 } from "react";
import { Fragment as Fragment26, jsx as jsx47, jsxs as jsxs37 } from "react/jsx-runtime";
var SEVERITIES2 = ["error", "warning", "message"];
function useFooter(status, notifications) {
  const shown2 = useShown(status);
  const opened = useOpened(OPEN_AT_START);
  return { status, shown: shown2, settled: isSettled(status, shown2), notifications, ...opened };
}
function usePills(notifications) {
  const opened = useOpened(NONE_OPEN);
  return { status: void 0, shown: void 0, settled: true, notifications, ...opened };
}
function useOpened(atStart) {
  const [open, setOpen] = useState13(atStart);
  const panel = useId2();
  const toggle = useCallback4((severity) => {
    setOpen((was) => {
      const next = new Set(was);
      if (!next.delete(severity)) next.add(severity);
      return next;
    });
  }, []);
  return { open, toggle, panel };
}
function Footer({ status, shown: shown2, settled: settled2 }) {
  return /* @__PURE__ */ jsx47(
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
  return /* @__PURE__ */ jsx47(
    "div",
    {
      role: "group",
      "aria-label": "Status",
      "aria-busy": status?.running === true,
      className: `bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:font-mono bc:text-xs bc:transition-opacity ${className} ${settled2 ? "" : "bc:opacity-50"}`,
      children: pills.map(({ severity, n }) => {
        const mark = MARKS[severity];
        return /* @__PURE__ */ jsxs37(
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
              /* @__PURE__ */ jsx47(SeverityIcon, { severity }),
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
  return /* @__PURE__ */ jsx47("ul", { id: panel, "aria-label": "Conditions", className: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0", children: listed.map(({ severity, message }, i) => /* @__PURE__ */ jsx47(Banner, { severity, message }, i)) });
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
  return /* @__PURE__ */ jsxs37(
    "li",
    {
      "data-severity": severity,
      className: `bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`,
      children: [
        /* @__PURE__ */ jsx47("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx47(SeverityIcon, { severity }) }),
        /* @__PURE__ */ jsxs37("span", { className: "bc:grid bc:min-w-0 bc:gap-0.5", children: [
          /* @__PURE__ */ jsx47("span", { className: `bc:font-sans bc:text-xs bc:font-semibold ${mark.text}`, children: mark.label }),
          /* @__PURE__ */ jsx47("span", { className: "bc:font-mono bc:text-xs bc:whitespace-pre-wrap", children: message })
        ] })
      ]
    }
  );
}
function SeverityIcon({ severity }) {
  return /* @__PURE__ */ jsx47(
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
      children: severity === "warning" ? /* @__PURE__ */ jsxs37(Fragment26, { children: [
        /* @__PURE__ */ jsx47("path", { d: "M8 1.8 14.6 13.6H1.4Z" }),
        /* @__PURE__ */ jsx47("path", { d: "M8 6.2v3.2M8 11.6v.1" })
      ] }) : /* @__PURE__ */ jsxs37(Fragment26, { children: [
        /* @__PURE__ */ jsx47("circle", { cx: "8", cy: "8", r: "6.4" }),
        /* @__PURE__ */ jsx47("path", { d: severity === "error" ? "M5.7 5.7l4.6 4.6M10.3 5.7l-4.6 4.6" : "M8 7.4v3.6M8 4.9v.1" })
      ] })
    }
  );
}
function useShown(status) {
  const [kept, setKept] = useState13(void 0);
  const now = status !== void 0 && status.status !== "stale" && status.status !== "unevaluated" ? status : void 0;
  if (now !== void 0 && now !== kept) setKept(now);
  return now ?? kept;
}
var OPEN_AT_START = /* @__PURE__ */ new Set(["error"]);
var NONE_OPEN = /* @__PURE__ */ new Set();

// srcts/card/mounted.tsx
import { useLayoutEffect as useLayoutEffect5, useRef as useRef11 } from "react";
import { jsx as jsx48 } from "react/jsx-runtime";
function Mounted({
  mount: mount2,
  label,
  className,
  data
}) {
  const ref = useRef11(null);
  useLayoutEffect5(() => {
    const el = ref.current;
    if (el === null) return;
    const cleanup = mount2(el);
    return () => {
      queueMicrotask(cleanup);
    };
  }, [mount2]);
  return /* @__PURE__ */ jsx48("div", { ref, role: label === void 0 ? void 0 : "region", "aria-label": label, className, ...data });
}

// srcts/card/status.ts
import { useState as useState14 } from "react";
function useSized(status) {
  const ready = status?.status === "ready";
  const [sized, setSized] = useState14(ready);
  if (ready && !sized) setSized(true);
  return sized || ready;
}

// srcts/ui/ui.tsx
import { jsx as jsx49, jsxs as jsxs38 } from "react/jsx-runtime";
var classes = {
  button: buttons.secondary,
  primary: buttons.primary,
  quiet: buttons.quiet,
  // The board's page, which draws no background of its own, so that the
  // page's, which the browser draws in the page's theme, shows to its edges.
  page: "bc:text-gray-900 bc:dark:text-gray-100",
  // What sits over the board, as a dialog or a menu does,
  // on the background that core's theme gives it, bg-raised.
  surface: "bc:bg-bg-raised bc:text-gray-900 bc:dark:text-gray-100",
  card: "bc:bg-bg-subtle",
  heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold",
  list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0"
};
function NameField({
  label,
  name: name2,
  onSubmit,
  onDone
}) {
  const [text, setText] = useState15(name2);
  const done = useRef12(false);
  function finish(submit) {
    if (done.current) return;
    done.current = true;
    const trimmed = text.trim();
    if (submit && trimmed !== "" && trimmed !== name2) onSubmit(trimmed);
    onDone();
  }
  return /* @__PURE__ */ jsx49(
    TextField2,
    {
      "aria-label": label,
      value: text,
      autoFocus: true,
      spellCheck: "false",
      className: "bc:min-w-40",
      onChange: setText,
      onFocus: (event) => {
        event.target.select();
      },
      onKeyDown: (event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) finish(true);
        else if (event.key === "Escape") finish(false);
        else event.continuePropagation();
      },
      onBlur: () => {
        finish(true);
      }
    }
  );
}
function ActionsButton({
  ref,
  label,
  onPress
}) {
  return /* @__PURE__ */ jsx49(Button2, { ref, variant: "quiet", "aria-label": label, "aria-haspopup": "menu", onPress, children: /* @__PURE__ */ jsx49("span", { "aria-hidden": "true", className: "bc:text-base bc:leading-none", children: "\u22EF" }) });
}
function drawForm(el, form) {
  const root = createRoot2(el);
  flushSync2(() => {
    root.render(form);
  });
  return () => {
    root.unmount();
  };
}
var identifier = { spellCheck: "false", autoCapitalize: "none", autoCorrect: "off" };
function Refusal2({ refusal: refusal2 }) {
  if (refusal2 === null) return null;
  const mark = MARKS.error;
  const { title, reasons: reasons2 } = refusal2;
  return /* @__PURE__ */ jsxs38(
    "div",
    {
      role: "alert",
      className: `bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`,
      children: [
        /* @__PURE__ */ jsx49("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx49(SeverityIcon, { severity: "error" }) }),
        /* @__PURE__ */ jsxs38("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-1 bc:text-sm bc:break-words", children: [
          /* @__PURE__ */ jsx49("p", { className: `bc:m-0 bc:font-semibold ${mark.text}`, children: title }),
          reasons2.length === 1 ? /* @__PURE__ */ jsx49("p", { className: "bc:m-0 bc:whitespace-pre-wrap", children: reasons2[0] }) : /* @__PURE__ */ jsx49("ul", { className: "bc:m-0 bc:list-disc bc:pl-5", children: reasons2.map((reason, i) => /* @__PURE__ */ jsx49("li", { className: "bc:whitespace-pre-wrap", children: reason }, i)) })
        ] })
      ]
    }
  );
}

// srcts/ui/table.tsx
import { jsx as jsx50, jsxs as jsxs39 } from "react/jsx-runtime";
var PAGE_SIZE = 10;
function useTableView(rows, text, size = PAGE_SIZE) {
  const [query, setQueryState] = useState16("");
  const [page, setPage] = useState16(0);
  const needle = query.trim().toLowerCase();
  const matching = needle === "" ? rows : rows.filter((row) => text(row).toLowerCase().includes(needle));
  const pages = Math.max(1, Math.ceil(matching.length / size));
  const shown2 = Math.min(page, pages - 1);
  const first = shown2 * size;
  return {
    rows: matching.slice(first, first + size),
    query,
    setQuery: (next) => {
      setQueryState(next);
      setPage(0);
    },
    page: shown2,
    pages,
    setPage,
    found: matching.length,
    first
  };
}
function TableSearch({ label, view }) {
  return /* @__PURE__ */ jsx50("search", { children: /* @__PURE__ */ jsx50(SearchField2, { label, spellCheck: "false", autoComplete: "off", value: view.query, onChange: view.setQuery }) });
}
function Pager({ label, view }) {
  const { page, pages, found, first, rows } = view;
  if (pages < 2) return null;
  return /* @__PURE__ */ jsxs39("nav", { "aria-label": label, className: "bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-text-muted", children: [
    /* @__PURE__ */ jsxs39("span", { "aria-live": "polite", children: [
      "Rows ",
      first + 1,
      "\u2013",
      first + rows.length,
      " of ",
      found
    ] }),
    /* @__PURE__ */ jsxs39("span", { className: "bc:flex bc:gap-1", children: [
      /* @__PURE__ */ jsx50(
        "button",
        {
          type: "button",
          className: classes.button,
          disabled: page === 0,
          onClick: () => {
            view.setPage(page - 1);
          },
          children: "Previous"
        }
      ),
      /* @__PURE__ */ jsx50(
        "button",
        {
          type: "button",
          className: classes.button,
          disabled: page >= pages - 1,
          onClick: () => {
            view.setPage(page + 1);
          },
          children: "Next"
        }
      )
    ] })
  ] });
}

// srcts/picker/picker.tsx
import { useMemo as useMemo5, useState as useState17 } from "react";
import { flushSync as flushSync3 } from "react-dom";
import { jsx as jsx51, jsxs as jsxs40 } from "react/jsx-runtime";
function BlockPicker({ core, mode, done, filter = "", autofocus = false }) {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const edits = useEdits(core);
  const [picked, setPicked] = useState17(void 0);
  const [input2, setInput] = useState17(void 0);
  const [inputName, setInputName] = useState17("");
  const [id2, setId] = useState17("");
  const [name2, setName] = useState17("");
  const [errors, setErrors] = useState17({});
  const [busy, setBusy] = useState17(false);
  const takes = mode.kind === "after" || mode.kind === "on";
  const options = useMemo5(() => blockClasses(Object.values(registry).filter((entry2) => !takes || entry2.inputs.length > 0)), [registry, takes]);
  const search = useSearch(
    options,
    (item) => [item.name, item.description, item.id, item.category, item.pkg ?? "", ...item.keywords],
    "No block class to pick.",
    filter
  );
  const entry = picked === void 0 ? void 0 : own(registry, picked);
  const link = linkChoice(mode, board, registry, entry);
  const chosen = link.kind === "pick" ? input2 !== void 0 && link.inputs.includes(input2) ? input2 : link.inputs[0] : void 0;
  const on = mode.kind === "on" ? own(board.links, mode.link) : void 0;
  if (mode.kind === "on" && on === void 0) return null;
  function check2() {
    return {
      ...entry === void 0 ? { registry: "Pick the class of the new block." } : {},
      ...optional2("id", blockIdError(id2, board)),
      ...link.kind === "name" ? optional2("input", inputNameError(inputName, link.taken)) : {}
    };
  }
  async function submit(event) {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check2();
    flushSync3(() => {
      setErrors(found);
      edits.clear();
    });
    if (Object.keys(found).length > 0 || entry === void 0 || busy) {
      focusFirstInvalid(form);
      return;
    }
    const args = {
      registry_id: entry.id,
      ...filled("id", id2),
      ...filled("name", name2)
    };
    const into = {
      ...chosen === void 0 ? {} : { input: chosen },
      ...link.kind === "name" ? filled("input", inputName) : {}
    };
    setBusy(true);
    await edits.run("Could not add the block.", pickedBatch(core, mode, on, args, into), done);
    setBusy(false);
  }
  return /* @__PURE__ */ jsxs40(
    Form2,
    {
      "aria-label": "Add block",
      validationBehavior: "aria",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        /* @__PURE__ */ jsx51(
          ComboBox2,
          {
            label: "Block class",
            autoFocus: autofocus,
            defaultInputValue: filter,
            ...search,
            items: byCategory(search.items),
            value: picked ?? null,
            placeholder: options.length === 0 ? "No block class to pick" : "Search by name, description or keyword",
            isInvalid: errors.registry !== void 0,
            errorMessage: errors.registry,
            description: entry?.description === "" ? void 0 : entry?.description,
            onChange: (key2) => {
              setPicked(key2 === null ? void 0 : String(key2));
              if (takes) setInput(void 0);
              setErrors((now) => without3(now, "registry"));
            },
            children: (category) => /* @__PURE__ */ jsx51(ComboBoxSection, { id: category.id, title: category.name, items: category.items, children: (item) => /* @__PURE__ */ jsxs40(ComboBoxItem, { id: item.id, textValue: item.name, children: [
              /* @__PURE__ */ jsx51(Text, { slot: "label", className: "bc:shrink-0 bc:truncate", children: item.name }),
              item.description === "" ? null : /* @__PURE__ */ jsx51(Text, { slot: "description", children: item.description }),
              item.pkg === null ? null : /* @__PURE__ */ jsx51("span", { className: BADGE, children: item.pkg })
            ] }) })
          }
        ),
        link.kind === "pick" ? /* @__PURE__ */ jsx51(
          Select2,
          {
            label: link.label,
            isRequired: true,
            items: link.inputs.map((input3) => ({ id: input3 })),
            value: chosen ?? null,
            onChange: (key2) => {
              if (key2 !== null) setInput(String(key2));
            },
            children: (item) => /* @__PURE__ */ jsx51(SelectItem, { id: item.id, children: item.id })
          }
        ) : null,
        link.kind === "name" ? /* @__PURE__ */ jsx51(
          TextField2,
          {
            label: link.label,
            value: inputName,
            description: "Optional. Leave it empty for an input without a name, which the block takes in the order of its links.",
            errorMessage: errors.input,
            isInvalid: errors.input !== void 0,
            autoComplete: "off",
            onChange: (value2) => {
              setInputName(value2);
              setErrors((now) => without3(now, "input"));
            },
            onBlur: () => {
              setErrors((now) => ({ ...now, ...optional2("input", inputNameError(inputName, link.taken)) }));
            }
          }
        ) : null,
        link.kind === "no_free_inputs" ? /* @__PURE__ */ jsx51("p", { className: styles.alert, children: link.why }) : null,
        /* @__PURE__ */ jsxs40("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx51(
            TextField2,
            {
              label: "ID",
              value: id2,
              description: "Optional. Made up where left empty.",
              errorMessage: errors.id,
              isInvalid: errors.id !== void 0,
              autoComplete: "off",
              ...identifier,
              onChange: (value2) => {
                setId(value2);
                setErrors((now) => without3(now, "id"));
              },
              onBlur: () => {
                setErrors((now) => ({ ...now, ...optional2("id", blockIdError(id2, board)) }));
              }
            }
          ),
          /* @__PURE__ */ jsx51(
            TextField2,
            {
              label: "Name",
              value: name2,
              placeholder: entry === void 0 ? void 0 : defaultBlockName(entry.id),
              description: "Optional. The class's name where left empty.",
              autoComplete: "off",
              onChange: setName
            }
          )
        ] }),
        /* @__PURE__ */ jsx51("div", { children: /* @__PURE__ */ jsx51("button", { type: "submit", className: classes.primary, disabled: busy || link.kind === "no_free_inputs", children: "Add block" }) }),
        /* @__PURE__ */ jsx51(Refusal2, { refusal: edits.refusal })
      ]
    }
  );
}
function linkChoice(mode, board, registry, entry) {
  if (mode.kind === "before") {
    const target2 = own(board.blocks, mode.to);
    const free = freeInputs(board, registry, mode.to) ?? [];
    const of = `of "${target2?.name ?? mode.to}"`;
    if (isVariadic(free)) return { kind: "name", label: `Input name ${of}`, taken: namesInto(board, mode.to) };
    if (free.length === 0) return { kind: "no_free_inputs", why: `Every input ${of} takes a link already.` };
    return free.length > 1 ? { kind: "pick", label: `Input ${of}`, inputs: free } : { kind: "none" };
  }
  if ((mode.kind === "after" || mode.kind === "on") && entry !== void 0) {
    if (entry.inputs.includes("...")) return { kind: "name", label: `Input name of the new ${entry.name}`, taken: [] };
    if (entry.inputs.length > 1) return { kind: "pick", label: `Input of the new ${entry.name}`, inputs: entry.inputs };
  }
  return { kind: "none" };
}
function pickedBatch(core, mode, on, args, into) {
  const block = typeof args.id === "string" ? args.id : core.mintId((id2) => Object.hasOwn(core.store.board().blocks, id2));
  const add = { id: "blockr.core-add_block", args: { ...args, id: block } };
  const linked = (from, to) => ({ id: "blockr.core-add_link", args: { from, to, ...into } });
  switch (mode.kind) {
    case "add":
      return [add];
    case "after":
      return [add, linked(mode.from, block)];
    case "before":
      return [add, linked(block, mode.to)];
    case "on":
      return on === void 0 ? [] : [add, linked(on.from, block), { id: "blockr.core-change_link", args: { id: mode.link, from: block } }];
  }
}
function namesInto(board, to, except) {
  return Object.entries(board.links).filter(([id2, link]) => id2 !== except && link.to === to && link.input !== "").map(([, link]) => link.input);
}
function inputNameError(value2, taken) {
  const trimmed = value2.trim();
  if (trimmed === "") return void 0;
  if (taken.includes(trimmed)) return `Another link gives the input the name "${trimmed}" already.`;
  return void 0;
}
function isVariadic(free) {
  return free.length === 1 && free[0] === "";
}
function blockClasses(entries2) {
  return entries2.map(classItem).sort(byGroupAndName);
}
function classItem(entry) {
  return {
    id: entry.id,
    name: entry.name,
    description: entry.description,
    icon: entry.icon,
    pkg: packageFromId(entry.id),
    category: entry.category === "" ? UNCATEGORIZED : entry.category,
    keywords: entry.keywords
  };
}
var UNCATEGORIZED = "uncategorized";
function byGroupAndName(a, b) {
  const last = (item) => item.category.toLowerCase() === UNCATEGORIZED ? 1 : 0;
  return last(a) - last(b) || a.category.localeCompare(b.category) || a.name.localeCompare(b.name);
}
function byCategory(items) {
  const categories = /* @__PURE__ */ new Map();
  for (const item of items) {
    const members = categories.get(item.category) ?? [];
    members.push(item);
    categories.set(item.category, members);
  }
  return [...categories].map(([name2, members]) => ({ id: `category:${name2}`, name: name2, items: members }));
}
var BADGE = "bc:ml-auto bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:text-text-muted";
function filled(arg, value2) {
  const trimmed = value2.trim();
  return trimmed === "" ? {} : { [arg]: trimmed };
}
function optional2(key2, error) {
  return error === void 0 ? {} : { [key2]: error };
}
function focusFirstInvalid(form) {
  form.querySelector('[aria-invalid="true"]')?.focus();
}
function blockIdError(value2, board) {
  return idError(value2, "block", (id2) => own(board.blocks, id2) !== void 0);
}
function idError(value2, what, taken) {
  const id2 = value2.trim();
  if (id2 === "") return void 0;
  if (/^\d+$/.test(id2)) return `An ID needs a character other than a digit, such as "${what}_${id2}".`;
  if (taken(id2)) return `The board holds a ${what} "${id2}" already.`;
  return void 0;
}
function without3(errors, key2) {
  return Object.fromEntries(Object.entries(errors).filter(([field]) => field !== key2));
}

// srcts/actions/icons.ts
var PLUS = '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.25" stroke-linecap="round" aria-hidden="true"><path d="M8 3v10M3 8h10"/></svg>';
function swatch(color2) {
  const fill = color2 === null ? 'fill="none" stroke="currentColor" stroke-width="1.25"' : `fill="${escaped(color2)}"`;
  return `<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true"><rect x="2.5" y="2.5" width="11" height="11" rx="3" ${fill}/></svg>`;
}
function escaped(color2) {
  return color2.replace(/[&<>"']/g, (char) => `&#${String(char.charCodeAt(0))};`);
}

// srcts/actions/targets.ts
function blockOf(target2) {
  return "block" in target2 ? target2.block : void 0;
}
function heldBlock(board, target2) {
  const block = blockOf(target2);
  return block !== void 0 && Object.hasOwn(board.blocks, block) ? block : void 0;
}

// srcts/actions/add.tsx
import { jsx as jsx52 } from "react/jsx-runtime";
function addingBoardActions(registry) {
  const adds = (taking2) => Object.values(registry()).some((entry) => !taking2 || hasInput(entry));
  return [
    {
      id: "blockr.core-add_block_action",
      label: "Add block",
      icon: PLUS,
      on: ["board"],
      category: "add",
      offers: (_board, _target, lock) => !lock.board && adds(false),
      quick: (_board, _target, core) => classLevel(core, false, (entry, block) => [addBlockCall(entry, block)]),
      advanced: pickerModule(() => ({ kind: "add" }))
    },
    {
      id: "blockr.core-append_block_action",
      label: "Append block",
      icon: PLUS,
      on: ["block"],
      category: "add",
      offers: (board, target2, lock) => !lock.board && heldBlock(board, target2) !== void 0 && adds(true),
      quick: (_board, target2, core) => classLevel(core, true, (entry, block, board) => {
        const from = heldBlock(board, target2);
        return from === void 0 ? [] : [addBlockCall(entry, block), addLinkCall(from, block, newBlockInput(entry))];
      }),
      advanced: pickerModule((target2) => "block" in target2 ? { kind: "after", from: target2.block } : void 0)
    },
    {
      id: "blockr.core-prepend_block_action",
      label: "Prepend block",
      icon: PLUS,
      on: ["block"],
      category: "add",
      offers: (board, target2, lock) => !lock.board && freeInputOf(board, registry(), target2) !== void 0 && adds(false),
      quick: (_board, target2, core) => classLevel(core, false, (entry, block, board) => {
        const to = heldBlock(board, target2);
        return to === void 0 ? [] : [addBlockCall(entry, block), addLinkCall(block, to, freeInputOf(board, core.blockRegistry(), target2))];
      }),
      advanced: pickerModule((target2) => "block" in target2 ? { kind: "before", to: target2.block } : void 0)
    },
    {
      id: "blockr.core-insert_block_action",
      label: "Insert block",
      icon: PLUS,
      on: ["link"],
      category: "add",
      offers: (board, target2, lock) => !lock.board && "link" in target2 && Object.hasOwn(board.links, target2.link) && adds(true),
      quick: (_board, target2, core) => classLevel(core, true, (entry, block, board) => {
        const id2 = "link" in target2 ? target2.link : void 0;
        const link = id2 === void 0 ? void 0 : own(board.links, id2);
        return id2 === void 0 || link === void 0 ? [] : [
          addBlockCall(entry, block),
          addLinkCall(link.from, block, newBlockInput(entry)),
          { id: "blockr.core-change_link", args: { id: id2, from: block } }
        ];
      }),
      advanced: pickerModule((target2) => "link" in target2 ? { kind: "on", link: target2.link } : void 0)
    }
  ];
}
function hasInput(entry) {
  return entry.inputs.length > 0;
}
function classLevel(core, taking2, batch2) {
  const registry = core.blockRegistry();
  const entries2 = Object.values(registry).filter((entry) => !taking2 || hasInput(entry));
  return {
    filter: true,
    rows: blockClasses(entries2).map((item) => ({
      kind: "item",
      label: item.name,
      heading: item.category,
      ...item.icon === "" ? {} : { icon: item.icon },
      ...item.pkg === null ? {} : { detail: item.pkg },
      pick() {
        const entry = own(registry, item.id);
        const board = core.store.board();
        if (entry === void 0) return [];
        return batch2(entry, core.mintId((id2) => Object.hasOwn(board.blocks, id2)), board);
      }
    }))
  };
}
function addBlockCall(entry, block) {
  return { id: "blockr.core-add_block", args: { registry_id: entry.id, id: block } };
}
function addLinkCall(from, to, input2) {
  return { id: "blockr.core-add_link", args: { from, to, ...input2 === void 0 ? {} : { input: input2 } } };
}
function newBlockInput(entry) {
  return entry.inputs.includes("...") ? "" : entry.inputs[0];
}
function freeInputOf(board, registry, target2) {
  const block = heldBlock(board, target2);
  return block === void 0 ? void 0 : freeInputs(board, registry, block)?.[0];
}
function pickerModule(mode) {
  const focused = /* @__PURE__ */ new WeakSet();
  return {
    render({ el, core, target: target2, filter, done }) {
      const at = mode(target2);
      if (at === void 0) return () => void 0;
      const autofocus = filter !== void 0 || focused.delete(el);
      return drawForm(
        el,
        /* @__PURE__ */ jsx52(
          BlockPicker,
          {
            core,
            mode: at,
            ...filter === void 0 ? {} : { filter },
            autofocus,
            done: (calls, opts) => {
              if (el.contains(el.ownerDocument.activeElement)) focused.add(el);
              return done(calls, opts).finally(() => {
                focused.delete(el);
              });
            }
          }
        )
      );
    }
  };
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

// srcts/actions/inputs.tsx
import { useState as useState18 } from "react";
import { Fragment as Fragment27, jsx as jsx53, jsxs as jsxs41 } from "react/jsx-runtime";
var editInputs = {
  render({ el, core, target: target2 }) {
    if (!("block" in target2)) return void 0;
    return drawForm(el, /* @__PURE__ */ jsx53(EditInputs, { core, block: target2.block }));
  }
};
function EditInputs({ core, block }) {
  const board = useBoard(core);
  const edits = useEdits(core);
  if (own(board.blocks, block) === void 0) return null;
  const registry = core.blockRegistry();
  const inputs = classInputs(board, registry, block);
  const parts = { edits, board, registry, block, sources: sourcesOf(board, registry, block) };
  const variadic2 = isVariadicBlock(board, registry, block);
  const name2 = blockName(board, block);
  return /* @__PURE__ */ jsxs41("div", { className: "bc:grid bc:gap-3 bc:font-sans bc:text-sm bc:text-text-default", children: [
    /* @__PURE__ */ jsx53("p", { className: styles.hint, children: variadic2 ? `The inputs of the block "${name2}", which it takes in this order.` : `The inputs of the block "${name2}".` }),
    variadic2 ? /* @__PURE__ */ jsx53(VariadicInputs, { ...parts }) : /* @__PURE__ */ jsx53(FixedInputs, { ...parts, inputs }),
    /* @__PURE__ */ jsx53(Refusal2, { refusal: edits.refusal })
  ] });
}
function sourcesOf(board, registry, block) {
  const downstream = createGraph(board).withDownstream([block]);
  return Object.keys(board.blocks).filter((id2) => !downstream.has(id2)).map((id2) => blockItem(board, registry, id2));
}
function FixedInputs({ inputs, ...parts }) {
  return /* @__PURE__ */ jsx53("div", { className: "bc:grid bc:gap-2", children: inputs.map((input2) => /* @__PURE__ */ jsx53(FixedInput, { ...parts, input: input2 }, input2)) });
}
function FixedInput({ edits, board, registry, block, sources, input: input2 }) {
  const search = useSearch(sources, blockTexts, "No block can feed the input.");
  const id2 = linkFeeding(board, registry, block, input2);
  const link = id2 === void 0 ? void 0 : own(board.links, id2);
  const what = `the input "${input2}"`;
  return /* @__PURE__ */ jsxs41("div", { className: "bc:flex bc:items-end bc:gap-2", children: [
    /* @__PURE__ */ jsx53(
      ComboBox2,
      {
        label: input2,
        ...search,
        value: link?.from ?? null,
        placeholder: "Search blocks",
        isRequired: link !== void 0,
        className: "bc:min-w-0 bc:flex-1",
        onChange: (key2) => {
          if (key2 === null) return;
          const from = String(key2);
          const title = `Could not change the source of ${what}.`;
          if (id2 === void 0 || link === void 0) {
            void edits.run(title, "blockr.core-add_link", { from, to: block, input: input2 });
          } else if (from !== link.from) {
            void edits.run(title, "blockr.core-change_link", { id: id2, from });
          }
        },
        children: blockRow
      }
    ),
    /* @__PURE__ */ jsx53("div", { className: "bc:flex bc:w-7 bc:shrink-0 bc:justify-center bc:pb-1", children: id2 === void 0 ? null : /* @__PURE__ */ jsx53(
      RemoveButton,
      {
        label: `Remove the source of ${input2}`,
        onPress: () => {
          void edits.run(`Could not remove the source of ${what}.`, "blockr.core-remove_link", { id: id2 });
        }
      }
    ) })
  ] });
}
function VariadicInputs(parts) {
  const { edits, board, block } = parts;
  const order = createGraph(board).linksInto(block);
  const rows = order.flatMap((id2, i) => {
    const link = own(board.links, id2);
    return link === void 0 ? [] : [{ id: id2, link, place: i + 1 }];
  });
  const { dragAndDropHooks } = useDragAndDrop2({
    getItems: (keys) => [...keys].map((key2) => ({ "text/plain": String(key2) })),
    onReorder(event) {
      const [moved] = [...event.keys];
      if (moved === void 0) return;
      const id2 = String(moved);
      const before = placeBefore(order, id2, String(event.target.key), event.target.dropPosition);
      if (before === null) return;
      void edits.run(`Could not move input ${String(order.indexOf(id2) + 1)}.`, "blockr.core-move_link", {
        id: id2,
        ...before === void 0 ? {} : { before }
      });
    }
  });
  return /* @__PURE__ */ jsxs41(Fragment27, { children: [
    /* @__PURE__ */ jsx53(
      GridList2,
      {
        "aria-label": `Inputs of ${blockName(board, block)}`,
        items: rows,
        dragAndDropHooks,
        keyboardNavigationBehavior: "tab",
        renderEmptyState: () => "No link feeds the block yet.",
        className: "bc:gap-1.5",
        children: (row) => /* @__PURE__ */ jsx53(GridListItem2, { id: row.id, textValue: inputText(board, row.link, row.place), className: ROW3, children: /* @__PURE__ */ jsx53(VariadicInput, { ...parts, id: row.id, link: row.link, place: row.place }) })
      }
    ),
    /* @__PURE__ */ jsx53(AddInput, { ...parts })
  ] });
}
function placeBefore(order, moved, target2, position) {
  const others = order.filter((id2) => id2 !== moved);
  const at = others.indexOf(target2);
  if (moved === target2 || position === "on" || at === -1) return null;
  const index = position === "before" ? at : at + 1;
  const next = [...others.slice(0, index), moved, ...others.slice(index)];
  if (next.every((id2, i) => id2 === order[i])) return null;
  return others[index];
}
function inputText(board, link, place) {
  const from = blockName(board, link.from);
  return link.input === "" ? `Input ${String(place)}, from ${from}` : `Input ${String(place)}, ${link.input}, from ${from}`;
}
function VariadicInput({ edits, board, block, sources, id: id2, link, place }) {
  const search = useSearch(sources, blockTexts, "No block can feed the block.");
  const what = `input ${String(place)}`;
  return /* @__PURE__ */ jsxs41("div", { className: "bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2", onPointerDownCapture: holdStill, children: [
    /* @__PURE__ */ jsx53(
      ComboBox2,
      {
        "aria-label": `Source of ${what}`,
        ...search,
        value: link.from,
        placeholder: "Search blocks",
        isRequired: true,
        className: "bc:min-w-0 bc:flex-1",
        onChange: (key2) => {
          if (key2 === null || String(key2) === link.from) return;
          void edits.run(`Could not change the source of ${what}.`, "blockr.core-change_link", { id: id2, from: String(key2) });
        },
        children: blockRow
      }
    ),
    /* @__PURE__ */ jsx53(InputName, { edits, board, block, id: id2, link, what }),
    /* @__PURE__ */ jsx53(
      RemoveButton,
      {
        label: `Remove ${what}`,
        onPress: () => {
          void edits.run(`Could not remove ${what}.`, "blockr.core-remove_link", { id: id2 });
        }
      }
    )
  ] });
}
function holdStill(event) {
  const item = event.currentTarget.closest('[role="row"][draggable="true"]');
  if (item === null) return;
  item.setAttribute("draggable", "false");
  const doc = item.ownerDocument;
  const release = () => {
    item.setAttribute("draggable", "true");
    doc.removeEventListener("pointerup", release, true);
    doc.removeEventListener("pointercancel", release, true);
  };
  doc.addEventListener("pointerup", release, true);
  doc.addEventListener("pointercancel", release, true);
}
function InputName({
  edits,
  board,
  block,
  id: id2,
  link,
  what
}) {
  const [text, setText] = useState18(link.input);
  const [error, setError] = useState18(void 0);
  const [held4, setHeld] = useState18(link.input);
  if (held4 !== link.input) {
    setHeld(link.input);
    setText(link.input);
    setError(void 0);
  }
  function submit() {
    const name2 = text.trim();
    const found = inputNameError(name2, namesInto(board, block, id2));
    setError(found);
    if (found !== void 0) {
      edits.clear();
      return;
    }
    if (name2 !== link.input) void edits.run(`Could not rename ${what}.`, "blockr.core-change_link", { id: id2, input: name2 });
  }
  return /* @__PURE__ */ jsx53(
    TextField2,
    {
      "aria-label": `Name of ${what}`,
      value: text,
      placeholder: "No name",
      isInvalid: error !== void 0,
      errorMessage: error,
      autoComplete: "off",
      ...identifier,
      className: "bc:w-1/3 bc:shrink-0",
      onChange: (value2) => {
        setText(value2);
        setError(void 0);
      },
      onKeyDown: (event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) submit();
        else event.continuePropagation();
      },
      onBlur: submit
    }
  );
}
function AddInput({ edits, board, block, sources }) {
  const search = useSearch(sources, blockTexts, "No block can feed the block.");
  const [picked, setPicked] = useState18(null);
  return /* @__PURE__ */ jsx53(
    ComboBox2,
    {
      label: "Add input",
      ...search,
      value: picked,
      placeholder: "Search blocks",
      onChange: (key2) => {
        if (key2 === null) return;
        const from = String(key2);
        setPicked(from);
        void edits.run(`Could not add an input from "${blockName(board, from)}".`, "blockr.core-add_link", { from, to: block }).then(() => {
          setPicked(null);
        });
      },
      children: blockRow
    }
  );
}
function RemoveButton({ label, onPress }) {
  return /* @__PURE__ */ jsx53(Button2, { variant: "quiet", "aria-label": label, onPress, className: "bc:size-7 bc:shrink-0 bc:justify-center bc:p-0", children: /* @__PURE__ */ jsx53("span", { title: label, className: "bc:flex bc:size-full bc:items-center bc:justify-center", children: /* @__PURE__ */ jsx53(Cross2, {}) }) });
}
function Cross2() {
  return /* @__PURE__ */ jsx53("svg", { width: "10", height: "10", viewBox: "0 0 10 10", fill: "none", stroke: "currentColor", strokeWidth: "1.4", strokeLinecap: "round", "aria-hidden": "true", children: /* @__PURE__ */ jsx53("path", { d: "M2.5 2.5l5 5M7.5 2.5l-5 5" }) });
}
var ROW3 = "bc:rounded-md bc:border bc:border-border-default bc:bg-bg-subtle bc:py-1 bc:data-dragging:opacity-60";

// srcts/actions/link-form.tsx
import { useState as useState19 } from "react";
import { flushSync as flushSync4 } from "react-dom";
import { jsx as jsx54, jsxs as jsxs42 } from "react/jsx-runtime";
var ADD_LINK = "blockr.core-add_link";
var CHANGE_LINK = "blockr.core-change_link";
var MOVE_LINK = "blockr.core-move_link";
var createLinkForm = {
  render({ el, core, target: target2, picked, done }) {
    const start3 = picked === void 0 ? "block" in target2 ? { from: target2.block } : {} : startOf(picked);
    return drawForm(el, /* @__PURE__ */ jsx54(AddLink, { core, start: start3, done }));
  }
};
var editLinkForm = {
  render({ el, core, target: target2, done }) {
    if (!("link" in target2)) return void 0;
    return drawForm(el, /* @__PURE__ */ jsx54(EditLink, { core, id: target2.link, done }));
  }
};
function startOf(picked) {
  if (!isObject(picked)) return {};
  const { from, to, input: input2 } = picked;
  return {
    ...typeof from === "string" ? { from } : {},
    ...typeof to === "string" ? { to } : {},
    ...typeof input2 === "string" ? { input: input2 } : {}
  };
}
function AddLink({ core, start: start3, done }) {
  const board = useBoard(core);
  return /* @__PURE__ */ jsx54(LinkForm, { core, board, start: start3, done });
}
function EditLink({ core, id: id2, done }) {
  const board = useBoard(core);
  const link = own(board.links, id2);
  return link === void 0 ? null : /* @__PURE__ */ jsx54(LinkForm, { core, board, editing: { id: id2, link }, done });
}
function LinkForm({
  core,
  board,
  start: start3 = {},
  editing,
  done
}) {
  const registry = core.blockRegistry();
  const except = editing?.id;
  const [from, setFrom] = useState19(editing?.link.from ?? start3.from);
  const [to, setTo] = useState19(editing?.link.to ?? start3.to);
  const [input2, setInput] = useState19(editing?.link.input ?? start3.input);
  const [name2, setName] = useState19(editing !== void 0 && isVariadicBlock(board, registry, editing.link.to) ? editing.link.input : "");
  const [place, setPlace] = useState19(void 0);
  const [errors, setErrors] = useState19({});
  const [busy, setBusy] = useState19(false);
  const edits = useEdits(core);
  const others = withoutLink(board, except);
  const held4 = (block) => block !== void 0 && own(board.blocks, block) !== void 0;
  const source = held4(from) ? from : void 0;
  const target2 = held4(to) ? to : void 0;
  const { fromOptions, toOptions } = ends(others, registry, source, target2);
  const fromSearch = useSearch(fromOptions, blockTexts);
  const toSearch = useSearch(toOptions, blockTexts);
  const free = target2 === void 0 ? [] : freeInputs(others, registry, target2) ?? [];
  const variadic2 = isVariadic(free);
  const offered = !variadic2 && editing !== void 0 && target2 === editing.link.to ? classInputs(board, registry, target2) : free;
  const chosen = variadic2 ? void 0 : input2 !== void 0 && offered.includes(input2) ? input2 : free[0];
  const hint = editing !== void 0 && target2 === editing.link.to && chosen !== void 0 ? swapHint(board, registry, editing, chosen) : void 0;
  const taken = target2 === void 0 ? [] : namesInto(board, target2, except);
  const among = editing === void 0 || target2 === void 0 || !variadic2 ? void 0 : placeAmong(board, editing.id, target2);
  const position = among === void 0 ? void 0 : place !== void 0 && place <= among.others.length + 1 ? place : among.place;
  function check2() {
    return {
      ...source === void 0 ? { from: "Pick the block that the link comes from." } : {},
      ...target2 === void 0 ? { to: "Pick the block that the link goes to." } : {},
      ...variadic2 ? optional3("input", inputNameError(name2, taken)) : {}
    };
  }
  function batch2(source2, target3) {
    const linkInput = variadic2 ? name2.trim() : chosen;
    const link = { from: source2, to: target3, ...linkInput === void 0 ? {} : { input: linkInput } };
    if (editing === void 0) {
      return [{ id: ADD_LINK, args: { id: core.mintId((minted) => Object.hasOwn(core.store.board().links, minted)), ...link } }];
    }
    const changed = [{ id: CHANGE_LINK, args: { id: editing.id, ...link } }];
    if (among === void 0 || position === void 0 || position === among.place) return changed;
    const before = among.others[position - 1];
    return [...changed, { id: MOVE_LINK, args: { id: editing.id, ...before === void 0 ? {} : { before } } }];
  }
  async function submit(event) {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check2();
    flushSync4(() => {
      setErrors(found);
      edits.clear();
    });
    if (Object.keys(found).length > 0 || source === void 0 || target2 === void 0 || busy) {
      focusFirstInvalid(form);
      return;
    }
    const title = editing === void 0 ? "Could not add the link." : `Could not change the link from "${blockName(board, editing.link.from)}" to "${blockName(board, editing.link.to)}".`;
    const calls = batch2(source, target2);
    setBusy(true);
    await edits.run(title, calls, done);
    setBusy(false);
  }
  return /* @__PURE__ */ jsxs42(
    Form2,
    {
      "aria-label": editing === void 0 ? "Add link" : "Change link",
      validationBehavior: "aria",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        /* @__PURE__ */ jsxs42("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx54(
            ComboBox2,
            {
              label: "From",
              ...fromSearch,
              value: source ?? null,
              placeholder: "Search blocks",
              isInvalid: errors.from !== void 0,
              errorMessage: errors.from,
              onChange: (key2) => {
                setFrom(key2 === null ? void 0 : String(key2));
                setErrors((now) => without3(now, "from"));
              },
              children: blockRow
            }
          ),
          /* @__PURE__ */ jsx54(
            ComboBox2,
            {
              label: "To",
              ...toSearch,
              value: target2 ?? null,
              placeholder: "Search blocks",
              isInvalid: errors.to !== void 0,
              errorMessage: errors.to,
              description: toOptions.length === 0 ? "No block takes a link from this block." : void 0,
              onChange: (key2) => {
                setTo(key2 === null ? void 0 : String(key2));
                setInput(void 0);
                setPlace(void 0);
                setErrors((now) => without3(without3(now, "to"), "input"));
              },
              children: blockRow
            }
          )
        ] }),
        target2 === void 0 ? null : variadic2 ? /* @__PURE__ */ jsxs42("div", { className: among === void 0 ? "bc:grid" : "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx54(
            TextField2,
            {
              label: "Input name",
              value: name2,
              description: `Optional. The block "${blockName(board, target2)}" takes any number of links, each by its name or, where it has none, by its position.`,
              errorMessage: errors.input,
              isInvalid: errors.input !== void 0,
              autoComplete: "off",
              ...identifier,
              onChange: (value2) => {
                setName(value2);
                setErrors((now) => without3(now, "input"));
              },
              onBlur: () => {
                setErrors((now) => ({ ...now, ...optional3("input", inputNameError(name2, taken)) }));
              }
            }
          ),
          among === void 0 ? null : /* @__PURE__ */ jsx54(
            Select2,
            {
              label: "Position",
              isRequired: true,
              items: Array.from({ length: among.others.length + 1 }, (_, i) => ({ id: String(i + 1) })),
              value: position === void 0 ? null : String(position),
              isDisabled: among.others.length === 0,
              onChange: (key2) => {
                if (key2 !== null) setPlace(Number(key2));
              },
              children: (item) => /* @__PURE__ */ jsx54(SelectItem, { id: item.id, children: item.id })
            }
          )
        ] }) : /* @__PURE__ */ jsx54(
          Select2,
          {
            label: "Input",
            isRequired: true,
            items: offered.map((id2) => ({ id: id2 })),
            value: chosen ?? null,
            isDisabled: offered.length < 2,
            description: hint,
            onChange: (key2) => {
              if (key2 !== null) setInput(String(key2));
            },
            children: (item) => /* @__PURE__ */ jsx54(SelectItem, { id: item.id, children: item.id })
          }
        ),
        editing === void 0 && Object.keys(board.blocks).length < 2 ? /* @__PURE__ */ jsx54("p", { className: styles.hint, children: "A link joins two blocks, so the board needs two blocks first." }) : null,
        /* @__PURE__ */ jsx54("div", { className: "bc:flex bc:gap-2", children: /* @__PURE__ */ jsx54("button", { type: "submit", className: classes.primary, disabled: busy, children: editing === void 0 ? "Add link" : "Save" }) }),
        /* @__PURE__ */ jsx54(Refusal2, { refusal: edits.refusal })
      ]
    }
  );
}
function ends(board, registry, source, target2) {
  const graph = createGraph(board);
  const upstream = source === void 0 ? /* @__PURE__ */ new Set() : graph.withUpstream([source]);
  const downstream = target2 === void 0 ? /* @__PURE__ */ new Set() : graph.withDownstream([target2]);
  const ids3 = Object.keys(board.blocks);
  return {
    fromOptions: ids3.filter((id2) => !downstream.has(id2)).map((id2) => blockItem(board, registry, id2)),
    toOptions: ids3.filter((id2) => !upstream.has(id2) && takesLink(board, registry, id2)).map((id2) => blockItem(board, registry, id2))
  };
}
function swapHint(board, registry, editing, input2) {
  const other = linkFeeding(board, registry, editing.link.to, input2, editing.id);
  const from = other === void 0 ? void 0 : own(board.links, other)?.from;
  if (from === void 0) return void 0;
  return `The link from "${blockName(board, from)}" feeds "${input2}". Saving gives that link "${editing.link.input}", so the two links swap their inputs.`;
}
function placeAmong(board, id2, to) {
  const ids3 = Object.keys(board.links).filter((other) => other === id2 || own(board.links, other)?.to === to);
  return { others: ids3.filter((other) => other !== id2), place: ids3.indexOf(id2) + 1 };
}
function withoutLink(board, id2) {
  if (id2 === void 0 || !Object.hasOwn(board.links, id2)) return board;
  const links = Object.fromEntries(Object.entries(board.links).filter(([other]) => other !== id2));
  return { ...board, links };
}
function optional3(key2, error) {
  return error === void 0 ? {} : { [key2]: error };
}

// srcts/actions/links.ts
var CREATE_LINK = "blockr.core-add_link_action";
var EDIT_LINK = "blockr.core-edit_link_action";
var ADD_LINK2 = "blockr.core-add_link";
var CHANGE_LINK2 = "blockr.core-change_link";
function createLinkAction(parts) {
  return {
    id: CREATE_LINK,
    label: "Create link",
    icon: PLUS,
    on: ["block", "board"],
    category: "add",
    offers(board, target2, lock) {
      if (lock.board) return false;
      const joining = joiningOn(board, parts);
      if ("board" in target2) return [...joining.takers].some((to) => fedBy(joining, to, true).length > 0);
      const block = heldBlock(board, target2);
      return block !== void 0 && (feeds(joining, block, true).length > 0 || fedBy(joining, block, true).length > 0);
    },
    quick(board, target2, core) {
      const block = heldBlock(board, target2);
      if (block === void 0) return void 0;
      const joining = joiningOn(board, parts);
      const rows = [
        ...feeds(joining, block).map(
          (to) => ({ ...quickBlock(board, joining.registry, to), heading: "Send to", pick: () => newLink(core, parts, block, to) })
        ),
        ...fedBy(joining, block).map(
          (from) => ({ ...quickBlock(board, joining.registry, from), heading: "Take from", pick: () => newLink(core, parts, from, block) })
        )
      ];
      return rows.length === 0 ? void 0 : { filter: true, rows };
    },
    advanced: createLinkForm
  };
}
function editLinkAction(parts) {
  return {
    id: EDIT_LINK,
    label: "Edit link",
    on: ["link"],
    category: "edit",
    offers: (board, target2, lock) => !lock.board && heldLink(board, target2) !== void 0,
    quick(board, target2) {
      const held4 = heldLink(board, target2);
      if (held4 === void 0) return void 0;
      const [id2, link] = held4;
      const registry = parts.blockRegistry();
      const into = blockName(board, link.to);
      const rows = [];
      const sources = sourceRows(board, registry, parts, id2, link);
      if (sources.length > 0) {
        rows.push(levelItem("Change source\u2026", { title: `Feed ${into} from`, filter: true, rows: sources }));
      }
      if (isVariadicBlock(board, registry, link.to)) {
        rows.push(levelItem("Rename input\u2026", { title: `Rename input of ${into}`, rows: [renameRow(board, id2, link)] }));
      } else {
        const inputs = inputRows(board, registry, parts, id2, link);
        if (inputs.length > 0) rows.push(levelItem("Move to input\u2026", { title: `Move to which input of ${into}?`, rows: inputs }));
      }
      return rows.length === 0 ? void 0 : { rows };
    },
    advanced: editLinkForm
  };
}
function newLink(core, parts, from, to) {
  const board = core.store.board();
  const free = freeInputs(board, parts.blockRegistry(), to) ?? [];
  const adding = (input2) => [
    {
      id: ADD_LINK2,
      args: { id: core.mintId((id2) => Object.hasOwn(core.store.board().links, id2)), from, to, ...input2 === void 0 ? {} : { input: input2 } }
    }
  ];
  if (free.length < 2) return adding();
  return {
    title: `Into which input of ${blockName(board, to)}?`,
    picked: { from, to },
    rows: free.filter((input2) => parts.preflight(ADD_LINK2, { from, to, input: input2 })).map((input2) => ({ kind: "item", label: input2, pick: () => adding(input2) }))
  };
}
function joiningOn(board, parts) {
  const registry = parts.blockRegistry();
  return {
    board,
    registry,
    graph: createGraph(board),
    takers: new Set(Object.keys(board.blocks).filter((block) => takesLink(board, registry, block))),
    preflight: parts.preflight
  };
}
function feeds(joining, block, first = false) {
  const upstream = joining.graph.withUpstream([block]);
  return passing(
    [...joining.takers].filter((to) => !upstream.has(to)),
    (to) => joining.preflight(ADD_LINK2, { from: block, to }),
    first
  );
}
function fedBy(joining, block, first = false) {
  if (!joining.takers.has(block)) return [];
  const downstream = joining.graph.withDownstream([block]);
  return passing(
    Object.keys(joining.board.blocks).filter((from) => !downstream.has(from)),
    (from) => joining.preflight(ADD_LINK2, { from, to: block }),
    first
  );
}
function passing(blocks, check2, first) {
  if (!first) return blocks.filter(check2);
  const found = blocks.find(check2);
  return found === void 0 ? [] : [found];
}
function sourceRows(board, registry, parts, id2, link) {
  const downstream = createGraph(board).withDownstream([link.to]);
  return Object.keys(board.blocks).filter((from) => from !== link.from && !downstream.has(from) && parts.preflight(CHANGE_LINK2, { id: id2, from })).map((from) => ({ ...quickBlock(board, registry, from), pick: () => [{ id: CHANGE_LINK2, args: { id: id2, from } }] }));
}
function inputRows(board, registry, parts, id2, link) {
  return classInputs(board, registry, link.to).filter((input2) => input2 !== link.input && parts.preflight(CHANGE_LINK2, { id: id2, input: input2 })).map((input2) => {
    const other = linkFeeding(board, registry, link.to, input2, id2);
    const from = other === void 0 ? void 0 : own(board.links, other)?.from;
    return {
      kind: "item",
      label: input2,
      ...from === void 0 ? {} : { detail: `swaps with ${blockName(board, from)}` },
      pick: () => [{ id: CHANGE_LINK2, args: { id: id2, input: input2 } }]
    };
  });
}
function renameRow(board, id2, link) {
  const taken = namesInto(board, link.to, id2);
  return {
    kind: "text",
    label: "Name",
    value: link.input,
    check: (value2) => inputNameError(value2, taken),
    submit(value2) {
      const input2 = value2.trim();
      return input2 === link.input ? [] : [{ id: CHANGE_LINK2, args: { id: id2, input: input2 } }];
    }
  };
}
function quickBlock(board, registry, block) {
  const icon = own(registry, own(board.blocks, block)?.registry_id ?? "")?.icon;
  return { kind: "item", label: blockName(board, block), detail: block, ...icon === void 0 || icon === "" ? {} : { icon } };
}
function heldLink(board, target2) {
  if (!("link" in target2)) return void 0;
  const link = own(board.links, target2.link);
  return link === void 0 ? void 0 : [target2.link, link];
}

// srcts/actions/stack-form.tsx
import { useState as useState20 } from "react";
import { jsx as jsx55, jsxs as jsxs43 } from "react/jsx-runtime";
var ADD_STACK = "blockr.core-add_stack";
var CHANGE_STACK = "blockr.core-change_stack";
var RENAME_STACK = "blockr.core-rename_stack";
var PALETTE = ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"];
function suggestedColor(stacks) {
  const used = new Set(stacks.map((stack) => stack.color?.toLowerCase()));
  return PALETTE.find((color2) => !used.has(color2)) ?? PALETTE[stacks.length % PALETTE.length] ?? PALETTE[0];
}
function suggestedName(stacks) {
  const names = new Set(stacks.map((stack) => stack.name));
  let n = 1;
  while (names.has(`Stack ${String(n)}`)) n++;
  return `Stack ${String(n)}`;
}
var createStackForm = {
  render({ el, core, target: target2, done }) {
    return drawForm(el, /* @__PURE__ */ jsx55(AddStack, { core, given: stackedBlocks(core.store.board(), target2), done }));
  }
};
var editStackForm = {
  render({ el, core, target: target2, done }) {
    if (!("stack" in target2)) return void 0;
    return drawForm(el, /* @__PURE__ */ jsx55(EditStack, { core, id: target2.stack, done }));
  }
};
function stackedBlocks(board, target2) {
  if ("block" in target2) return Object.hasOwn(board.blocks, target2.block) ? [target2.block] : [];
  if (!("selection" in target2)) return [];
  return target2.selection.flatMap((object2) => "block" in object2 && Object.hasOwn(board.blocks, object2.block) ? [object2.block] : []);
}
function AddStack({ core, given, done }) {
  const board = useBoard(core);
  return /* @__PURE__ */ jsx55(StackForm, { core, board, given, done });
}
function EditStack({ core, id: id2, done }) {
  const board = useBoard(core);
  const stack = own(board.stacks, id2);
  return stack === void 0 ? null : /* @__PURE__ */ jsx55(StackForm, { core, board, editing: { id: id2, stack }, done });
}
function stackColor(color2) {
  return typeof color2 === "string" ? color2 : color2.toString("hex");
}
function sameColor(a, b) {
  return a === null || b === null ? a === b : a.toLowerCase() === b.toLowerCase();
}
function StackForm({
  core,
  board,
  given = [],
  editing,
  done
}) {
  const registry = core.blockRegistry();
  const stacks = Object.values(board.stacks);
  const [name2, setName] = useState20(editing?.stack.name ?? "");
  const [color2, setColor] = useState20(() => editing === void 0 ? suggestedColor(stacks) : editing.stack.color);
  const [blocks, setBlocks] = useState20(editing?.stack.blocks ?? given);
  const [busy, setBusy] = useState20(false);
  const edits = useEdits(core);
  const fallbackName = editing === void 0 ? suggestedName(stacks) : editing.stack.name;
  const holder = /* @__PURE__ */ new Map();
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (id2 === editing?.id) continue;
    for (const block of stack.blocks) holder.set(block, stack.name);
  }
  const options = Object.keys(board.blocks).map((id2) => {
    const item = blockItem(board, registry, id2);
    const other = holder.get(id2);
    return other === void 0 ? { ...item, free: true } : { ...item, free: false, detail: `${item.detail} \xB7 leaves the stack ${other}` };
  });
  const search = useSearch(options, blockTexts, "The board holds no block.");
  const sections = [
    { id: "free", title: "In no stack", items: search.items.filter((item) => item.free) },
    { id: "held", title: "In another stack", items: search.items.filter((item) => !item.free) }
  ].filter((section) => section.items.length > 0);
  const kept = blocks.filter((block) => Object.hasOwn(board.blocks, block));
  function batch2() {
    const trimmed = name2.trim();
    const hex4 = color2 === null ? null : stackColor(color2);
    if (editing === void 0) {
      const id3 = core.mintId((minted) => Object.hasOwn(core.store.board().stacks, minted));
      return [{ id: ADD_STACK, args: { id: id3, blocks: kept, color: hex4, name: trimmed === "" ? fallbackName : trimmed } }];
    }
    const { id: id2, stack } = editing;
    const changed = {
      ...sameBlocks(kept, stack.blocks) ? {} : { blocks: kept },
      ...sameColor(hex4, stack.color) ? {} : { color: hex4 }
    };
    return [
      ...trimmed === "" || trimmed === stack.name ? [] : [{ id: RENAME_STACK, args: { id: id2, name: trimmed } }],
      ...Object.keys(changed).length === 0 ? [] : [{ id: CHANGE_STACK, args: { id: id2, ...changed } }]
    ];
  }
  async function submit(event) {
    event.preventDefault();
    if (busy) return;
    const title = editing === void 0 ? "Could not add the stack." : `Could not change the stack "${editing.stack.name}".`;
    const calls = batch2();
    setBusy(true);
    await edits.run(title, calls, done);
    setBusy(false);
  }
  return /* @__PURE__ */ jsxs43(
    Form2,
    {
      "aria-label": editing === void 0 ? "Add stack" : "Change stack",
      validationBehavior: "aria",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        /* @__PURE__ */ jsxs43("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx55(
            TextField2,
            {
              label: "Name",
              value: name2,
              placeholder: fallbackName,
              description: editing === void 0 ? `Optional. "${fallbackName}" where left empty.` : void 0,
              autoComplete: "off",
              onChange: setName
            }
          ),
          /* @__PURE__ */ jsx55(StackColor, { color: color2, onChange: setColor })
        ] }),
        /* @__PURE__ */ jsx55(
          ComboBox2,
          {
            selectionMode: "multiple",
            label: "Blocks",
            ...search,
            items: sections,
            value: kept,
            placeholder: "No blocks",
            description: "A block sits in one stack at most.",
            onChange: (keys) => {
              setBlocks(keys.map(String));
            },
            children: (section) => /* @__PURE__ */ jsx55(ComboBoxSection, { id: section.id, title: section.title, items: section.items, children: blockRow })
          }
        ),
        /* @__PURE__ */ jsx55("div", { className: "bc:flex bc:gap-2", children: /* @__PURE__ */ jsx55("button", { type: "submit", className: classes.primary, disabled: busy, children: editing === void 0 ? "Add stack" : "Save" }) }),
        /* @__PURE__ */ jsx55(Refusal2, { refusal: edits.refusal })
      ]
    }
  );
}
function StackColor({ color: color2, onChange }) {
  const [last, setLast] = useState20(color2 ?? PALETTE[0]);
  const shown2 = color2 ?? last;
  return /* @__PURE__ */ jsxs43("fieldset", { className: styles.fieldset, children: [
    /* @__PURE__ */ jsx55("legend", { className: styles.legend, children: "Colour" }),
    /* @__PURE__ */ jsxs43("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-x-3 bc:gap-y-1", children: [
      /* @__PURE__ */ jsx55(
        Checkbox,
        {
          isSelected: color2 !== null,
          onChange: (selected) => {
            onChange(selected ? last : null);
          },
          children: "A colour of its own"
        }
      ),
      /* @__PURE__ */ jsxs43(
        ColorPicker2,
        {
          label: stackColor(shown2).toUpperCase(),
          value: shown2,
          isDisabled: color2 === null,
          onChange: (picked) => {
            setLast(picked);
            onChange(picked);
          },
          children: [
            /* @__PURE__ */ jsx55(ColorArea2, { colorSpace: "hsb", xChannel: "saturation", yChannel: "brightness" }),
            /* @__PURE__ */ jsx55(ColorSlider2, { colorSpace: "hsb", channel: "hue" }),
            /* @__PURE__ */ jsx55(ColorField2, { label: "Hex" }),
            /* @__PURE__ */ jsx55(ColorSwatchPicker2, { "aria-label": "Palette", className: "bc:max-w-36", children: PALETTE.map((hex4) => /* @__PURE__ */ jsx55(ColorSwatchPickerItem2, { color: hex4 }, hex4)) })
          ]
        }
      )
    ] })
  ] });
}
function sameBlocks(a, b) {
  return a.length === b.length && a.every((block, i) => block === b[i]);
}

// srcts/actions/stacks.ts
var CREATE_STACK = "blockr.core-add_stack_action";
var EDIT_STACK = "blockr.core-edit_stack_action";
var ADD_STACK2 = "blockr.core-add_stack";
var CHANGE_STACK2 = "blockr.core-change_stack";
function createStackAction() {
  return {
    id: CREATE_STACK,
    label: "Create stack",
    icon: PLUS,
    on: ["block", "selection", "board"],
    category: "add",
    offers: (board, target2, lock) => !lock.board && ("board" in target2 || stackedBlocks(board, target2).length > 0),
    quick(board, target2, core) {
      const blocks = stackedBlocks(board, target2);
      if (!("board" in target2) && blocks.length === 0) return void 0;
      const joined = blocks.length === 0 ? [] : Object.entries(board.stacks).filter(([, stack]) => blocks.some((block) => !stack.blocks.includes(block)));
      const rows = [
        ...joined.map(([id2, stack]) => ({
          ...stackRow(stack),
          pick: () => [{ id: CHANGE_STACK2, args: { id: id2, blocks: [...stack.blocks, ...blocks.filter((block) => !stack.blocks.includes(block))] } }]
        })),
        { kind: "item", label: "New stack", icon: PLUS, pick: () => newStack(core, blocks) }
      ];
      return blocks.length === 0 ? { rows } : { title: `Add ${blocksLabel(board, blocks)} to`, rows };
    },
    advanced: createStackForm
  };
}
function editStackAction() {
  return {
    id: EDIT_STACK,
    label: "Edit stack",
    on: ["stack"],
    category: "edit",
    offers: (board, target2, lock) => !lock.board && heldStack(board, target2) !== void 0,
    quick(board, target2) {
      const held4 = heldStack(board, target2);
      if (held4 === void 0) return void 0;
      const [id2, stack] = held4;
      return {
        rows: [
          levelItem("Colour\u2026", { title: `Colour of ${stack.name}`, rows: colourRows(board, id2, stack) }),
          levelItem("Blocks\u2026", { title: `Blocks in ${stack.name}`, filter: true, rows: [blocksRow(board, id2, stack)] })
        ]
      };
    },
    advanced: editStackForm
  };
}
function newStack(core, blocks) {
  const stacks = core.store.board().stacks;
  const others = Object.values(stacks);
  return [
    {
      id: ADD_STACK2,
      args: {
        id: core.mintId((id2) => Object.hasOwn(core.store.board().stacks, id2)),
        name: suggestedName(others),
        color: suggestedColor(others),
        blocks
      }
    }
  ];
}
function colourRows(board, id2, stack) {
  const used = new Set(
    Object.entries(board.stacks).filter(([other]) => other !== id2).map(([, other]) => other.color?.toLowerCase())
  );
  const current2 = stack.color?.toLowerCase();
  return [
    { kind: "item", label: "Current", icon: swatch(stack.color), detail: stack.color === null ? "None" : stack.color.toUpperCase(), pick: () => [] },
    ...PALETTE.filter((color2) => color2 !== current2 && !used.has(color2)).map(
      (color2) => ({
        kind: "item",
        label: color2.toUpperCase(),
        icon: swatch(color2),
        pick: () => [{ id: CHANGE_STACK2, args: { id: id2, color: color2 } }]
      })
    )
  ];
}
function blocksRow(board, id2, stack) {
  const stacked = new Set(Object.values(board.stacks).flatMap((other) => other.blocks));
  const members = stack.blocks.filter((block) => Object.hasOwn(board.blocks, block));
  const free = Object.keys(board.blocks).filter((block) => !stacked.has(block));
  return {
    kind: "multi",
    label: "Blocks",
    choices: [
      ...members.map((block) => ({ id: block, label: blockName(board, block), picked: true })),
      ...free.map((block) => ({ id: block, label: blockName(board, block), picked: false }))
    ],
    apply: (picked) => [{ id: CHANGE_STACK2, args: { id: id2, blocks: [...picked] } }]
  };
}
function stackRow(stack) {
  return { kind: "item", label: stack.name, icon: swatch(stack.color), detail: count(stack.blocks.length) };
}
function count(blocks) {
  return blocks === 1 ? "1 block" : `${String(blocks)} blocks`;
}
function blocksLabel(board, blocks) {
  const [only] = blocks;
  return blocks.length === 1 && only !== void 0 ? blockName(board, only) : count(blocks.length);
}
function heldStack(board, target2) {
  if (!("stack" in target2)) return void 0;
  const stack = own(board.stacks, target2.stack);
  return stack === void 0 ? void 0 : [target2.stack, stack];
}

// srcts/actions/core.ts
function coreBoardActions(parts) {
  const { raise } = parts;
  return [
    ...addingBoardActions(parts.blockRegistry),
    createLinkAction(parts),
    createStackAction(),
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
                const name2 = value2.trim();
                return name2 === object2.name ? [] : [{ id: object2.command, args: { id: object2.id, name: name2 } }];
              }
            }
          ]
        };
      }
    },
    editLinkAction(parts),
    {
      id: "blockr.core-edit_inputs_action",
      label: "Edit inputs",
      on: ["block"],
      category: "edit",
      offers(board, target2, lock) {
        const block = blockOf(target2);
        return !lock.board && block !== void 0 && classInputs(board, parts.blockRegistry(), block).length > 0;
      },
      advanced: editInputs
    },
    editStackAction(),
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
function createBoardActions(parts) {
  let registered = coreBoardActions(parts);
  const classes2 = /* @__PURE__ */ new Map();
  return {
    register(plugin, given) {
      if (given === void 0) return () => void 0;
      const actions = checked2(plugin, given, new Set(registered.map(({ id: id2 }) => id2)));
      registered = [...registered, ...actions];
      for (const { id: id2 } of actions) classes2.set(id2, plugin);
      return () => {
        registered = registered.filter((action2) => !actions.includes(action2));
        for (const { id: id2 } of actions) classes2.delete(id2);
      };
    },
    registered: () => registered,
    classOf: (id2) => classes2.get(id2)
  };
}
function checked2(plugin, given, taken) {
  const of = `The plugin class "${plugin}"`;
  if (!Array.isArray(given)) {
    throw new Error(`${of} gives board actions that are no array.`);
  }
  const ids3 = new Set(taken);
  return given.map((action2) => {
    if (!isObject(action2)) {
      throw new Error(`${of} gives a board action that is no object.`);
    }
    const { id: id2 } = action2;
    if (typeof id2 !== "string" || !isCommandId(id2)) {
      throw new Error(`${of} gives a board action whose id ${JSON.stringify(id2)} is not named as a command's id is, "{pkg}-{name}".`);
    }
    const named5 = `${of} gives the board action "${id2}"`;
    if (ids3.has(id2)) {
      throw new Error(`${named5}, which has the id of another board action.`);
    }
    ids3.add(id2);
    if (typeof action2.label !== "string" || action2.label === "") {
      throw new Error(`${named5} without a label.`);
    }
    if (action2.icon !== void 0 && typeof action2.icon !== "string") {
      throw new Error(`${named5}, whose icon is no string of SVG markup.`);
    }
    const on = action2.on;
    if (!Array.isArray(on)) {
      throw new Error(`${named5}, whose on is no array of the kinds of target that it applies to.`);
    }
    if (on.length === 0) {
      throw new Error(`${named5}, whose on is empty, so that no menu would offer it.`);
    }
    const other = on.find((kind) => typeof kind !== "string" || !Object.hasOwn(KINDS2, kind));
    if (other !== void 0) {
      throw new Error(`${named5}, whose on names ${JSON.stringify(other)}, which is none of the kinds of target, ${quoted(Object.keys(KINDS2))}.`);
    }
    if (typeof action2.category !== "string" || action2.category === "") {
      throw new Error(`${named5} without a category.`);
    }
    for (const member of ["detail", "offers", "run", "quick"]) {
      if (action2[member] !== void 0 && typeof action2[member] !== "function") {
        throw new Error(`${named5}, whose ${member} is no function.`);
      }
    }
    const { advanced } = action2;
    if (advanced !== void 0 && (!isObject(advanced) || typeof advanced.render !== "function")) {
      throw new Error(`${named5}, whose advanced has no render() function.`);
    }
    const asks = ["quick", "advanced"].filter((member) => action2[member] !== void 0);
    if (action2.run !== void 0 && asks.length > 0) {
      throw new Error(`${named5}, which has run beside ${asks.join(" and ")}, while run is for a board action that asks the user nothing.`);
    }
    if (action2.run === void 0 && asks.length === 0) {
      throw new Error(`${named5}, which has none of run, quick and advanced.`);
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
  function registered(id2) {
    const action2 = actions.registered().find((each) => each.id === id2);
    if (action2 === void 0) {
      throw new Error(`No board action "${id2}" is registered.`);
    }
    return action2;
  }
  async function batchOf(action2, target2, notifier) {
    let batch2;
    try {
      batch2 = await action2.run?.(store.board(), target2, parts.core());
    } catch (error) {
      throw failed3(action2, new Error(`The board action "${action2.id}" failed: ${errorMessage(error)}`, { cause: error }), notifier);
    }
    if (batch2 !== void 0 && !isBatch(batch2)) {
      throw failed3(
        action2,
        new TypeError(`The run of the board action "${action2.id}" returned no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].`),
        notifier
      );
    }
    return batch2;
  }
  function failed3(action2, failure, notifier) {
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
  async function runBatch(batch2, batching2, planned = false) {
    if (batch2.length === 0) return [];
    const core = parts.core();
    const { extend, notifier } = batching2;
    let deltas = [];
    let added = [];
    if (extend !== void 0 || planned) {
      try {
        deltas = await core.commands.plan(batch2);
      } catch (error) {
        refused2(error, void 0, notifier);
        throw error;
      }
    }
    if (extend !== void 0) {
      const extra = extend(deltas);
      if (!isBatch(extra)) {
        throw new TypeError(
          `The \`extend\` of ${batching2.call ?? "the call"} returned no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].`
        );
      }
      added = extra;
    }
    await core.commands.run([...batch2, ...added], "user", notifier === void 0 ? void 0 : { notifier });
    return deltas;
  }
  function batching(given, action2) {
    return { call: given.call, extend: given.extend, notifier: notifierOf(given, action2) };
  }
  async function runOf(action2, target2, given) {
    const batch2 = await batchOf(action2, target2, notifierOf(given, action2));
    if (batch2 !== void 0) await runBatch(batch2, batching(given, action2));
  }
  const advanced = createAdvanced({
    store,
    core: parts.core,
    applies: (action2, target2) => store.hasBoard() && applies(action2, store.board(), target2),
    run: (batch2, how) => runBatch(batch2, how, true),
    frontend: () => parts.frontend?.(),
    dialog: parts.dialog,
    raise: (notification) => {
      raiseAgain(notification);
    },
    classOf: (id2) => actions.classOf(id2),
    ...signal5 === void 0 ? {} : { signal: signal5 }
  });
  function openingOf(given, action2, opened = {}) {
    return { ...batching(given, action2), el: given.el, place: given.advanced, ...opened };
  }
  function open(start3, target2, given, closed) {
    void shown2?.close().catch(() => void 0);
    let hide;
    const menu = openMenu(start3, {
      board: () => store.board(),
      target: target2,
      core: parts.core,
      applies: (action2) => store.hasBoard() && applies(action2, store.board(), target2),
      run: async (batch2, action2) => {
        await runBatch(batch2, batching(given, action2));
      },
      trigger: (action2) => runOf(action2, target2, given),
      advanced: (action2, opened) => advanced.open(action2, target2, openingOf(given, action2, opened)),
      failed(action2, failure) {
        failed3(action2, failure, notifierOf(given, action2));
      },
      closed(ran) {
        if (shown2?.closed() === true) shown2 = void 0;
        hide?.();
        hide = void 0;
        closed?.(ran);
      }
    });
    if (menu.closed()) return;
    shown2 = menu;
    hide = parts.show(menu, { el: given.el, anchor: given.anchor ?? given.el.getBoundingClientRect() });
  }
  return {
    on: (target2) => on(targetGiven(target2)),
    menu(target2, opts) {
      const checked4 = targetGiven(target2);
      const given = optionsGiven(opts, "menu()");
      if (signal5?.aborted === true) return;
      const offered = on(checked4);
      if (offered.length > 0) open({ actions: offered }, checked4, given);
    },
    async trigger(id2, target2, opts) {
      const checked4 = targetGiven(target2);
      const given = optionsGiven(opts, "trigger()");
      const action2 = registered(id2);
      if (signal5?.aborted === true || !store.hasBoard() || !applies(action2, store.board(), checked4)) return;
      if (action2.run !== void 0) {
        await runOf(action2, checked4, given);
        return;
      }
      if (action2.quick === void 0) {
        await advanced.open(action2, checked4, openingOf(given, action2));
        return;
      }
      await new Promise((resolve, reject) => {
        open({ action: action2 }, checked4, given, (ran) => {
          if (ran === void 0) resolve();
          else ran.then(resolve, reject);
        });
      });
    },
    mountAdvanced(el, id2, target2, picked) {
      if (signal5?.aborted === true) throw new Error(REPLACED);
      const given = el;
      if (!isElement(given)) {
        throw new TypeError("The `el` of mountAdvanced() is the element in which core renders the advanced action.");
      }
      const checked4 = targetGiven(target2);
      const action2 = registered(id2);
      if (action2.advanced === void 0) {
        throw new Error(`The board action "${id2}" has no advanced action to mount.`);
      }
      return advanced.mount(given, action2, checked4, picked);
    }
  };
}
function optionsGiven(opts, call) {
  const given = isObject(opts) ? opts : {};
  const { el, anchor, extend, notifier, advanced } = given;
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
  if (advanced !== void 0 && typeof advanced !== "function") {
    throw new TypeError(`The \`advanced\` of ${call} is a function, which shows an advanced action in a place of the caller's.`);
  }
  return {
    call,
    el,
    anchor,
    // Each is called as a method of the options, as a caller may write it.
    extend: extend === void 0 ? void 0 : (deltas) => extend.call(given, deltas),
    notifier: notifier === void 0 ? void 0 : (notifications, action2) => {
      notifier.call(given, notifications, action2);
    },
    advanced: advanced === void 0 ? void 0 : { show: (args) => advanced.call(given, args), key: advanced }
  };
}
function isRect(value2) {
  return isObject(value2) && ["x", "y", "width", "height"].every((key2) => typeof value2[key2] === "number");
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

// srcts/actions/dialog.tsx
import { flushSync as flushSync5 } from "react-dom";
import { createRoot as createRoot3 } from "react-dom/client";
import { jsx as jsx56, jsxs as jsxs44 } from "react/jsx-runtime";
var showDialog = (args, at) => {
  const doc = at.el.ownerDocument;
  const host = doc.createElement("div");
  doc.body.append(host);
  const root = createRoot3(host);
  let open = true;
  const offAbort = at.signal === void 0 ? () => void 0 : onAbort(at.signal, hide);
  function hide() {
    if (!open) return;
    open = false;
    offAbort();
    root.unmount();
    host.remove();
    args.close();
    const active = doc.activeElement;
    if ((active === null || active === doc.body) && at.el.isConnected) at.el.focus({ preventScroll: true });
  }
  flushSync5(() => {
    root.render(/* @__PURE__ */ jsx56(CoreDialog, { args, onClose: hide }));
  });
  return () => {
    hide();
  };
};
function CoreDialog({ args, onClose }) {
  const { mount: mount2 } = args;
  return /* @__PURE__ */ jsx56(
    Modal2,
    {
      isOpen: true,
      onOpenChange: (isOpen) => {
        if (!isOpen) onClose();
      },
      className: WIDTH,
      children: /* @__PURE__ */ jsx56(Dialog2, { children: /* @__PURE__ */ jsxs44("div", { className: "bc:grid bc:gap-3", children: [
        /* @__PURE__ */ jsxs44("div", { className: "bc:flex bc:items-center bc:justify-between bc:gap-2", children: [
          /* @__PURE__ */ jsx56(Heading, { slot: "title", className: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold", children: args.title }),
          /* @__PURE__ */ jsx56(Button2, { variant: "secondary", onPress: onClose, children: "Close" })
        ] }),
        /* @__PURE__ */ jsx56(Mounted, { mount: mount2, className: "bc:min-w-0" })
      ] }) })
    }
  );
}
var WIDTH = "bc:min-w-[min(420px,100%)]";

// srcts/actions/menu.tsx
import { useEffect as useEffect2, useRef as useRef13, useSyncExternalStore as useSyncExternalStore3 } from "react";
import { flushSync as flushSync6 } from "react-dom";
import { createRoot as createRoot4 } from "react-dom/client";
import { Fragment as Fragment28, jsx as jsx57, jsxs as jsxs45 } from "react/jsx-runtime";
var showMenu = (menu, at) => {
  const doc = at.el.ownerDocument;
  const host = doc.createElement("div");
  doc.body.append(host);
  const root = createRoot4(host);
  const insideRef = { current: null };
  const tab = (event) => {
    if (event.key !== "Tab" || event.isComposing || !(event.target instanceof Node) || insideRef.current?.contains(event.target) !== true) return;
    event.preventDefault();
    event.stopPropagation();
    void menu.close().catch(() => void 0);
  };
  doc.defaultView?.addEventListener("keydown", tab, true);
  flushSync6(() => {
    root.render(/* @__PURE__ */ jsx57(CoreMenu, { menu, at, insideRef }));
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
  const level = useSyncExternalStore3(menu.subscribe, menu.get);
  const anchor = useRef13(null);
  const { x, y, width, height } = at.anchor;
  const tip = openTip(at.el.ownerDocument.defaultView ?? window);
  return /* @__PURE__ */ jsxs45(Fragment28, { children: [
    /* @__PURE__ */ jsx57("div", { ref: anchor, "aria-hidden": "true", style: { position: "fixed", left: x, top: y, width, height, pointerEvents: "none" } }),
    /* @__PURE__ */ jsx57(
      Popover2,
      {
        triggerRef: anchor,
        isOpen: true,
        placement: "bottom start",
        isKeyboardDismissDisabled: true,
        onOpenChange: (open) => {
          if (!open) void menu.close().catch(() => void 0);
        },
        className: "bc:max-w-80 bc:min-w-45",
        children: /* @__PURE__ */ jsx57(
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
              } else if (level.advanced && matchesShortcut(OPEN_KEY, event.nativeEvent)) {
                event.preventDefault();
                event.stopPropagation();
                void menu.openAdvanced(openRow(event.target)).catch(() => void 0);
              } else if (event.key === "Enter" && isFilter(event.target) && !event.target.hasAttribute("aria-activedescendant")) {
                event.preventDefault();
                event.stopPropagation();
                void menu.pick().catch(() => void 0);
              }
            },
            children: /* @__PURE__ */ jsx57(Level, { menu, level, insideRef, tip }, level.id)
          }
        )
      }
    )
  ] });
}
function Level({
  menu,
  level,
  insideRef,
  tip
}) {
  const texts = level.rows.flatMap((row, i) => row.kind === "text" ? [{ row, i }] : []);
  const listed = level.rows.flatMap((row, i) => row.kind === "text" ? [] : [{ row, i }]);
  const rows = level.rows.length > 0;
  useEffect2(() => {
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
  const openAdvanced = (i) => {
    void menu.openAdvanced(i).catch(() => void 0);
  };
  const grouped = groups(listed);
  const list = /* @__PURE__ */ jsx57(Menu2, { "aria-label": level.label, renderEmptyState: () => /* @__PURE__ */ jsx57("p", { className: NOTE3, children: "No matches" }), children: grouped.map((group, g) => {
    const divider = g > 0 && group.category !== grouped[g - 1]?.category;
    const section = group.kind === "multi" ? /* @__PURE__ */ jsx57(
      MenuSection2,
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
        children: group.row.choices.map((choice) => /* @__PURE__ */ jsx57(MenuItem2, { id: choiceKey(group.i, choice.id), textValue: choice.label, children: choice.label }, choice.id))
      },
      `multi-${String(group.i)}`
    ) : /* @__PURE__ */ jsx57(
      MenuSection2,
      {
        id: `items-${String(group.items[0]?.i ?? g)}`,
        title: group.continues ? void 0 : group.heading,
        ...group.checks ? { selectionMode: "multiple", selectedKeys: group.items.filter(({ row }) => row.checked === true).map(({ i }) => itemKey(i)) } : {},
        children: group.items.map(({ row, i }) => /* @__PURE__ */ jsx57(
          Item,
          {
            row,
            index: i,
            tip,
            onPick: () => {
              pick2(i);
            },
            onOpen: () => {
              openAdvanced(i);
            }
          },
          i
        ))
      },
      `items-${String(group.items[0]?.i ?? g)}`
    );
    return divider ? [/* @__PURE__ */ jsx57(Separator, { className: DIVIDER }, `divider-${String(g)}`), section] : [section];
  }) });
  return /* @__PURE__ */ jsxs45(Fragment28, { children: [
    level.advanced ? /* @__PURE__ */ jsxs45("div", { className: "bc:flex bc:items-center bc:gap-2", children: [
      level.title === void 0 ? null : /* @__PURE__ */ jsx57("p", { className: `${TITLE} bc:min-w-0 bc:flex-1`, children: level.title }),
      /* @__PURE__ */ jsx57(
        "button",
        {
          type: "button",
          "aria-label": OPEN_NAME,
          "aria-keyshortcuts": ariaShortcuts(OPEN_KEY),
          title: tip,
          className: OPEN,
          onClick: () => {
            openAdvanced();
          },
          children: /* @__PURE__ */ jsx57(OpenIcon, {})
        }
      )
    ] }) : level.title === void 0 ? null : /* @__PURE__ */ jsx57("p", { className: TITLE, children: level.title }),
    level.loading ? /* @__PURE__ */ jsx57("p", { role: "status", className: `${NOTE3} bc:animate-[bc-appear_300ms_ease-out_300ms_both]`, children: "Loading\u2026" }) : level.error === void 0 ? /* @__PURE__ */ jsxs45(Fragment28, { children: [
      texts.map(({ row, i }) => /* @__PURE__ */ jsx57(
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
      level.filter === void 0 ? listed.length === 0 ? null : list : /* @__PURE__ */ jsxs45(
        Autocomplete,
        {
          inputValue: level.filter,
          onInputChange: (value2) => {
            menu.type(value2);
          },
          children: [
            /* @__PURE__ */ jsx57(TextField, { "aria-label": "Filter", className: "bc:px-1 bc:pt-1", children: /* @__PURE__ */ jsx57(
              Input,
              {
                "data-filter": "",
                placeholder: "Filter",
                spellCheck: "false",
                "aria-keyshortcuts": level.advanced ? ariaShortcuts(OPEN_KEY) : void 0,
                className: FILTER
              }
            ) }),
            list
          ]
        }
      )
    ] }) : /* @__PURE__ */ jsx57("p", { role: "alert", className: "bc:m-0 bc:px-2 bc:py-1.5 bc:text-sm bc:break-words bc:text-text-danger", children: level.error })
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
    const checks = row.checked !== void 0;
    const under = last?.kind === "items" && last.heading === row.heading && last.category === row.category;
    if (under && last.checks === checks) {
      last.items.push({ row, i });
    } else {
      all.push({
        kind: "items",
        ...row.heading === void 0 ? {} : { heading: row.heading },
        ...row.category === void 0 ? {} : { category: row.category },
        checks,
        continues: under,
        items: [{ row, i }]
      });
    }
  }
  return all;
}
function Item({
  row,
  index,
  tip,
  onPick,
  onOpen
}) {
  const { label, icon, detail } = row;
  const removes = row.category === "remove";
  return /* @__PURE__ */ jsxs45(
    MenuItem2,
    {
      id: itemKey(index),
      textValue: label,
      "aria-label": detail === void 0 ? label : `${label}, ${detail}`,
      onAction: onPick,
      className: removes ? REMOVES : void 0,
      children: [
        /* @__PURE__ */ jsxs45(
          "span",
          {
            className: `bc:flex bc:min-w-0 bc:items-center bc:gap-2 ${removes ? "bc:group-data-focused:text-text-danger bc:group-data-hovered:text-text-danger" : ""}`,
            children: [
              icon === void 0 ? null : /* @__PURE__ */ jsx57(
                "span",
                {
                  "aria-hidden": "true",
                  className: "bc:flex bc:size-4 bc:shrink-0 bc:[&>svg]:size-full",
                  dangerouslySetInnerHTML: { __html: icon }
                }
              ),
              /* @__PURE__ */ jsx57("span", { className: "bc:min-w-0 bc:truncate", children: label })
            ]
          }
        ),
        detail === void 0 ? null : /* @__PURE__ */ jsx57("span", { className: "bc:text-xs bc:text-text-muted", children: detail }),
        row.advanced === true ? /* @__PURE__ */ jsxs45(Fragment28, { children: [
          /* @__PURE__ */ jsx57(
            "span",
            {
              "aria-hidden": "true",
              "data-open-row": index,
              title: tip,
              className: `${OPEN} bc:-my-1`,
              onPointerDown: (event) => {
                event.stopPropagation();
              },
              onClick: (event) => {
                event.stopPropagation();
                onOpen();
              },
              children: /* @__PURE__ */ jsx57(OpenIcon, {})
            }
          ),
          /* @__PURE__ */ jsx57(Text, { slot: "description", className: "bc:sr-only", children: tip })
        ] }) : null
      ]
    }
  );
}
function OpenIcon() {
  return /* @__PURE__ */ jsxs45(
    "svg",
    {
      "aria-hidden": "true",
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: 1.25,
      strokeLinecap: "round",
      strokeLinejoin: "round",
      children: [
        /* @__PURE__ */ jsx57("path", { d: "M7 2.5H4A1.5 1.5 0 0 0 2.5 4v8A1.5 1.5 0 0 0 4 13.5h8a1.5 1.5 0 0 0 1.5-1.5V9" }),
        /* @__PURE__ */ jsx57("path", { d: "M10 2.5h3.5V6M13.5 2.5 8 8" })
      ]
    }
  );
}
function openRow(target2) {
  if (!(target2 instanceof Element)) return void 0;
  const active = isFilter(target2) ? target2.getAttribute("aria-activedescendant") : null;
  const item = active === null ? target2.closest('[role="menuitem"], [role="menuitemcheckbox"]') : target2.ownerDocument.getElementById(active);
  const marked = item?.querySelector("[data-open-row]")?.getAttribute("data-open-row");
  return marked === void 0 || marked === null ? void 0 : Number(marked);
}
function TextRow({
  row,
  onType,
  onSubmit
}) {
  return /* @__PURE__ */ jsx57(
    TextField2,
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
function itemKey(row) {
  return String(row);
}
function choiceKey(row, choice) {
  return `${String(row)}:${choice}`;
}
function isFilter(target2) {
  return target2 instanceof HTMLInputElement && target2.hasAttribute("data-filter");
}
var TITLE = "bc:m-0 bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted";
var OPEN = "bc:ml-auto bc:inline-flex bc:size-6 bc:shrink-0 bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:cursor-pointer bc:outline-none bc:hover:bg-bg-subtle bc:hover:text-text-default bc:focus-visible:ring-2 bc:focus-visible:ring-border-accent";
var OPEN_NAME = "Show all options";
var OPEN_KEY = "Mod+Enter";
function openTip(win) {
  return `${OPEN_NAME} (${shortcutLabel(OPEN_KEY, onMac(win)) ?? OPEN_KEY})`;
}
var NOTE3 = "bc:m-0 bc:px-2 bc:py-1.5 bc:text-center bc:text-sm bc:text-text-muted bc:italic";
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
  function keep(ids3, by) {
    if (ids3.length === 0) return NOTHING_FAILED;
    const kept = keepOptions(ids3, by);
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
  let memo2;
  function appPlugins() {
    if (!store.hasBoard()) return NONE;
    const classes2 = store.plugins.registered();
    if (memo2?.classes !== classes2) {
      const registered = Object.entries(deps.plugins).flatMap(
        ([id2, { name: name2 }]) => Object.hasOwn(classes2, id2) ? [[id2, Object.freeze({ name: name2 })]] : []
      );
      memo2 = { classes: classes2, plugins: Object.freeze(Object.fromEntries(registered)) };
    }
    return memo2.plugins;
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
    const drawn3 = id2 === void 0 ? void 0 : pluginClass(id2).frontend;
    if (id2 === void 0 || drawn3 === void 0) return void 0;
    return { frontend: drawn3, plugin: id2, added: added.get(id2) ?? NOTHING_FAILED };
  }
  let started = false;
  async function start3() {
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
      if (deps.uis.get(el) === plugin) deps.uis.delete(el);
      stopKeeping?.();
      controller.abort();
      if (cleanup !== void 0) attempt(cleanup);
      el.replaceChildren();
    };
    const offEnd = onAbort(replaced2, end);
    deps.uis.set(el, plugin);
    function fail(error) {
      if (ended()) return;
      log.error(asError(error));
      cannotDraw(errorMessage(error));
    }
    function cannotDraw(message) {
      deps.raise(coreNotification("module_failed", "error", message, { module: sourceModule({ plugin }, packageFromId(plugin)) }));
    }
    function lacking2(ids3) {
      const missing = ids3.filter((id2) => !Object.hasOwn(store.board().options, id2));
      cannotDraw(withoutOptions(`the ui of the plugin class "${plugin}"`, missing));
    }
    void (async () => {
      const named5 = optionsOf(plugin);
      const kept = keepOptions(named5, `the plugin class "${plugin}"`, () => {
        if (ended()) return;
        end();
        lacking2(named5);
      });
      stopKeeping = kept.stop;
      const failures = await kept.added;
      if (ended()) return;
      if (failures.length > 0) {
        kept.stop();
        lacking2(named5);
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
  return { appPlugins, pluginClass, storage, mountPlugin, frontend, start: start3 };
}

// srcts/core/requests.ts
var ATTEMPTS3 = 3;
async function pluginRequest(deps, plugin, name2, args, opts) {
  if (typeof plugin !== "string" || plugin === "") {
    throw new TypeError("A plugin's request names the plugin by the id of its class, a non-empty string.");
  }
  if (!Object.hasOwn(deps.plugins.appPlugins(), plugin)) {
    throw new Error(`The app runs no plugin "${plugin}" on the open board, so its request did not go to R.`);
  }
  if (typeof name2 !== "string" || name2 === "") {
    throw new TypeError(`A request of the plugin "${plugin}" is named by a non-empty string.`);
  }
  if (!isObject(args)) {
    throw new TypeError(`The args of the request "${name2}" of the plugin "${plugin}" are an object.`);
  }
  const reads = opts.reads ?? [];
  if (!Array.isArray(reads) || !reads.every((block) => typeof block === "string")) {
    throw new TypeError(`The blocks that the request "${name2}" of the plugin "${plugin}" reads are an array of their ids.`);
  }
  const notifier = givenNotifier(opts, `the request "${name2}" of the plugin "${plugin}"`);
  const what = `The request "${name2}" of the plugin "${plugin}"`;
  const module = sourceModule({ plugin }, packageFromId(plugin));
  const inCaller = keepContext();
  const handOn = (conditions) => {
    deps.replied(module, { type: "plugin", name: name2 }, conditions, notifier);
  };
  for (let attempt3 = 1; ; attempt3++) {
    const revisions = await evaluateReads2(deps, what, reads, opts.signal);
    const { reply, body } = await inCaller(
      () => deps.scheduler.request(
        { kind: "request", type: "plugin", plugin, name: name2, args: { ...args }, reads: revisions },
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
        throw new RequestError(`R cancelled the request "${name2}" of the plugin "${plugin}".`, { status: "cancelled" });
      default:
        handOn(conditions);
        throw new RequestError(`R replied ${reply.status} to the request "${name2}" of the plugin "${plugin}".`, {
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
  const onScreen = createOnScreen(parts.signal);
  const uis = /* @__PURE__ */ new WeakMap();
  const outputOnScreen = {
    holds: (el, block) => holdsOutput(onScreen.get(), uis, parts.page, el, block),
    subscribe: (fn) => onScreen.subscribe(fn)
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
    onScreen: outputOnScreen,
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
    keepOptions: (ids3, by, failed3) => mounts.keepOptions(ids3, by, failed3),
    core: () => api,
    plugins: parts.plugins,
    raise,
    uis,
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
      dialog: parts.showDialog,
      ...parts.advanced === void 0 ? {} : { frontend: parts.advanced },
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
    onScreen,
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
    request: (plugin, name2, args = {}, opts = {}) => pluginRequest({ plugins, scheduler, replied: notifications.replied }, plugin, name2, args, opts),
    module: (id2) => loader.module(id2),
    blockrOption: (name2) => blockrOptions.read(name2),
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
      const lacking2 = Object.values(store.options.registered()).filter((def) => def.onChange !== void 0 && !Object.hasOwn(board.options, def.id)).map((def) => def.id);
      if (lacking2.length > 0) void mounts.addOptions(lacking2, "its onChange");
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
    const entries2 = Object.entries(given);
    const [entry] = entries2;
    if (entries2.length === 1 && entry !== void 0 && typeof entry[1] === "string") {
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
      if (seen !== null && sameIds2(seen.blocks, next.blocks) && sameIds2(seen.plugins, next.plugins)) return;
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
function sameIds2(a, b) {
  const inB = new Set(b);
  return new Set(a).size === inB.size && a.every((id2) => inB.has(id2));
}
function holdsOutput(shown2, uis, page, el, block) {
  if (shown2 === null) return true;
  for (let at = el; at !== null; at = enclosing(at)) {
    const plugin = uis.get(at);
    if (plugin !== void 0) return shown2.plugins.includes(plugin);
    if (at === page) return shown2.blocks.includes(block);
  }
  return true;
}
function enclosing(el) {
  if (el.parentElement !== null) return el.parentElement;
  const root = el.parentNode;
  return root !== null && "host" in root ? root.host : null;
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
  const holds2 = (object2, board) => holdsObject(board, object2, plugins.appPlugins());
  const offStore = store.subscribe((_change, board) => {
    const kept = selected.filter((object2) => holds2(object2, board));
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
      const lacking2 = given.find((object2) => !store.hasBoard() || !holds2(object2, store.board()));
      if (lacking2 !== void 0) throw new Error(unheld(lacking2));
      if (sameIds2(selected.map(objectKey), given.map(objectKey))) return;
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
  let drawn3;
  let next;
  function end() {
    const current2 = drawn3;
    if (current2 === void 0) return;
    drawn3 = void 0;
    current2.off();
    current2.controller.abort();
    if (current2.cleanup !== void 0) attempt(current2.cleanup);
    el.replaceChildren(loadingNote());
  }
  return {
    el,
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
        if (drawn3 === current2) end();
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
      drawn3 = current2;
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
    advanced(signal5) {
      const current2 = drawn3;
      if (current2?.board !== signal5 || current2.frontend.advanced === void 0) return void 0;
      const { frontend } = current2;
      return (args) => frontend.advanced?.(args) ?? (() => void 0);
    },
    async show(object2, signal5) {
      const current2 = drawn3;
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
  const known2 = /* @__PURE__ */ new Map();
  async function load(registryId2, module) {
    const pkg = packageFromId(registryId2);
    let migration;
    try {
      migration = migrationFromExports(await loader.load({ module }, pkg), module, pkg);
    } catch (error) {
      log.error(asError(error));
    }
    known2.set(registryId2, { module, migration });
  }
  return {
    get: (registryId2) => known2.get(registryId2)?.migration,
    async prepare(board, registry) {
      const needed = /* @__PURE__ */ new Map();
      for (const block of olderBlocks(board, registry)) {
        const entry = own(registry, block);
        if (entry === void 0 || known2.get(block)?.module === entry.module) continue;
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
  const held4 = /* @__PURE__ */ new Map();
  const raised = /* @__PURE__ */ new Map();
  const again = /* @__PURE__ */ new Map();
  const replacing = /* @__PURE__ */ new Map();
  const left2 = /* @__PURE__ */ new Set();
  const listeners = /* @__PURE__ */ new Set();
  let all = NONE2;
  let count2 = 0;
  let ended = false;
  const stops = [];
  function publish() {
    all = Object.freeze([...held4.values()]);
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
  function known2(object2) {
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
    for (const [id2, notification] of held4) {
      if (!goes(notification.about) && !goes(notification.source.module?.object)) continue;
      held4.delete(id2);
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
    const had = held4.size > 0;
    held4.clear();
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
      if (!next.has(id2) && held4.delete(id2)) moved = true;
    }
    for (const [id2, notification] of next) {
      const had = held4.get(id2);
      if (had !== void 0 && jsonEqual(had, notification)) continue;
      held4.set(id2, notification);
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
    if (!held4.has(id2)) {
      held4.set(id2, frozen(id2, given));
      publish();
    }
    return () => {
      const holding = raised.get(id2);
      if (holding?.delete(token) !== true || holding.size > 0) return;
      raised.delete(id2);
      if (held4.delete(id2)) publish();
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
    if (about !== void 0 && !known2(about)) {
      if (left2.has(key(about))) {
        log.debug(`Core drops a notification that ${raiser(module.object)} raised about ${named3(about)}, which has left the board.`);
      } else {
        log.error(`Core drops a notification that ${raiser(module.object)} raised about ${named3(about)}, ${unknown(about)}.`);
      }
      return nothing;
    }
    const id2 = `module-${String(++count2)}`;
    held4.set(
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
      if (held4.delete(id2)) publish();
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
function start2(options) {
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
      blockrOption: (name2) => blockrOptions.read(name2)
    });
    const boardActions = createBoardActions({
      raise,
      preflight: (id2, args) => commands.preflight(id2, args),
      blockRegistry: () => store.blockRegistry()
    });
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
      showDialog: options.showDialog ?? showDialog,
      advanced: () => options.frontEnds?.advanced?.(signal5),
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
      ...options.measure === void 0 ? {} : { measure: options.measure },
      ...options.frontEnds?.el === void 0 ? {} : { page: options.frontEnds.el }
    });
    running2 = { store, scheduler, core, notifications, controller };
    let dropped = [];
    let failure;
    if (later === void 0) {
      const instead = alone ? void 0 : await named5(reply, store, core, signal5, raise, opening2);
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
    const failed3 = (error, concerns) => {
      log.error(asError(error));
      if (!signal5.aborted) raise(coreNotification("module_failed", "error", errorMessage(error), concerns));
    };
    const [defs, opts] = await Promise.all([
      Promise.all(
        Object.entries(reply.plugins).map(
          ([id2, entry]) => listed(entry, packageFromId(id2), (error) => {
            failed3(error, { module: sourceModule({ plugin: id2 }, packageFromId(id2)) });
          })
        )
      ),
      Promise.all(
        Object.entries(reply.options).map(
          ([id2, entry]) => listed(entry, entry.package, (error) => {
            failed3(error, { module: sourceModule({ option: id2 }, entry.package) });
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
        failed3(error, { module: sourceModule({ option: id2 }, entry.package) });
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
        failed3(error, { module: sourceModule({ plugin: id2 }, packageFromId(id2)) });
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
  async function listed(source, pkg, failed3) {
    try {
      const at = source.css === void 0 ? { module: source.module } : { module: source.module, css: source.css };
      return listedDefault(await loader.load(at, pkg), source.module, pkg);
    } catch (error) {
      failed3(error);
      return void 0;
    }
  }
  async function named5(reply, store, core, signal5, raise, opening2) {
    if (reply.lock.load) return void 0;
    const classes2 = store.plugins.registered();
    const plugin = Object.keys(reply.plugins).find((id2) => own(classes2, id2)?.storage !== void 0);
    const storage = plugin === void 0 ? void 0 : own(classes2, plugin)?.storage;
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

// srcts/modules/basic/index.tsx
import { flushSync as flushSync7 } from "react-dom";
import { createRoot as createRoot5 } from "react-dom/client";

// srcts/modules/basic/board.tsx
import { useCallback as useCallback8, useMemo as useMemo6, useState as useState22 } from "react";

// srcts/modules/basic/card.tsx
import { memo, useCallback as useCallback5, useLayoutEffect as useLayoutEffect6, useRef as useRef14, useSyncExternalStore as useSyncExternalStore4 } from "react";

// srcts/modules/basic/context.ts
import { createContext as createContext6, useContext as useContext12 } from "react";
var FrontEndContext = createContext6(null);
function useFrontEnd() {
  const given = useContext12(FrontEndContext);
  if (given === null) throw new Error("A component of the basic frontend is drawn outside of it.");
  return given;
}

// srcts/modules/basic/card.tsx
import { jsx as jsx58, jsxs as jsxs46 } from "react/jsx-runtime";
var BlockCard = memo(function BlockCard2({ id: id2, block }) {
  const front = useFrontEnd();
  const { core, inView: inView2 } = front;
  const status = useStatus(core, id2);
  const notified = useNotified(core, { block: id2 });
  const footer = useFooter(status, notified);
  const sized = useSized(status);
  const ref = useRef14(null);
  const actions = useRef14(null);
  useLayoutEffect6(() => {
    const el = ref.current;
    return el === null ? void 0 : inView2.watch({ block: id2 }, el);
  }, [inView2, id2]);
  const mountUi = useCallback5((el) => core.mountBlock(el, id2), [core, id2]);
  const mountOutput2 = useCallback5((el) => core.mountOutput(el, id2), [core, id2]);
  const focus = useCallback5(() => {
    if (core.focus.get() !== id2) core.focus.set(id2);
  }, [core, id2]);
  const inputs = !core.store.locked(id2);
  const entry = own(core.blockRegistry(), block.registry_id);
  const ready = status?.status === "ready";
  const offered = core.boardActions.on({ block: id2 }).length > 0;
  return /* @__PURE__ */ jsxs46(
    "article",
    {
      ref,
      "aria-label": block.name,
      className: `${classes.card} bc:grid bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:p-3`,
      onPointerDown: focus,
      onFocus: focus,
      children: [
        /* @__PURE__ */ jsxs46("header", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
          /* @__PURE__ */ jsx58("h3", { className: classes.heading, children: block.name }),
          /* @__PURE__ */ jsx58("span", { className: "bc:text-xs bc:text-text-muted", children: entry?.name ?? block.registry_id }),
          /* @__PURE__ */ jsx58(StatusBadge, { status }),
          offered ? /* @__PURE__ */ jsx58("div", { className: "bc:ml-auto bc:flex bc:items-center bc:gap-1", children: /* @__PURE__ */ jsx58(
            ActionsButton,
            {
              ref: actions,
              label: `Actions of ${block.name}`,
              onPress: () => {
                const el = actions.current;
                if (el !== null) core.boardActions.menu({ block: id2 }, { el, anchor: el.getBoundingClientRect() });
              }
            }
          ) }) : null
        ] }),
        inputs ? /* @__PURE__ */ jsx58(Mounted, { mount: mountUi, label: "Inputs", className: "bc:min-w-0" }) : null,
        /* @__PURE__ */ jsxs46(
          "div",
          {
            className: `bc:min-w-0 ${sized ? "bc:flex bc:h-104 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`,
            children: [
              /* @__PURE__ */ jsx58(
                Mounted,
                {
                  mount: mountOutput2,
                  label: "Output",
                  data: { "data-ready": String(ready) },
                  className: `bc:min-w-0 bc:transition-opacity ${ready ? "" : "bc:opacity-50 bc:grayscale"} ${sized ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"}`
                }
              ),
              sized ? /* @__PURE__ */ jsx58(Footer, { ...footer }) : null
            ]
          }
        ),
        /* @__PURE__ */ jsx58(Pills, { ...footer }),
        /* @__PURE__ */ jsx58(Banners, { ...footer })
      ]
    }
  );
});
function StackGroup({ id: id2, stack, children }) {
  const { groups: groups2 } = useFrontEnd();
  const collapsed = useSyncExternalStore4(groups2.subscribe, () => groups2.collapsed(id2));
  const color2 = stack.color ?? void 0;
  return /* @__PURE__ */ jsx58(
    "section",
    {
      "aria-label": stack.name,
      className: "bc:rounded-xl bc:border-2 bc:border-border-default bc:p-3",
      style: color2 === void 0 ? void 0 : { borderColor: color2 },
      children: /* @__PURE__ */ jsxs46(
        Disclosure2,
        {
          isExpanded: !collapsed,
          onExpandedChange: (expanded) => {
            groups2.collapse(id2, !expanded);
          },
          children: [
            /* @__PURE__ */ jsx58(DisclosureHeader, { children: stack.name }),
            /* @__PURE__ */ jsx58(DisclosurePanel2, { children: /* @__PURE__ */ jsx58("div", { hidden: collapsed, children: /* @__PURE__ */ jsx58("div", { className: "bc:grid bc:gap-3 bc:pt-3", children }) }) })
          ]
        }
      )
    }
  );
}
var TONES = {
  ready: "bc:bg-status-ready",
  failed: "bc:bg-status-failed",
  invalid: "bc:bg-status-invalid",
  waiting: "bc:bg-status-waiting",
  stale: "bc:bg-status-stale",
  unevaluated: "bc:bg-status-unevaluated"
};
function StatusBadge({ status }) {
  if (status === void 0) return null;
  return /* @__PURE__ */ jsxs46(
    "span",
    {
      "data-status": status.status,
      "aria-busy": status.running,
      className: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-full bc:border bc:border-border-default bc:px-2 bc:py-0.5 bc:text-xs bc:text-text-muted",
      children: [
        /* @__PURE__ */ jsx58(
          "span",
          {
            "aria-hidden": "true",
            className: `bc:size-2 bc:rounded-full ${TONES[status.status]} ${status.running ? "bc:animate-pulse" : ""}`
          }
        ),
        status.running ? `${status.status}, running` : status.status
      ]
    }
  );
}

// srcts/modules/basic/layout.ts
function layoutItems(board) {
  const stackByBlock = /* @__PURE__ */ new Map();
  for (const [id2, stack] of Object.entries(board.stacks)) {
    for (const block of stack.blocks) stackByBlock.set(block, id2);
  }
  const items = [];
  const placed2 = /* @__PURE__ */ new Set();
  for (const block of Object.keys(board.blocks)) {
    const stack = stackByBlock.get(block);
    if (stack === void 0) {
      items.push({ block });
    } else if (!placed2.has(stack)) {
      placed2.add(stack);
      items.push({ stack, blocks: stackBlocks(board, stack) });
    }
  }
  for (const stack of Object.keys(board.stacks)) {
    if (!placed2.has(stack)) items.push({ stack, blocks: [] });
  }
  return items;
}
function stackBlocks(board, stack) {
  return (board.stacks[stack]?.blocks ?? []).filter((block) => Object.hasOwn(board.blocks, block));
}

// srcts/modules/basic/options.tsx
import { useCallback as useCallback6 } from "react";

// srcts/modules/basic/ui.tsx
import { jsx as jsx59, jsxs as jsxs47 } from "react/jsx-runtime";
function Dialog3({
  title,
  onClose,
  children
}) {
  return /* @__PURE__ */ jsx59(
    Modal2,
    {
      isOpen: true,
      onOpenChange: (open) => {
        if (!open) onClose();
      },
      children: /* @__PURE__ */ jsx59(Dialog2, { children: /* @__PURE__ */ jsxs47("div", { className: "bc:grid bc:gap-3", children: [
        /* @__PURE__ */ jsxs47("div", { className: "bc:flex bc:items-center bc:justify-between bc:gap-2", children: [
          /* @__PURE__ */ jsx59(Heading, { slot: "title", className: classes.heading, children: title }),
          /* @__PURE__ */ jsx59("button", { type: "button", className: classes.button, onClick: onClose, children: "Close" })
        ] }),
        children
      ] }) })
    }
  );
}

// srcts/modules/basic/options.tsx
import { jsx as jsx60, jsxs as jsxs48 } from "react/jsx-runtime";
function OptionsDialog({ onClose }) {
  const { core } = useFrontEnd();
  const board = useBoard(core);
  const registered = core.options.registered();
  const categories = /* @__PURE__ */ new Map();
  for (const id2 of Object.keys(board.options)) {
    const def = Object.hasOwn(registered, id2) ? registered[id2] : void 0;
    if (def === void 0) continue;
    categories.set(def.category, [...categories.get(def.category) ?? [], id2]);
  }
  return /* @__PURE__ */ jsx60(Dialog3, { title: "Board options", onClose, children: categories.size === 0 ? /* @__PURE__ */ jsx60("p", { className: styles.hint, children: "The board holds no board options." }) : /* @__PURE__ */ jsx60("div", { className: "bc:grid bc:divide-y bc:divide-border-default", children: [...categories].map(([category, ids3]) => /* @__PURE__ */ jsxs48("section", { "aria-label": category, className: "bc:grid bc:gap-2 bc:py-4 bc:first:pt-0 bc:last:pb-0", children: [
    /* @__PURE__ */ jsx60("h3", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: category }),
    ids3.map((id2) => /* @__PURE__ */ jsx60(OptionUi, { id: id2 }, id2))
  ] }, category)) }) });
}
function OptionUi({ id: id2 }) {
  const { core } = useFrontEnd();
  const mount2 = useCallback6((el) => core.mountOption(el, id2), [core, id2]);
  return /* @__PURE__ */ jsx60(Mounted, { mount: mount2 });
}

// srcts/modules/basic/plugins.tsx
import { useCallback as useCallback7, useLayoutEffect as useLayoutEffect7, useRef as useRef15, useSyncExternalStore as useSyncExternalStore5 } from "react";
import { jsx as jsx61, jsxs as jsxs49 } from "react/jsx-runtime";
function createSections() {
  const collapsed = /* @__PURE__ */ new Set();
  const listeners = /* @__PURE__ */ new Set();
  return {
    collapsed: (id2) => collapsed.has(id2),
    collapse(id2, collapse) {
      if (collapsed.has(id2) === collapse) return;
      if (collapse) collapsed.add(id2);
      else collapsed.delete(id2);
      for (const fn of [...listeners]) fn();
    },
    subscribe(fn) {
      const wrapped = () => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    }
  };
}
function hasSection(core, plugin) {
  return Object.hasOwn(core.plugins.appPlugins(), plugin) && core.plugins.pluginClass(plugin).ui !== void 0;
}
function placed(core) {
  return Object.entries(core.plugins.appPlugins()).filter(([id2]) => hasSection(core, id2));
}
function Plugins() {
  const { core } = useFrontEnd();
  const shown2 = placed(core);
  if (shown2.length === 0) return null;
  return /* @__PURE__ */ jsx61(
    "aside",
    {
      "aria-label": "Plugins",
      className: "bc:grid bc:min-w-0 bc:content-start bc:gap-4 bc:md:sticky bc:md:top-4 bc:md:max-h-[calc(100dvh-2rem)] bc:md:w-1/3 bc:md:min-w-md bc:md:shrink-0 bc:md:overflow-auto",
      children: shown2.map(([id2, plugin]) => /* @__PURE__ */ jsx61(PluginSection, { id: id2, plugin }, id2))
    }
  );
}
function PluginSection({ id: id2, plugin }) {
  const { core, inView: inView2, sections } = useFrontEnd();
  const notified = useNotified(core, { plugin: id2 });
  const pills = usePills(notified);
  const collapsed = useSyncExternalStore5(sections.subscribe, () => sections.collapsed(id2));
  const ref = useRef15(null);
  useLayoutEffect7(() => {
    const el = ref.current;
    return el === null ? void 0 : inView2.watch({ plugin: id2 }, el);
  }, [inView2, id2]);
  const mount2 = useCallback7((el) => core.mountPlugin(el, id2), [core, id2]);
  return /* @__PURE__ */ jsx61(
    "section",
    {
      "aria-label": plugin.name,
      className: `${classes.card} bc:min-w-0 bc:rounded-lg bc:border bc:border-border-default bc:p-3`,
      children: /* @__PURE__ */ jsxs49(
        Disclosure2,
        {
          isExpanded: !collapsed,
          onExpandedChange: (expanded) => {
            sections.collapse(id2, !expanded);
          },
          children: [
            /* @__PURE__ */ jsxs49("div", { className: "bc:grid bc:min-w-0 bc:gap-3", children: [
              /* @__PURE__ */ jsxs49("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
                /* @__PURE__ */ jsx61(DisclosureHeader, { children: plugin.name }),
                /* @__PURE__ */ jsx61(Pills, { ...pills, className: "" })
              ] }),
              /* @__PURE__ */ jsx61(Banners, { ...pills })
            ] }),
            /* @__PURE__ */ jsx61(DisclosurePanel2, { className: "bc:min-w-0", children: /* @__PURE__ */ jsx61("div", { ref, hidden: collapsed, className: "bc:min-w-0 bc:pt-3", children: /* @__PURE__ */ jsx61(Mounted, { mount: mount2, className: "bc:min-w-0" }) }) })
          ]
        }
      )
    }
  );
}

// srcts/modules/basic/toasts.ts
var SHOWN_FOR = 5e3;
function createToasts(notifications, sectioned) {
  const queue = new UNSTABLE_ToastQueue();
  const keys = /* @__PURE__ */ new Map();
  const gone = /* @__PURE__ */ new Set();
  function update(heard) {
    const holding = new Set(heard.map((notification) => notification.id));
    for (const [id2, key2] of keys) {
      if (holding.has(id2)) continue;
      keys.delete(id2);
      queue.close(key2);
    }
    for (const id2 of gone) {
      if (!holding.has(id2)) gone.delete(id2);
    }
    for (const notification of heard) {
      const { id: id2 } = notification;
      if (shownElsewhere(notification, sectioned) || gone.has(id2) || keys.has(id2)) continue;
      const key2 = queue.add(notification, {
        timeout: notification.severity === "message" ? SHOWN_FOR : void 0,
        // A toast that faded or that the user closed, rather than one whose
        // notification core withdrew, which no longer has its key.
        onClose: () => {
          if (keys.get(id2) !== key2) return;
          keys.delete(id2);
          gone.add(id2);
        }
      });
      keys.set(id2, key2);
    }
  }
  const off = notifications.subscribe(update);
  update(notifications.get());
  return {
    queue,
    stop() {
      off();
    }
  };
}
function shownElsewhere(notification, sectioned) {
  const { about } = notification;
  if (about === void 0) return false;
  return "block" in about || "plugin" in about && sectioned(about.plugin);
}
function concerned(about, core, board) {
  if (about === void 0) return "Board";
  if ("option" in about) return `Board option "${own(core.options.registered(), about.option)?.label ?? about.option}"`;
  if ("plugin" in about) return `Plugin "${own(core.plugins.appPlugins(), about.plugin)?.name ?? about.plugin}"`;
  if ("stack" in about) return `Stack "${own(board.stacks, about.stack)?.name ?? about.stack}"`;
  const block = (id2) => `"${own(board.blocks, id2)?.name ?? id2}"`;
  if ("block" in about) return `Block ${block(about.block)}`;
  const link = own(board.links, about.link);
  return link === void 0 ? `Link "${about.link}"` : `Link from ${block(link.from)} to ${block(link.to)}`;
}
function sourcePackage(notification) {
  return notification.source.module?.package;
}
function isReload(notification) {
  return notification.source.kind === "core" && notification.source.code === "page_reload_needed";
}

// srcts/modules/basic/toolbar.tsx
import { useEffect as useEffect3, useId as useId3, useReducer, useState as useState21 } from "react";

// srcts/modules/basic/storage.ts
var KINDS3 = /* @__PURE__ */ new Set(["button", "menu", "status"]);
function drawnControls(core) {
  const storage = core.plugins.storage();
  const plugin = storage === void 0 ? void 0 : Object.keys(core.plugins.appPlugins()).find((id2) => core.plugins.pluginClass(id2).storage === storage);
  if (storage === void 0 || plugin === void 0) return void 0;
  try {
    const twice = /* @__PURE__ */ new Set();
    const controls = distinct(storage.controls({ core, plugin }), twice);
    return {
      controls,
      failed: false,
      ...twice.size === 0 ? {} : { problem: `Storage controls of one list share the ids ${quoted([...twice])}, so only the first of each draws.` }
    };
  } catch (error) {
    return { controls: [], failed: true, problem: `The storage controls failed: ${errorMessage(error)}` };
  }
}
function buttonsFromControls(controls) {
  return controls.flatMap((control) => {
    if (control.kind === "button") return [control];
    if (control.kind === "menu") return buttonsFromControls(control.items);
    return [];
  });
}
async function runButton(core, button) {
  try {
    await button.run();
  } catch (error) {
    core.log("blockr.core").error(asError(error));
  }
}
function distinct(list, twice) {
  const seen = /* @__PURE__ */ new Set();
  const kept = [];
  for (const control of list) {
    if (!KINDS3.has(control.kind)) continue;
    if (seen.has(control.id)) {
      twice.add(control.id);
      continue;
    }
    seen.add(control.id);
    kept.push(control.kind === "menu" ? { ...control, items: distinct(control.items, twice) } : control);
  }
  return kept;
}

// srcts/modules/basic/toolbar.tsx
import { Fragment as Fragment29, jsx as jsx62, jsxs as jsxs50 } from "react/jsx-runtime";
function Toolbar3({ board, showOptions }) {
  const { core } = useFrontEnd();
  const { canUndo, canRedo } = useHistory(core);
  const [renaming, setRenaming] = useState21(false);
  const locked2 = core.lock.board;
  function step2(back) {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch {
    }
  }
  return /* @__PURE__ */ jsxs50("header", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:border-b bc:border-border-default bc:pb-3", children: [
    renaming ? /* @__PURE__ */ jsx62(
      NameField,
      {
        label: "Board name",
        name: board.name,
        onSubmit: (name2) => {
          void runCommand(core, "blockr.core-rename_board", { name: name2 });
        },
        onDone: () => {
          setRenaming(false);
        }
      }
    ) : /* @__PURE__ */ jsx62("h1", { className: "bc:m-0 bc:font-sans bc:text-lg bc:font-semibold", children: board.name }),
    locked2 || renaming ? null : /* @__PURE__ */ jsx62(
      "button",
      {
        type: "button",
        className: classes.quiet,
        "aria-label": "Rename board",
        onClick: () => {
          setRenaming(true);
        },
        children: "Rename"
      }
    ),
    /* @__PURE__ */ jsxs50(Toolbar2, { "aria-label": "Board", className: "bc:ml-auto", children: [
      /* @__PURE__ */ jsx62(
        Button2,
        {
          variant: "secondary",
          onPress: () => {
            showOptions();
          },
          children: "Options"
        }
      ),
      /* @__PURE__ */ jsx62(Separator2, { orientation: "vertical" }),
      /* @__PURE__ */ jsx62(
        Button2,
        {
          variant: "secondary",
          isDisabled: !canUndo,
          "aria-keyshortcuts": "Control+Z Meta+Z",
          onPress: () => {
            step2(true);
          },
          children: "Undo"
        }
      ),
      /* @__PURE__ */ jsx62(
        Button2,
        {
          variant: "secondary",
          isDisabled: !canRedo,
          "aria-keyshortcuts": "Control+Shift+Z Meta+Shift+Z Control+Y",
          onPress: () => {
            step2(false);
          },
          children: "Redo"
        }
      ),
      /* @__PURE__ */ jsx62(StorageControls, {})
    ] })
  ] });
}
function StorageControls() {
  const { core } = useFrontEnd();
  const storage = core.plugins.storage();
  const [, redraw] = useReducer((n) => n + 1, 0);
  useEffect3(() => {
    if (storage?.subscribe === void 0) return void 0;
    try {
      return storage.subscribe(redraw);
    } catch (error) {
      core.log("blockr.core").error(asError(error));
      return void 0;
    }
  }, [core, storage]);
  const drawn3 = drawnControls(core);
  const problem = drawn3?.problem;
  useEffect3(() => {
    if (problem !== void 0) core.log("blockr.core").error(problem);
  }, [core, problem]);
  if (drawn3 === void 0 || !drawn3.failed && drawn3.controls.length === 0) return null;
  return /* @__PURE__ */ jsxs50(Fragment29, { children: [
    /* @__PURE__ */ jsx62(Separator2, { orientation: "vertical" }),
    drawn3.failed ? /* @__PURE__ */ jsx62("span", { role: "alert", className: "bc:font-sans bc:text-sm bc:text-text-danger", children: /* @__PURE__ */ jsx62(Tipped, { tooltip: problem, children: "Storage failed" }) }) : drawn3.controls.map((control) => /* @__PURE__ */ jsx62(Control, { control }, control.id))
  ] });
}
function Control({ control }) {
  switch (control.kind) {
    case "button":
      return /* @__PURE__ */ jsx62(StorageButton, { button: control });
    case "menu":
      return /* @__PURE__ */ jsx62(StorageMenu, { label: control.label, items: control.items });
    case "status":
      return /* @__PURE__ */ jsx62("span", { role: "status", className: "bc:font-sans bc:text-sm bc:text-text-muted", children: /* @__PURE__ */ jsx62(Tipped, { tooltip: control.tooltip, children: control.text }) });
  }
}
function StorageButton({ button }) {
  const { core } = useFrontEnd();
  const reason = useId3();
  const off = button.disabled === true;
  const drawn3 = /* @__PURE__ */ jsx62(
    Button2,
    {
      variant: "secondary",
      isDisabled: off,
      "aria-keyshortcuts": ariaShortcuts(button.shortcut),
      "aria-describedby": off && button.tooltip !== void 0 ? reason : void 0,
      onPress: () => {
        void runButton(core, button);
      },
      children: button.label
    }
  );
  if (button.tooltip === void 0) return drawn3;
  if (off) {
    return /* @__PURE__ */ jsxs50(Fragment29, { children: [
      drawn3,
      /* @__PURE__ */ jsx62("span", { id: reason, className: "bc:font-sans bc:text-sm bc:text-text-muted", children: button.tooltip })
    ] });
  }
  return /* @__PURE__ */ jsxs50(TooltipTrigger, { children: [
    drawn3,
    /* @__PURE__ */ jsx62(Tooltip2, { children: button.tooltip })
  ] });
}
function Tipped({ tooltip, children }) {
  if (tooltip === void 0) return /* @__PURE__ */ jsx62(Fragment29, { children });
  return /* @__PURE__ */ jsxs50(TooltipTrigger, { children: [
    /* @__PURE__ */ jsx62(Focusable, { children: /* @__PURE__ */ jsx62("span", { role: "button", tabIndex: 0, className: "bc:cursor-default bc:rounded bc:outline-none bc:focus-visible:outline-2 bc:focus-visible:outline-border-accent", children }) }),
    /* @__PURE__ */ jsx62(Tooltip2, { children: tooltip })
  ] });
}
function StorageMenu({ label, items }) {
  const { core } = useFrontEnd();
  const mac = onMac(window);
  return /* @__PURE__ */ jsxs50(MenuTrigger2, { placement: "bottom end", children: [
    /* @__PURE__ */ jsxs50(Button2, { variant: "secondary", children: [
      label,
      /* @__PURE__ */ jsx62("span", { "aria-hidden": "true", children: "\u25BE" })
    ] }),
    /* @__PURE__ */ jsx62(Menu2, { "aria-label": label, children: entries(items, core, mac) })
  ] });
}
function entries(items, core, mac) {
  return items.map((item) => {
    if (item.kind === "menu") {
      return /* @__PURE__ */ jsxs50(SubmenuTrigger2, { children: [
        /* @__PURE__ */ jsx62(MenuItem2, { id: item.id, textValue: item.label, children: item.label }),
        /* @__PURE__ */ jsx62(Menu2, { "aria-label": item.label, children: entries(item.items, core, mac) })
      ] }, item.id);
    }
    const label = item.kind === "button" ? item.label : item.text;
    const shortcut = item.kind === "button" ? shortcutLabel(item.shortcut, mac) : void 0;
    return /* @__PURE__ */ jsxs50(
      MenuItem2,
      {
        id: item.id,
        textValue: label,
        isDisabled: item.kind === "status" || item.disabled,
        onAction: item.kind === "button" ? () => {
          void runButton(core, item);
        } : void 0,
        children: [
          /* @__PURE__ */ jsx62(Text, { slot: "label", children: label }),
          item.tooltip === void 0 ? null : /* @__PURE__ */ jsx62(Text, { slot: "description", children: item.tooltip }),
          shortcut === void 0 ? null : /* @__PURE__ */ jsx62(Keyboard, { children: shortcut })
        ]
      },
      item.id
    );
  });
}

// srcts/modules/basic/board.tsx
import { jsx as jsx63, jsxs as jsxs51 } from "react/jsx-runtime";
function BasicBoard({ front }) {
  return /* @__PURE__ */ jsx63(FrontEndContext, { value: front, children: /* @__PURE__ */ jsx63(Page, {}) });
}
function Page() {
  const { core } = useFrontEnd();
  const board = useBoard(core);
  const items = useMemo6(() => layoutItems(board), [board]);
  const [options, setOptions] = useState22(false);
  const close = useCallback8(() => {
    setOptions(false);
  }, []);
  const card = (id2) => {
    const block = board.blocks[id2];
    if (block === void 0) return null;
    return /* @__PURE__ */ jsx63(BlockCard, { id: id2, block }, `block-${id2}`);
  };
  return /* @__PURE__ */ jsxs51("div", { className: `${classes.page} bc:flex bc:min-h-full bc:flex-col bc:gap-4 bc:p-4 bc:font-sans`, children: [
    /* @__PURE__ */ jsx63(
      Toolbar3,
      {
        board,
        showOptions: () => {
          setOptions(true);
        }
      }
    ),
    /* @__PURE__ */ jsxs51("div", { className: "bc:flex bc:flex-col bc:gap-4 bc:md:flex-row-reverse bc:md:items-start", children: [
      /* @__PURE__ */ jsx63(Plugins, {}),
      /* @__PURE__ */ jsxs51("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-4", children: [
        items.length === 0 ? /* @__PURE__ */ jsx63("p", { className: styles.hint, children: "The board holds no blocks yet." }) : null,
        items.map((item) => {
          if ("block" in item) return card(item.block);
          const stack = board.stacks[item.stack];
          if (stack === void 0) return null;
          return /* @__PURE__ */ jsx63(StackGroup, { id: item.stack, stack, children: item.blocks.length === 0 ? /* @__PURE__ */ jsx63("p", { className: styles.hint, children: "The stack holds no blocks." }) : item.blocks.map(card) }, `stack-${item.stack}`);
        })
      ] })
    ] }),
    options ? /* @__PURE__ */ jsx63(OptionsDialog, { onClose: close }) : null,
    /* @__PURE__ */ jsx63(Toasts, {})
  ] });
}
function Toasts() {
  const { core, toasts } = useFrontEnd();
  const board = useBoard(core);
  return /* @__PURE__ */ jsx63(ToastRegion, { queue: toasts.queue, children: ({ toast }) => /* @__PURE__ */ jsx63(ToastItem, { toast, title: concerned(toast.content.about, core, board) }) });
}
function ToastItem({ toast, title }) {
  const notification = toast.content;
  const { severity } = notification;
  const mark = MARKS[severity];
  const pkg = sourcePackage(notification);
  return /* @__PURE__ */ jsxs51(Toast, { toast, "data-severity": severity, className: `bc:border-l-4 ${LEFT[severity]}`, children: [
    /* @__PURE__ */ jsx63("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx63(SeverityIcon, { severity }) }),
    /* @__PURE__ */ jsxs51(UNSTABLE_ToastContent, { role: severity === "message" ? "status" : void 0, className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-1", children: [
      /* @__PURE__ */ jsxs51(Text, { slot: "title", elementType: "p", className: "bc:m-0 bc:text-xs bc:font-semibold", children: [
        title,
        pkg === void 0 ? null : /* @__PURE__ */ jsxs51("span", { className: "bc:font-normal bc:text-text-muted", children: [
          " \xB7 ",
          pkg
        ] })
      ] }),
      /* @__PURE__ */ jsx63(Text, { slot: "description", elementType: "p", className: "bc:m-0 bc:text-sm bc:break-words bc:whitespace-pre-wrap", children: notification.message }),
      isReload(notification) ? /* @__PURE__ */ jsx63("div", { children: /* @__PURE__ */ jsx63(
        Button2,
        {
          variant: "secondary",
          onPress: () => {
            location.reload();
          },
          children: "Reload"
        }
      ) }) : null
    ] }),
    /* @__PURE__ */ jsx63(Button2, { slot: "close", variant: "quiet", children: /* @__PURE__ */ jsx63("span", { "aria-hidden": "true", children: "\xD7" }) })
  ] });
}
var LEFT = {
  error: "bc:border-l-border-danger",
  warning: "bc:border-l-amber-500",
  message: "bc:border-l-border-accent"
};

// srcts/modules/basic/demand.ts
var OWNER = "blockr.core-basic";
var SHOWN = 1;
var IN_VIEW = 2;
function demandShown(core, view = window) {
  const every = core.scheduler.demand(OWNER, (board) => Object.keys(board.blocks), { priority: SHOWN });
  const near = core.scheduler.demand(OWNER, [], { priority: IN_VIEW });
  const watched = /* @__PURE__ */ new Map();
  const seen = /* @__PURE__ */ new Set();
  let released = false;
  function update() {
    if (released) return;
    const blocks = /* @__PURE__ */ new Set();
    const plugins = /* @__PURE__ */ new Set();
    for (const el of seen) {
      const object2 = watched.get(el);
      if (object2 === void 0) continue;
      if ("block" in object2) blocks.add(object2.block);
      else plugins.add(object2.plugin);
    }
    near.set([...blocks]);
    core.onScreen.set({ blocks: [...blocks], plugins: [...plugins] });
  }
  const Observer = view.IntersectionObserver;
  const observer = Observer === void 0 ? void 0 : new Observer((entries2) => {
    for (const entry of entries2) {
      if (!watched.has(entry.target)) continue;
      if (entry.isIntersecting) seen.add(entry.target);
      else seen.delete(entry.target);
    }
    update();
  });
  return {
    watch(object2, el) {
      watched.set(el, object2);
      observer?.observe(el);
      return () => {
        observer?.unobserve(el);
        watched.delete(el);
        if (seen.delete(el)) update();
      };
    },
    element(object2) {
      for (const [el, at] of watched) {
        if ("block" in object2 ? "block" in at && at.block === object2.block : "plugin" in at && at.plugin === object2.plugin) return el;
      }
      return void 0;
    },
    measure() {
      seen.clear();
      for (const el of watched.keys()) if (inView(el, view)) seen.add(el);
      update();
    },
    release() {
      released = true;
      observer?.disconnect();
      every.release();
      near.release();
    }
  };
}
function inView(el, view) {
  const rect = el.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0 && rect.bottom > 0 && rect.right > 0 && rect.top < view.innerHeight && rect.left < view.innerWidth;
}

// srcts/modules/basic/shortcuts.ts
function bindShortcuts(doc, core) {
  function onKey(event) {
    if (event.defaultPrevented || editable(event.target)) return;
    const step2 = undoOrRedo(event);
    if (step2 !== void 0) {
      event.preventDefault();
      try {
        if (step2 === "undo") core.store.undo();
        else core.store.redo();
      } catch {
      }
      return;
    }
    const button = buttonsFromControls(drawnControls(core)?.controls ?? []).find(
      (b) => b.disabled !== true && b.shortcut !== void 0 && matchesShortcut(b.shortcut, event)
    );
    if (button === void 0) return;
    event.preventDefault();
    if (!event.repeat) void runButton(core, button);
  }
  doc.addEventListener("keydown", onKey);
  return () => {
    doc.removeEventListener("keydown", onKey);
  };
}
function undoOrRedo(event) {
  if (!(event.ctrlKey || event.metaKey) || event.altKey) return void 0;
  const key2 = event.key.toLowerCase();
  if (key2 === "z") return event.shiftKey ? "redo" : "undo";
  if (key2 === "y" && !event.shiftKey) return "redo";
  return void 0;
}
function editable(target2) {
  if (!(target2 instanceof HTMLElement)) return false;
  return target2.isContentEditable || ["INPUT", "TEXTAREA", "SELECT"].includes(target2.tagName);
}

// srcts/modules/basic/index.tsx
import { jsx as jsx64 } from "react/jsx-runtime";
var basic_default = {
  frontend: { render, show }
};
var drawn2 = /* @__PURE__ */ new WeakMap();
function render({ el, core }) {
  const doc = el.ownerDocument;
  const shown2 = demandShown(core, doc.defaultView ?? window);
  const toasts = createToasts(core.notifications, (plugin) => hasSection(core, plugin));
  const front = { core, toasts, inView: shown2, sections: createSections(), groups: createSections() };
  const root = createRoot5(el);
  flushSync7(() => {
    root.render(/* @__PURE__ */ jsx64(BasicBoard, { front }));
  });
  shown2.measure();
  const unbind = bindShortcuts(doc, core);
  drawn2.set(el, front);
  return () => {
    if (drawn2.get(el) === front) drawn2.delete(el);
    unbind();
    toasts.stop();
    shown2.release();
    root.unmount();
  };
}
function show({ object: object2, el }) {
  const front = drawn2.get(el);
  const target2 = front?.inView.element(object2);
  if (front === void 0 || target2 === void 0) return;
  flushSync7(() => {
    if ("plugin" in object2) {
      front.sections.collapse(object2.plugin, false);
      return;
    }
    const stack = Object.entries(front.core.store.board().stacks).find(([, held4]) => held4.blocks.includes(object2.block));
    if (stack !== void 0) front.groups.collapse(stack[0], false);
  });
  const into = "plugin" in object2 ? target2.closest("section") ?? target2 : target2;
  into.scrollIntoView({ block: "nearest", inline: "nearest" });
}

// srcts/modules/edit-board/index.tsx
import { flushSync as flushSync8 } from "react-dom";
import { createRoot as createRoot6 } from "react-dom/client";

// srcts/modules/edit-board/board.tsx
import { useCallback as useCallback11, useMemo as useMemo8 } from "react";

// srcts/modules/edit-board/menus.tsx
import { useCallback as useCallback9, useContext as useContext13, useEffect as useEffect4, useLayoutEffect as useLayoutEffect8, useMemo as useMemo7, useRef as useRef16, useState as useState23 } from "react";
import { jsx as jsx65, jsxs as jsxs52 } from "react/jsx-runtime";
function useMenus(core) {
  const [refusal2, setRefusal] = useState23(null);
  const [shown2, setShown] = useState23(() => /* @__PURE__ */ new Map());
  const [back, setBack] = useState23(null);
  const places = useRef16(/* @__PURE__ */ new Map());
  const latest = useRef16(shown2);
  useEffect4(() => {
    latest.current = shown2;
  }, [shown2]);
  useEffect4(
    () => () => {
      const open2 = [...latest.current.values()];
      queueMicrotask(() => {
        for (const args of open2) args.close();
      });
    },
    []
  );
  useEffect4(() => {
    if (back === null) return void 0;
    const timer = setTimeout(() => {
      setBack(null);
    });
    return () => {
      clearTimeout(timer);
    };
  }, [back]);
  const ended = useCallback9((key2, args) => {
    setShown((before) => before.get(key2) === args ? without4(before, key2) : before);
    setBack(key2);
  }, []);
  const placeOf = useCallback9(
    (key2) => {
      const known2 = places.current.get(key2);
      if (known2 !== void 0) return known2;
      const place = (args) => {
        setShown((before) => new Map(before).set(key2, args));
        return () => {
          ended(key2, args);
        };
      };
      places.current.set(key2, place);
      return place;
    },
    [ended]
  );
  const open = useCallback9(
    (target2, key2, button) => {
      setRefusal(null);
      core.boardActions.menu(target2, {
        el: button,
        anchor: button.getBoundingClientRect(),
        notifier: (notifications, action2) => {
          setRefusal({ title: editTitle(core.store.board(), action2, target2), reasons: notifiedReasons(notifications) });
        },
        advanced: placeOf(key2)
      });
    },
    [core, placeOf]
  );
  const close = useCallback9(
    (key2, args) => {
      args.close();
      ended(key2, args);
    },
    [ended]
  );
  return useMemo7(() => ({ refusal: refusal2, shown: shown2, open, close, back }), [refusal2, shown2, open, close, back]);
}
function MenuButton({ menus, target: target2, place, label }) {
  const ref = useRef16(null);
  const back = menus.back === place;
  useLayoutEffect8(() => {
    if (back) ref.current?.focus();
  }, [back]);
  return /* @__PURE__ */ jsx65(
    ActionsButton,
    {
      ref,
      label,
      onPress: () => {
        const el = ref.current;
        if (el !== null) menus.open(target2, place, el);
      }
    }
  );
}
function PlaceForm({ args, onClose }) {
  const box = useRef16(null);
  const mount2 = useCallback9((el) => args.mount(el), [args]);
  useEffect4(() => {
    const el = box.current;
    if (el === null) return void 0;
    let listOpen = false;
    const down = (event) => {
      listOpen = event.target instanceof Element && event.target.getAttribute("aria-expanded") === "true";
    };
    const heard = (event) => {
      if (event.key === "Escape" && !event.defaultPrevented && !listOpen) onClose();
    };
    el.addEventListener("keydown", down, { capture: true });
    el.addEventListener("keydown", heard);
    return () => {
      el.removeEventListener("keydown", down, { capture: true });
      el.removeEventListener("keydown", heard);
    };
  }, [onClose]);
  return /* @__PURE__ */ jsxs52("div", { ref: box, role: "group", "aria-label": args.title, className: "bc:grid bc:gap-2 bc:py-1 bc:whitespace-normal", ...KEPT, children: [
    /* @__PURE__ */ jsx65(TakesKeys, {}),
    /* @__PURE__ */ jsx65(Mounted, { mount: mount2, className: "bc:min-w-0" }),
    /* @__PURE__ */ jsx65("div", { className: "bc:flex bc:gap-2", children: /* @__PURE__ */ jsx65(Button2, { variant: "secondary", onPress: onClose, children: "Close" }) })
  ] });
}
var KEPT = Object.fromEntries(
  ["onPointerDown", "onMouseDown", "onTouchStart", "onClick", "onDoubleClick", "onKeyDown", "onKeyUp"].map((handler) => [
    handler,
    (event) => {
      event.stopPropagation();
    }
  ])
);
var taking = /* @__PURE__ */ new WeakMap();
function TakesKeys() {
  const disable = useContext13(TableStateContext)?.setKeyboardNavigationDisabled;
  useEffect4(() => {
    if (disable === void 0) return void 0;
    taking.set(disable, (taking.get(disable) ?? 0) + 1);
    disable(true);
    return () => {
      const left2 = (taking.get(disable) ?? 1) - 1;
      taking.set(disable, left2);
      if (left2 === 0) disable(false);
    };
  }, [disable]);
  return null;
}
function editTitle(board, action2, target2) {
  const object2 = named4(board, target2);
  const edit = own(CORE_EDITS, action2.id);
  return edit === void 0 ? `${action2.label} failed for ${object2}.` : `Could not ${edit(object2)}.`;
}
var CORE_EDITS = {
  "blockr.core-append_block_action": (object2) => `append a block to ${object2}`,
  "blockr.core-prepend_block_action": (object2) => `prepend a block to ${object2}`,
  "blockr.core-insert_block_action": (object2) => `insert a block into ${object2}`,
  "blockr.core-add_link_action": (object2) => `add a link for ${object2}`,
  "blockr.core-add_stack_action": (object2) => `put ${object2} in a stack`,
  "blockr.core-rename_action": (object2) => `rename ${object2}`,
  "blockr.core-edit_link_action": (object2) => `change ${object2}`,
  "blockr.core-edit_stack_action": (object2) => `change ${object2}`,
  "blockr.core-remove_block_action": (object2) => `remove ${object2}`,
  "blockr.core-remove_link_action": (object2) => `remove ${object2}`,
  "blockr.core-remove_stack_action": (object2) => `remove ${object2}`,
  "blockr.core-remove_selected_action": (object2) => `remove ${object2}`
};
function named4(board, target2) {
  if ("selection" in target2) return "the selection";
  if ("board" in target2) return "the board";
  if ("block" in target2) return `the block "${blockName(board, target2.block)}"`;
  if ("link" in target2) {
    const link = own(board.links, target2.link);
    return link === void 0 ? `the link "${target2.link}"` : `the link from "${blockName(board, link.from)}" to "${blockName(board, link.to)}"`;
  }
  if ("stack" in target2) return `the stack "${own(board.stacks, target2.stack)?.name ?? target2.stack}"`;
  if ("option" in target2) return `the board option "${target2.option}"`;
  return `the plugin "${target2.plugin}"`;
}
function without4(map, key2) {
  const next = new Map(map);
  next.delete(key2);
  return next;
}

// srcts/modules/edit-board/blocks.tsx
import { Fragment as Fragment30, jsx as jsx66, jsxs as jsxs53 } from "react/jsx-runtime";
var ADD_BLOCK = "blockr.core-add_block_action";
var COLUMNS = [
  { id: "name", name: "Name", rowHeader: true },
  { id: "id", name: "ID" },
  { id: "class", name: "Class" }
];
function BlocksSection({ core, board }) {
  const registry = core.blockRegistry();
  const className = (block) => own(registry, block.registry_id)?.name ?? block.registry_id;
  const view = useTableView(Object.entries(board.blocks), ([id2, block]) => `${block.name} ${id2} ${className(block)}`);
  const count2 = Object.keys(board.blocks).length;
  const menus = useMenus(core);
  return /* @__PURE__ */ jsxs53(Section, { title: "Blocks", count: count2, children: [
    offers(core, ADD_BLOCK, BOARD) ? /* @__PURE__ */ jsx66(Part, { title: "Add a block", children: /* @__PURE__ */ jsx66(Advanced, { core, id: ADD_BLOCK, target: BOARD }) }) : null,
    /* @__PURE__ */ jsx66(Part, { title: "Blocks on the board", children: count2 === 0 ? /* @__PURE__ */ jsx66("p", { className: styles.hint, children: "The board holds no blocks yet." }) : /* @__PURE__ */ jsxs53(Fragment30, { children: [
      /* @__PURE__ */ jsx66(TableSearch, { label: "Search blocks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx66("p", { className: styles.hint, children: "No block matches the search." }) : /* @__PURE__ */ jsx66(
        ObjectsTable,
        {
          core,
          menus,
          kind: "block",
          label: "The board's blocks",
          columns: COLUMNS,
          rows: view.rows.map(([id2, block]) => ({
            id: id2,
            text: block.name,
            menuLabel: `Actions of the block ${block.name}`,
            cells: [
              { content: /* @__PURE__ */ jsx66("span", { className: "bc:font-medium", children: block.name }), className: WRAP },
              { content: id2, className: "bc:font-mono bc:text-xs bc:whitespace-normal! bc:break-all" },
              { content: className(block), className: `${WRAP} bc:text-text-muted` }
            ]
          }))
        }
      ),
      /* @__PURE__ */ jsx66(Refusal2, { refusal: menus.refusal }),
      /* @__PURE__ */ jsx66(Pager, { label: "Pages of blocks", view })
    ] }) })
  ] });
}

// srcts/modules/edit-board/links.tsx
import { Fragment as Fragment31, jsx as jsx67, jsxs as jsxs54 } from "react/jsx-runtime";
var CREATE_LINK2 = "blockr.core-add_link_action";
var COLUMNS2 = [
  { id: "from", name: "From", rowHeader: true },
  { id: "to", name: "To" },
  { id: "input", name: "Input" }
];
function LinksSection({ core, board, locked: locked2 }) {
  const links = Object.entries(board.links);
  const view = useTableView(
    links,
    ([, link]) => [blockName(board, link.from), link.from, blockName(board, link.to), link.to, link.input].join(" ")
  );
  const variadic2 = variadicBlocks(board, core.blockRegistry());
  const menus = useMenus(core);
  return /* @__PURE__ */ jsxs54(Section, { title: "Links", count: links.length, children: [
    locked2 ? null : /* @__PURE__ */ jsx67(Part, { title: "Add a link", children: offers(core, CREATE_LINK2, BOARD) ? /* @__PURE__ */ jsx67(Advanced, { core, id: CREATE_LINK2, target: BOARD }) : /* @__PURE__ */ jsx67("p", { className: styles.hint, children: Object.keys(board.blocks).length < 2 ? "A link joins two blocks, so the board needs two blocks first." : "No new link can join two blocks of the board." }) }),
    /* @__PURE__ */ jsx67(Part, { title: "Links on the board", children: links.length === 0 ? /* @__PURE__ */ jsx67("p", { className: styles.hint, children: "No link joins two blocks yet." }) : /* @__PURE__ */ jsxs54(Fragment31, { children: [
      /* @__PURE__ */ jsx67(TableSearch, { label: "Search links", view }),
      view.found === 0 ? /* @__PURE__ */ jsx67("p", { className: styles.hint, children: "No link matches the search." }) : /* @__PURE__ */ jsx67(
        ObjectsTable,
        {
          core,
          menus,
          kind: "link",
          label: "The board's links",
          columns: COLUMNS2,
          rows: view.rows.map(([id2, link]) => {
            const from = blockName(board, link.from);
            const to = blockName(board, link.to);
            return {
              id: id2,
              text: `${from} \u2192 ${to}`,
              menuLabel: `Actions of the link from ${from} to ${to}`,
              cells: [
                { content: /* @__PURE__ */ jsx67(BlockLabel, { board, id: link.from }), className: WRAP },
                { content: /* @__PURE__ */ jsx67(BlockLabel, { board, id: link.to }), className: WRAP },
                {
                  content: variadic2.has(link.to) ? /* @__PURE__ */ jsx67(VariadicInput2, { board, id: id2, link }) : /* @__PURE__ */ jsx67("span", { className: "bc:font-mono bc:text-xs", children: link.input }),
                  className: WRAP
                }
              ]
            };
          })
        }
      ),
      /* @__PURE__ */ jsx67(Refusal2, { refusal: menus.refusal }),
      /* @__PURE__ */ jsx67(Pager, { label: "Pages of links", view })
    ] }) })
  ] });
}
function BlockLabel({ board, id: id2 }) {
  return /* @__PURE__ */ jsxs54("span", { className: "bc:grid", children: [
    /* @__PURE__ */ jsx67("span", { children: blockName(board, id2) }),
    /* @__PURE__ */ jsx67("span", { className: "bc:font-mono bc:text-xs bc:text-text-muted bc:break-all", children: id2 })
  ] });
}
function VariadicInput2({ board, id: id2, link }) {
  const place = createGraph(board).linksInto(link.to).indexOf(id2) + 1;
  return /* @__PURE__ */ jsxs54("span", { className: "bc:grid", children: [
    link.input === "" ? /* @__PURE__ */ jsx67("span", { className: "bc:text-text-muted", children: "No name" }) : /* @__PURE__ */ jsx67("span", { className: "bc:font-mono bc:text-xs", children: link.input }),
    /* @__PURE__ */ jsxs54("span", { className: "bc:text-xs bc:text-text-muted", children: [
      "Position ",
      place
    ] })
  ] });
}
function variadicBlocks(board, registry) {
  return new Set(Object.keys(board.blocks).filter((id2) => isVariadicBlock(board, registry, id2)));
}

// srcts/modules/edit-board/selection.ts
import { useCallback as useCallback10, useSyncExternalStore as useSyncExternalStore6 } from "react";
function useSelected(core) {
  const subscribe = useCallback10(
    (notify) => core.selection.subscribe(() => {
      notify();
    }),
    [core]
  );
  return useSyncExternalStore6(subscribe, () => core.selection.get());
}
function selectedIds(selected, kind) {
  return new Set(selected.flatMap((object2) => idOf(object2, kind) ?? []));
}
function tickRows(core, kind, shown2, keys) {
  const ticked = (id2) => keys === "all" || keys.has(id2);
  const rows = new Set(shown2);
  const current2 = core.selection.get();
  const kept = current2.filter((object2) => {
    const id2 = idOf(object2, kind);
    return id2 === void 0 || !rows.has(id2) || ticked(id2);
  });
  const held4 = selectedIds(current2, kind);
  const added = shown2.filter((id2) => ticked(id2) && !held4.has(id2)).map((id2) => objectOf(kind, id2));
  core.selection.set([...kept, ...added]);
}
function objectOf(kind, id2) {
  if (kind === "block") return { block: id2 };
  if (kind === "link") return { link: id2 };
  return { stack: id2 };
}
function idOf(object2, kind) {
  if (kind === "block") return "block" in object2 ? object2.block : void 0;
  if (kind === "link") return "link" in object2 ? object2.link : void 0;
  return "stack" in object2 ? object2.stack : void 0;
}

// srcts/modules/edit-board/stacks.tsx
import { Fragment as Fragment32, jsx as jsx68, jsxs as jsxs55 } from "react/jsx-runtime";
var CREATE_STACK2 = "blockr.core-add_stack_action";
var COLUMNS3 = [
  { id: "name", name: "Name", rowHeader: true },
  { id: "blocks", name: "Blocks" }
];
function StacksSection({ core, board, locked: locked2 }) {
  const stacks = Object.entries(board.stacks);
  const view = useTableView(stacks, ([id2, stack]) => [stack.name, id2, ...stack.blocks.map((block) => blockName(board, block))].join(" "));
  const menus = useMenus(core);
  return /* @__PURE__ */ jsxs55(Section, { title: "Stacks", count: stacks.length, children: [
    locked2 || !offers(core, CREATE_STACK2, BOARD) ? null : /* @__PURE__ */ jsx68(Part, { title: "Add a stack", children: /* @__PURE__ */ jsx68(Advanced, { core, id: CREATE_STACK2, target: BOARD }) }),
    /* @__PURE__ */ jsx68(Part, { title: "Stacks on the board", children: stacks.length === 0 ? /* @__PURE__ */ jsx68("p", { className: styles.hint, children: "The board has no stacks yet." }) : /* @__PURE__ */ jsxs55(Fragment32, { children: [
      /* @__PURE__ */ jsx68(TableSearch, { label: "Search stacks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx68("p", { className: styles.hint, children: "No stack matches the search." }) : /* @__PURE__ */ jsx68(
        ObjectsTable,
        {
          core,
          menus,
          kind: "stack",
          label: "The board's stacks",
          columns: COLUMNS3,
          rows: view.rows.map(([id2, stack]) => ({
            id: id2,
            text: stack.name,
            menuLabel: `Actions of the stack ${stack.name}`,
            cells: [
              {
                content: /* @__PURE__ */ jsxs55("span", { className: "bc:flex bc:items-center bc:gap-1.5", children: [
                  /* @__PURE__ */ jsx68(Swatch, { color: stack.color }),
                  /* @__PURE__ */ jsx68("span", { className: "bc:font-medium", children: stack.name })
                ] }),
                className: WRAP
              },
              { content: /* @__PURE__ */ jsx68(StackBlocks, { board, blocks: stack.blocks }), className: WRAP }
            ]
          }))
        }
      ),
      /* @__PURE__ */ jsx68(Refusal2, { refusal: menus.refusal }),
      /* @__PURE__ */ jsx68(Pager, { label: "Pages of stacks", view })
    ] }) })
  ] });
}
var SHOWN_BLOCKS = 6;
function StackBlocks({ board, blocks }) {
  if (blocks.length === 0) return /* @__PURE__ */ jsx68("span", { className: "bc:text-text-muted", children: "No blocks" });
  const rest = blocks.length - SHOWN_BLOCKS;
  return /* @__PURE__ */ jsxs55("ul", { "aria-label": "Blocks", className: "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0", children: [
    blocks.slice(0, SHOWN_BLOCKS).map((block) => /* @__PURE__ */ jsxs55("li", { className: "bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs", children: [
      blockName(board, block),
      " ",
      /* @__PURE__ */ jsx68("span", { className: "bc:font-mono bc:text-text-muted", children: block })
    ] }, block)),
    rest > 0 ? /* @__PURE__ */ jsxs55("li", { className: "bc:px-1 bc:text-xs bc:text-text-muted", children: [
      "and ",
      rest,
      " more"
    ] }) : null
  ] });
}
function Swatch({ color: color2 }) {
  return color2 === null ? /* @__PURE__ */ jsx68("span", { "aria-hidden": "true", className: RING }) : /* @__PURE__ */ jsx68(ColorSwatch2, { color: color2, className: DOT });
}
var RING = "bc:inline-block bc:size-3 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default";
var DOT = "bc:size-3! bc:rounded-full!";

// srcts/modules/edit-board/board.tsx
import { jsx as jsx69, jsxs as jsxs56 } from "react/jsx-runtime";
function EditBoard({ core }) {
  const board = useBoard(core);
  const locked2 = core.lock.board;
  return /* @__PURE__ */ jsxs56("div", { className: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsx69(SelectionBar, { core }),
    locked2 ? null : /* @__PURE__ */ jsx69(BlocksSection, { core, board }),
    /* @__PURE__ */ jsx69(LinksSection, { core, board, locked: locked2 }),
    /* @__PURE__ */ jsx69(StacksSection, { core, board, locked: locked2 })
  ] });
}
var SELECTION = "selection";
function SelectionBar({ core }) {
  const selected = useSelected(core);
  const menus = useMenus(core);
  const form = menus.shown.get(SELECTION);
  if (selected.length === 0 && form === void 0) return null;
  const target2 = { selection: selected };
  return /* @__PURE__ */ jsxs56("div", { role: "region", "aria-label": "Selection", className: "bc:grid bc:gap-2", children: [
    selected.length === 0 ? null : /* @__PURE__ */ jsxs56("div", { className: "bc:flex bc:items-center bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:px-3 bc:py-1.5 bc:text-sm", children: [
      /* @__PURE__ */ jsxs56("span", { "aria-live": "polite", children: [
        selected.length,
        " selected"
      ] }),
      /* @__PURE__ */ jsxs56("span", { className: "bc:ml-auto bc:flex bc:items-center bc:gap-1", children: [
        core.boardActions.on(target2).length > 0 ? /* @__PURE__ */ jsx69(MenuButton, { menus, target: target2, place: SELECTION, label: "Actions of the selection" }) : null,
        /* @__PURE__ */ jsx69(
          Button2,
          {
            variant: "quiet",
            onPress: () => {
              core.selection.set([]);
            },
            children: "Clear selection"
          }
        )
      ] })
    ] }),
    form === void 0 ? null : /* @__PURE__ */ jsx69(
      PlaceForm,
      {
        args: form,
        onClose: () => {
          menus.close(SELECTION, form);
        }
      }
    ),
    /* @__PURE__ */ jsx69(Refusal2, { refusal: menus.refusal })
  ] });
}
function Section({ title, count: count2, children }) {
  return /* @__PURE__ */ jsx69("section", { "aria-label": title, className: "bc:rounded-md bc:border bc:border-border-default", children: /* @__PURE__ */ jsxs56(Disclosure2, { defaultExpanded: true, children: [
    /* @__PURE__ */ jsxs56(DisclosureHeader, { className: "bc:text-sm bc:font-semibold bc:[&>button]:gap-2 bc:[&>button]:px-3 bc:[&>button]:py-2", children: [
      title,
      /* @__PURE__ */ jsx69("span", { className: "bc:rounded-full bc:border bc:border-border-default bc:px-1.5 bc:text-xs bc:font-normal bc:text-text-muted", children: count2 })
    ] }),
    /* @__PURE__ */ jsx69(DisclosurePanel2, { children: /* @__PURE__ */ jsx69("div", { className: "bc:grid bc:gap-4 bc:border-t bc:border-border-default bc:p-3", children }) })
  ] }) });
}
function Part({ title, children }) {
  return /* @__PURE__ */ jsxs56("div", { role: "group", "aria-label": title, className: "bc:grid bc:gap-2", children: [
    /* @__PURE__ */ jsx69("h4", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: title }),
    children
  ] });
}
var WRAP = "bc:whitespace-normal! bc:break-words";
function ObjectsTable({
  core,
  menus,
  kind,
  label,
  columns,
  rows
}) {
  const selected = useSelected(core);
  const keys = useMemo8(() => selectedIds(selected, kind), [selected, kind]);
  const offered = new Set(rows.flatMap(({ id: id2 }) => core.boardActions.on(objectOf(kind, id2)).length > 0 ? [id2] : []));
  const actions = offered.size > 0;
  const span = columns.length + (actions ? 1 : 0);
  return /* @__PURE__ */ jsxs56(
    Table2,
    {
      "aria-label": label,
      selectionMode: "multiple",
      selectedKeys: keys,
      onSelectionChange: (picked) => {
        tickRows(
          core,
          kind,
          rows.map(({ id: id2 }) => id2),
          picked
        );
      },
      children: [
        /* @__PURE__ */ jsxs56(TableHeader2, { children: [
          columns.map((column) => /* @__PURE__ */ jsx69(Column2, { id: column.id, isRowHeader: column.rowHeader === true, children: column.name }, column.id)),
          actions ? /* @__PURE__ */ jsx69(Column2, { id: "actions", textValue: "Actions", className: "bc:w-px", children: /* @__PURE__ */ jsx69("span", { className: "bc:sr-only", children: "Actions" }) }) : null
        ] }),
        /* @__PURE__ */ jsx69(TableBody2, { children: rows.map((row) => {
          const form = menus.shown.get(row.id);
          return /* @__PURE__ */ jsx69(Row2, { id: row.id, textValue: row.text, children: form === void 0 ? [
            ...row.cells.map(({ content, className }, i) => /* @__PURE__ */ jsx69(Cell2, { className, children: content }, i)),
            ...actions ? [
              /* @__PURE__ */ jsx69(Cell2, { className: "bc:text-right", children: offered.has(row.id) ? /* @__PURE__ */ jsx69(MenuButton, { menus, target: objectOf(kind, row.id), place: row.id, label: row.menuLabel }) : null }, "actions")
            ] : []
          ] : /* @__PURE__ */ jsx69(Cell2, { colSpan: span, className: WRAP, children: /* @__PURE__ */ jsx69(
            PlaceForm,
            {
              args: form,
              onClose: () => {
                menus.close(row.id, form);
              }
            }
          ) }) }, row.id);
        }) })
      ]
    }
  );
}
var BOARD = { board: true };
function offers(core, id2, target2) {
  return core.boardActions.on(target2).some((action2) => action2.id === id2);
}
function Advanced({ core, id: id2, target: target2 }) {
  const key2 = JSON.stringify(target2);
  const mount2 = useCallback11((el) => core.boardActions.mountAdvanced(el, id2, JSON.parse(key2)), [core, id2, key2]);
  return /* @__PURE__ */ jsx69(Mounted, { mount: mount2, className: "bc:min-w-0" });
}

// srcts/modules/edit-board/index.tsx
import { jsx as jsx70 } from "react/jsx-runtime";
var edit_board_default = {
  ui: {
    render({ el, core }) {
      const root = createRoot6(el);
      flushSync8(() => {
        root.render(/* @__PURE__ */ jsx70(EditBoard, { core }));
      });
      return () => {
        root.unmount();
      };
    }
  }
};

// srcts/storage/storage.ts
function coreStorage(doc) {
  return {
    controls({ core, plugin }) {
      const save = {
        kind: "button",
        id: "save",
        label: "Save",
        run: () => saveBoard(core, doc ?? document)
      };
      const open = {
        kind: "button",
        id: "open",
        label: "Open",
        run: () => openBoard(core, plugin, doc ?? document)
      };
      return core.lock.load ? [save] : [save, open];
    }
  };
}
function saveBoard(core, doc) {
  try {
    download(core.store.save(), fileName(core.store.board()), doc);
    return Promise.resolve();
  } catch (error) {
    return Promise.reject(asError(error));
  }
}
async function openBoard(core, plugin, doc) {
  let text;
  try {
    text = await pickFile(doc);
  } catch (error) {
    core.notifications.notify(plugin, { severity: "error", code: "board_open_failed", message: errorMessage(error) });
    return;
  }
  if (text !== null) await core.store.open(text);
}
function fileName(board, now = /* @__PURE__ */ new Date()) {
  const two = (n) => String(n).padStart(2, "0");
  const date = `${String(now.getFullYear())}-${two(now.getMonth() + 1)}-${two(now.getDate())}`;
  const time = `${two(now.getHours())}-${two(now.getMinutes())}-${two(now.getSeconds())}`;
  return `${board.id}_${date}_${time}.json`;
}
function download(text, name2, doc = document) {
  const url = URL.createObjectURL(new Blob([text], { type: "application/json" }));
  const link = doc.createElement("a");
  link.href = url;
  link.download = name2;
  link.hidden = true;
  doc.body.append(link);
  link.click();
  link.remove();
  setTimeout(() => {
    URL.revokeObjectURL(url);
  }, 0);
}
function pickFile(doc = document) {
  return new Promise((resolve, reject) => {
    const input2 = doc.createElement("input");
    input2.type = "file";
    input2.accept = ".json,application/json";
    input2.hidden = true;
    input2.addEventListener(
      "change",
      () => {
        const file = input2.files?.[0];
        input2.remove();
        if (file === void 0) resolve(null);
        else file.text().then(resolve, reject);
      },
      { once: true }
    );
    input2.addEventListener(
      "cancel",
      () => {
        input2.remove();
        resolve(null);
      },
      { once: true }
    );
    doc.body.append(input2);
    input2.click();
  });
}

// srcts/modules/file-storage/index.ts
var file_storage_default = {
  storage: coreStorage()
};

// srcts/options/schemas.ts
var optionSchemas = {
  n_rows: {
    type: "integer",
    // A page of a table holds a row at least, as the coercion of table
    // outputs takes it.
    minimum: 1,
    description: "How many rows of a table a block's output shows"
  },
  dark_mode: {
    enum: ["light", "dark"],
    description: "Whether the board shows in its light or its dark theme"
  }
};

// srcts/options/ui.tsx
import { jsx as jsx71 } from "react/jsx-runtime";
function numberUi(label, min) {
  function NumberUi({ host }) {
    return /* @__PURE__ */ jsx71(NumberField2, { bound: useValue(host), label, minValue: min, step: 1 });
  }
  return reactModule(NumberUi);
}
function toggleButtonGroupUi(label, items) {
  function ToggleButtonGroupUi({ host }) {
    return /* @__PURE__ */ jsx71(ToggleButtonGroup2, { bound: useValue(host), label, disallowEmptySelection: true, children: items.map((item) => /* @__PURE__ */ jsx71(ToggleButton2, { id: item.id, children: item.name }, item.id)) });
  }
  return reactModule(ToggleButtonGroupUi);
}

// srcts/options/options.ts
var nRows = {
  label: "Preview rows",
  category: "Table options",
  schema: optionSchemas.n_rows,
  // A string reads as the number that it holds.
  default: async (core) => {
    const value2 = await core.blockrOption("n_rows");
    if (value2 === void 0) return 10;
    return checked3("n_rows", takesRows, value2, typeof value2 === "string" ? asNumber(value2) : value2);
  },
  ui: numberUi("Preview rows", optionSchemas.n_rows.minimum)
};
var darkMode = {
  label: "Light/dark mode",
  category: "Theme options",
  schema: optionSchemas.dark_mode,
  default: async (core) => {
    const value2 = await core.blockrOption("dark_mode");
    if (value2 === void 0) return preferredTheme();
    const logical = typeof value2 === "string" ? asLogical(value2) : value2;
    if (typeof logical === "boolean") return logical ? "dark" : "light";
    return checked3("dark_mode", takesTheme, value2);
  },
  ui: toggleButtonGroupUi("Light/dark mode", [
    { id: "light", name: "Light" },
    { id: "dark", name: "Dark" }
  ]),
  onChange(value2) {
    setTheme(value2);
  }
};
var coreOptions = {
  n_rows: nRows,
  dark_mode: darkMode
};
function setTheme(theme) {
  document.documentElement.setAttribute("data-bc-theme", theme === "dark" ? "dark" : "light");
}
function preferredTheme() {
  return matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light";
}
function checked3(id2, takes, sent2, value2 = sent2) {
  if (takes(value2)) return value2;
  throw new TypeError(`The blockr option ${id2} is ${JSON.stringify(sent2)}, which the board option ${id2} does not take.`);
}
function takesRows(value2) {
  return typeof value2 === "number" && Number.isInteger(value2) && value2 >= optionSchemas.n_rows.minimum;
}
function takesTheme(value2) {
  const allowed = optionSchemas.dark_mode.enum;
  return allowed.includes(value2);
}

// srcts/stubs/advanced.ts
function stubDialogs() {
  let open;
  return {
    show(args, at) {
      const doc = at.el.ownerDocument;
      const el = doc.createElement("div");
      doc.body.append(el);
      let closed = false;
      const unmounts = [];
      const hide = () => {
        if (closed) return;
        closed = true;
        offAbort();
        for (const unmount of unmounts.splice(0)) unmount();
        el.remove();
        if (open === shown2) open = void 0;
        args.close();
      };
      const offAbort = at.signal === void 0 ? () => void 0 : onAbort(at.signal, hide);
      const shown2 = { id: args.id, title: args.title, target: args.target, el, closed: () => closed, close: hide };
      open = shown2;
      unmounts.push(args.mount(el));
      return () => {
        hide();
      };
    },
    open: () => open
  };
}

// srcts/stubs/menu.ts
function stubMenus() {
  let open;
  const driven = /* @__PURE__ */ new WeakMap();
  return {
    show(menu) {
      open = menu;
      return () => {
        if (open === menu) open = void 0;
      };
    },
    open() {
      if (open === void 0) return void 0;
      const existing = driven.get(open);
      if (existing !== void 0) return existing;
      const made = drive(open);
      driven.set(open, made);
      return made;
    }
  };
}
function drive(menu) {
  function find(label, kinds) {
    const { rows } = menu.get();
    const index = rows.findIndex((row) => row.label === label && kinds.includes(row.kind));
    if (index < 0) {
      throw new Error(`The menu shows no ${kinds.join(" or ")} row "${label}". It shows ${rows.length === 0 ? "none" : quoted(rows.map((row) => row.label))}.`);
    }
    return index;
  }
  return {
    level() {
      return menu.closed() ? void 0 : withoutNumber(menu.get());
    },
    closed: () => menu.closed(),
    pick(label) {
      if (menu.closed()) return Promise.resolve();
      return label === void 0 ? menu.pick() : menu.pick(find(label, ["item", "text"]));
    },
    openAdvanced(label) {
      if (menu.closed()) return Promise.resolve();
      if (label === void 0) {
        if (!menu.get().advanced) throw new Error("The level shows no open icon.");
        return menu.openAdvanced();
      }
      const index = find(label, ["item"]);
      const row = menu.get().rows[index];
      if (row?.kind !== "item" || row.advanced !== true) throw new Error(`The item "${label}" shows no open icon.`);
      return menu.openAdvanced(index);
    },
    tick(row, choice) {
      if (menu.closed()) return;
      const index = find(row, ["multi"]);
      const shown2 = menu.get().rows[index];
      const found = shown2?.kind === "multi" ? shown2.choices.find((each) => each.label === choice) : void 0;
      if (found === void 0) throw new Error(`The multi "${row}" shows no choice "${choice}".`);
      menu.tick(index, found.id);
    },
    type(value2, row) {
      if (menu.closed()) return;
      menu.type(value2, row === void 0 ? void 0 : find(row, ["text"]));
    },
    escape() {
      menu.escape();
    },
    close: () => menu.close()
  };
}
function withoutNumber(level) {
  return Object.fromEntries(Object.entries(level).filter(([key2]) => key2 !== "id"));
}

// inst/protocol/fixtures/hello.json
var hello_default = {
  description: "The handshake of a core app. The client names every capability, and R, which runs requests in the session's process, names cancel and progress but not interrupt. Its reply carries everything the client needs before it draws the board: the block registry, which holds every class registered, the upload block's included, each with the board options that its UI module reads, none for core's classes, and the renderers of its outputs, where the module that core offers other packages' UIs is served, the app's plugins, core's basic frontend, its storage plugin and its Edit board plugin, each under the id of its class with its class's name and version, the board options that it reads, dark_mode for core's basic frontend, and where the module of its class is served, since the script passes serve() no plugins and no other package lists a plugin class, nothing in frontend, since core's basic frontend takes no argument of serve(), core's board options, each with where its module is served and the version of its shape, since every app uses core, no renderers, since the app names none for a board option, no lock, and the opening board that the app script built, as a JSON board. R drew the ids that the script left out, that of the link from a to f and the board's, which the script gave a name but no id, so the JSON board holds every id, as one that the client saves does.",
  client: [
    {
      id: "1",
      kind: "hello",
      protocol: "1.0",
      capabilities: ["cancel", "interrupt", "progress"]
    }
  ],
  r: [
    [
      {
        id: "1",
        status: "ok",
        protocol: "1.0",
        capabilities: ["cancel", "progress"],
        packages: { "blockr.core": "0.2.0" },
        block_registry: {
          "blockr.core-dataset_block": {
            id: "blockr.core-dataset_block",
            name: "dataset block",
            description: "Choose a dataset from a package",
            keywords: ["dataset", "data", "import", "package", "source"],
            category: "input",
            icon: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-database" aria-hidden="true" role="img"><path d="M2 2h12v12H2z"></path></svg>',
            inputs: [],
            module: "blockr.core/dataset-block.js?v=5e0c8b1d",
            version: 1,
            schema: {
              type: "object",
              properties: {
                dataset: {
                  type: "string",
                  description: "Selects the dataset to use."
                },
                package: {
                  type: "string",
                  description: "R package to source the dataset from."
                }
              },
              required: ["package"],
              additionalProperties: false
            },
            state: { package: "datasets" },
            options: [],
            requests: {
              datasets: {
                inputs: [],
                state_args: ["package"],
                options: []
              }
            },
            output_options: ["n_rows"],
            renderers: [
              {
                id: "blockr.core-table_output",
                module: "blockr.core/basic-table.js?v=b7e2a915"
              }
            ]
          },
          "blockr.core-head_block": {
            id: "blockr.core-head_block",
            name: "head/tail block",
            description: "Data head/tail",
            url: "https://bristolmyerssquibb.github.io/blockr.core/reference/new_head_block.html",
            guidance: 'Keeps the first or last `n` rows of the input with `utils::head()` / `utils::tail()`. `direction` chooses the end: "head" for the first rows, "tail" for the last.',
            keywords: ["head", "tail", "first", "last", "rows", "limit"],
            category: "transform",
            icon: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-eye" aria-hidden="true" role="img"><path d="M2 2h12v12H2z"></path></svg>',
            inputs: ["data"],
            module: "blockr.core/head-block.js?v=9a41f2c7",
            css: "blockr.core/head-block.css?v=c03e7b58",
            version: 1,
            schema: {
              type: "object",
              properties: {
                n: {
                  type: "integer",
                  minimum: 0,
                  description: "Number of rows to keep."
                },
                direction: {
                  enum: ["head", "tail"],
                  description: "Row end to keep, either head (first) or tail (last)."
                }
              },
              required: ["n", "direction"],
              additionalProperties: false
            },
            state: { n: 6, direction: "head" },
            options: [],
            requests: {
              n_max: { inputs: ["data"], state_args: [], options: [] }
            },
            output_options: ["n_rows"],
            renderers: [
              {
                id: "blockr.core-table_output",
                module: "blockr.core/basic-table.js?v=b7e2a915"
              }
            ]
          },
          "blockr.core-merge_block": {
            id: "blockr.core-merge_block",
            name: "merge block",
            description: "Joining or merging datasets",
            keywords: [
              "merge",
              "join",
              "inner join",
              "outer join",
              "left join",
              "right join"
            ],
            category: "transform",
            icon: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-union" aria-hidden="true" role="img"><path d="M2 2h12v12H2z"></path></svg>',
            inputs: ["x", "y"],
            module: "blockr.core/merge-block.js?v=77d0a9e3",
            version: 1,
            schema: {
              type: "object",
              properties: {
                by: {
                  type: "array",
                  items: { type: "string" },
                  description: "Shared key column(s) to join on."
                },
                all_x: {
                  type: "boolean",
                  description: "Keep unmatched rows from the first input."
                },
                all_y: {
                  type: "boolean",
                  description: "Keep unmatched rows from the second input."
                }
              },
              required: ["all_x", "all_y"],
              additionalProperties: false
            },
            state: { all_x: false, all_y: false },
            options: [],
            requests: {
              shared: {
                inputs: ["x", "y"],
                state_args: [],
                options: []
              }
            },
            output_options: ["n_rows"],
            renderers: [
              {
                id: "blockr.core-table_output",
                module: "blockr.core/basic-table.js?v=b7e2a915"
              }
            ]
          },
          "blockr.core-subset_block": {
            id: "blockr.core-subset_block",
            name: "subset block",
            description: "Row and column subsetting",
            keywords: [
              "filter",
              "subset",
              "select",
              "rows",
              "columns",
              "where"
            ],
            category: "transform",
            icon: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-funnel" aria-hidden="true" role="img"><path d="M2 2h12v12H2z"></path></svg>',
            inputs: ["data"],
            module: "blockr.core/subset-block.js?v=2b6f04d9",
            version: 1,
            schema: {
              type: "object",
              properties: {
                subset: {
                  type: "string",
                  description: "Logical row-filter expression, as a string."
                },
                select: {
                  type: "string",
                  description: "Column-selection expression, as a string."
                }
              },
              required: ["subset", "select"],
              additionalProperties: false
            },
            state: { subset: "", select: "" },
            options: [],
            requests: {},
            output_options: ["n_rows"],
            renderers: [
              {
                id: "blockr.core-table_output",
                module: "blockr.core/basic-table.js?v=b7e2a915"
              }
            ]
          },
          "blockr.core-rbind_block": {
            id: "blockr.core-rbind_block",
            name: "rbind block",
            description: "Row-binding of datasets",
            keywords: [
              "rbind",
              "bind",
              "concatenate",
              "rows",
              "append",
              "union"
            ],
            category: "transform",
            icon: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-chevron-bar-expand" aria-hidden="true" role="img"><path d="M2 2h12v12H2z"></path></svg>',
            inputs: ["..."],
            module: "blockr.core/rbind-block.js?v=e8c1d6a2",
            version: 1,
            schema: {
              type: "object",
              properties: {},
              additionalProperties: false
            },
            state: {},
            options: [],
            requests: {},
            output_options: ["n_rows"],
            renderers: [
              {
                id: "blockr.core-table_output",
                module: "blockr.core/basic-table.js?v=b7e2a915"
              }
            ]
          },
          "blockr.core-scatter_block": {
            id: "blockr.core-scatter_block",
            name: "scatter plot block",
            description: "Scatter plotting",
            keywords: [
              "scatter",
              "scatter plot",
              "chart",
              "visualization",
              "points"
            ],
            category: "plot",
            icon: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-dice-5" aria-hidden="true" role="img"><path d="M2 2h12v12H2z"></path></svg>',
            inputs: ["data"],
            module: "blockr.core/scatter-block.js?v=41a7e0bf",
            version: 1,
            schema: {
              type: "object",
              properties: {
                x: {
                  type: "string",
                  description: "Column for the horizontal axis."
                },
                y: {
                  type: "string",
                  description: "Column for the vertical axis."
                }
              },
              additionalProperties: false
            },
            state: {},
            options: [],
            requests: {
              columns: {
                inputs: ["data"],
                state_args: [],
                options: []
              }
            },
            output_options: [],
            renderers: [
              {
                id: "blockr.core-plot_output",
                module: "blockr.core/basic-plot.js?v=f19d4a73"
              }
            ]
          },
          "blockr.core-upload_block": {
            id: "blockr.core-upload_block",
            name: "data upload block",
            description: "Upload data",
            keywords: ["upload", "file", "import", "data", "session"],
            category: "input",
            icon: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-upload" aria-hidden="true" role="img"><path d="M2 2h12v12H2z"></path></svg>',
            inputs: [],
            module: "blockr.core/upload-block.js?v=d5932c60",
            version: 1,
            schema: {
              type: "object",
              properties: { file_path: { type: "string" } },
              additionalProperties: false
            },
            state: {},
            options: [],
            requests: {
              upload: {
                inputs: [],
                state_args: [],
                options: [],
                action: true,
                file: true
              }
            },
            output_options: [],
            renderers: [
              {
                id: "blockr.core-text_output",
                module: "blockr.core/basic-text.js?v=3c8f61d0"
              }
            ]
          }
        },
        modules: {
          "blockr.core-controls": {
            module: "blockr.core/blockr-core-controls.js?v=4d2a6c91"
          }
        },
        plugins: {
          "blockr.core-basic_frontend_plugin": {
            name: "Basic frontend",
            version: 1,
            options: ["dark_mode"],
            module: "blockr.core/basic.js?v=0f3d8e21",
            css: "blockr.core/basic.css?v=6a9b2c4e"
          },
          "blockr.core-file_storage_plugin": {
            name: "File storage",
            version: 1,
            options: [],
            module: "blockr.core/file-storage.js?v=8e1f0b2d"
          },
          "blockr.core-edit_board_plugin": {
            name: "Edit board",
            version: 1,
            options: [],
            module: "blockr.core/edit-board.js?v=5c1e7a39"
          }
        },
        frontend: {},
        options: {
          n_rows: {
            package: "blockr.core",
            module: "blockr.core/n-rows-option.js?v=2b9c7e14",
            version: 1
          },
          dark_mode: {
            package: "blockr.core",
            module: "blockr.core/dark-mode-option.js?v=a47e2c19",
            version: 1
          }
        },
        renderers: {},
        lock: { board: false, load: false },
        board: {
          format: 1,
          packages: { "blockr.core": "0.2.0" },
          board: {
            id: "brisk_otter",
            name: "Iris review",
            blocks: {
              a: {
                registry_id: "blockr.core-dataset_block",
                version: 1,
                state: { dataset: "iris", package: "datasets" },
                name: "Iris",
                locked: null
              },
              b: {
                registry_id: "blockr.core-head_block",
                version: 1,
                state: { n: 6, direction: "head" },
                name: "Head",
                locked: null
              },
              c: {
                registry_id: "blockr.core-merge_block",
                version: 1,
                state: { by: ["Species"], all_x: false, all_y: false },
                name: "Merge",
                locked: null
              },
              d: {
                registry_id: "blockr.core-subset_block",
                version: 1,
                state: {
                  subset: "log(Sepal.Width - 3) > 0",
                  select: ""
                },
                name: "Wide sepals",
                locked: null
              },
              e: {
                registry_id: "blockr.core-rbind_block",
                version: 1,
                state: {},
                name: "Bind",
                locked: null
              },
              f: {
                registry_id: "blockr.core-scatter_block",
                version: 1,
                state: { x: "Sepal.Length", y: "Sepal.Width" },
                name: "Scatter",
                locked: null
              }
            },
            links: {
              ab: { from: "a", to: "b", input: "data" },
              ac: { from: "a", to: "c", input: "x" },
              bc: { from: "b", to: "c", input: "y" },
              ad: { from: "a", to: "d", input: "data" },
              be: { from: "b", to: "e", input: "" },
              de: { from: "d", to: "e", input: "" },
              keen_finch: { from: "a", to: "f", input: "data" }
            },
            stacks: { s1: { name: "Input", blocks: ["a", "b"], color: null } },
            options: {
              n_rows: { value: 50, version: 1 }
            },
            plugins: {}
          }
        }
      }
    ]
  ]
};

// inst/outputs/fixtures/table-output.json
var table_output_default = {
  description: "Sample table outputs, as the method of table_output() for data frames makes them, with the renderer that core gives table outputs.",
  outputs: [
    {
      description: "The second page of the first seven rows of iris, at three rows to a page, as the board option n_rows gives.",
      options: {
        n_rows: 3
      },
      params: {
        page: 2
      },
      output: {
        renderer: {
          id: "blockr.core-table_output",
          module: "blockr.core/basic-table.js?v=b7e2a915"
        },
        columns: [
          {
            name: "Sepal.Length",
            class: "numeric"
          },
          {
            name: "Sepal.Width",
            class: "numeric"
          },
          {
            name: "Petal.Length",
            class: "numeric"
          },
          {
            name: "Petal.Width",
            class: "numeric"
          },
          {
            name: "Species",
            class: "factor"
          }
        ],
        rows: [
          [
            4.6,
            3.1,
            1.5,
            0.2,
            "setosa"
          ],
          [
            5,
            3.6,
            1.4,
            0.2,
            "setosa"
          ],
          [
            5.4,
            3.9,
            1.7,
            0.4,
            "setosa"
          ]
        ],
        total: 7,
        page: 2,
        page_size: 3
      }
    },
    {
      description: "The last page of airquality, sorted by Ozone from the largest, at fifty rows to a page, where the rows without Ozone come last and a missing value is null.",
      options: {
        n_rows: 50
      },
      params: {
        page: 4,
        sort: {
          column: "Ozone",
          desc: true
        }
      },
      output: {
        renderer: {
          id: "blockr.core-table_output",
          module: "blockr.core/basic-table.js?v=b7e2a915"
        },
        columns: [
          {
            name: "Ozone",
            class: "integer"
          },
          {
            name: "Solar.R",
            class: "integer"
          },
          {
            name: "Wind",
            class: "numeric"
          },
          {
            name: "Temp",
            class: "integer"
          },
          {
            name: "Month",
            class: "integer"
          },
          {
            name: "Day",
            class: "integer"
          }
        ],
        rows: [
          [
            null,
            255,
            12.6,
            75,
            8,
            23
          ],
          [
            null,
            153,
            5.7,
            88,
            8,
            27
          ],
          [
            null,
            145,
            13.2,
            77,
            9,
            27
          ]
        ],
        total: 153,
        page: 4,
        page_size: 50
      }
    },
    {
      description: "Cells of many kinds: an infinite number and NaN as null, which JSON cannot hold, a factor as its labels, a date and a time as R prints them, a list-column as a summary of each cell that is not a single value, and names that R would change, such as 2019 and a name that repeats.",
      options: {
        n_rows: 50
      },
      params: {},
      output: {
        renderer: {
          id: "blockr.core-table_output",
          module: "blockr.core/basic-table.js?v=b7e2a915"
        },
        columns: [
          {
            name: "number",
            class: "numeric"
          },
          {
            name: "count",
            class: "integer"
          },
          {
            name: "flag",
            class: "logical"
          },
          {
            name: "text",
            class: "character"
          },
          {
            name: "level",
            class: "factor"
          },
          {
            name: "date",
            class: "Date"
          },
          {
            name: "time",
            class: "POSIXct"
          },
          {
            name: "2019",
            class: "integer"
          },
          {
            name: "text",
            class: "character"
          },
          {
            name: "cells",
            class: "list"
          }
        ],
        rows: [
          [
            1.5,
            1,
            true,
            "a",
            "low",
            "2024-01-31",
            "2024-01-31 12:30:00",
            1,
            "v",
            "<integer> [3]"
          ],
          [
            null,
            null,
            false,
            null,
            "high",
            null,
            "2024-01-31 13:30:00",
            2,
            "w",
            "one"
          ],
          [
            null,
            3,
            null,
            "c",
            null,
            "2024-02-02",
            "2024-01-31 14:30:00",
            3,
            "x",
            "<NULL>"
          ],
          [
            null,
            4,
            true,
            "d",
            "low",
            "2024-02-03",
            "2024-01-31 15:30:00",
            4,
            "y",
            "<data.frame> [6 \xD7 2]"
          ],
          [
            null,
            5,
            true,
            "e",
            "high",
            "2024-02-04",
            "2024-01-31 16:30:00",
            5,
            "z",
            "<lm>"
          ]
        ],
        total: 5,
        page: 1,
        page_size: 50
      }
    },
    {
      description: "A table without rows, which has one page, holding no rows.",
      options: {
        n_rows: 50
      },
      params: {},
      output: {
        renderer: {
          id: "blockr.core-table_output",
          module: "blockr.core/basic-table.js?v=b7e2a915"
        },
        columns: [
          {
            name: "Sepal.Length",
            class: "numeric"
          },
          {
            name: "Sepal.Width",
            class: "numeric"
          },
          {
            name: "Petal.Length",
            class: "numeric"
          },
          {
            name: "Petal.Width",
            class: "numeric"
          },
          {
            name: "Species",
            class: "factor"
          }
        ],
        rows: [],
        total: 0,
        page: 1,
        page_size: 50
      }
    },
    {
      description: "The first page of a table whose total row count is unknown, as for a lazy table, whose count would run its whole query, so that the renderer shows no page count.",
      options: {
        n_rows: 3
      },
      params: {},
      output: {
        renderer: {
          id: "blockr.core-table_output",
          module: "blockr.core/basic-table.js?v=b7e2a915"
        },
        columns: [
          {
            name: "Sepal.Length",
            class: "numeric"
          },
          {
            name: "Sepal.Width",
            class: "numeric"
          },
          {
            name: "Petal.Length",
            class: "numeric"
          },
          {
            name: "Petal.Width",
            class: "numeric"
          },
          {
            name: "Species",
            class: "factor"
          }
        ],
        rows: [
          [
            5.1,
            3.5,
            1.4,
            0.2,
            "setosa"
          ],
          [
            4.9,
            3,
            1.4,
            0.2,
            "setosa"
          ],
          [
            4.7,
            3.2,
            1.3,
            0.2,
            "setosa"
          ]
        ],
        total: null,
        page: 1,
        page_size: 3
      }
    }
  ]
};

// inst/outputs/fixtures/text-output.json
var text_output_default = {
  description: "Sample text outputs, as text_output() prints a result, at R's width of 80, with the renderer that core gives text outputs.",
  outputs: [
    {
      description: "Thirty integers, as print() wraps them at a width of 80.",
      options: {},
      params: {},
      output: {
        renderer: {
          id: "blockr.core-text_output",
          module: "blockr.core/basic-text.js?v=3c8f61d0"
        },
        text: " [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25\n[26] 26 27 28 29 30"
      }
    },
    {
      description: "A named character vector, as print() quotes and aligns it.",
      options: {},
      params: {},
      output: {
        renderer: {
          id: "blockr.core-text_output",
          module: "blockr.core/basic-text.js?v=3c8f61d0"
        },
        text: ' first second \n   "a"   "bb" '
      }
    }
  ]
};

// inst/outputs/fixtures/plot-output.json
var plot_output_default = {
  description: "Sample plot outputs, as plot_output() draws a captured base plot, with the renderer that core gives plot outputs. The PNG of each is the file its body_file names, beside this one.",
  outputs: [
    {
      description: "A scatter plot of iris, drawn at 320 by 200 CSS pixels and a pixel ratio of 2, so that its PNG has 640 by 400 pixels.",
      options: {},
      params: {},
      output: {
        renderer: {
          id: "blockr.core-plot_output",
          module: "blockr.core/basic-plot.js?v=f19d4a73"
        },
        width: 320,
        height: 200
      },
      body: {
        type: "image/png"
      },
      body_file: "plot-output-scatter.png"
    }
  ]
};

// srcts/stubs/core.ts
var HELLO = hello_default.r[0]?.[0];
var SAMPLES = {
  "blockr.core-table_output": table_output_default,
  "blockr.core-text_output": text_output_default,
  "blockr.core-plot_output": plot_output_default
};
var CORE_CLASSES = {
  "blockr.core-basic_frontend_plugin": { api: 1, default: basic_default },
  "blockr.core-file_storage_plugin": { api: 1, default: file_storage_default },
  "blockr.core-edit_board_plugin": { api: 1, default: edit_board_default }
};
var INFO = Object.freeze({
  width: 640,
  height: 400,
  pixel_ratio: 1,
  bg: "#ffffff",
  fg: "#212529",
  accent: "#0d6efd",
  font: "sans-serif"
});
async function stubCore(options = {}) {
  const base = options.hello ?? HELLO;
  const given = options.modules ?? {};
  const offered = Object.keys(given).filter(
    (id2) => isModuleId(id2) && !Object.hasOwn(base.modules, id2) && !Object.hasOwn(base.plugins, id2) && !Object.hasOwn(base.options, id2)
  );
  const hello = {
    ...base,
    modules: { ...base.modules, ...Object.fromEntries(offered.map((id2) => [id2, { module: moduleUrl(id2) }])) }
  };
  const named5 = (url) => {
    if (Object.hasOwn(given, url)) return given[url];
    const ids3 = [
      ...Object.entries(hello.plugins).filter(([, entry]) => entry.module === url).map(([id3]) => id3),
      ...Object.entries(hello.options).filter(([, entry]) => entry.module === url).map(([id3]) => id3),
      ...Object.entries(hello.modules).filter(([, source]) => source.module === url).map(([id3]) => id3)
    ];
    const id2 = ids3.find((key2) => Object.hasOwn(given, key2));
    if (id2 !== void 0) return given[id2];
    const own2 = ids3.find((key2) => Object.hasOwn(CORE_CLASSES, key2) || Object.hasOwn(coreOptions, key2));
    if (own2 === void 0) return void 0;
    return Object.hasOwn(CORE_CLASSES, own2) ? CORE_CLASSES[own2] : { api: 1, default: coreOptions[own2] };
  };
  const logged = [];
  const restore = replaceLogger((line) => {
    logged.push(line);
  }, true);
  const r = fixtureR(hello, options);
  const menus = stubMenus();
  const dialogs = stubDialogs();
  const started = start2({
    transport: r.transport,
    loader: {
      importer(url) {
        const exports = named5(url);
        if (exports !== void 0) return Promise.resolve(exports);
        return Promise.reject(new TypeError(`The test gives no module ${url}.`));
      },
      stylesheet: () => Promise.resolve()
    },
    ...options.el === void 0 ? {} : { frontEnds: pageFrontEnds(options.el) },
    measure: options.measure ?? { info: () => INFO, watch: () => () => void 0 },
    showMenu: menus.show,
    showDialog: dialogs.show
  });
  let first;
  try {
    first = await started.ready;
  } catch (error) {
    restore();
    throw error;
  }
  const core = () => started.core() ?? first;
  function menu(target2, opts = {}) {
    if (target2 === void 0) return menus.open();
    const before = menus.open();
    core().boardActions.menu(target2, { ...opts, el: document.body, anchor: new DOMRect(0, 0, 0, 0) });
    const opened = menus.open();
    if (opened === void 0 || opened === before) {
      throw new Error(`No board action applies to the target ${JSON.stringify(target2)}, so that no menu opens.`);
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
        await new Promise((resolve) => setTimeout(resolve, 0));
        quiet = r.pending() === 0 && r.sent.length === before ? quiet + 1 : 0;
      }
    },
    logged,
    stop: () => {
      started.stop();
      restore();
    }
  };
}
function moduleUrl(id2) {
  return `${packageFromId(id2) ?? "app"}/${id2}.js`;
}
function isModuleId(key2) {
  return packageFromId(key2) !== null && !key2.includes("/");
}
function fixtureR(hello, options) {
  const sent2 = [];
  let pending = 0;
  let listener;
  const results = /* @__PURE__ */ new Map();
  const transport = {
    send(message, bodies) {
      if (message.some((request) => options.refuse?.includes(request.kind) === true)) {
        return Promise.reject(new Error("The transport is closed."));
      }
      sent2.push(...message);
      for (const request of message) {
        if (request.kind === "cancel" || request.kind === "forget") {
          if (request.kind === "forget") for (const block of request.blocks) results.delete(block);
          continue;
        }
        pending++;
        setTimeout(() => {
          void replyTo(request, bodies.get(request.id)).then(([reply, body]) => {
            pending--;
            listener?.received(reply, body === void 0 ? null : Promise.resolve(body));
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
    }
  };
  async function replyTo(request, body) {
    const { id: id2 } = request;
    switch (request.kind) {
      case "hello":
        results.clear();
        return [{ ...hello, id: id2 }];
      case "blockr_options": {
        const values2 = options.blockrOptions ?? {};
        return [
          {
            id: id2,
            status: "ok",
            values: Object.fromEntries(request.names.flatMap((name2) => Object.hasOwn(values2, name2) ? [[name2, values2[name2] ?? null]] : []))
          }
        ];
      }
      case "evaluate": {
        const given = options.evaluate?.(request) ?? {};
        const reply = {
          status: "ok",
          summary: null,
          reason: null,
          conditions: [],
          stdout: "",
          stderr: "",
          ...given,
          id: id2,
          block: request.block,
          revision: request.revision
        };
        if (reply.status === "ok") results.set(request.block, { revision: request.revision, registry_id: request.recipe.registry_id });
        return [reply];
      }
      case "output": {
        const held4 = results.get(request.block);
        if (held4?.revision !== request.revision) {
          return [{ id: id2, status: "missing", missing: [{ block: request.block, revision: request.revision }] }];
        }
        const given = options.output?.(request);
        const made = given ?? sample(held4.registry_id);
        if (made === void 0) return [failed2(id2, "output", `The fixtures hold no output of block "${request.block}".`)];
        const reply = { id: id2, status: "ok", output: made.output, conditions: [...given?.conditions ?? []] };
        return made.body === void 0 ? [reply] : [{ ...reply, body: { type: made.body.type } }, made.body];
      }
      default: {
        const raised = [...options.conditions?.(request) ?? []];
        let answer2;
        try {
          answer2 = await options.answer?.(request, body);
        } catch (error) {
          return [failed2(id2, "request", errorMessage(error), raised)];
        }
        if (answer2 === void 0) return [failed2(id2, "request", `The test gives no answer to the request ${JSON.stringify(request)}.`)];
        if (answer2 instanceof Blob) {
          const announced = answer2 instanceof File ? { type: answer2.type, name: answer2.name } : { type: answer2.type };
          return [{ id: id2, status: "ok", body: announced, conditions: raised }, answer2];
        }
        return [{ id: id2, status: "ok", value: answer2, conditions: raised }];
      }
    }
  }
  function sample(registryId2) {
    const renderer = own(hello.block_registry, registryId2)?.renderers?.[0];
    const samples = renderer === void 0 ? void 0 : own(SAMPLES, renderer.id);
    const first = samples?.outputs[0];
    if (renderer === void 0 || first === void 0) return void 0;
    const output = { ...first.output, renderer };
    const announced = first.body;
    return isObject(announced) && typeof announced.type === "string" ? { output, body: new Blob([], { type: announced.type }) } : { output };
  }
  return { transport, sent: sent2, pending: () => pending };
}
function failed2(id2, phase, message, before = []) {
  return {
    id: id2,
    status: "error",
    conditions: [...before, { severity: "error", class: ["blockr_error", "error", "condition"], phase, message }]
  };
}
export {
  HELLO,
  INFO,
  READY,
  mount,
  stubBlockHost,
  stubCore,
  stubOptionHost,
  stubOutputHost
};

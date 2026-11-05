// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
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
    return keys.length === Object.keys(b).length && keys.every((key) => Object.hasOwn(b, key) && jsonEqual(a[key], b[key]));
  }
  return false;
}

// srcts/stubs/util.ts
var READY = Object.freeze({
  status: "ready",
  revision: 1,
  evaluated: 1,
  running: false,
  conditions: Object.freeze([]),
  omitted: Object.freeze({ warning: 0, message: 0 }),
  stdout: "",
  stderr: "",
  summary: null
});
function contextOf(given = {}) {
  return Object.freeze({
    name: given.name ?? "Block",
    status: given.status ?? READY,
    inputs: given.inputs ?? [],
    options: given.options ?? {}
  });
}
function announcer(events) {
  const listeners = new Map(events.map((event) => [event, /* @__PURE__ */ new Set()]));
  return {
    listen: (event, fn) => {
      const heard = listeners.get(event);
      if (heard === void 0) {
        throw new Error(`The host announces ${events.map((name) => `"${name}"`).join(" and ")}, not ${JSON.stringify(event)}.`);
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

// srcts/stubs/block.ts
function stubBlockHost(fixture, options = {}) {
  let state = options.state ?? fixture.states[0] ?? {};
  let context = contextOf(options.context);
  const submitted = [];
  const requested = [];
  const evaluated = [];
  const on = announcer(["state", "context"]);
  return {
    block: options.block ?? "block",
    class: options.class ?? "stub",
    state: () => state,
    context: () => context,
    on: on.listen,
    submitted,
    requested,
    evaluated,
    submit(change) {
      if (!isObject(change)) {
        return Promise.reject(new TypeError("A submit takes the state args it changes, as an object."));
      }
      const next = { ...state, ...defined(change) };
      submitted.push(next);
      if (!fixture.states.some((listed) => jsonEqual(listed, next))) {
        return Promise.reject(
          new Error(`The module submitted the state ${JSON.stringify(next)}, which the fixture does not list.`)
        );
      }
      state = next;
      on.announce("state");
      return Promise.resolve();
    },
    request(name, args = {}, opts = {}) {
      const given = defined(args);
      requested.push({ name, args: given, ...opts.file === void 0 ? {} : { file: opts.file } });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      const found = fixture.requests.find(
        (call) => call.name === name && Object.keys(given).every((arg) => Object.hasOwn(call.args, arg)) && // The host fills in a state arg from the block's state
        // where the module passes none,
        // and R gives a file the module sends to the arg `file`.
        Object.entries(call.args).every(
          ([arg, value]) => arg === "file" && opts.file !== void 0 && !Object.hasOwn(given, arg) ? sameFile(value, opts.file) : jsonEqual(Object.hasOwn(given, arg) ? given[arg] : own(state, arg), value)
        ) && Object.entries(call.options).every(([id, value]) => jsonEqual(own(context.options, id), value))
      );
      if (found === void 0) {
        return Promise.reject(
          new Error(
            `The fixture records no answer of the request "${name}" with the args ${JSON.stringify(given)}, on the state ${JSON.stringify(state)} and the board options ${JSON.stringify(context.options)}.`
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
      return options.evaluate === void 0 ? context.status : await options.evaluate(params);
    },
    module(id) {
      const modules = options.modules ?? {};
      return Object.hasOwn(modules, id) ? Promise.resolve(modules[id]) : Promise.reject(new Error(`No package offers a module "${id}".`));
    },
    setState(next) {
      state = next;
      on.announce("state");
    },
    setContext(changes) {
      context = contextOf({ ...context, ...changes });
      on.announce("context");
    }
  };
}
function sameFile(recorded, file) {
  return isObject(recorded) && recorded.name === file.name && recorded.type === file.type && recorded.size === file.size;
}
function defined(x) {
  return Object.fromEntries(Object.entries(x).filter(([, value]) => value !== void 0));
}

// srcts/stubs/option.ts
function stubOptionHost(options) {
  let value = options.value;
  const submitted = [];
  const requested = [];
  const on = announcer(["value"]);
  return {
    option: options.option,
    value: () => value,
    on: on.listen,
    submitted,
    requested,
    submit(next) {
      submitted.push(next);
      value = next;
      on.announce("value");
      return Promise.resolve();
    },
    request(name, args = {}, opts = {}) {
      requested.push({ name, args });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      return answer(options.requests ?? [], name, args, `the board option "${options.option}"`);
    },
    setValue(next) {
      value = next;
      on.announce("value");
    }
  };
}
function answer(answers, name, args, of) {
  if (!isObject(args)) {
    return Promise.reject(new TypeError(`The args of the request "${name}" are an object.`));
  }
  const found = answers.find((call) => call.name === name && jsonEqual(call.args ?? {}, args));
  return found === void 0 ? Promise.reject(new Error(`No answer is given for the request "${name}" of ${of} with the args ${JSON.stringify(args)}.`)) : Promise.resolve(found.answer);
}

// srcts/stubs/output.ts
function stubOutputHost(options = {}) {
  const first = options.fixtures?.outputs[0];
  let output = options.output ?? first?.output ?? null;
  let body = options.body ?? null;
  let params = options.params ?? (options.output === void 0 ? first?.params ?? {} : {});
  const values = options.options ?? (options.output === void 0 ? first?.options ?? {} : {});
  let loading = false;
  let error = null;
  const emitted = [];
  const asked = [];
  const requested = [];
  const drawn = [];
  const on = announcer(["output"]);
  return {
    block: options.block ?? "block",
    output: () => output,
    body: () => body,
    params: () => params,
    options: () => values,
    loading: () => loading,
    error: () => error,
    on: on.listen,
    emitted,
    asked,
    requested,
    drawn,
    setParams(next) {
      asked.push(next);
      const found = options.fixtures?.outputs.find(
        (fixture) => jsonEqual(fixture.params, next) && Object.entries(fixture.options).every(([id, value]) => jsonEqual(values[id], value))
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
    request(name, args = {}, opts = {}) {
      const scope = opts.scope ?? "class";
      requested.push({ name, args, scope });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      const answers = (options.requests ?? []).filter((call) => (call.scope ?? "class") === scope);
      return answer(answers, name, args, scope === "class" ? "the output class" : "the renderer");
    },
    emit(name, payload) {
      emitted.push({ name, payload });
    },
    draw(el, key, nested) {
      const entry = { key, el, outputs: [nested], removed: false };
      drawn.push(entry);
      return {
        update(next) {
          entry.outputs.push(next);
        },
        remove() {
          entry.removed = true;
        }
      };
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

// srcts/scheduler/status.ts
var NONE = Object.freeze({ warning: 0, message: 0 });
var NO_CONDITIONS = Object.freeze([]);
function unevaluated(revision, running) {
  return {
    status: "unevaluated",
    revision,
    evaluated: null,
    running,
    conditions: NO_CONDITIONS,
    omitted: NONE,
    stdout: "",
    stderr: "",
    summary: null
  };
}

// srcts/mount/load.ts
var API = 1;

// srcts/mount/output.ts
var EMPTY = Object.freeze({});

// srcts/mount/block.ts
var NO_STATUS = Object.freeze(unevaluated(0, false));

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
  const render = mod.render;
  const controller = new AbortController();
  const cleanup = await render.call(mod, { el, host, signal: controller.signal });
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
export {
  READY,
  mount,
  stubBlockHost,
  stubOptionHost,
  stubOutputHost
};

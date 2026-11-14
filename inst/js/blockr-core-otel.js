// node_modules/@opentelemetry/api/dist/version.mjs
var VERSION = "1.10.0";

// node_modules/@opentelemetry/api/dist/internal/semver.mjs
var re = /^(\d+)\.(\d+)\.(\d+)(-(.+))?$/;
function _makeCompatibilityCheck(ownVersion) {
  const acceptedVersions = /* @__PURE__ */ new Set([ownVersion]);
  const rejectedVersions = /* @__PURE__ */ new Set();
  const myVersionMatch = ownVersion.match(re);
  if (!myVersionMatch) return () => false;
  const ownVersionParsed = {
    major: +myVersionMatch[1],
    minor: +myVersionMatch[2],
    patch: +myVersionMatch[3],
    prerelease: myVersionMatch[4]
  };
  if (ownVersionParsed.prerelease != null) return function isExactmatch(globalVersion) {
    return globalVersion === ownVersion;
  };
  function _reject(v) {
    rejectedVersions.add(v);
    return false;
  }
  function _accept(v) {
    acceptedVersions.add(v);
    return true;
  }
  return function isCompatible2(globalVersion) {
    if (acceptedVersions.has(globalVersion)) return true;
    if (rejectedVersions.has(globalVersion)) return false;
    const globalVersionMatch = globalVersion.match(re);
    if (!globalVersionMatch) return _reject(globalVersion);
    const globalVersionParsed = {
      major: +globalVersionMatch[1],
      minor: +globalVersionMatch[2],
      patch: +globalVersionMatch[3],
      prerelease: globalVersionMatch[4]
    };
    if (globalVersionParsed.prerelease != null) return _reject(globalVersion);
    if (ownVersionParsed.major !== globalVersionParsed.major) return _reject(globalVersion);
    if (ownVersionParsed.major === 0) {
      if (ownVersionParsed.minor === globalVersionParsed.minor && ownVersionParsed.patch <= globalVersionParsed.patch) return _accept(globalVersion);
      return _reject(globalVersion);
    }
    if (ownVersionParsed.minor <= globalVersionParsed.minor) return _accept(globalVersion);
    return _reject(globalVersion);
  };
}
var isCompatible = _makeCompatibilityCheck(VERSION);

// node_modules/@opentelemetry/api/dist/internal/global-utils.mjs
var major = VERSION.split(".")[0];
var GLOBAL_OPENTELEMETRY_API_KEY = /* @__PURE__ */ Symbol.for(`opentelemetry.js.api.${major}`);
var _global = typeof globalThis === "object" ? globalThis : typeof self === "object" ? self : typeof window === "object" ? window : typeof global === "object" ? global : {};
function _makeGlobalApi() {
  return Object.defineProperty({}, "version", {
    value: VERSION,
    enumerable: true,
    writable: false,
    configurable: false
  });
}
function registerGlobal(type, instance, diag2, allowOverride = false) {
  var _global$GLOBAL_OPENTE;
  const api = _global[GLOBAL_OPENTELEMETRY_API_KEY] = (_global$GLOBAL_OPENTE = _global[GLOBAL_OPENTELEMETRY_API_KEY]) !== null && _global$GLOBAL_OPENTE !== void 0 ? _global$GLOBAL_OPENTE : _makeGlobalApi();
  if (!allowOverride && api[type]) {
    const err = /* @__PURE__ */ new Error(`@opentelemetry/api: Attempted duplicate registration of API: ${type}`);
    diag2.error(err.stack || err.message);
    return false;
  }
  if (api.version !== "1.10.0") {
    const err = /* @__PURE__ */ new Error(`@opentelemetry/api: Registration of version v${api.version} for ${type} does not match previously registered API v${VERSION}`);
    diag2.error(err.stack || err.message);
    return false;
  }
  api[type] = instance;
  diag2.debug(`@opentelemetry/api: Registered a global for ${type} v${VERSION}.`);
  return true;
}
var _compatibleGlobalApi;
function getGlobal(type) {
  const api = _global[GLOBAL_OPENTELEMETRY_API_KEY];
  if (api == null) return;
  if (api !== _compatibleGlobalApi) {
    if (!api.version || !isCompatible(api.version)) return;
    _compatibleGlobalApi = api;
  }
  return api[type];
}
function unregisterGlobal(type, diag2) {
  diag2.debug(`@opentelemetry/api: Unregistering a global for ${type} v${VERSION}.`);
  const api = _global[GLOBAL_OPENTELEMETRY_API_KEY];
  if (api) delete api[type];
}

// node_modules/@opentelemetry/api/dist/diag/ComponentLogger.mjs
var DiagComponentLogger = class {
  constructor(props) {
    this._namespace = props.namespace || "DiagComponentLogger";
  }
  debug(...args) {
    return logProxy("debug", this._namespace, args);
  }
  error(...args) {
    return logProxy("error", this._namespace, args);
  }
  info(...args) {
    return logProxy("info", this._namespace, args);
  }
  warn(...args) {
    return logProxy("warn", this._namespace, args);
  }
  verbose(...args) {
    return logProxy("verbose", this._namespace, args);
  }
};
function logProxy(funcName, namespace, args) {
  const logger = getGlobal("diag");
  if (!logger) return;
  return logger[funcName](namespace, ...args);
}

// node_modules/@opentelemetry/api/dist/diag/internal/logLevelLogger.mjs
function createLogLevelDiagLogger(maxLevel, logger) {
  if (maxLevel < 0) maxLevel = 0;
  else if (maxLevel > 9999) maxLevel = 9999;
  logger = logger || {};
  function _filterFunc(funcName, theLevel) {
    const theFunc = logger[funcName];
    if (typeof theFunc === "function" && maxLevel >= theLevel) return theFunc.bind(logger);
    return function() {
    };
  }
  return {
    error: _filterFunc("error", 30),
    warn: _filterFunc("warn", 50),
    info: _filterFunc("info", 60),
    debug: _filterFunc("debug", 70),
    verbose: _filterFunc("verbose", 80)
  };
}

// node_modules/@opentelemetry/api/dist/api/diag.mjs
var API_NAME = "diag";
var DiagAPI = class DiagAPI2 {
  /** Get the singleton instance of the DiagAPI API */
  static instance() {
    if (!this._instance) this._instance = new DiagAPI2();
    return this._instance;
  }
  /**
  * Private internal constructor
  * @private
  */
  constructor() {
    function _logProxy(funcName) {
      return function(...args) {
        const logger = getGlobal("diag");
        if (!logger) return;
        return logger[funcName](...args);
      };
    }
    const self2 = this;
    const setLogger = (logger, optionsOrLogLevel = { logLevel: 60 }) => {
      var _optionsOrLogLevel$lo;
      if (logger === self2) {
        var _err$stack;
        const err = /* @__PURE__ */ new Error("Cannot use diag as the logger for itself. Please use a DiagLogger implementation like ConsoleDiagLogger or a custom implementation");
        self2.error((_err$stack = err.stack) !== null && _err$stack !== void 0 ? _err$stack : err.message);
        return false;
      }
      if (typeof optionsOrLogLevel === "number") optionsOrLogLevel = { logLevel: optionsOrLogLevel };
      const oldLogger = getGlobal("diag");
      const newLogger = createLogLevelDiagLogger((_optionsOrLogLevel$lo = optionsOrLogLevel.logLevel) !== null && _optionsOrLogLevel$lo !== void 0 ? _optionsOrLogLevel$lo : 60, logger);
      if (oldLogger && !optionsOrLogLevel.suppressOverrideMessage) {
        var _Error$stack;
        const stack = (_Error$stack = (/* @__PURE__ */ new Error()).stack) !== null && _Error$stack !== void 0 ? _Error$stack : "<failed to generate stacktrace>";
        oldLogger.warn(`Current logger will be overwritten from ${stack}`);
        newLogger.warn(`Current logger will overwrite one already registered from ${stack}`);
      }
      return registerGlobal("diag", newLogger, self2, true);
    };
    self2.setLogger = setLogger;
    self2.disable = () => {
      unregisterGlobal(API_NAME, self2);
    };
    self2.createComponentLogger = (options) => {
      return new DiagComponentLogger(options);
    };
    self2.verbose = _logProxy("verbose");
    self2.debug = _logProxy("debug");
    self2.info = _logProxy("info");
    self2.warn = _logProxy("warn");
    self2.error = _logProxy("error");
  }
};

// node_modules/@opentelemetry/api/dist/context/context.mjs
function createContextKey(description) {
  return Symbol.for(description);
}
var ROOT_CONTEXT = new class BaseContext {
  /**
  * Construct a new context which inherits values from an optional parent context.
  *
  * @param parentContext a context from which to inherit values
  */
  constructor(parentContext) {
    const self2 = this;
    self2._currentContext = parentContext ? new Map(parentContext) : /* @__PURE__ */ new Map();
    self2.getValue = (key) => self2._currentContext.get(key);
    self2.setValue = (key, value) => {
      const context2 = new BaseContext(self2._currentContext);
      context2._currentContext.set(key, value);
      return context2;
    };
    self2.deleteValue = (key) => {
      const context2 = new BaseContext(self2._currentContext);
      context2._currentContext.delete(key);
      return context2;
    };
  }
}();

// node_modules/@opentelemetry/api/dist/metrics/NoopMeter.mjs
var NoopMeter = class {
  constructor() {
  }
  /**
  * @see {@link Meter.createGauge}
  */
  createGauge(_name, _options) {
    return NOOP_GAUGE_METRIC;
  }
  /**
  * @see {@link Meter.createHistogram}
  */
  createHistogram(_name, _options) {
    return NOOP_HISTOGRAM_METRIC;
  }
  /**
  * @see {@link Meter.createCounter}
  */
  createCounter(_name, _options) {
    return NOOP_COUNTER_METRIC;
  }
  /**
  * @see {@link Meter.createUpDownCounter}
  */
  createUpDownCounter(_name, _options) {
    return NOOP_UP_DOWN_COUNTER_METRIC;
  }
  /**
  * @see {@link Meter.createObservableGauge}
  */
  createObservableGauge(_name, _options) {
    return NOOP_OBSERVABLE_GAUGE_METRIC;
  }
  /**
  * @see {@link Meter.createObservableCounter}
  */
  createObservableCounter(_name, _options) {
    return NOOP_OBSERVABLE_COUNTER_METRIC;
  }
  /**
  * @see {@link Meter.createObservableUpDownCounter}
  */
  createObservableUpDownCounter(_name, _options) {
    return NOOP_OBSERVABLE_UP_DOWN_COUNTER_METRIC;
  }
  /**
  * @see {@link Meter.addBatchObservableCallback}
  */
  addBatchObservableCallback(_callback, _observables) {
  }
  /**
  * @see {@link Meter.removeBatchObservableCallback}
  */
  removeBatchObservableCallback(_callback) {
  }
};
var NoopMetric = class {
};
var NoopCounterMetric = class extends NoopMetric {
  add(_value, _attributes) {
  }
};
var NoopUpDownCounterMetric = class extends NoopMetric {
  add(_value, _attributes) {
  }
};
var NoopGaugeMetric = class extends NoopMetric {
  record(_value, _attributes) {
  }
};
var NoopHistogramMetric = class extends NoopMetric {
  record(_value, _attributes) {
  }
};
var NoopObservableMetric = class {
  addCallback(_callback) {
  }
  removeCallback(_callback) {
  }
};
var NoopObservableCounterMetric = class extends NoopObservableMetric {
};
var NoopObservableGaugeMetric = class extends NoopObservableMetric {
};
var NoopObservableUpDownCounterMetric = class extends NoopObservableMetric {
};
var NOOP_METER = new NoopMeter();
var NOOP_COUNTER_METRIC = new NoopCounterMetric();
var NOOP_GAUGE_METRIC = new NoopGaugeMetric();
var NOOP_HISTOGRAM_METRIC = new NoopHistogramMetric();
var NOOP_UP_DOWN_COUNTER_METRIC = new NoopUpDownCounterMetric();
var NOOP_OBSERVABLE_COUNTER_METRIC = new NoopObservableCounterMetric();
var NOOP_OBSERVABLE_GAUGE_METRIC = new NoopObservableGaugeMetric();
var NOOP_OBSERVABLE_UP_DOWN_COUNTER_METRIC = new NoopObservableUpDownCounterMetric();
function createNoopMeter() {
  return NOOP_METER;
}

// node_modules/@opentelemetry/api/dist/logs/NoopLogger.mjs
var NoopLogger = class {
  emit(_logRecord) {
  }
  enabled() {
    return false;
  }
};
var NOOP_LOGGER = new NoopLogger();
function createNoopLogger() {
  return NOOP_LOGGER;
}

// node_modules/@opentelemetry/api/dist/logs/LogRecord.mjs
var SeverityNumber = /* @__PURE__ */ (function(SeverityNumber2) {
  SeverityNumber2[SeverityNumber2["UNSPECIFIED"] = 0] = "UNSPECIFIED";
  SeverityNumber2[SeverityNumber2["TRACE"] = 1] = "TRACE";
  SeverityNumber2[SeverityNumber2["TRACE2"] = 2] = "TRACE2";
  SeverityNumber2[SeverityNumber2["TRACE3"] = 3] = "TRACE3";
  SeverityNumber2[SeverityNumber2["TRACE4"] = 4] = "TRACE4";
  SeverityNumber2[SeverityNumber2["DEBUG"] = 5] = "DEBUG";
  SeverityNumber2[SeverityNumber2["DEBUG2"] = 6] = "DEBUG2";
  SeverityNumber2[SeverityNumber2["DEBUG3"] = 7] = "DEBUG3";
  SeverityNumber2[SeverityNumber2["DEBUG4"] = 8] = "DEBUG4";
  SeverityNumber2[SeverityNumber2["INFO"] = 9] = "INFO";
  SeverityNumber2[SeverityNumber2["INFO2"] = 10] = "INFO2";
  SeverityNumber2[SeverityNumber2["INFO3"] = 11] = "INFO3";
  SeverityNumber2[SeverityNumber2["INFO4"] = 12] = "INFO4";
  SeverityNumber2[SeverityNumber2["WARN"] = 13] = "WARN";
  SeverityNumber2[SeverityNumber2["WARN2"] = 14] = "WARN2";
  SeverityNumber2[SeverityNumber2["WARN3"] = 15] = "WARN3";
  SeverityNumber2[SeverityNumber2["WARN4"] = 16] = "WARN4";
  SeverityNumber2[SeverityNumber2["ERROR"] = 17] = "ERROR";
  SeverityNumber2[SeverityNumber2["ERROR2"] = 18] = "ERROR2";
  SeverityNumber2[SeverityNumber2["ERROR3"] = 19] = "ERROR3";
  SeverityNumber2[SeverityNumber2["ERROR4"] = 20] = "ERROR4";
  SeverityNumber2[SeverityNumber2["FATAL"] = 21] = "FATAL";
  SeverityNumber2[SeverityNumber2["FATAL2"] = 22] = "FATAL2";
  SeverityNumber2[SeverityNumber2["FATAL3"] = 23] = "FATAL3";
  SeverityNumber2[SeverityNumber2["FATAL4"] = 24] = "FATAL4";
  return SeverityNumber2;
})({});

// node_modules/@opentelemetry/api/dist/trace/trace_flags.mjs
var TraceFlags = /* @__PURE__ */ (function(TraceFlags2) {
  TraceFlags2[TraceFlags2["NONE"] = 0] = "NONE";
  TraceFlags2[TraceFlags2["SAMPLED"] = 1] = "SAMPLED";
  return TraceFlags2;
})({});

// node_modules/@opentelemetry/api/dist/trace/invalid-span-constants.mjs
var INVALID_SPANID = "0000000000000000";
var INVALID_TRACEID = "00000000000000000000000000000000";
var INVALID_SPAN_CONTEXT = {
  traceId: INVALID_TRACEID,
  spanId: INVALID_SPANID,
  traceFlags: 0
};

// node_modules/@opentelemetry/api/dist/trace/NonRecordingSpan.mjs
var NonRecordingSpan = class {
  constructor(spanContext = INVALID_SPAN_CONTEXT) {
    this._spanContext = spanContext;
  }
  spanContext() {
    return this._spanContext;
  }
  setAttribute(_key, _value) {
    return this;
  }
  setAttributes(_attributes) {
    return this;
  }
  addEvent(_name, _attributes) {
    return this;
  }
  addLink(_link) {
    return this;
  }
  addLinks(_links) {
    return this;
  }
  setStatus(_status) {
    return this;
  }
  updateName(_name) {
    return this;
  }
  end(_endTime) {
  }
  isRecording() {
    return false;
  }
  recordException(_exception, _time) {
  }
};

// node_modules/@opentelemetry/api/dist/trace/spancontext-utils.mjs
var isHex = new Uint8Array([
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  1,
  1,
  1,
  1,
  1,
  1
]);
function isValidHex(id, length) {
  if (typeof id !== "string" || id.length !== length) return false;
  let r = 0;
  for (let i = 0; i < id.length; i += 4) r += (isHex[id.charCodeAt(i)] | 0) + (isHex[id.charCodeAt(i + 1)] | 0) + (isHex[id.charCodeAt(i + 2)] | 0) + (isHex[id.charCodeAt(i + 3)] | 0);
  return r === length;
}
function isValidTraceId(traceId) {
  return isValidHex(traceId, 32) && traceId !== "00000000000000000000000000000000";
}
function isValidSpanId(spanId) {
  return isValidHex(spanId, 16) && spanId !== "0000000000000000";
}
function isSpanContextValid(spanContext) {
  return isValidTraceId(spanContext.traceId) && isValidSpanId(spanContext.spanId);
}
function wrapSpanContext(spanContext) {
  return new NonRecordingSpan(spanContext);
}

// node_modules/@opentelemetry/api/dist/context/NoopContextManager.mjs
var NoopContextManager = class {
  active() {
    return ROOT_CONTEXT;
  }
  with(_context, fn, thisArg, ...args) {
    return fn.call(thisArg, ...args);
  }
  bind(_context, target) {
    return target;
  }
  enable() {
    return this;
  }
  disable() {
    return this;
  }
  /** @experimental This API is experimental and may change in minor releases without prior notice. */
  attach(_context) {
    return { dispose() {
    } };
  }
};

// node_modules/@opentelemetry/api/dist/api/context.mjs
var API_NAME2 = "context";
var NOOP_CONTEXT_MANAGER = new NoopContextManager();
var NOOP_TOKEN = Object.freeze({ dispose: () => {
} });
var ContextAPI = class ContextAPI2 {
  /** Empty private constructor prevents end users from constructing a new instance of the API */
  constructor() {
    this._attachUnsupportedWarned = false;
  }
  /** Get the singleton instance of the Context API */
  static getInstance() {
    if (!this._instance) this._instance = new ContextAPI2();
    return this._instance;
  }
  /**
  * Set the current context manager.
  *
  * @returns true if the context manager was successfully registered, else false
  */
  setGlobalContextManager(contextManager) {
    return registerGlobal(API_NAME2, contextManager, DiagAPI.instance());
  }
  /**
  * Get the currently active context
  */
  active() {
    return this._getContextManager().active();
  }
  /**
  * Execute a function with an active context
  *
  * @param context context to be active during function execution
  * @param fn function to execute in a context
  * @param thisArg optional receiver to be used for calling fn
  * @param args optional arguments forwarded to fn
  */
  with(context2, fn, thisArg, ...args) {
    return this._getContextManager().with(context2, fn, thisArg, ...args);
  }
  /**
  * Bind a context to a target function or event emitter
  *
  * @param context context to bind to the event emitter or function. Defaults to the currently active context
  * @param target function or event emitter to bind
  */
  bind(context2, target) {
    return this._getContextManager().bind(context2, target);
  }
  /**
  * Imperatively sets `context` as active, returning a {@link ContextManagementToken} whose
  * {@link ContextManagementToken.dispose} restores the previous context.
  *
  * This is a delicate, low-level API - prefer {@link with}/{@link bind}, which
  * restore context automatically. Use `attach` only to bridge callback
  * boundaries that `with` cannot wrap. Call `token.dispose()` when the
  * operation is done.
  *
  * **Caveat for async functions:** If called inside an async function before
  * the first `await`, the context change may leak into the caller's context
  * and remain active there. Prefer {@link with} for async code.
  *
  * Support is best-effort and varies by the active ContextManager (see
  * {@link ContextManager.attach}); if it does not implement `attach`, this logs
  * a warning and returns a no-op token.
  *
  * @param context The Context to attach
  * @returns A {@link ContextManagementToken} whose dispose() restores the previous Context
  * @since 1.10.0
  * @experimental This API is experimental and may change in minor releases without prior notice.
  */
  attach(context2) {
    const contextManager = this._getContextManager();
    if (contextManager.attach) return contextManager.attach(context2);
    if (!this._attachUnsupportedWarned) {
      this._attachUnsupportedWarned = true;
      DiagAPI.instance().warn("The current ContextManager does not implement attach(). The context will not be attached. Use a ContextManager that supports attach() (e.g. AsyncLocalStorageContextManager) or use with()/bind() instead.");
    }
    return NOOP_TOKEN;
  }
  _getContextManager() {
    return getGlobal(API_NAME2) || NOOP_CONTEXT_MANAGER;
  }
  /** Disable and remove the global context manager */
  disable() {
    this._getContextManager().disable();
    unregisterGlobal(API_NAME2, DiagAPI.instance());
  }
};

// node_modules/@opentelemetry/api/dist/trace/context-utils.mjs
var SPAN_KEY = createContextKey("OpenTelemetry Context Key SPAN");
function getSpan(context2) {
  return context2.getValue(SPAN_KEY) || void 0;
}
function getActiveSpan() {
  return getSpan(ContextAPI.getInstance().active());
}
function setSpan(context2, span) {
  return context2.setValue(SPAN_KEY, span);
}
function deleteSpan(context2) {
  return context2.deleteValue(SPAN_KEY);
}
function setSpanContext(context2, spanContext) {
  return setSpan(context2, new NonRecordingSpan(spanContext));
}
function getSpanContext(context2) {
  var _getSpan;
  return (_getSpan = getSpan(context2)) === null || _getSpan === void 0 ? void 0 : _getSpan.spanContext();
}

// node_modules/@opentelemetry/api/dist/trace/NoopTracer.mjs
var contextApi = ContextAPI.getInstance();
var NoopTracer = class {
  startSpan(name, options, context2 = contextApi.active()) {
    if (Boolean(options === null || options === void 0 ? void 0 : options.root)) return new NonRecordingSpan();
    const parentFromContext = context2 && getSpanContext(context2);
    if (isSpanContext(parentFromContext) && isSpanContextValid(parentFromContext)) return new NonRecordingSpan(parentFromContext);
    else return new NonRecordingSpan();
  }
  startActiveSpan(name, arg2, arg3, arg4) {
    var _ctx;
    let opts;
    let ctx;
    let fn;
    if (arguments.length < 2) return;
    else if (arguments.length === 2) fn = arg2;
    else if (arguments.length === 3) {
      opts = arg2;
      fn = arg3;
    } else {
      opts = arg2;
      ctx = arg3;
      fn = arg4;
    }
    const parentContext = (_ctx = ctx) !== null && _ctx !== void 0 ? _ctx : contextApi.active();
    const span = this.startSpan(name, opts, parentContext);
    const contextWithSpanSet = setSpan(parentContext, span);
    return contextApi.with(contextWithSpanSet, fn, void 0, span);
  }
};
function isSpanContext(spanContext) {
  return spanContext !== null && typeof spanContext === "object" && "spanId" in spanContext && typeof spanContext["spanId"] === "string" && "traceId" in spanContext && typeof spanContext["traceId"] === "string" && "traceFlags" in spanContext && typeof spanContext["traceFlags"] === "number";
}

// node_modules/@opentelemetry/api/dist/trace/ProxyTracer.mjs
var NOOP_TRACER = new NoopTracer();
var ProxyTracer = class {
  constructor(provider, name, version, options) {
    this._provider = provider;
    this.name = name;
    this.version = version;
    this.options = options;
  }
  startSpan(name, options, context2) {
    return this._getTracer().startSpan(name, options, context2);
  }
  startActiveSpan(_name, _options, _context, _fn) {
    const tracer = this._getTracer();
    return Reflect.apply(tracer.startActiveSpan, tracer, arguments);
  }
  /**
  * Try to get a tracer from the proxy tracer provider.
  * If the proxy tracer provider has no delegate, return a noop tracer.
  */
  _getTracer() {
    if (this._delegate) return this._delegate;
    const tracer = this._provider.getDelegateTracer(this.name, this.version, this.options);
    if (!tracer) return NOOP_TRACER;
    this._delegate = tracer;
    return this._delegate;
  }
};

// node_modules/@opentelemetry/api/dist/trace/NoopTracerProvider.mjs
var NoopTracerProvider = class {
  getTracer(_name, _version, _options) {
    return new NoopTracer();
  }
};

// node_modules/@opentelemetry/api/dist/trace/ProxyTracerProvider.mjs
var NOOP_TRACER_PROVIDER = new NoopTracerProvider();
var ProxyTracerProvider = class {
  /**
  * Get a {@link ProxyTracer}
  */
  getTracer(name, version, options) {
    var _this$getDelegateTrac;
    return (_this$getDelegateTrac = this.getDelegateTracer(name, version, options)) !== null && _this$getDelegateTrac !== void 0 ? _this$getDelegateTrac : new ProxyTracer(this, name, version, options);
  }
  getDelegate() {
    var _this$_delegate;
    return (_this$_delegate = this._delegate) !== null && _this$_delegate !== void 0 ? _this$_delegate : NOOP_TRACER_PROVIDER;
  }
  /**
  * Set the delegate tracer provider
  */
  setDelegate(delegate) {
    this._delegate = delegate;
  }
  getDelegateTracer(name, version, options) {
    var _this$_delegate2;
    return (_this$_delegate2 = this._delegate) === null || _this$_delegate2 === void 0 ? void 0 : _this$_delegate2.getTracer(name, version, options);
  }
};

// node_modules/@opentelemetry/api/dist/context-api.mjs
var context = ContextAPI.getInstance();

// node_modules/@opentelemetry/api/dist/diag-api.mjs
var diag = DiagAPI.instance();

// node_modules/@opentelemetry/api/dist/api/trace.mjs
var API_NAME3 = "trace";
var TraceAPI = class TraceAPI2 {
  /** Empty private constructor prevents end users from constructing a new instance of the API */
  constructor() {
    this._proxyTracerProvider = new ProxyTracerProvider();
    this.wrapSpanContext = wrapSpanContext;
    this.isSpanContextValid = isSpanContextValid;
    this.deleteSpan = deleteSpan;
    this.getSpan = getSpan;
    this.getActiveSpan = getActiveSpan;
    this.getSpanContext = getSpanContext;
    this.setSpan = setSpan;
    this.setSpanContext = setSpanContext;
  }
  /** Get the singleton instance of the Trace API */
  static getInstance() {
    if (!this._instance) this._instance = new TraceAPI2();
    return this._instance;
  }
  /**
  * Set the current global tracer.
  *
  * @returns true if the tracer provider was successfully registered, else false
  */
  setGlobalTracerProvider(provider) {
    const success = registerGlobal(API_NAME3, this._proxyTracerProvider, DiagAPI.instance());
    if (success) this._proxyTracerProvider.setDelegate(provider);
    return success;
  }
  /**
  * Returns the global tracer provider.
  */
  getTracerProvider() {
    return getGlobal(API_NAME3) || this._proxyTracerProvider;
  }
  /**
  * Returns a tracer from the global tracer provider.
  */
  getTracer(name, version) {
    return this.getTracerProvider().getTracer(name, version);
  }
  /** Remove the global tracer provider */
  disable() {
    unregisterGlobal(API_NAME3, DiagAPI.instance());
    this._proxyTracerProvider = new ProxyTracerProvider();
  }
};

// node_modules/@opentelemetry/api/dist/trace-api.mjs
var trace = TraceAPI.getInstance();

// node_modules/@opentelemetry/exporter-logs-otlp-http/dist/semconv.mjs
var OTEL_COMPONENT_TYPE_VALUE_OTLP_HTTP_LOG_EXPORTER = "otlp_http_log_exporter";

// node_modules/@opentelemetry/otlp-exporter-base/dist/OTLPExporterBase.mjs
var OTLPExporterBase = class {
  _delegate;
  constructor(delegate) {
    this._delegate = delegate;
  }
  /**
  * Export items.
  * @param items
  * @param resultCallback
  */
  export(items, resultCallback) {
    this._delegate.export(items, resultCallback);
  }
  forceFlush() {
    return this._delegate.forceFlush();
  }
  shutdown() {
    return this._delegate.shutdown();
  }
  setMetrics(metrics) {
    this._delegate.setMetrics(metrics);
  }
};

// node_modules/@opentelemetry/otlp-exporter-base/dist/types.mjs
var OTLPExporterError = class extends Error {
  code;
  name = "OTLPExporterError";
  data;
  constructor(message, code, data) {
    super(message);
    this.data = data;
    this.code = code;
  }
};

// node_modules/@opentelemetry/otlp-exporter-base/dist/configuration/shared-configuration.mjs
function validateTimeoutMillis(timeoutMillis) {
  if (Number.isFinite(timeoutMillis) && timeoutMillis > 0) return timeoutMillis;
  throw new Error(`Configuration: timeoutMillis is invalid, expected number greater than 0 (actual: '${timeoutMillis}')`);
}
function wrapStaticHeadersInFunction(headers) {
  if (headers == null) return;
  return async () => headers;
}
function mergeOtlpSharedConfigurationWithDefaults(userProvidedConfiguration, fallbackConfiguration, defaultConfiguration) {
  return {
    timeoutMillis: validateTimeoutMillis(userProvidedConfiguration.timeoutMillis ?? fallbackConfiguration.timeoutMillis ?? defaultConfiguration.timeoutMillis),
    concurrencyLimit: userProvidedConfiguration.concurrencyLimit ?? fallbackConfiguration.concurrencyLimit ?? defaultConfiguration.concurrencyLimit,
    compression: userProvidedConfiguration.compression ?? fallbackConfiguration.compression ?? defaultConfiguration.compression
  };
}
function getSharedConfigurationDefaults() {
  return {
    timeoutMillis: 1e4,
    concurrencyLimit: 30,
    compression: "none"
  };
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/bounded-queue-export-promise-handler.mjs
var BoundedQueueExportPromiseHandler = class {
  _concurrencyLimit;
  _sendingPromises = [];
  /**
  * @param concurrencyLimit maximum promises allowed in a queue at the same time.
  */
  constructor(concurrencyLimit) {
    this._concurrencyLimit = concurrencyLimit;
  }
  pushPromise(promise) {
    if (this.hasReachedLimit()) throw new Error("Concurrency Limit reached");
    this._sendingPromises.push(promise);
    const popPromise = () => {
      const index = this._sendingPromises.indexOf(promise);
      this._sendingPromises.splice(index, 1);
    };
    promise.then(popPromise, popPromise);
  }
  hasReachedLimit() {
    return this._sendingPromises.length >= this._concurrencyLimit;
  }
  async awaitAll() {
    await Promise.all(this._sendingPromises);
  }
};
function createBoundedQueueExportPromiseHandler(options) {
  return new BoundedQueueExportPromiseHandler(options.concurrencyLimit);
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/logging-response-handler.mjs
function isPartialSuccessResponse(response) {
  return Object.prototype.hasOwnProperty.call(response, "partialSuccess");
}
function createLoggingPartialSuccessResponseHandler() {
  return { handleResponse(response) {
    if (response == null || !isPartialSuccessResponse(response) || response.partialSuccess == null || Object.keys(response.partialSuccess).length === 0) return;
    diag.warn("Received Partial Success response:", JSON.stringify(response.partialSuccess));
  } };
}

// node_modules/@opentelemetry/core/dist/trace/suppress-tracing.mjs
var SUPPRESS_TRACING_KEY = createContextKey("OpenTelemetry SDK Context Key SUPPRESS_TRACING");
function suppressTracing(context2) {
  return context2.setValue(SUPPRESS_TRACING_KEY, true);
}

// node_modules/@opentelemetry/core/dist/common/attributes.mjs
var AddAttributeDecision = /* @__PURE__ */ (function(AddAttributeDecision2) {
  AddAttributeDecision2[AddAttributeDecision2["DROP_UNDEFINED"] = 0] = "DROP_UNDEFINED";
  AddAttributeDecision2[AddAttributeDecision2["DROP_INVALID"] = 1] = "DROP_INVALID";
  AddAttributeDecision2[AddAttributeDecision2["DROP_LIMIT_REACHED"] = 2] = "DROP_LIMIT_REACHED";
  AddAttributeDecision2[AddAttributeDecision2["ADD_NEW"] = 3] = "ADD_NEW";
  AddAttributeDecision2[AddAttributeDecision2["ADD_OVERWRITE_EXISTING"] = 4] = "ADD_OVERWRITE_EXISTING";
  return AddAttributeDecision2;
})({});
function maybeAddAttribute(opts) {
  return maybeAddAttributeInternal(opts, isAnyValue);
}
function maybeAddAttributeInternal(opts, isValidValue) {
  if (opts.value === void 0) return 0;
  if (opts.key.length === 0) {
    diag.warn(`Invalid attribute key: ${opts.key}`);
    return 1;
  }
  const isNewKey = !Object.hasOwn(opts.attributes, opts.key);
  if (isNewKey && opts.currentAttributesCount >= opts.limits.attributeCountLimit) return 2;
  if (!isValidValue(opts.value)) {
    diag.warn(`Invalid attribute value set for key: ${opts.key}`);
    return 1;
  }
  let attributeValueLengthLimit = opts.limits.attributeValueLengthLimit;
  if (attributeValueLengthLimit <= 0) {
    diag.warn(`Invalid attributeValueLengthLimit: must be positive, got ${attributeValueLengthLimit} (ignoring)`);
    attributeValueLengthLimit = Infinity;
  }
  opts.attributes[opts.key] = copyAndTruncAnyValue(opts.value, attributeValueLengthLimit);
  if (isNewKey) return 3;
  return 4;
}
function cleanSimpleAttributes(inAttributes, limits) {
  return cleanAttributesInternal(inAttributes, limits, isSimpleAttributeValue);
}
function cleanAttributesInternal(inAttributes, limits, isValidValue) {
  if (typeof inAttributes !== "object" || inAttributes == null) return {};
  const attributes = {};
  let droppedAttributesCount = 0;
  let attributeValueLengthLimit = limits.attributeValueLengthLimit;
  if (attributeValueLengthLimit <= 0) {
    diag.warn(`Invalid attributeValueLengthLimit: must be positive, got ${attributeValueLengthLimit} (ignoring)`);
    attributeValueLengthLimit = Infinity;
  }
  let count = 0;
  for (const [key, val] of Object.entries(inAttributes)) {
    if (val === void 0) continue;
    if (count >= limits.attributeCountLimit) {
      droppedAttributesCount++;
      continue;
    }
    if (key.length === 0) {
      diag.warn(`Invalid attribute key: ${key}`);
      droppedAttributesCount++;
      continue;
    }
    if (!isValidValue(val)) {
      diag.warn(`Invalid attribute value set for key: ${key}`);
      droppedAttributesCount++;
      continue;
    }
    attributes[key] = copyAndTruncAnyValue(val, attributeValueLengthLimit);
    count++;
  }
  return {
    attributes,
    droppedAttributesCount
  };
}
function isAnyValue(val) {
  return isAnyValueInternal(val, /* @__PURE__ */ new WeakSet());
}
function isAnyValueInternal(val, visited) {
  switch (typeof val) {
    case "undefined":
    case "bigint":
    case "symbol":
    case "function":
      return false;
    case "string":
    case "number":
    case "boolean":
      return true;
    case "object":
      break;
    default:
      return false;
  }
  if (val === null) return true;
  if (val instanceof Uint8Array) return true;
  if (visited.has(val)) return false;
  visited.add(val);
  const isValid = isAnyValueObject(val, visited);
  visited.delete(val);
  return isValid;
}
function isAnyValueObject(val, visited) {
  if (Array.isArray(val)) {
    for (const item of val) if (item !== void 0 && !isAnyValueInternal(item, visited)) return false;
    return true;
  }
  const obj = val;
  if (obj.constructor !== Object && obj.constructor !== void 0) return false;
  for (const k of Object.keys(obj)) {
    const v = obj[k];
    if (v !== void 0 && !isAnyValueInternal(v, visited)) return false;
  }
  return true;
}
function copyAndTruncAnyValue(val, limit) {
  switch (typeof val) {
    case "number":
    case "boolean":
    case "undefined":
    case "bigint":
    case "symbol":
    case "function":
      return val;
    case "string":
      if (val.length <= limit) return val;
      else return val.substring(0, limit);
    case "object":
      break;
    default:
      return val;
  }
  if (val === null) return val;
  if (val instanceof Uint8Array) {
    if (val.length <= limit) return val;
    else return val.slice(0, limit);
  }
  if (Array.isArray(val)) return val.map((elem) => copyAndTruncAnyValue(elem, limit));
  const truncObj = {};
  const obj = val;
  for (const k of Object.keys(obj)) if (obj[k] !== void 0) truncObj[k] = copyAndTruncAnyValue(obj[k], limit);
  return truncObj;
}
function isSimpleAttributeValue(val) {
  if (Array.isArray(val)) return isHomogeneousAttributeValueArray(val);
  return isValidPrimitiveAttributeValueType(typeof val);
}
function isHomogeneousAttributeValueArray(arr) {
  let type;
  for (const element of arr) {
    if (element == null) continue;
    const elementType = typeof element;
    if (elementType === type) continue;
    if (!type) {
      if (isValidPrimitiveAttributeValueType(elementType)) {
        type = elementType;
        continue;
      }
      return false;
    }
    return false;
  }
  return true;
}
function isValidPrimitiveAttributeValueType(valType) {
  switch (valType) {
    case "number":
    case "boolean":
    case "string":
      return true;
  }
  return false;
}

// node_modules/@opentelemetry/core/dist/common/logging-error-handler.mjs
function loggingErrorHandler() {
  return (ex) => {
    diag.error(stringifyException(ex));
  };
}
function stringifyException(ex) {
  if (typeof ex === "string") return ex;
  else return JSON.stringify(flattenException(ex));
}
function flattenException(ex) {
  const result = {};
  let current = ex;
  while (current !== null) {
    Object.getOwnPropertyNames(current).forEach((propertyName) => {
      if (result[propertyName]) return;
      const value = current[propertyName];
      if (value) result[propertyName] = String(value);
    });
    current = Object.getPrototypeOf(current);
  }
  return result;
}

// node_modules/@opentelemetry/core/dist/common/global-error-handler.mjs
var delegateHandler = loggingErrorHandler();
function globalErrorHandler(ex) {
  try {
    delegateHandler(ex);
  } catch {
  }
}

// node_modules/@opentelemetry/core/dist/common/time.mjs
var NANOSECOND_DIGITS = 9;
var MILLISECONDS_TO_NANOSECONDS = Math.pow(10, 6);
var SECOND_TO_NANOSECONDS = Math.pow(10, NANOSECOND_DIGITS);
function millisToHrTime(epochMillis) {
  const epochSeconds = epochMillis / 1e3;
  return [Math.trunc(epochSeconds), Math.round(epochMillis % 1e3 * MILLISECONDS_TO_NANOSECONDS)];
}
function hrTime(performanceNow) {
  return addHrTimes(millisToHrTime(performance.timeOrigin), millisToHrTime(typeof performanceNow === "number" ? performanceNow : performance.now()));
}
function timeInputToHrTime(time) {
  if (isTimeInputHrTime(time)) return time;
  else if (typeof time === "number") {
    if (time < performance.timeOrigin / 2) return hrTime(time);
    else return millisToHrTime(time);
  } else if (time instanceof Date) return millisToHrTime(time.getTime());
  else throw TypeError("Invalid input type");
}
function hrTimeDuration(startTime, endTime) {
  let seconds = endTime[0] - startTime[0];
  let nanos = endTime[1] - startTime[1];
  if (nanos < 0) {
    seconds -= 1;
    nanos += SECOND_TO_NANOSECONDS;
  }
  return [seconds, nanos];
}
function hrTimeToNanoseconds(time) {
  return time[0] * SECOND_TO_NANOSECONDS + time[1];
}
function hrTimeToMilliseconds(time) {
  return time[0] * 1e3 + time[1] / 1e6;
}
function isTimeInputHrTime(value) {
  return Array.isArray(value) && value.length === 2 && typeof value[0] === "number" && typeof value[1] === "number";
}
function addHrTimes(time1, time2) {
  const out = [time1[0] + time2[0], time1[1] + time2[1]];
  if (out[1] >= SECOND_TO_NANOSECONDS) {
    out[1] -= SECOND_TO_NANOSECONDS;
    out[0] += 1;
  }
  return out;
}

// node_modules/@opentelemetry/core/dist/ExportResult.mjs
var ExportResultCode = /* @__PURE__ */ (function(ExportResultCode2) {
  ExportResultCode2[ExportResultCode2["SUCCESS"] = 0] = "SUCCESS";
  ExportResultCode2[ExportResultCode2["FAILED"] = 1] = "FAILED";
  return ExportResultCode2;
})({});

// node_modules/@opentelemetry/core/dist/utils/timeout.mjs
var TimeoutError = class TimeoutError2 extends Error {
  constructor(message) {
    super(message);
    Object.setPrototypeOf(this, TimeoutError2.prototype);
  }
};
function callWithTimeout(promise, timeout) {
  let timeoutHandle;
  const timeoutPromise = new Promise(function timeoutFunction(_resolve, reject) {
    timeoutHandle = setTimeout(function timeoutHandler() {
      reject(new TimeoutError("Operation timed out."));
    }, timeout);
  });
  return Promise.race([promise, timeoutPromise]).then((result) => {
    clearTimeout(timeoutHandle);
    return result;
  }, (reason) => {
    clearTimeout(timeoutHandle);
    throw reason;
  });
}

// node_modules/@opentelemetry/core/dist/utils/promise.mjs
var Deferred = class {
  _promise;
  _resolve;
  _reject;
  constructor() {
    this._promise = new Promise((resolve, reject) => {
      this._resolve = resolve;
      this._reject = reject;
    });
  }
  get promise() {
    return this._promise;
  }
  resolve(val) {
    this._resolve(val);
  }
  reject(err) {
    this._reject(err);
  }
};

// node_modules/@opentelemetry/core/dist/utils/callback.mjs
var BindOnceFuture = class {
  _isCalled = false;
  _deferred = new Deferred();
  _callback;
  _that;
  constructor(callback, that) {
    this._callback = callback;
    this._that = that;
  }
  get isCalled() {
    return this._isCalled;
  }
  get promise() {
    return this._deferred.promise;
  }
  call(...args) {
    if (!this._isCalled) {
      this._isCalled = true;
      try {
        Promise.resolve(this._callback.call(this._that, ...args)).then((val) => this._deferred.resolve(val), (err) => this._deferred.reject(err));
      } catch (err) {
        this._deferred.reject(err);
      }
    }
    return this._deferred.promise;
  }
};

// node_modules/@opentelemetry/core/dist/version.mjs
var VERSION2 = "3.1.1";

// node_modules/@opentelemetry/core/dist/semconv.mjs
var ATTR_PROCESS_RUNTIME_NAME = "process.runtime.name";

// node_modules/@opentelemetry/semantic-conventions/dist/stable_attributes.mjs
var ATTR_EXCEPTION_MESSAGE = "exception.message";
var ATTR_EXCEPTION_STACKTRACE = "exception.stacktrace";
var ATTR_EXCEPTION_TYPE = "exception.type";
var ATTR_SERVICE_NAME = "service.name";
var ATTR_TELEMETRY_SDK_LANGUAGE = "telemetry.sdk.language";
var TELEMETRY_SDK_LANGUAGE_VALUE_WEBJS = "webjs";
var ATTR_TELEMETRY_SDK_NAME = "telemetry.sdk.name";
var ATTR_TELEMETRY_SDK_VERSION = "telemetry.sdk.version";

// node_modules/@opentelemetry/core/dist/platform/browser/sdk-info.mjs
var SDK_INFO = {
  [ATTR_TELEMETRY_SDK_NAME]: "opentelemetry",
  [ATTR_PROCESS_RUNTIME_NAME]: "browser",
  [ATTR_TELEMETRY_SDK_LANGUAGE]: TELEMETRY_SDK_LANGUAGE_VALUE_WEBJS,
  [ATTR_TELEMETRY_SDK_VERSION]: VERSION2
};

// node_modules/@opentelemetry/otlp-exporter-base/dist/otlp-export-delegate.mjs
var OTLPExportDelegate = class {
  _metrics;
  _diagLogger;
  _transport;
  _serializer;
  _responseHandler;
  _promiseQueue;
  _timeout;
  constructor(transport, serializer, responseHandler, promiseQueue, metrics, timeout) {
    this._transport = transport;
    this._serializer = serializer;
    this._responseHandler = responseHandler;
    this._promiseQueue = promiseQueue;
    this._timeout = timeout;
    this._diagLogger = diag.createComponentLogger({ namespace: "OTLPExportDelegate" });
    this._metrics = metrics;
  }
  export(internalRepresentation, resultCallback) {
    this._diagLogger.debug("items to be sent", internalRepresentation);
    if (this._promiseQueue.hasReachedLimit()) {
      resultCallback({
        code: ExportResultCode.FAILED,
        error: /* @__PURE__ */ new Error("Concurrent export limit reached")
      });
      return;
    }
    const serializedRequest = this._serializer.serializeRequest(internalRepresentation);
    if (serializedRequest == null) {
      resultCallback({
        code: ExportResultCode.FAILED,
        error: /* @__PURE__ */ new Error("Nothing to send")
      });
      return;
    }
    const finishExport = this._metrics.startExport(internalRepresentation);
    this._promiseQueue.pushPromise(this._transport.send(serializedRequest, this._timeout).then((response) => {
      if (response.status === "success") {
        finishExport(void 0);
        if (response.data != null) try {
          this._responseHandler.handleResponse(this._serializer.deserializeResponse(response.data));
        } catch (e) {
          this._diagLogger.warn("Export succeeded but could not deserialize response - is the response specification compliant?", e, response.data);
        }
        resultCallback({ code: ExportResultCode.SUCCESS });
        return;
      } else if (response.status === "failure" && response.error) {
        finishExport(response.error);
        resultCallback({
          code: ExportResultCode.FAILED,
          error: response.error
        });
        return;
      } else if (response.status === "retryable") {
        finishExport("export_max_retries");
        resultCallback({
          code: ExportResultCode.FAILED,
          error: response.error ?? new OTLPExporterError("Export failed with retryable status")
        });
      } else {
        finishExport("export_failed");
        resultCallback({
          code: ExportResultCode.FAILED,
          error: new OTLPExporterError("Export failed with unknown error")
        });
      }
    }, (reason) => {
      finishExport(reason);
      resultCallback({
        code: ExportResultCode.FAILED,
        error: reason
      });
    }));
  }
  forceFlush() {
    return this._promiseQueue.awaitAll();
  }
  setMetrics(metrics) {
    this._metrics = metrics;
  }
  async shutdown() {
    this._diagLogger.debug("shutdown started");
    await this.forceFlush();
    this._transport.shutdown();
  }
};
function createOtlpExportDelegate(components, settings) {
  return new OTLPExportDelegate(components.transport, components.serializer, createLoggingPartialSuccessResponseHandler(), components.promiseHandler, components.metrics, settings.timeout);
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/otlp-network-export-delegate.mjs
function createOtlpNetworkExportDelegate(options, serializer, metrics, transport) {
  return createOtlpExportDelegate({
    transport,
    serializer,
    promiseHandler: createBoundedQueueExportPromiseHandler(options),
    metrics
  }, { timeout: options.timeoutMillis });
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/semconv.mjs
var ATTR_HTTP_RESPONSE_STATUS_CODE2 = "http.response.status_code";
var ATTR_OTEL_COMPONENT_NAME = "otel.component.name";
var ATTR_OTEL_COMPONENT_TYPE = "otel.component.type";
var ATTR_SERVER_ADDRESS2 = "server.address";
var ATTR_SERVER_PORT2 = "server.port";
var ATTR_ERROR_TYPE2 = "error.type";

// node_modules/@opentelemetry/otlp-exporter-base/dist/version.mjs
var VERSION3 = "0.301.2";

// node_modules/@opentelemetry/otlp-exporter-base/dist/ExporterMetrics.mjs
var componentCounter = /* @__PURE__ */ new Map();
var ExporterMetrics = class {
  inflight;
  exported;
  duration;
  standardAttrs;
  responseAttributesFromError;
  helper;
  constructor(options) {
    const { componentType, metricsHelper, meterProvider, url, responseAttributesFromError } = options;
    this.responseAttributesFromError = responseAttributesFromError;
    const meter = meterProvider ? meterProvider.getMeter("@opentelemetry/otlp-exporter", VERSION3) : createNoopMeter();
    const counter = componentCounter.get(componentType) ?? 0;
    componentCounter.set(componentType, counter + 1);
    this.standardAttrs = {
      [ATTR_OTEL_COMPONENT_TYPE]: componentType,
      [ATTR_OTEL_COMPONENT_NAME]: `${componentType}/${counter}`
    };
    if (url) {
      let urlToParse = url;
      if (!url.includes("://")) urlToParse = `http://${url}`;
      try {
        const parsedUrl = new URL(urlToParse);
        this.standardAttrs[ATTR_SERVER_ADDRESS2] = parsedUrl.hostname;
        let port = void 0;
        if (parsedUrl.port) port = Number(parsedUrl.port);
        else if (parsedUrl.protocol === "http:") port = 80;
        else if (parsedUrl.protocol === "https:") port = 443;
        if (typeof port === "number") this.standardAttrs[ATTR_SERVER_PORT2] = port;
      } catch {
      }
    }
    this.helper = metricsHelper;
    this.inflight = meter.createUpDownCounter(`otel.sdk.exporter.${this.helper.name}.inflight`, {
      unit: `{${this.helper.name}}`,
      description: `The number of ${this.helper.name}s which were passed to the exporter, but that have not been exported yet (neither successful, nor failed).`
    });
    this.exported = meter.createCounter(`otel.sdk.exporter.${this.helper.name}.exported`, {
      unit: `{${this.helper.name}}`,
      description: `The number of ${this.helper.name}s for which the export has finished, either successful or failed.`
    });
    this.duration = meter.createHistogram("otel.sdk.exporter.operation.duration", {
      unit: "s",
      description: "The duration of exporting a batch of telemetry records.",
      advice: { explicitBucketBoundaries: [] }
    });
  }
  startExport(request) {
    const numItems = this.helper.countItems(request);
    const startTime = hrTime();
    this.inflight.add(numItems, this.standardAttrs);
    return (error) => {
      const endTime = hrTime();
      this.inflight.add(-numItems, this.standardAttrs);
      const exportedAttrs = error ? {
        ...this.standardAttrs,
        [ATTR_ERROR_TYPE2]: error instanceof Error ? error.name : "export_failed"
      } : this.standardAttrs;
      this.exported.add(numItems, exportedAttrs);
      const durationAttrs = {
        ...exportedAttrs,
        ...this.responseAttributesFromError(error)
      };
      const duration = hrTimeToMilliseconds(hrTimeDuration(startTime, endTime)) / 1e3;
      this.duration.record(duration, durationAttrs);
    };
  }
};

// node_modules/@opentelemetry/otlp-transformer/dist/logs/index.mjs
var LogsExporterMetricsHelper = {
  name: "log",
  countItems: (request) => request.length
};

// node_modules/@opentelemetry/otlp-transformer/dist/platform/browser/base64.mjs
function encodeBase64(bytes) {
  const chars = new Array(bytes.length);
  for (let i = 0; i < bytes.length; i++) chars[i] = String.fromCharCode(bytes[i]);
  return btoa(chars.join(""));
}

// node_modules/@opentelemetry/resources/dist/utils.mjs
var isPromiseLike = (val) => {
  return val !== null && typeof val === "object" && typeof val.then === "function";
};

// node_modules/@opentelemetry/resources/dist/detectors/platform/browser/default-service-name.mjs
function defaultServiceName() {
  return "unknown_service";
}

// node_modules/@opentelemetry/resources/dist/ResourceImpl.mjs
var ResourceImpl = class ResourceImpl2 {
  _rawAttributes;
  _asyncAttributesPending = false;
  _schemaUrl;
  _memoizedAttributes;
  static FromAttributeList(attributes, options) {
    const res = new ResourceImpl2({}, options);
    res._rawAttributes = guardedRawAttributes(attributes);
    res._asyncAttributesPending = attributes.filter(([_, val]) => isPromiseLike(val)).length > 0;
    return res;
  }
  constructor(resource, options) {
    const attributes = resource.attributes ?? {};
    this._rawAttributes = Object.entries(attributes).map(([k, v]) => {
      if (isPromiseLike(v)) this._asyncAttributesPending = true;
      return [k, v];
    });
    this._rawAttributes = guardedRawAttributes(this._rawAttributes);
    this._schemaUrl = validateSchemaUrl(options?.schemaUrl);
  }
  get asyncAttributesPending() {
    return this._asyncAttributesPending;
  }
  async waitForAsyncAttributes() {
    if (!this.asyncAttributesPending) return;
    for (let i = 0; i < this._rawAttributes.length; i++) {
      const [k, v] = this._rawAttributes[i];
      this._rawAttributes[i] = [k, isPromiseLike(v) ? await v : v];
    }
    this._asyncAttributesPending = false;
  }
  get attributes() {
    if (this.asyncAttributesPending) diag.error("Accessing resource attributes before async attributes settled");
    if (this._memoizedAttributes) return this._memoizedAttributes;
    const attrs = {};
    for (const [k, v] of this._rawAttributes) {
      if (isPromiseLike(v)) {
        diag.debug(`Unsettled resource attribute ${k} skipped`);
        continue;
      }
      if (Object.hasOwn(attrs, k)) {
      } else if (k.length === 0) diag.warn("dropping invalid resource attribute key: <empty string>");
      else if (!isSimpleAttributeValue(v)) diag.warn(`dropping invalid resource attribute value for key "${k}"`);
      else attrs[k] = v;
    }
    if (!this._asyncAttributesPending) this._memoizedAttributes = attrs;
    return attrs;
  }
  getRawAttributes() {
    return this._rawAttributes;
  }
  get schemaUrl() {
    return this._schemaUrl;
  }
  merge(resource) {
    if (resource == null) return this;
    const mergedSchemaUrl = mergeSchemaUrl(this, resource);
    const mergedOptions = mergedSchemaUrl ? { schemaUrl: mergedSchemaUrl } : void 0;
    return ResourceImpl2.FromAttributeList([...resource.getRawAttributes(), ...this.getRawAttributes()], mergedOptions);
  }
};
function resourceFromAttributes(attributes, options) {
  return ResourceImpl.FromAttributeList(Object.entries(attributes), options);
}
function defaultResource() {
  return resourceFromAttributes({
    [ATTR_SERVICE_NAME]: defaultServiceName(),
    [ATTR_TELEMETRY_SDK_LANGUAGE]: SDK_INFO[ATTR_TELEMETRY_SDK_LANGUAGE],
    [ATTR_TELEMETRY_SDK_NAME]: SDK_INFO[ATTR_TELEMETRY_SDK_NAME],
    [ATTR_TELEMETRY_SDK_VERSION]: SDK_INFO[ATTR_TELEMETRY_SDK_VERSION]
  });
}
function guardedRawAttributes(attributes) {
  return attributes.map(([k, v]) => {
    if (isPromiseLike(v)) return [k, v.then((val) => val, (err) => {
      diag.debug("promise rejection for resource attribute: %s - %s", k, err);
    })];
    return [k, v];
  });
}
function validateSchemaUrl(schemaUrl) {
  if (typeof schemaUrl === "string" || schemaUrl === void 0) return schemaUrl;
  diag.warn("Schema URL must be string or undefined, got %s. Schema URL will be ignored.", schemaUrl);
}
function mergeSchemaUrl(old, updating) {
  const oldSchemaUrl = old?.schemaUrl;
  const updatingSchemaUrl = updating?.schemaUrl;
  const isOldEmpty = oldSchemaUrl === void 0 || oldSchemaUrl === "";
  const isUpdatingEmpty = updatingSchemaUrl === void 0 || updatingSchemaUrl === "";
  if (isOldEmpty) return updatingSchemaUrl;
  if (isUpdatingEmpty) return oldSchemaUrl;
  if (oldSchemaUrl === updatingSchemaUrl) return oldSchemaUrl;
  diag.warn('Schema URL merge conflict: old resource has "%s", updating resource has "%s". Resulting resource will have undefined Schema URL.', oldSchemaUrl, updatingSchemaUrl);
}

// node_modules/@opentelemetry/otlp-transformer/dist/common/internal.mjs
function createResource(resource, encoder) {
  const result = {
    attributes: toAttributes(resource.attributes, encoder),
    droppedAttributesCount: 0
  };
  const schemaUrl = resource.schemaUrl;
  if (schemaUrl && schemaUrl !== "") result.schemaUrl = schemaUrl;
  return result;
}
function createInstrumentationScope(scope, encoder) {
  const result = {
    name: scope.name,
    version: scope.version
  };
  if (scope.attributes && Object.keys(scope.attributes).length > 0) {
    result.attributes = toAttributes(scope.attributes, encoder);
    result.droppedAttributesCount = scope.droppedAttributesCount ?? 0;
  }
  return result;
}
function toAttributes(attributes, encoder) {
  return Object.keys(attributes).map((key) => toKeyValue(key, attributes[key], encoder));
}
function toKeyValue(key, value, encoder) {
  return {
    key,
    value: toAnyValue(value, encoder)
  };
}
function toAnyValue(value, encoder) {
  const t = typeof value;
  if (t === "string") return { stringValue: value };
  if (t === "number") {
    if (!Number.isInteger(value)) return { doubleValue: value };
    return { intValue: value };
  }
  if (t === "boolean") return { boolValue: value };
  if (value instanceof Uint8Array) return { bytesValue: encoder.encodeUint8Array(value) };
  if (Array.isArray(value)) {
    const values = new Array(value.length);
    for (let i = 0; i < value.length; i++) values[i] = toAnyValue(value[i], encoder);
    return { arrayValue: { values } };
  }
  if (t === "object" && value != null) {
    const keys = Object.keys(value);
    const values = new Array(keys.length);
    for (let i = 0; i < keys.length; i++) values[i] = {
      key: keys[i],
      value: toAnyValue(value[keys[i]], encoder)
    };
    return { kvlistValue: { values } };
  }
  return {};
}

// node_modules/@opentelemetry/otlp-transformer/dist/logs/internal.mjs
function createExportLogsServiceRequest(logRecords, encoder) {
  return { resourceLogs: logRecordsToResourceLogs(logRecords, encoder) };
}
function createResourceMap(logRecords) {
  const resourceMap = /* @__PURE__ */ new Map();
  for (const record of logRecords) {
    const { resource, instrumentationScope } = record;
    let ismMap = resourceMap.get(resource);
    if (!ismMap) {
      ismMap = /* @__PURE__ */ new Map();
      resourceMap.set(resource, ismMap);
    }
    let records = ismMap.get(instrumentationScope);
    if (!records) {
      records = [];
      ismMap.set(instrumentationScope, records);
    }
    records.push(record);
  }
  return resourceMap;
}
function logRecordsToResourceLogs(logRecords, encoder) {
  const resourceMap = createResourceMap(logRecords);
  return Array.from(resourceMap, ([resource, ismMap]) => {
    const processedResource = createResource(resource, encoder);
    return {
      resource: processedResource,
      scopeLogs: Array.from(ismMap, ([, scopeLogs]) => {
        return {
          scope: createInstrumentationScope(scopeLogs[0].instrumentationScope, encoder),
          logRecords: scopeLogs.map((log) => toLogRecord(log, encoder)),
          schemaUrl: scopeLogs[0].instrumentationScope.schemaUrl
        };
      }),
      schemaUrl: processedResource.schemaUrl
    };
  });
}
function toLogRecord(log, encoder) {
  return {
    timeUnixNano: encoder.encodeHrTime(log.hrTime),
    observedTimeUnixNano: encoder.encodeHrTime(log.hrTimeObserved),
    severityNumber: toSeverityNumber(log.severityNumber),
    severityText: log.severityText,
    body: toAnyValue(log.body, encoder),
    eventName: log.eventName,
    attributes: toAttributes(log.attributes, encoder),
    droppedAttributesCount: log.droppedAttributesCount,
    flags: log.spanContext?.traceFlags,
    traceId: encoder.encodeOptionalSpanContext(log.spanContext?.traceId),
    spanId: encoder.encodeOptionalSpanContext(log.spanContext?.spanId)
  };
}
function toSeverityNumber(severityNumber) {
  return severityNumber;
}

// node_modules/@opentelemetry/otlp-transformer/dist/common/utils.mjs
function hrTimeToNanos(hrTime2) {
  const NANOSECONDS = BigInt(1e9);
  return BigInt(Math.trunc(hrTime2[0])) * NANOSECONDS + BigInt(Math.trunc(hrTime2[1]));
}
function encodeAsString(hrTime2) {
  return hrTimeToNanos(hrTime2).toString();
}
var encodeTimestamp = typeof BigInt !== "undefined" ? encodeAsString : hrTimeToNanoseconds;
function identity(value) {
  return value;
}
var JSON_ENCODER = {
  encodeHrTime: encodeTimestamp,
  encodeSpanContext: identity,
  encodeOptionalSpanContext: identity,
  encodeUint8Array: encodeBase64
};

// node_modules/@opentelemetry/otlp-transformer/dist/logs/json/logs.mjs
var JsonLogsSerializer = {
  serializeRequest: (arg) => {
    const request = createExportLogsServiceRequest(arg, JSON_ENCODER);
    return new TextEncoder().encode(JSON.stringify(request));
  },
  deserializeResponse: (arg) => {
    if (arg.length === 0) return {};
    const decoder = new TextDecoder();
    try {
      return JSON.parse(decoder.decode(arg));
    } catch (err) {
      diag.warn(`Failed to parse logs export response: ${err.message}. Returning empty response`);
      return {};
    }
  }
};

// node_modules/@opentelemetry/otlp-exporter-base/dist/retrying-transport.mjs
var MAX_ATTEMPTS = 5;
var INITIAL_BACKOFF = 1e3;
var MAX_BACKOFF = 5e3;
var BACKOFF_MULTIPLIER = 1.5;
var JITTER = 0.2;
function getJitter() {
  return Math.random() * (2 * JITTER) - JITTER;
}
var RetryingTransport = class {
  _transport;
  constructor(transport) {
    this._transport = transport;
  }
  retry(data, timeoutMillis, inMillis) {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        this._transport.send(data, timeoutMillis).then(resolve, reject);
      }, inMillis);
    });
  }
  async send(data, timeoutMillis) {
    let attempts = MAX_ATTEMPTS;
    let nextBackoff = INITIAL_BACKOFF;
    const deadline = Date.now() + timeoutMillis;
    let result = await this._transport.send(data, timeoutMillis);
    while (result.status === "retryable" && attempts > 0) {
      attempts--;
      const backoff = Math.max(Math.min(nextBackoff * (1 + getJitter()), MAX_BACKOFF), 0);
      nextBackoff = nextBackoff * BACKOFF_MULTIPLIER;
      const retryInMillis = result.retryInMillis ?? backoff;
      const remainingTimeoutMillis = deadline - Date.now();
      if (retryInMillis > remainingTimeoutMillis) {
        diag.info(`Export retry time ${Math.round(retryInMillis)}ms exceeds remaining timeout ${Math.round(remainingTimeoutMillis)}ms, not retrying further.`);
        return result;
      }
      diag.verbose(`Scheduling export retry in ${Math.round(retryInMillis)}ms`);
      result = await this.retry(data, remainingTimeoutMillis, retryInMillis);
    }
    if (result.status === "success") diag.verbose(`Export succeeded after ${MAX_ATTEMPTS - attempts} retry attempts.`);
    else if (result.status === "retryable") diag.info(`Export failed after maximum retry attempts (${MAX_ATTEMPTS}).`);
    else diag.info(`Export failed with non-retryable error: ${result.error}`);
    return result;
  }
  shutdown() {
    return this._transport.shutdown();
  }
};
function createRetryingTransport(options) {
  return new RetryingTransport(options.transport);
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/is-export-retryable.mjs
function isExportHTTPErrorRetryable(statusCode) {
  return statusCode === 429 || statusCode === 502 || statusCode === 503 || statusCode === 504;
}
function parseRetryAfterToMills(retryAfter) {
  if (retryAfter == null) return;
  const seconds = Number.parseInt(retryAfter, 10);
  if (Number.isInteger(seconds)) return seconds > 0 ? seconds * 1e3 : -1;
  const delay = new Date(retryAfter).getTime() - Date.now();
  if (delay >= 0) return delay;
  return 0;
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/platform/browser/transport/fetch-transport.mjs
var MAX_KEEPALIVE_BODY_SIZE = 61440;
var MAX_KEEPALIVE_REQUESTS = 9;
var pendingBodySize = 0;
var pendingKeepaliveCount = 0;
var FetchTransport = class {
  _parameters;
  constructor(parameters) {
    this._parameters = parameters;
  }
  async send(data, timeoutMillis) {
    const abortController = new AbortController();
    const timeout = setTimeout(() => abortController.abort(), timeoutMillis);
    let fetchApi = globalThis.fetch;
    if (typeof fetchApi.__original === "function") fetchApi = fetchApi.__original;
    const requestSize = data.byteLength;
    const wouldExceedSize = pendingBodySize + requestSize > MAX_KEEPALIVE_BODY_SIZE;
    const useKeepalive = !wouldExceedSize && !(pendingKeepaliveCount >= MAX_KEEPALIVE_REQUESTS);
    if (useKeepalive) {
      pendingBodySize += requestSize;
      pendingKeepaliveCount++;
    } else {
      const reason = wouldExceedSize ? "size limit" : "count limit";
      diag.debug(`keepalive disabled: ${(requestSize / 1024).toFixed(1)}KB payload, ${pendingKeepaliveCount} pending (${reason})`);
    }
    const suppressedContext = suppressTracing(context.active());
    let released = false;
    const release = () => {
      if (released) return;
      released = true;
      if (useKeepalive) {
        pendingBodySize -= requestSize;
        pendingKeepaliveCount--;
      }
      clearTimeout(timeout);
    };
    let drainOwnsCleanup = false;
    try {
      const url = new URL(this._parameters.url);
      const headers = await this._parameters.headers();
      const response = await context.with(suppressedContext, () => fetchApi(url.href, {
        method: "POST",
        headers,
        body: data,
        signal: abortController.signal,
        keepalive: useKeepalive,
        mode: globalThis.location ? globalThis.origin === url.origin ? "same-origin" : "cors" : "no-cors"
      }));
      drainResponseBody(response).then(release, release);
      drainOwnsCleanup = true;
      if (response.status >= 200 && response.status <= 299) {
        diag.debug(`export response success (status: ${response.status})`);
        return { status: "success" };
      } else if (isExportHTTPErrorRetryable(response.status)) {
        diag.warn(`export response retryable (status: ${response.status})`);
        const retryAfter = response.headers.get("Retry-After");
        return {
          status: "retryable",
          retryInMillis: parseRetryAfterToMills(retryAfter)
        };
      }
      diag.error(`export response failure (status: ${response.status})`);
      return {
        status: "failure",
        error: /* @__PURE__ */ new Error(`Fetch request failed with non-retryable status ${response.status}`)
      };
    } catch (error) {
      if (isFetchNetworkErrorRetryable(error)) {
        diag.warn(`export request retryable (network error: ${error})`);
        return {
          status: "retryable",
          error: new Error("Fetch request encountered a network error", { cause: error })
        };
      }
      diag.error(`export request failure (error: ${error})`);
      return {
        status: "failure",
        error: new Error("Fetch request errored", { cause: error })
      };
    } finally {
      if (!drainOwnsCleanup) release();
    }
  }
  shutdown() {
  }
};
function createFetchTransport(parameters) {
  return new FetchTransport(parameters);
}
function isFetchNetworkErrorRetryable(error) {
  return error instanceof TypeError && !error.cause;
}
async function drainResponseBody(response) {
  try {
    const body = response.body;
    if (body == null) return;
    const reader = body.getReader();
    try {
      let chunk = await reader.read();
      while (!chunk.done) chunk = await reader.read();
    } finally {
      reader.releaseLock();
    }
  } catch (error) {
    diag.debug(`error reading export response body: ${error}`);
  }
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/platform/browser/otlp-browser-http-export-delegate.mjs
function createOtlpFetchExportDelegate(options, serializer, metrics) {
  return createOtlpNetworkExportDelegate(options, serializer, metrics, createRetryingTransport({ transport: createFetchTransport(options) }));
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/util.mjs
function validateAndNormalizeHeaders(partialHeaders) {
  const headers = {};
  Object.entries(partialHeaders ?? {}).forEach(([key, value]) => {
    if (typeof value !== "undefined") headers[key] = String(value);
    else diag.warn(`Header "${key}" has invalid value (${value}) and will be ignored`);
  });
  return headers;
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/configuration/otlp-http-configuration.mjs
function mergeHeaders(userProvidedHeaders, fallbackHeaders, defaultHeaders) {
  return async () => {
    const requiredHeaders = { ...await defaultHeaders() };
    const headers = {};
    if (fallbackHeaders != null) Object.assign(headers, await fallbackHeaders());
    if (userProvidedHeaders != null) Object.assign(headers, validateAndNormalizeHeaders(await userProvidedHeaders()));
    return Object.assign(headers, requiredHeaders);
  };
}
function validateUserProvidedUrl(url) {
  if (url == null) return;
  try {
    const base = globalThis.location?.href;
    return new URL(url, base).href;
  } catch {
    throw new Error(`Configuration: Could not parse user-provided export URL: '${url}'`);
  }
}
function mergeOtlpHttpConfigurationWithDefaults(userProvidedConfiguration, fallbackConfiguration, defaultConfiguration) {
  return {
    ...mergeOtlpSharedConfigurationWithDefaults(userProvidedConfiguration, fallbackConfiguration, defaultConfiguration),
    headers: mergeHeaders(userProvidedConfiguration.headers, fallbackConfiguration.headers, defaultConfiguration.headers),
    url: validateUserProvidedUrl(userProvidedConfiguration.url) ?? fallbackConfiguration.url ?? defaultConfiguration.url
  };
}
function getHttpConfigurationDefaults(requiredHeaders, signalResourcePath) {
  return {
    ...getSharedConfigurationDefaults(),
    headers: async () => requiredHeaders,
    url: "http://localhost:4318/" + signalResourcePath
  };
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/configuration/convert-legacy-http-options.mjs
function convertLegacyHeaders(config) {
  if (typeof config.headers === "function") return config.headers;
  return wrapStaticHeadersInFunction(config.headers);
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/platform/browser/configuration/convert-legacy-browser-http-options.mjs
function convertLegacyBrowserHttpOptions(config, signalResourcePath, requiredHeaders) {
  return mergeOtlpHttpConfigurationWithDefaults({
    url: config.url,
    timeoutMillis: config.timeoutMillis,
    headers: convertLegacyHeaders(config),
    concurrencyLimit: config.concurrencyLimit
  }, {}, getHttpConfigurationDefaults(requiredHeaders, signalResourcePath));
}

// node_modules/@opentelemetry/otlp-exporter-base/dist/platform/browser/configuration/create-legacy-browser-delegate.mjs
function createLegacyOtlpBrowserExporterMetrics(metricsComponentType, exporterMetricsHelper, url, meterProvider) {
  return new ExporterMetrics({
    componentType: metricsComponentType,
    metricsHelper: exporterMetricsHelper,
    url,
    meterProvider,
    responseAttributesFromError: (error) => {
      if (!error) return { [ATTR_HTTP_RESPONSE_STATUS_CODE2]: 200 };
      if (!(error instanceof Error)) return {};
      if (error.message.startsWith("Fetch request failed with non-retryable status ")) {
        const statusStr = error.message.substring(47);
        return { [ATTR_HTTP_RESPONSE_STATUS_CODE2]: Number(statusStr) };
      }
      return {};
    }
  });
}
function createLegacyOtlpBrowserExportDelegate(config, serializer, metricsComponentType, exporterMetricsHelper, meterProvider, signalResourcePath, requiredHeaders) {
  const options = convertLegacyBrowserHttpOptions(config, signalResourcePath, requiredHeaders);
  return createOtlpFetchExportDelegate(options, serializer, createLegacyOtlpBrowserExporterMetrics(metricsComponentType, exporterMetricsHelper, options.url, config.selfObsMeterProvider));
}

// node_modules/@opentelemetry/exporter-logs-otlp-http/dist/platform/browser/OTLPLogExporter.mjs
var OTLPLogExporter = class extends OTLPExporterBase {
  constructor(config = {}) {
    super(createLegacyOtlpBrowserExportDelegate(config, JsonLogsSerializer, OTEL_COMPONENT_TYPE_VALUE_OTLP_HTTP_LOG_EXPORTER, LogsExporterMetricsHelper, config.selfObsMeterProvider, "v1/logs", { "Content-Type": "application/json" }));
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/LogRecordImpl.mjs
var LogRecordImpl = class {
  resource;
  instrumentationScope;
  attributes = {};
  _hrTime;
  _hrTimeObserved;
  _spanContext;
  _severityText;
  _severityNumber;
  _body;
  _eventName;
  _attributesCount = 0;
  _droppedAttributesCount = 0;
  _isReadonly = false;
  _logRecordLimits;
  get hrTime() {
    return this._hrTime;
  }
  set hrTime(hrTime2) {
    if (this._isLogRecordReadonly()) return;
    this._hrTime = hrTime2;
  }
  get hrTimeObserved() {
    return this._hrTimeObserved;
  }
  set hrTimeObserved(hrTimeObserved) {
    if (this._isLogRecordReadonly()) return;
    this._hrTimeObserved = hrTimeObserved;
  }
  get spanContext() {
    return this._spanContext;
  }
  set spanContext(spanContext) {
    if (this._isLogRecordReadonly()) return;
    this._spanContext = spanContext;
  }
  set severityText(severityText) {
    if (this._isLogRecordReadonly()) return;
    this._severityText = severityText;
  }
  get severityText() {
    return this._severityText;
  }
  set severityNumber(severityNumber) {
    if (this._isLogRecordReadonly()) return;
    this._severityNumber = severityNumber;
  }
  get severityNumber() {
    return this._severityNumber;
  }
  set body(body) {
    if (this._isLogRecordReadonly()) return;
    this._body = body;
  }
  get body() {
    return this._body;
  }
  get eventName() {
    return this._eventName;
  }
  set eventName(eventName) {
    if (this._isLogRecordReadonly()) return;
    this._eventName = eventName;
  }
  get droppedAttributesCount() {
    return this._droppedAttributesCount;
  }
  constructor(_sharedState, instrumentationScope, logRecord) {
    const { timestamp, observedTimestamp, eventName, severityNumber, severityText, body, attributes = {}, exception: exception2, context: context2 } = logRecord;
    const now = Date.now();
    this._hrTime = timeInputToHrTime(timestamp ?? now);
    this._hrTimeObserved = timeInputToHrTime(observedTimestamp ?? now);
    if (context2) {
      const spanContext = trace.getSpanContext(context2);
      if (spanContext && isSpanContextValid(spanContext)) this._spanContext = spanContext;
    }
    this.severityNumber = severityNumber;
    this.severityText = severityText;
    this.body = body;
    this.resource = _sharedState.resource;
    this.instrumentationScope = instrumentationScope;
    this._logRecordLimits = _sharedState.logRecordLimits;
    this._eventName = eventName;
    this.setAttributes(attributes);
    if (exception2 != null) this._setException(exception2);
  }
  setAttribute(key, value) {
    if (this._isLogRecordReadonly()) return this;
    const decision = maybeAddAttribute({
      key,
      value,
      attributes: this.attributes,
      limits: this._logRecordLimits,
      currentAttributesCount: this._attributesCount
    });
    if (decision === AddAttributeDecision.DROP_LIMIT_REACHED) {
      this._droppedAttributesCount++;
      if (this._droppedAttributesCount === 1) diag.warn("Dropping extra attributes.");
    } else if (decision === AddAttributeDecision.ADD_NEW) this._attributesCount++;
    return this;
  }
  setAttributes(attributes) {
    for (const [k, v] of Object.entries(attributes)) this.setAttribute(k, v);
    return this;
  }
  setBody(body) {
    this.body = body;
    return this;
  }
  setEventName(eventName) {
    this.eventName = eventName;
    return this;
  }
  setSeverityNumber(severityNumber) {
    this.severityNumber = severityNumber;
    return this;
  }
  setSeverityText(severityText) {
    this.severityText = severityText;
    return this;
  }
  /**
  * @internal
  * A LogRecordProcessor may freely modify logRecord for the duration of the OnEmit call.
  * If logRecord is needed after OnEmit returns (i.e. for asynchronous processing) only reads are permitted.
  */
  _makeReadonly() {
    this._isReadonly = true;
  }
  _setException(exception2) {
    let hasMinimumAttributes = false;
    if (typeof exception2 === "string" || typeof exception2 === "number") {
      if (!Object.hasOwn(this.attributes, ATTR_EXCEPTION_MESSAGE)) this.setAttribute(ATTR_EXCEPTION_MESSAGE, String(exception2));
      hasMinimumAttributes = true;
    } else if (exception2 && typeof exception2 === "object") {
      const exceptionObj = exception2;
      if (exceptionObj.code) {
        if (!Object.hasOwn(this.attributes, ATTR_EXCEPTION_TYPE)) this.setAttribute(ATTR_EXCEPTION_TYPE, exceptionObj.code.toString());
        hasMinimumAttributes = true;
      } else if (exceptionObj.name) {
        if (!Object.hasOwn(this.attributes, ATTR_EXCEPTION_TYPE)) this.setAttribute(ATTR_EXCEPTION_TYPE, exceptionObj.name);
        hasMinimumAttributes = true;
      }
      if (exceptionObj.message) {
        if (!Object.hasOwn(this.attributes, ATTR_EXCEPTION_MESSAGE)) this.setAttribute(ATTR_EXCEPTION_MESSAGE, exceptionObj.message);
        hasMinimumAttributes = true;
      }
      if (exceptionObj.stack) {
        if (!Object.hasOwn(this.attributes, ATTR_EXCEPTION_STACKTRACE)) this.setAttribute(ATTR_EXCEPTION_STACKTRACE, exceptionObj.stack);
        hasMinimumAttributes = true;
      }
    }
    if (!hasMinimumAttributes) diag.warn(`Failed to record an exception ${exception2}`);
  }
  _isLogRecordReadonly() {
    if (this._isReadonly) diag.warn("Can not execute the operation on emitted log record");
    return this._isReadonly;
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/Logger.mjs
var Logger$1 = class {
  _instrumentationScope;
  _sharedState;
  _loggerConfig;
  constructor(instrumentationScope, sharedState) {
    this._instrumentationScope = instrumentationScope;
    this._sharedState = sharedState;
    this._loggerConfig = this._sharedState.getLoggerConfig(this._instrumentationScope);
  }
  emit(logRecord) {
    const currentContext = logRecord.context || context.active();
    if (!this.enabled(logRecord)) return;
    const logRecordInstance = new LogRecordImpl(this._sharedState, this._instrumentationScope, {
      context: currentContext,
      ...logRecord
    });
    this._sharedState.loggerMetrics.emitLog();
    this._sharedState.activeProcessor.onEmit(logRecordInstance, currentContext);
    logRecordInstance._makeReadonly();
  }
  enabled(options) {
    if (this._sharedState.hasShutdown) return false;
    const loggerConfig = this._loggerConfig;
    if (!loggerConfig.enabled) return false;
    const severityNumber = options?.severityNumber;
    if (typeof severityNumber === "number" && severityNumber !== SeverityNumber.UNSPECIFIED && severityNumber < loggerConfig.minimumSeverity) return false;
    const currentContext = options?.context || context.active();
    if (loggerConfig.traceBased) {
      const spanContext = trace.getSpanContext(currentContext);
      if (spanContext && isSpanContextValid(spanContext)) {
        if (!((spanContext.traceFlags & TraceFlags.SAMPLED) === TraceFlags.SAMPLED)) return false;
      }
    }
    const enabledOpts = {
      context: currentContext,
      instrumentationScope: this._instrumentationScope,
      severityNumber: options?.severityNumber,
      eventName: options?.eventName
    };
    for (const processor of this._sharedState.processors) if (!processor.enabled || processor.enabled(enabledOpts)) return true;
    return false;
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/internal/utils.mjs
function normalizeAnyValue(value) {
  if (value === void 0) return ["u", null];
  if (value === null) return ["n", null];
  const valueType = typeof value;
  if (valueType === "string") return ["s", value];
  if (valueType === "boolean") return ["b", value];
  if (valueType === "number") {
    if (Number.isNaN(value)) return ["nan", null];
    if (value === Infinity) return ["inf", null];
    if (value === -Infinity) return ["-inf", null];
    if (Object.is(value, -0)) return ["n0", null];
    return ["d", value];
  }
  if (value instanceof Uint8Array) return ["bytes", Array.from(value)];
  if (Array.isArray(value)) return ["arr", value.map(normalizeAnyValue)];
  return ["map", Object.entries(value).sort(([a], [b]) => a.localeCompare(b)).map(([k, v]) => [k, normalizeAnyValue(v)])];
}
function getInstrumentationScopeKey(scope) {
  return JSON.stringify([
    scope.name,
    scope.version || "",
    scope.schemaUrl || "",
    normalizeAnyValue(scope.attributes ?? {}),
    scope.droppedAttributesCount ?? 0
  ]);
}

// node_modules/@opentelemetry/sdk-logs/dist/export/NoopLogRecordProcessor.mjs
var NoopLogRecordProcessor = class {
  forceFlush() {
    return Promise.resolve();
  }
  onEmit(_logRecord, _context) {
  }
  shutdown() {
    return Promise.resolve();
  }
  enabled(_options) {
    return false;
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/MultiLogRecordProcessor.mjs
var MultiLogRecordProcessor = class {
  processors;
  constructor(processors) {
    this.processors = processors;
  }
  async forceFlush(options) {
    const timeout = options?.timeoutMillis ?? 3e4;
    await Promise.all(this.processors.map((processor) => callWithTimeout(processor.forceFlush(), timeout)));
  }
  onEmit(logRecord, context2) {
    this.processors.forEach((processors) => processors.onEmit(logRecord, context2));
  }
  async shutdown() {
    await Promise.all(this.processors.map((processor) => processor.shutdown()));
  }
  enabled(options) {
    for (const processor of this.processors) if (!processor.enabled || processor.enabled(options)) return true;
    return false;
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/semconv.mjs
var METRIC_OTEL_SDK_LOG_CREATED = "otel.sdk.log.created";
var METRIC_OTEL_SDK_PROCESSOR_LOG_PROCESSED = "otel.sdk.processor.log.processed";
var METRIC_OTEL_SDK_PROCESSOR_LOG_QUEUE_CAPACITY = "otel.sdk.processor.log.queue.capacity";
var METRIC_OTEL_SDK_PROCESSOR_LOG_QUEUE_SIZE = "otel.sdk.processor.log.queue.size";
var ATTR_OTEL_COMPONENT_NAME2 = "otel.component.name";
var ATTR_OTEL_COMPONENT_TYPE2 = "otel.component.type";
var OTEL_COMPONENT_TYPE_VALUE_BATCHING_LOG_PROCESSOR = "batching_log_processor";
var ATTR_ERROR_TYPE3 = "error.type";

// node_modules/@opentelemetry/sdk-logs/dist/LoggerMetrics.mjs
var LoggerMetrics = class {
  createdLogs;
  constructor(meter) {
    this.createdLogs = meter.createCounter(METRIC_OTEL_SDK_LOG_CREATED, {
      unit: "{log_record}",
      description: "The number of logs submitted to enabled SDK Loggers."
    });
  }
  emitLog() {
    this.createdLogs.add(1);
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/version.mjs
var VERSION4 = "3.1.1";

// node_modules/@opentelemetry/sdk-logs/dist/internal/LoggerProviderSharedState.mjs
var DEFAULT_LOGGER_CONFIG = {
  enabled: true,
  minimumSeverity: SeverityNumber.UNSPECIFIED,
  traceBased: false
};
var DEFAULT_LOGGER_CONFIGURATOR = () => ({ ...DEFAULT_LOGGER_CONFIG });
var LoggerProviderSharedState = class {
  loggers = /* @__PURE__ */ new Map();
  activeProcessor;
  registeredLogRecordProcessors = [];
  resource;
  logRecordLimits;
  processors;
  loggerMetrics;
  hasShutdown = false;
  _loggerConfigurator;
  _loggerConfigs = /* @__PURE__ */ new Map();
  constructor(resource, logRecordLimits, processors, loggerConfigurator, meterProvider) {
    this.resource = resource;
    this.logRecordLimits = logRecordLimits;
    this.processors = processors;
    if (processors.length > 0) {
      this.registeredLogRecordProcessors = processors;
      this.activeProcessor = new MultiLogRecordProcessor(this.registeredLogRecordProcessors);
    } else this.activeProcessor = new NoopLogRecordProcessor();
    this._loggerConfigurator = loggerConfigurator ?? DEFAULT_LOGGER_CONFIGURATOR;
    const meter = meterProvider ? meterProvider.getMeter("@opentelemetry/sdk-logs", VERSION4) : createNoopMeter();
    this.loggerMetrics = new LoggerMetrics(meter);
  }
  /**
  * Get the LoggerConfig for a given instrumentation scope.
  * Uses the LoggerConfigurator function to compute the config on first access
  * and caches the result.
  *
  * @experimental This feature is in development as per the OpenTelemetry specification.
  */
  getLoggerConfig(instrumentationScope) {
    const key = getInstrumentationScopeKey(instrumentationScope);
    let config = this._loggerConfigs.get(key);
    if (config) return config;
    config = this._loggerConfigurator(instrumentationScope);
    this._loggerConfigs.set(key, config);
    return config;
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/LoggerProvider.mjs
var LoggerProvider$1 = class {
  _shutdownOnce;
  _sharedState;
  constructor(config = {}) {
    const mergedConfig = {
      resource: config.resource ?? defaultResource(),
      logRecordLimits: {
        attributeCountLimit: config.logRecordLimits?.attributeCountLimit ?? 128,
        attributeValueLengthLimit: config.logRecordLimits?.attributeValueLengthLimit ?? Infinity
      },
      loggerConfigurator: config.loggerConfigurator ?? DEFAULT_LOGGER_CONFIGURATOR,
      processors: config.processors ?? [],
      meterProvider: config.meterProvider
    };
    this._sharedState = new LoggerProviderSharedState(mergedConfig.resource, mergedConfig.logRecordLimits, mergedConfig.processors, mergedConfig.loggerConfigurator, mergedConfig.meterProvider);
    this._shutdownOnce = new BindOnceFuture(this._shutdown, this);
  }
  /**
  * Get a logger with the configuration of the LoggerProvider.
  */
  getLogger(name, version, options) {
    if (this._shutdownOnce.isCalled) {
      diag.warn("A shutdown LoggerProvider cannot provide a Logger");
      return createNoopLogger();
    }
    if (!name) diag.warn("Logger requested without instrumentation scope name.");
    const instrumentationScope = {
      name: name || "unknown",
      version,
      schemaUrl: options?.schemaUrl,
      ...cleanSimpleAttributes(options?.attributes, this._sharedState.logRecordLimits)
    };
    const key = getInstrumentationScopeKey(instrumentationScope);
    if (!this._sharedState.loggers.has(key)) this._sharedState.loggers.set(key, new Logger$1(instrumentationScope, this._sharedState));
    return this._sharedState.loggers.get(key);
  }
  /**
  * Notifies all registered LogRecordProcessor to flush any buffered data.
  *
  * Returns a promise which is resolved when all flushes are complete.
  */
  forceFlush(options) {
    if (this._shutdownOnce.isCalled) {
      diag.warn("invalid attempt to force flush after LoggerProvider shutdown");
      return this._shutdownOnce.promise;
    }
    return this._sharedState.activeProcessor.forceFlush(options);
  }
  /**
  * Flush all buffered data and shut down the LoggerProvider and all registered
  * LogRecordProcessor.
  *
  * Returns a promise which is resolved when all flushes are complete.
  */
  shutdown() {
    if (this._shutdownOnce.isCalled) {
      diag.warn("shutdown may only be called once per LoggerProvider");
      return this._shutdownOnce.promise;
    }
    return this._shutdownOnce.call();
  }
  _shutdown() {
    this._sharedState.hasShutdown = true;
    return this._sharedState.activeProcessor.shutdown();
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/export/LogRecordProcessorMetrics.mjs
var componentCounter2 = /* @__PURE__ */ new Map();
var LogRecordProcessorMetrics = class {
  processedLogs;
  queueSize;
  queueSizeCallback;
  standardAttrs;
  droppedAttrs;
  constructor(componentType, meter, queueConfig) {
    const counter = componentCounter2.get(componentType) ?? 0;
    componentCounter2.set(componentType, counter + 1);
    this.standardAttrs = {
      [ATTR_OTEL_COMPONENT_TYPE2]: componentType,
      [ATTR_OTEL_COMPONENT_NAME2]: `${componentType}/${counter}`
    };
    this.droppedAttrs = {
      ...this.standardAttrs,
      [ATTR_ERROR_TYPE3]: "queue_full"
    };
    this.processedLogs = meter.createCounter(METRIC_OTEL_SDK_PROCESSOR_LOG_PROCESSED, {
      unit: "{log_record}",
      description: "The number of log records for which the processing has finished, either successful or failed."
    });
    if (queueConfig) {
      const { capacity, getQueueSize } = queueConfig;
      meter.createUpDownCounter(METRIC_OTEL_SDK_PROCESSOR_LOG_QUEUE_CAPACITY, {
        unit: "{log_record}",
        description: "The maximum number of log records the queue of a given instance of an SDK log processor can hold."
      }).add(capacity, this.standardAttrs);
      this.queueSize = meter.createObservableUpDownCounter(METRIC_OTEL_SDK_PROCESSOR_LOG_QUEUE_SIZE, {
        unit: "{log_record}",
        description: "The number of log records in the queue of a given instance of an SDK log processor."
      });
      this.queueSizeCallback = (result) => result.observe(getQueueSize(), this.standardAttrs);
      this.queueSize.addCallback(this.queueSizeCallback);
    }
  }
  dropLogs(count) {
    this.processedLogs.add(count, this.droppedAttrs);
  }
  finishLogs(count, error) {
    if (!error) {
      this.processedLogs.add(count, this.standardAttrs);
      return;
    }
    const attrs = {
      ...this.standardAttrs,
      [ATTR_ERROR_TYPE3]: error.name
    };
    this.processedLogs.add(count, attrs);
  }
  shutdown() {
    if (this.queueSize && this.queueSizeCallback) this.queueSize.removeCallback(this.queueSizeCallback);
  }
};

// node_modules/@opentelemetry/sdk-logs/dist/export/BatchLogRecordProcessorBase.mjs
async function waitForResources(logRecords) {
  let pendingResources = null;
  for (let i = 0, len = logRecords.length; i < len; i++) {
    const resource = logRecords[i].resource;
    if (resource.asyncAttributesPending && resource.waitForAsyncAttributes) {
      pendingResources ??= /* @__PURE__ */ new Set();
      pendingResources.add(resource);
    }
  }
  if (pendingResources !== null) await Promise.all(Array.from(pendingResources, (resource) => resource.waitForAsyncAttributes?.()));
}
var ExportOperation = class {
  _exportCompleted;
  _exportScheduledPromise;
  _metrics;
  _exportScheduledResolve;
  constructor(exporter, logRecords, exportTimeoutMillis, metrics) {
    this._exportScheduledPromise = new Promise((resolve) => {
      this._exportScheduledResolve = resolve;
    });
    this._exportCompleted = this._executeExport(exporter, logRecords, exportTimeoutMillis);
    this._metrics = metrics;
  }
  /** Get the promise that resolves when the export completes */
  get exportCompleted() {
    return this._exportCompleted;
  }
  /** Get the promise that resolves when exporter.export() has been called */
  get exportScheduled() {
    return this._exportScheduledPromise;
  }
  async _executeExport(exporter, logRecords, exportTimeoutMillis) {
    try {
      await waitForResources(logRecords);
      await context.with(suppressTracing(context.active()), async () => {
        return this._exportWithTimeout(exporter, logRecords, exportTimeoutMillis);
      });
    } catch (e) {
      globalErrorHandler(e);
      this._exportScheduledResolve();
    }
  }
  async _exportWithTimeout(exporter, logRecords, exportTimeoutMillis) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        reject(/* @__PURE__ */ new Error("Timeout"));
      }, exportTimeoutMillis);
      exporter.export(logRecords, (result) => {
        this._metrics.finishLogs(logRecords.length, result.error);
        clearTimeout(timer);
        if (result.code === ExportResultCode.SUCCESS) resolve();
        else reject(result.error ?? /* @__PURE__ */ new Error("BatchLogRecordProcessor: log record export failed"));
      });
      this._exportScheduledResolve();
    });
  }
};
var BatchLogRecordProcessorBase = class {
  _maxExportBatchSize;
  _maxQueueSize;
  _scheduledDelayMillis;
  _exportTimeoutMillis;
  _exporter;
  _metrics;
  _currentExport = null;
  _finishedLogRecords = [];
  _timer;
  _shutdownOnce;
  _flushing = false;
  constructor(options) {
    this._exporter = options.exporter;
    this._maxExportBatchSize = options.maxExportBatchSize ?? 512;
    this._maxQueueSize = options.maxQueueSize ?? 2048;
    this._scheduledDelayMillis = options.scheduledDelayMillis ?? 1e3;
    this._exportTimeoutMillis = options.exportTimeoutMillis ?? 3e4;
    this._shutdownOnce = new BindOnceFuture(this._shutdown, this);
    if (this._maxExportBatchSize > this._maxQueueSize) {
      diag.warn("BatchLogRecordProcessor: maxExportBatchSize must be smaller or equal to maxQueueSize, setting maxExportBatchSize to match maxQueueSize");
      this._maxExportBatchSize = this._maxQueueSize;
    }
    const meter = options?.selfObsMeterProvider ? options.selfObsMeterProvider.getMeter("@opentelemetry/sdk-logs") : createNoopMeter();
    this._metrics = new LogRecordProcessorMetrics(OTEL_COMPONENT_TYPE_VALUE_BATCHING_LOG_PROCESSOR, meter, {
      capacity: this._maxQueueSize,
      getQueueSize: () => this._finishedLogRecords.length
    });
  }
  onEmit(logRecord) {
    if (this._shutdownOnce.isCalled) return;
    this._addToBuffer(logRecord);
  }
  forceFlush() {
    if (this._shutdownOnce.isCalled) return this._shutdownOnce.promise;
    return this._flushAll();
  }
  /** Add a LogRecord in the buffer. */
  _addToBuffer(logRecord) {
    if (this._finishedLogRecords.length >= this._maxQueueSize) {
      this._metrics.dropLogs(1);
      return;
    }
    this._finishedLogRecords.push(logRecord);
    this._maybeStartTimer();
  }
  shutdown() {
    return this._shutdownOnce.call();
  }
  async _shutdown() {
    this.onShutdown();
    await this._flushAll();
    this._metrics.shutdown();
    await this._exporter.shutdown();
  }
  /**
  * Send all LogRecords to the exporter respecting the batch size limit
  * This function is used only on forceFlush or shutdown,
  * for all other cases _exportOneBatch should be used
  * */
  async _flushAll() {
    if (this._flushing) return;
    this._flushing = true;
    let toFlush = this._finishedLogRecords;
    this._finishedLogRecords = [];
    this._clearTimer();
    const inFlight = this._currentExport;
    if (inFlight !== null) {
      await this._exporter.forceFlush();
      await inFlight.exportCompleted;
      this._currentExport = null;
    }
    while (toFlush.length > 0) {
      let batch;
      if (toFlush.length <= this._maxExportBatchSize) {
        batch = toFlush;
        toFlush = [];
      } else batch = toFlush.splice(0, this._maxExportBatchSize);
      const exportOp = new ExportOperation(this._exporter, batch, this._exportTimeoutMillis, this._metrics);
      this._currentExport = exportOp;
      try {
        await exportOp.exportScheduled;
        await this._exporter.forceFlush();
        await exportOp.exportCompleted;
      } catch (e) {
        globalErrorHandler(e);
      } finally {
        this._currentExport = null;
      }
    }
    this._flushing = false;
    this._maybeStartTimer();
  }
  /**
  * Extracts one batch from the buffer.
  * Returns null if buffer is empty.
  */
  _extractBatch() {
    if (this._finishedLogRecords.length === 0) return null;
    if (this._finishedLogRecords.length <= this._maxExportBatchSize) {
      const batch = this._finishedLogRecords;
      this._finishedLogRecords = [];
      return batch;
    } else return this._finishedLogRecords.splice(0, this._maxExportBatchSize);
  }
  _exportOneBatch() {
    this._clearTimer();
    const logRecords = this._extractBatch();
    if (logRecords === null) return;
    const exportOp = new ExportOperation(this._exporter, logRecords, this._exportTimeoutMillis, this._metrics);
    this._currentExport = exportOp;
    exportOp.exportCompleted.then(() => {
      this._currentExport = null;
      this._maybeStartTimer();
    }).catch((error) => {
      this._currentExport = null;
      globalErrorHandler(error);
      this._maybeStartTimer();
    });
  }
  _maybeStartTimer() {
    if (this._shutdownOnce.isCalled) return;
    if (this._flushing) return;
    if (this._finishedLogRecords.length === 0) return;
    if (this._currentExport !== null) return;
    if (this._finishedLogRecords.length >= this._maxExportBatchSize) {
      this._exportOneBatch();
      return;
    }
    if (this._timer !== void 0) return;
    this._timer = setTimeout(() => {
      this._timer = void 0;
      this._exportOneBatch();
    }, this._scheduledDelayMillis);
    unrefTimer(this._timer);
  }
  _clearTimer() {
    if (this._timer !== void 0) {
      clearTimeout(this._timer);
      this._timer = void 0;
    }
  }
};
function unrefTimer(timer) {
  const handle = timer;
  if (typeof handle !== "number") handle.unref?.();
}

// node_modules/@opentelemetry/sdk-logs/dist/platform/browser/export/BatchLogRecordProcessor.mjs
var BatchLogRecordProcessor = class extends BatchLogRecordProcessorBase {
  _visibilityChangeListener;
  constructor(options) {
    super(options);
    this._onInit(options);
  }
  onShutdown() {
    if (typeof document === "undefined") return;
    if (this._visibilityChangeListener) document.removeEventListener("visibilitychange", this._visibilityChangeListener);
  }
  _onInit(options) {
    if (options.disableAutoFlushOnDocumentHide === true || typeof document === "undefined") return;
    this._visibilityChangeListener = () => {
      if (document.visibilityState === "hidden") this.forceFlush().catch((error) => {
        globalErrorHandler(error);
      });
    };
    document.addEventListener("visibilitychange", this._visibilityChangeListener);
  }
};

// srcts/otel/index.ts
var SEVERITIES = {
  error: SeverityNumber.ERROR,
  warn: SeverityNumber.WARN,
  info: SeverityNumber.INFO,
  debug: SeverityNumber.DEBUG,
  trace: SeverityNumber.TRACE
};
var CAUSES = 8;
function otelLogger(options) {
  const exporter = options.exporter ?? new OTLPLogExporter({ url: options.url });
  const provider = new LoggerProvider$1({ processors: [new BatchLogRecordProcessor({ exporter })] });
  const logger = provider.getLogger("blockr.core");
  const write = (line) => {
    const session = options.sessionId();
    logger.emit({
      timestamp: line.time,
      severityNumber: SEVERITIES[line.level],
      severityText: line.level.toUpperCase(),
      body: line.message,
      attributes: {
        ...line.attributes,
        ...line.package === null ? {} : { "blockr.package.name": line.package },
        ...session === void 0 ? {} : { "session.id": session },
        ...line.error === void 0 ? {} : exception(line.error)
      }
    });
  };
  return Object.assign(write, { flush: () => provider.forceFlush() });
}
function exception(error) {
  return {
    "exception.type": error.name,
    "exception.message": error.message,
    "exception.stacktrace": stackTrace(error)
  };
}
function stackTrace(error) {
  const traces = [];
  let at = error;
  while (at instanceof Error && traces.length < CAUSES) {
    const head = `${at.name}: ${at.message}`;
    const stack = at.stack ?? "";
    traces.push(stack.startsWith(head) ? stack : [head, stack].filter((text) => text !== "").join("\n"));
    at = at.cause;
  }
  return traces.join("\nCaused by: ");
}
export {
  otelLogger
};

// srcts/otel/index.ts
import { context, SeverityNumber, trace } from "@opentelemetry/api";
import {
  BatchLogRecordProcessor,
  BatchSpanProcessor,
  LoggerProvider,
  OTLPLogExporter,
  OTLPTraceExporter,
  StackContextManager,
  TracerProvider
} from "./opentelemetry-f97c2de3.js";
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
  const provider = new LoggerProvider({ processors: [new BatchLogRecordProcessor({ exporter })] });
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
function otelTracing(options) {
  const exporter = options.exporter ?? new OTLPTraceExporter({ url: options.url });
  const provider = new TracerProvider({
    spanProcessors: [sessionNamer(options.sessionId), new BatchSpanProcessor({ exporter })]
  });
  context.setGlobalContextManager(new StackContextManager().enable());
  trace.setGlobalTracerProvider(provider);
  return { flush: () => provider.forceFlush() };
}
function sessionNamer(sessionId) {
  return {
    onStart(span) {
      const session = sessionId();
      if (session !== void 0) span.setAttribute("session.id", session);
    },
    onEnd: () => void 0,
    forceFlush: () => Promise.resolve(),
    shutdown: () => Promise.resolve()
  };
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
  otelLogger,
  otelTracing
};

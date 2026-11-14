// srcts/otel/index.ts
import { BatchLogRecordProcessor, LoggerProvider, OTLPLogExporter, SeverityNumber } from "./opentelemetry-ab9396c4.js";
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

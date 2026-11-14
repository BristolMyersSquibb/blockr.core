// The parts of OpenTelemetry's JS SDK that core's OpenTelemetry module uses
// (./index.ts). The build bundles them, minified, into a file of their own,
// opentelemetry-<hash>.js, which the module imports by that name, so that the
// module's own bundle holds core's code alone (../build/build.ts). That file
// leaves its imports of OpenTelemetry's API imports, which the page's import
// map resolves to the one copy of the API on the page.

export { OTLPLogExporter } from "@opentelemetry/exporter-logs-otlp-http";
export { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
export { BatchLogRecordProcessor, LoggerProvider } from "@opentelemetry/sdk-logs";
export { BatchSpanProcessor, StackContextManager, TracerProvider } from "@opentelemetry/sdk-trace";

// The parts of OpenTelemetry's JS SDK that core's OpenTelemetry module uses
// (./index.ts). The build bundles them, minified, into a file of their own,
// opentelemetry-<hash>.js, which the module imports by that name, so that the
// module's own bundle holds core's code alone (../build/build.ts).

export { SeverityNumber } from "@opentelemetry/api";
export { OTLPLogExporter } from "@opentelemetry/exporter-logs-otlp-http";
export { BatchLogRecordProcessor, LoggerProvider } from "@opentelemetry/sdk-logs";

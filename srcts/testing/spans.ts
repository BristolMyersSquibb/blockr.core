// Records in memory the spans that core and the modules that it mounts start,
// in place of the page's tracing, until restore() runs: a tracer provider and
// the StackContextManager, registered as OpenTelemetry's API's globals, as
// core's OpenTelemetry module sets them up on the page (../otel/index.ts).
// The setup that runs around every test restores what a test leaves recording
// (./setup.ts).

import { context, trace } from "@opentelemetry/api";
import { InMemorySpanExporter, SimpleSpanProcessor, StackContextManager, TracerProvider } from "@opentelemetry/sdk-trace";
import type { ReadableSpan } from "@opentelemetry/sdk-trace";

export interface Recorded {
  // The spans that ended, in the order they ended.
  spans(): readonly ReadableSpan[];
  // The one span of the name that ended, or an error where none or several did.
  span(name: string): ReadableSpan;
  restore(): void;
}

// Records the spans that start from now on. Where `ids` are given, every span
// takes that trace id and span id, as a fixture's traceparent names them.
export function recordSpans(ids?: { readonly traceId: string; readonly spanId: string }): Recorded {
  const exporter = new InMemorySpanExporter();
  const provider = new TracerProvider({
    spanProcessors: [new SimpleSpanProcessor({ exporter })],
    ...(ids === undefined
      ? {}
      : { idGenerator: { generateTraceId: () => ids.traceId, generateSpanId: () => ids.spanId } }),
  });
  context.setGlobalContextManager(new StackContextManager().enable());
  trace.setGlobalTracerProvider(provider);
  return {
    spans: () => exporter.getFinishedSpans(),
    span(name) {
      const named = exporter.getFinishedSpans().filter((span) => span.name === name);
      const [span] = named;
      if (named.length !== 1 || span === undefined) {
        throw new Error(`${String(named.length)} spans named "${name}" ended, where one was expected.`);
      }
      return span;
    },
    restore: stopRecording,
  };
}

// Has the API record nothing, as before the page's tracing is set up.
export function stopRecording(): void {
  trace.disable();
  context.disable();
}

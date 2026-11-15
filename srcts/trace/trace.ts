// Core's spans, as Tracing has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing).
// Core records them through OpenTelemetry's API, which the page's import map
// names, so that core and the modules of packages record through one copy of
// it, into the same traces.
// Until core's OpenTelemetry module has set the page's tracing up
// (./settings.ts), the API records nothing. A span that core starts before
// then records nothing either, and its request carries no traceparent.
// The StackContextManager, which the module registers, keeps a span active
// through synchronous calls alone. So code that sends a request after an
// `await` sends it in the context that it kept before the `await`, as
// keepContext() keeps it, so that the request's span goes under the span that
// was active as its caller called.

import { context, isSpanContextValid, SpanStatusCode, trace } from "@opentelemetry/api";
import type { Span, SpanContext, Tracer } from "@opentelemetry/api";
import { asError } from "../util/index.ts";

// The name of core's tracer, as of its logger in the OpenTelemetry module.
const SCOPE = "blockr.core";

// Core's tracer. Core asks the API for it each time rather than keep one,
// since a tracer that the API gave keeps the tracer provider that it first
// found, and a test sets a provider of its own up.
export function tracer(): Tracer {
  return trace.getTracer(SCOPE);
}

// The span's context as W3C's Trace Context writes it in its header
// traceparent, as "00-<trace id>-<span id>-01", or undefined where the span
// records nothing, as before the page's tracing is set up.
export function traceparent(span: Span): string | undefined {
  if (!span.isRecording()) return undefined;
  const { traceId, spanId, traceFlags } = span.spanContext();
  if (!isSpanContextValid({ traceId, spanId, traceFlags })) return undefined;
  return `00-${traceId}-${spanId}-${(traceFlags & 0xff).toString(16).padStart(2, "0")}`;
}

// Keeps the context that is active now, and returns what runs a function in
// it, as after an `await`, so that a span that the function starts goes under
// the span that was active before the `await`.
export function keepContext(): <T>(fn: () => T) => T {
  const kept = context.active();
  return (fn) => context.with(kept, fn);
}

// The context of the span that is active now, where it records, or undefined.
export function activeSpanContext(): SpanContext | undefined {
  const span = trace.getActiveSpan();
  if (span?.isRecording() !== true) return undefined;
  const spanContext = span.spanContext();
  return isSpanContextValid(spanContext) ? spanContext : undefined;
}

// Marks the span as failed with the error, which it records as an exception,
// as OpenTelemetry's conventions for exceptions name its attributes.
export function failSpan(span: Span, error: unknown): void {
  const failure = asError(error);
  span.recordException(failure);
  span.setStatus({ code: SpanStatusCode.ERROR, message: failure.message });
}

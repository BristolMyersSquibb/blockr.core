import type { Span, SpanContext, Tracer } from "@opentelemetry/api";
export declare function tracer(): Tracer;
export declare function traceparent(span: Span): string | undefined;
export declare function keepContext(): <T>(fn: () => T) => T;
export declare function activeSpanContext(): SpanContext | undefined;
export declare function failSpan(span: Span, error: unknown): void;

// Core's spans: where the page sends them, setting its tracing up, the
// traceparent of a span, and the spans of an opening, which core records once
// the opening is done.

import { context, SpanStatusCode, trace } from "@opentelemetry/api";
import { describe, expect, test } from "vitest";
import type { OtelModule } from "../log/index.ts";
import { logged } from "../testing/logged.ts";
import { recordSpans } from "../testing/spans.ts";
import { keepContext, setUpTracing, steps, traceparent, tracer, tracesUrl } from "./index.ts";

describe("where the page sends its spans", () => {
  test("is the path v1/traces of the collector that otel_client_endpoint names, read against the page", () => {
    expect(tracesUrl("https://connect.example.com/otlp/")).toBe("https://connect.example.com/otlp/v1/traces");
    expect(tracesUrl("/otlp", "https://connect.example.com/content/1/")).toBe("https://connect.example.com/otlp/v1/traces");
    expect(tracesUrl(undefined)).toBeUndefined();
    expect(() => tracesUrl("ftp://collector")).toThrow(/takes the URL of an OpenTelemetry collector/);
  });
});

describe("setting the page's tracing up", () => {
  test("hands the module the URL and the session, once it has loaded", async () => {
    const given: { url: string; session: string | undefined }[] = [];
    const module = {
      otelLogger: () => () => undefined,
      otelTracing({ url, sessionId }) {
        given.push({ url, session: sessionId() });
      },
    } satisfies OtelModule;

    await setUpTracing("https://connect.example.com/otlp/v1/traces", () => "token-1", () => Promise.resolve(module));

    expect(given).toEqual([{ url: "https://connect.example.com/otlp/v1/traces", session: "token-1" }]);
  });

  test("loads nothing where no collector is named, and logs why where the module does not load", async () => {
    let loads = 0;
    await setUpTracing(undefined, () => undefined, () => {
      loads += 1;
      return Promise.reject(new Error("unreached"));
    });
    expect(loads).toBe(0);

    const reported = logged();
    await setUpTracing("https://connect.example.com/otlp/v1/traces", () => undefined, () => Promise.reject(new Error("404")));
    reported.restore();

    expect(reported.errors().map((line) => line.message)).toEqual([
      "Core could not set up its OpenTelemetry tracing, so the page records no spans: 404",
    ]);
  });
});

describe("a span's traceparent", () => {
  test("is its context as W3C's Trace Context writes it, while it records, and none otherwise", () => {
    const quiet = tracer().startSpan("quiet");
    expect(traceparent(quiet)).toBeUndefined();
    quiet.end();

    const recorded = recordSpans({ traceId: "0af7651916cd43dd8448eb211c80319c", spanId: "b7ad6b7169203331" });
    const span = tracer().startSpan("evaluate");
    expect(traceparent(span)).toBe("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01");
    span.end();
    recorded.restore();
  });
});

describe("a kept context", () => {
  test("runs a function after an await in the context active before it", async () => {
    const recorded = recordSpans();
    const outer = tracer().startSpan("outer");
    const active = await context.with(trace.setSpan(context.active(), outer), async () => {
      const inCaller = keepContext();
      await Promise.resolve();
      return [trace.getActiveSpan(), inCaller(() => trace.getActiveSpan())];
    });
    outer.end();

    expect(active).toEqual([undefined, outer]);
    recorded.restore();
  });
});

describe("the spans of an opening", () => {
  test("are recorded once the tracing is set up, with a span for each step inside the span of the whole, at the times noted", async () => {
    const opening = steps("start", 10);
    opening.noted("hello", 12, 20);
    await opening.step("modules", () => Promise.resolve());
    await expect(opening.step("store.open", () => Promise.reject(new Error("No JSON.")))).rejects.toThrow("No JSON.");

    let setUp: () => void = () => undefined;
    const tracing = new Promise<void>((resolve) => {
      setUp = resolve;
    });
    opening.record(tracing, new Error("The board did not open."));
    // A second record records nothing.
    opening.record(Promise.resolve());

    // Spans recorded before the tracing is set up would record nothing.
    const recorded = recordSpans();
    setUp();
    await tracing;
    await Promise.resolve();

    const whole = recorded.span("start");
    expect(whole.status).toEqual({ code: SpanStatusCode.ERROR, message: "The board did not open." });
    // The SDK corrects the times of each span by the gap between the system's
    // clock and the page's, as the span starts, so that two spans place their
    // times within a millisecond of each other, while each holds its duration.
    const nanos = ([seconds, part]: readonly [number, number]): number => seconds * 1e9 + part;
    expect(nanos(recorded.span("hello").startTime) - nanos(whole.startTime)).toBeGreaterThan(1e6);
    expect(nanos(recorded.span("hello").startTime) - nanos(whole.startTime)).toBeLessThan(3e6);
    const inside = recorded.spans().filter((span) => span.parentSpanContext?.spanId === whole.spanContext().spanId);
    expect(inside.map((span) => span.name)).toEqual(["hello", "modules", "store.open"]);
    expect(recorded.span("hello").duration).toEqual([0, 8_000_000]);
    expect(recorded.span("store.open").status).toEqual({ code: SpanStatusCode.ERROR, message: "No JSON." });
    expect(recorded.span("modules").status.code).toBe(SpanStatusCode.UNSET);
    recorded.restore();
  });

  test("go under the span that was active as the opening began", async () => {
    const recorded = recordSpans();
    const caller = tracer().startSpan("load board");
    const opening = context.with(trace.setSpan(context.active(), caller), () => steps("open"));
    caller.end();

    opening.record(Promise.resolve());
    await Promise.resolve();

    expect(recorded.span("open").parentSpanContext?.spanId).toBe(caller.spanContext().spanId);
    recorded.restore();
  });
});

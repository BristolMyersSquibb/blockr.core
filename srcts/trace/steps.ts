// The spans of starting the page and of each opening of a board, as Tracing
// has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing):
// a span of the whole, with a span inside it for each step, such as `hello`.
// Core notes when each step starts and ends, and records the spans once the
// opening is done, with those times. The page records spans only once its
// OpenTelemetry module has set its tracing up, and the module loads only once
// R has answered the first hello, whose reply leads to the blockr option that
// names the collector. So core cannot record the spans of the page's start as
// the steps run.

import { context, trace } from "@opentelemetry/api";
import type { Context } from "@opentelemetry/api";
import { failSpan, tracer } from "./trace.ts";

export interface Steps {
  // Runs `fn` as the step of the name, from now until what it returns
  // settles, and resolves with what it returns.
  step<T>(name: string, fn: () => T | Promise<T>): Promise<T>;
  // Notes a step that ran elsewhere, such as the hello that the protocol's
  // client sent, from `start` to `end`, as performance.now() gives them.
  noted(name: string, start: number, end: number): void;
  // Records the spans once `ready` settles, as once the page's tracing is set
  // up: the span of the whole from its start until now, under the context
  // given as the steps were made, with a span inside it for each step. A
  // failure marks the whole as failed, and a step that failed is marked so.
  // A later call records nothing.
  record(ready: Promise<unknown>, failure?: unknown): void;
}

interface Step {
  readonly name: string;
  readonly start: number;
  end?: number;
  error?: unknown;
}

// The steps of the span of the name, which starts at `start`, as
// performance.now() gives it, under `parent`, the context active now unless
// given.
export function steps(name: string, start = performance.now(), parent: Context = context.active()): Steps {
  const taken: Step[] = [];
  let recorded = false;

  return {
    async step(stepName, fn) {
      const step: Step = { name: stepName, start: performance.now() };
      taken.push(step);
      try {
        return await fn();
      } catch (error) {
        step.error = error;
        throw error;
      } finally {
        step.end = performance.now();
      }
    },
    noted(stepName, stepStart, end) {
      taken.push({ name: stepName, start: stepStart, end });
    },
    record(ready, failure) {
      if (recorded) return;
      recorded = true;
      const end = performance.now();
      const write = (): void => {
        const whole = tracer().startSpan(name, { startTime: start }, parent);
        const inside = trace.setSpan(parent, whole);
        for (const step of taken) {
          const span = tracer().startSpan(step.name, { startTime: step.start }, inside);
          if (step.error !== undefined) failSpan(span, step.error);
          span.end(step.end ?? end);
        }
        if (failure !== undefined) failSpan(whole, failure);
        whole.end(end);
      };
      void ready.then(write, write);
    },
  };
}

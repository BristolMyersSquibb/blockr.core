// A module whose code loads as it first renders, on a module that the test
// gives, which records what it renders and what it cleans up.

import { describe, expect, test } from "vitest";
import { loadedModule } from "./index.ts";

// What the test hands render(): an element and a signal of its own.
function args() {
  const controller = new AbortController();
  return {
    el: document.createElement("div"),
    signal: controller.signal,
    abort: () => {
      controller.abort();
    },
  };
}

// A module that writes into its element, and counts its cleanups.
function recorded() {
  let cleaned = 0;
  const module = {
    render({ el }: { el: HTMLElement; signal: AbortSignal }) {
      el.textContent = "drawn";
      return () => {
        cleaned++;
      };
    },
  };
  return { module, cleaned: () => cleaned };
}

describe("a module whose code loads as it first renders", () => {
  test("renders the module that its code holds, with the args that it gets, and returns the module's cleanup", async () => {
    const { module, cleaned } = recorded();
    const loaded = loadedModule(() => Promise.resolve(module));
    const given = args();

    const cleanup = await loaded.render(given);

    expect(given.el.textContent).toBe("drawn");
    cleanup?.();
    expect(cleaned()).toBe(1);
  });

  test("renders nothing where the signal aborts while its code loads", async () => {
    const { module } = recorded();
    let resolve: (value: typeof module) => void = () => undefined;
    const loaded = loadedModule(
      () =>
        new Promise<typeof module>((done) => {
          resolve = done;
        }),
    );
    const given = args();

    const rendering = loaded.render(given);
    given.abort();
    resolve(module);

    expect(await rendering).toBeUndefined();
    expect(given.el.textContent).toBe("");
  });

  test("rejects where its code fails to load, as a render() that fails does", async () => {
    const loaded = loadedModule(() => Promise.reject(new Error("The network is down.")));

    await expect(loaded.render(args())).rejects.toThrow("The network is down.");
  });
});

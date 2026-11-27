// Core's Mounted component, the element in which core mounts a module.
// Core mounts the module as React commits the element, or once React has
// finished its work where `deferred` is set.
// A module that draws with flushSync() as core mounts it, as core's forms do,
// can flush only then.

import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { Root } from "react-dom/client";
import { describe, expect, onTestFinished, test, vi } from "vitest";
import { step } from "@blockr/ui/stubs";
import { Mounted } from "./mounted.tsx";

// A module that draws a field in a React root of its own as core mounts it.
// Where `flush` is set, it draws with flushSync(), as drawForm() draws core's
// forms, which can flush only once React has finished its work.
// It keeps each element that it is mounted in, whether it had drawn its field
// once its mount returned, and how many times its cleanup ran.
function module({ flush = false }: { flush?: boolean } = {}) {
  const mounts: HTMLElement[] = [];
  const drawn: boolean[] = [];
  let cleanups = 0;
  const mount = (el: HTMLElement): (() => void) => {
    mounts.push(el);
    const root = createRoot(el);
    const field = <input aria-label="Name" />;
    if (flush) {
      flushSync(() => {
        root.render(field);
      });
    } else {
      root.render(field);
    }
    drawn.push(el.querySelector("input") !== null);
    return () => {
      cleanups++;
      root.unmount();
    };
  };
  return { mount, mounts, drawn, cleanups: () => cleanups };
}

// A React root in an element of its own on the page.
function page(): Root {
  const el = document.createElement("div");
  document.body.append(el);
  return createRoot(el);
}

describe("Mounted", () => {
  test("mounts the module as React commits the element, so that a component that React draws within flushSync() has mounted it once flushSync() returns", async () => {
    const m = module();
    const root = page();
    await step(() => {
      flushSync(() => {
        root.render(<Mounted mount={m.mount} />);
      });
      expect(m.mounts).toHaveLength(1);
    });
  });

  test("where deferred, mounts the module once React has finished the work in which it committed the element, so that a module that draws with flushSync() has drawn once its mount returns, and React logs no error", async () => {
    const error = vi.spyOn(console, "error");
    onTestFinished(() => {
      error.mockRestore();
    });
    const m = module({ flush: true });
    const root = page();
    await step(() => {
      flushSync(() => {
        root.render(<Mounted mount={m.mount} deferred />);
      });
      expect(m.mounts).toHaveLength(0);
    });
    expect(m.mounts).toHaveLength(1);
    expect(m.drawn).toEqual([true]);
    expect(m.mounts[0]?.querySelector("input")).not.toBeNull();
    expect(error).not.toHaveBeenCalled();
  });

  test("where deferred, mounts nothing where the component goes before React has finished its work", async () => {
    const m = module({ flush: true });
    const root = page();
    await step(() => {
      flushSync(() => {
        root.render(<Mounted mount={m.mount} deferred />);
      });
      flushSync(() => {
        root.render(null);
      });
    });
    expect(m.mounts).toHaveLength(0);
    expect(m.cleanups()).toBe(0);
  });

  test("unmounts the module once React has finished the work in which the component went, deferred or not, so that the module's own root removes what it drew", async () => {
    for (const deferred of [false, true]) {
      const m = module();
      const root = page();
      await step(() => {
        root.render(<Mounted mount={m.mount} deferred={deferred} />);
      });
      expect(m.mounts[0]?.querySelector("input")).not.toBeNull();
      await step(() => {
        flushSync(() => {
          root.render(null);
        });
        expect(m.cleanups()).toBe(0);
      });
      expect(m.cleanups()).toBe(1);
      expect(m.mounts[0]?.childNodes).toHaveLength(0);
    }
  });
});

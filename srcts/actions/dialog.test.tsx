// Core's dialog, as React draws it on the page: a modal dialog titled with the
// board action's label, at least 420 px wide, in which core renders the
// advanced action through mount(), and which closes on its Close button, on
// Escape, as the function that it returns runs and as the signal aborts,
// telling core through close() and giving the keyboard's focus back to the
// element that the user triggered from.
// Core loads the dialog's code as it first shows the dialog, so each test
// waits for the dialog to show.

import { act } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test, vi } from "vitest";
import { find, inert, openDialog, press, step } from "@blockr/ui/stubs";
import type { Delta } from "../store/index.ts";
import { waitForLoaded } from "../testing/loading.ts";
import { logged } from "../testing/logged.ts";
import { showDialog } from "./advanced.ts";
import type { PlaceArgs } from "./advanced.ts";

// What core hands its dialog for an advanced action that draws a field,
// opened from a button that has the keyboard's focus, with a close() that
// counts its calls. Where `react` is true, the advanced action draws the field
// in a React root of its own, as core's forms of a link and of a stack do.
function opened(react = false) {
  const trigger = document.createElement("button");
  trigger.textContent = "Open";
  document.body.append(trigger);
  trigger.focus();
  const mounted: HTMLElement[] = [];
  let unmounted = 0;
  let closed = 0;
  const args: PlaceArgs = {
    id: "blockr.test-edit_form_action",
    title: "Edit form",
    target: { block: "b" },
    mount(el) {
      mounted.push(el);
      if (react) {
        const root = createRoot(el);
        flushSync(() => {
          root.render(<input aria-label="Name" />);
        });
        return () => {
          unmounted++;
          root.unmount();
          el.replaceChildren();
        };
      }
      const input = document.createElement("input");
      input.setAttribute("aria-label", "Name");
      el.append(input);
      return () => {
        unmounted++;
        el.replaceChildren();
      };
    },
    close() {
      closed++;
    },
  };
  return { args, trigger, mounted, unmounted: () => unmounted, closed: () => closed };
}

// Core's dialog for an advanced action that draws a field, as opened() makes
// it, once the dialog shows.
async function drawn(signal?: AbortSignal, react = false) {
  const { args, ...made } = opened(react);
  let hide: (deltas?: readonly Delta[]) => void = () => undefined;
  await step(() => {
    hide = showDialog(args, { el: made.trigger, ...(signal === undefined ? {} : { signal }) });
  });
  hides.push(hide);
  await waitForLoaded(() => openDialog());
  await settle();
  return { ...made, hide };
}

// Waits for what React Aria scheduled, such as moving the focus.
async function settle(): Promise<void> {
  await act(() => new Promise((resolve) => setTimeout(resolve, 50)));
}

const shown = (): Element | null => document.querySelector('[role="dialog"]');

// What closes each dialog that a test drew, which closes any that the test
// left open before the page's body empties, so that React Aria keeps no focus
// in an element that has left the page.
const hides: ((deltas?: readonly Delta[]) => void)[] = [];

afterEach(async () => {
  await step(() => {
    for (const hide of hides.splice(0)) hide();
  });
});

describe("core's dialog", () => {
  test("is a modal dialog titled with the board action's label, at least 420 px wide, in which core renders the advanced action", async () => {
    const { trigger, mounted } = await drawn();

    const dialog = openDialog();
    const title = document.getElementById(dialog.getAttribute("aria-labelledby") ?? "");
    expect(title?.textContent).toBe("Edit form");
    expect(dialog.parentElement?.classList.contains("bc:min-w-[min(420px,100%)]")).toBe(true);
    expect(mounted).toHaveLength(1);
    expect(dialog.contains(mounted[0] ?? null)).toBe(true);
    expect(find(dialog, 'input[aria-label="Name"]')).toBeTruthy();
    // The modal keeps the rest of the page out of reach.
    expect(inert(trigger)).toBe(true);
  });

  test("closes on its Close button, unmounts the advanced action, tells core through close(), and gives the focus back to the element that the user triggered from", async () => {
    const { trigger, unmounted, closed } = await drawn();

    const close = [...openDialog().querySelectorAll("button")].find((button) => button.textContent.trim() === "Close");
    if (close === undefined) throw new Error("Found no Close button.");
    await step(() => {
      close.click();
    });
    await settle();

    expect(shown()).toBeNull();
    expect(unmounted()).toBe(1);
    expect(closed()).toBe(1);
    expect(document.activeElement).toBe(trigger);
  });

  test("unmounts an advanced action written in React once React has closed the dialog, so that the advanced action's root removes what it drew", async () => {
    const { mounted, unmounted, closed } = await drawn(undefined, true);
    expect(find(openDialog(), 'input[aria-label="Name"]')).toBeTruthy();

    const close = [...openDialog().querySelectorAll("button")].find((button) => button.textContent.trim() === "Close");
    await step(() => {
      close?.click();
    });
    await settle();

    expect(shown()).toBeNull();
    expect([unmounted(), closed()]).toEqual([1, 1]);
    expect(mounted[0]?.childNodes).toHaveLength(0);
  });

  test("closes on Escape, which React Aria hears where the focus is", async () => {
    const { trigger, closed } = await drawn();

    await step(() => {
      press(document.activeElement instanceof HTMLElement ? document.activeElement : openDialog(), "Escape");
    });
    await settle();

    expect(shown()).toBeNull();
    expect(closed()).toBe(1);
    expect(document.activeElement).toBe(trigger);
  });

  test("closes once as the function that it returns runs, and as the signal aborts", async () => {
    const first = await drawn();
    await step(() => {
      first.hide([]);
      first.hide();
    });
    expect(shown()).toBeNull();
    expect(first.unmounted()).toBe(1);
    expect(first.closed()).toBe(1);

    const board = new AbortController();
    const second = await drawn(board.signal);
    await step(() => {
      board.abort();
    });
    expect(shown()).toBeNull();
    expect(second.unmounted()).toBe(1);
    expect(second.closed()).toBe(1);
  });

  test("draws nothing, and tells core once that it closed, where it closes before its code has loaded", async () => {
    const { args, trigger, mounted, closed } = opened();

    await step(() => {
      showDialog(args, { el: trigger })();
    });
    await import("./dialog.tsx");
    await settle();

    expect(shown()).toBeNull();
    expect(mounted).toEqual([]);
    expect(closed()).toBe(1);
    expect(document.activeElement).toBe(trigger);
  });

  test("closes at once, telling core, and logs why, where its code fails to load", async () => {
    const log = logged();
    vi.doMock("./dialog.tsx", () => {
      throw new Error("The network is down.");
    });
    try {
      const { args, trigger, mounted, closed } = opened();
      await step(() => {
        hides.push(showDialog(args, { el: trigger }));
      });
      await waitForLoaded(() => {
        expect(closed()).toBe(1);
      });

      expect(shown()).toBeNull();
      expect(mounted).toEqual([]);
      expect(document.activeElement).toBe(trigger);
      expect(log.errors().map((line) => line.message)).toEqual([
        expect.stringContaining('Core\'s dialog failed to load, so it shows no advanced action of the board action "blockr.test-edit_form_action":'),
      ]);
    } finally {
      vi.doUnmock("./dialog.tsx");
      log.restore();
    }
  });
});

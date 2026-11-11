// The card of a block that serve() serves alone, drawn on what the test plays
// of CoreApi: a board of the head block "b", the statuses that the test gives
// the block, and mounts that write what they draw.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import type { Condition } from "../protocol/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";
import { READY } from "../stubs/index.ts";
import { find, texts } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import type { Block, Board } from "../types/board.ts";
import { drawCard, OWNER } from "./card.tsx";
import type { CardCore } from "./card.tsx";

const HEAD: Block = { registry_id: "blockr.core-head_block", state: { n: 6, direction: "head" }, name: "Head", locked: null };

function boardOf(blocks: Board["blocks"]): Board {
  return { id: "quick_wren", name: "Quick wren", blocks, links: {}, stacks: {}, options: {}, plugins: {} };
}

function condition(severity: Condition["severity"], message: string): Condition {
  return { severity, class: [`simple${severity}`, severity, "condition"], message };
}

const UNEVALUATED: BlockStatus = { ...READY, status: "unevaluated", evaluated: null, running: true };

// What the card reads of CoreApi, played: the board given, the block's
// status, which the test sets, the demands that the card makes, and what the
// mounts draw and end.
function played(board: Board = boardOf({ b: HEAD }), locked = false) {
  let status: BlockStatus | undefined = UNEVALUATED;
  const listeners = new Set<(changed: readonly string[]) => void>();
  const demands: { owner: string; blocks: string[]; released: boolean }[] = [];
  const events: string[] = [];
  const core: CardCore = {
    store: {
      board: () => board,
      subscribe: () => () => undefined,
      locked: () => locked,
    },
    scheduler: {
      demand(owner, blocks) {
        const made = { owner, blocks: typeof blocks === "function" ? [] : [...blocks], released: false };
        demands.push(made);
        return {
          set: () => undefined,
          release() {
            made.released = true;
          },
        };
      },
      status: () => status,
      subscribe(fn) {
        listeners.add(fn);
        return () => {
          listeners.delete(fn);
        };
      },
    },
    mountBlock(el, block) {
      el.textContent = `UI of ${block}`;
      events.push(`UI of ${block} mounts`);
      return () => {
        events.push(`UI of ${block} ends`);
      };
    },
    mountOutput(el, block) {
      el.textContent = `Output of ${block}`;
      events.push(`output of ${block} mounts`);
      return () => {
        events.push(`output of ${block} ends`);
      };
    },
  };
  return {
    core,
    demands,
    events,
    // Gives the block the status, as the scheduler announces a change.
    async set(next: Partial<BlockStatus>): Promise<void> {
      status = { ...READY, ...next };
      await step(() => {
        for (const fn of listeners) fn(["b"]);
      });
    },
  };
}

function draw(core: CardCore): { el: HTMLElement; controller: AbortController } {
  const el = document.createElement("div");
  document.body.append(el);
  const controller = new AbortController();
  act(() => {
    drawCard(el, core, controller.signal);
  });
  return { el, controller };
}

const footer = (el: HTMLElement): HTMLElement => find(el, "footer");
const output = (el: HTMLElement): HTMLElement => find(el, '[aria-label="Output"]');

// The output's box, whose last line is the footer.
function box(el: HTMLElement): HTMLElement {
  const found = footer(el).parentElement;
  if (found === null) throw new Error("Found no box around the footer.");
  return found;
}

// The footer's line: the summary, where it shows one, and the text of each
// pill.
const line = (el: HTMLElement): string[] => texts(footer(el), "span > span, button");

// The pill whose text is the one given.
function pill(el: HTMLElement, text: string): HTMLButtonElement {
  const found = [...footer(el).querySelectorAll("button")].find((button) => button.textContent === text);
  if (found === undefined) throw new Error(`Found no pill ${text}.`);
  return found;
}

// The severity and the message of each banner that is open, in order.
function banners(el: HTMLElement): string[][] {
  return [...el.querySelectorAll('ul[aria-label="Conditions"] li')].map((li) => [
    li.getAttribute("data-severity") ?? "",
    li.querySelector("span > span:last-child")?.textContent ?? "",
  ]);
}

async function click(button: HTMLButtonElement): Promise<void> {
  await step(() => {
    button.click();
  });
}

afterEach(() => {
  document.body.replaceChildren();
});

describe("the card of a block that serve() serves alone", () => {
  test("draws the block's name, its UI and its output, and demands the block, once drawCard() returns", () => {
    const r = played();
    const { el } = draw(r.core);

    expect(find(el, "article h1").textContent).toBe("Head");
    expect(find(el, '[aria-label="Inputs"]').textContent).toBe("UI of b");
    expect(output(el).textContent).toBe("Output of b");
    expect(r.events).toEqual(["UI of b mounts", "output of b mounts"]);
    expect(r.demands).toEqual([{ owner: OWNER, blocks: ["b"], released: false }]);
    // Nothing to show before the block first reads ready, so the output has
    // no height, and R draws nothing for it yet.
    expect(footer(el).textContent).toBe("");
    expect(output(el).className).toContain("bc:h-0");
  });

  test("mounts no UI where the board lock fixes the block", () => {
    const r = played(boardOf({ b: HEAD }), true);
    const { el } = draw(r.core);

    expect(el.querySelector('[aria-label="Inputs"]')).toBeNull();
    expect(output(el).textContent).toBe("Output of b");
  });

  test("shows a ready block's summary, and a pill that counts its warnings and one its messages, each opening its banners", async () => {
    const r = played();
    const { el } = draw(r.core);

    await r.set({ summary: "<data.frame> [50 × 5]" });
    expect(line(el)).toEqual(["<data.frame> [50 × 5]"]);
    expect(output(el).className).toContain("bc:flex-1");
    expect(output(el).dataset.current).toBe("true");

    await r.set({
      summary: "<data.frame> [0 × 5]",
      conditions: [
        condition("message", "Read 0 rows."),
        condition("warning", "NAs introduced by coercion"),
        condition("warning", "Dropped a column."),
      ],
    });
    expect(line(el)).toEqual(["<data.frame> [0 × 5]", "2 warnings", "1 message"]);
    expect(banners(el)).toEqual([]);

    const warnings = pill(el, "2 warnings");
    expect(warnings.getAttribute("aria-expanded")).toBe("false");
    expect(warnings.className).toContain("bc:text-text-warning");
    await click(warnings);
    expect(warnings.getAttribute("aria-expanded")).toBe("true");
    expect(banners(el)).toEqual([
      ["warning", "NAs introduced by coercion"],
      ["warning", "Dropped a column."],
    ]);

    // The banners of the pills that are open keep the order R raised them in.
    await click(pill(el, "1 message"));
    expect(banners(el)).toEqual([
      ["message", "Read 0 rows."],
      ["warning", "NAs introduced by coercion"],
      ["warning", "Dropped a column."],
    ]);

    await click(warnings);
    expect(banners(el)).toEqual([["message", "Read 0 rows."]]);
  });

  test("shows a failed block's pill that reads error, whose banner is open from the start with the whole message", async () => {
    const r = played();
    const { el } = draw(r.core);

    await r.set({
      status: "failed",
      conditions: [condition("warning", "NAs introduced by coercion"), condition("error", "object 'Sepal' not found\nDid you mean 'Sepal.Width'?")],
    });

    expect(line(el)).toEqual(["error", "1 warning"]);
    const error = pill(el, "error");
    expect(error.className).toContain("bc:text-text-danger");
    expect(error.getAttribute("aria-expanded")).toBe("true");
    expect(banners(el)).toEqual([["error", "object 'Sepal' not found\nDid you mean 'Sepal.Width'?"]]);
    expect(output(el).dataset.current).toBe("false");

    // A pill keeps its state as R evaluates the block again.
    await click(error);
    expect(banners(el)).toEqual([]);
    await r.set({ status: "failed", revision: 2, evaluated: 2, conditions: [condition("error", "object 'Petal' not found")] });
    expect(banners(el)).toEqual([]);
    await click(pill(el, "error"));
    expect(banners(el)).toEqual([["error", "object 'Petal' not found"]]);
  });

  test("shows why a waiting block waits, and an invalid block's reason as its error", async () => {
    const r = played();
    const { el } = draw(r.core);

    await r.set({
      status: "waiting",
      evaluated: null,
      conditions: [{ ...condition("message", 'This block is waiting for its input "data" to be linked.'), phase: "status" }],
    });
    expect(footer(el).textContent).toBe('This block is waiting for its input "data" to be linked.');
    expect(footer(el).querySelector("button")).toBeNull();
    expect(footer(el).className).not.toContain("bc:opacity-50");
    expect(banners(el)).toEqual([]);

    await r.set({ status: "invalid", conditions: [condition("error", "Save to write the file.")] });
    expect(line(el)).toEqual(["error"]);
    expect(banners(el)).toEqual([["error", "Save to write the file."]]);
  });

  test("dims the footer and the output while R evaluates the block, and keeps what they show", async () => {
    const r = played();
    const { el } = draw(r.core);

    await r.set({ summary: "<data.frame> [6 × 5]", conditions: [condition("warning", "Dropped a column.")] });
    expect(footer(el).className).not.toContain("bc:opacity-50");
    expect(output(el).className).not.toContain("bc:opacity-50");

    // The user changed the block's state, and R evaluates its next revision.
    await r.set({ status: "stale", revision: 2, running: true, summary: "<data.frame> [6 × 5]" });
    expect(line(el)).toEqual(["<data.frame> [6 × 5]", "1 warning"]);
    expect(footer(el).className).toContain("bc:opacity-50");
    expect(footer(el).getAttribute("aria-busy")).toBe("true");
    expect(output(el).className).toContain("bc:opacity-50");
    expect(output(el).textContent).toBe("Output of b");

    await r.set({ revision: 2, evaluated: 2, summary: "<data.frame> [3 × 5]" });
    expect(line(el)).toEqual(["<data.frame> [3 × 5]"]);
    expect(footer(el).className).not.toContain("bc:opacity-50");
    expect(output(el).dataset.current).toBe("true");
  });

  test("gives the output a height of its own from the first time the block reads ready on, and keeps it", async () => {
    const r = played();
    const { el } = draw(r.core);

    // A block that fails or reads invalid at once has nothing to draw, so the
    // card shows its footer and its banners alone.
    await r.set({ status: "failed", conditions: [condition("error", "object 'Sepal' not found")] });
    expect(output(el).className).toContain("bc:h-0");
    expect(box(el).className).not.toContain("bc:h-96");
    expect(line(el)).toEqual(["error"]);
    expect(banners(el)).toEqual([["error", "object 'Sepal' not found"]]);
    await r.set({ status: "invalid", revision: 2, evaluated: 2, conditions: [condition("error", "Save to write the file.")] });
    expect(output(el).className).toContain("bc:h-0");

    await r.set({ revision: 3, evaluated: 3, summary: "<data.frame> [6 × 5]" });
    expect(output(el).className).toContain("bc:flex-1");
    expect(box(el).className).toContain("bc:h-96");

    // Core keeps drawing the last output as the block fails or waits, so the
    // output keeps its height, greyed out.
    await r.set({ status: "failed", revision: 4, evaluated: 4, conditions: [condition("error", "object 'Petal' not found")] });
    expect(output(el).className).toContain("bc:flex-1");
    expect(output(el).className).toContain("bc:grayscale");
    await r.set({
      status: "waiting",
      revision: 5,
      evaluated: 4,
      conditions: [{ ...condition("message", 'This block is waiting for its input "data" to be linked.'), phase: "status" }],
    });
    expect(output(el).className).toContain("bc:flex-1");
    expect(box(el).className).toContain("bc:h-96");
  });

  test("ends the card and its mounts, and releases its demand, as the signal aborts", async () => {
    const r = played();
    const { el, controller } = draw(r.core);

    await step(() => {
      controller.abort();
    });
    await Promise.resolve();

    expect(r.demands[0]?.released).toBe(true);
    expect(r.events).toEqual(["UI of b mounts", "output of b mounts", "UI of b ends", "output of b ends"]);
    expect(el.textContent).toBe("Loading the board…");
  });

  test("says so in the element where the board holds no block or several", () => {
    const boards: Board["blocks"][] = [{}, { a: HEAD, b: HEAD }];
    for (const blocks of boards) {
      const r = played(boardOf(blocks));
      const { el } = draw(r.core);

      expect(find(el, '[role="alert"]').textContent).toContain("A block that serve() serves alone comes as a board of one block");
      expect(r.demands).toEqual([]);
    }
  });
});

// The parts of core's basic frontend that need no page: where it draws each
// block, what it demands, the severities that show_conditions names, and what
// it tells the user of what the store dropped.

import { describe, expect, test } from "vitest";
import type { Stack } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { asSeverities } from "./board.tsx";
import { demandShown, IN_VIEW, OWNER, SHOWN } from "./demand.ts";
import type { View } from "./demand.ts";
import { droppedNote } from "./dropped.ts";
import { itemsOf } from "./layout.ts";

describe("where each block goes", () => {
  const block = { registry_id: "blockr.core-head_block", state: {}, name: "Head", locked: null };
  const stack = (blocks: string[]): Stack => ({ name: "Stack", blocks, color: null });

  test("is the board's order, each stack at the place of its first block, with its blocks in its own order", () => {
    const blocks = { a: block, b: block, c: block, d: block };
    expect(itemsOf({ blocks, stacks: { s1: stack(["d", "b"]) } })).toEqual([
      { block: "a" },
      { stack: "s1", blocks: ["d", "b"] },
      { block: "c" },
    ]);
  });

  test("puts a stack without blocks last", () => {
    expect(itemsOf({ blocks: { a: block }, stacks: { empty: stack([]), s1: stack(["a"]) } })).toEqual([
      { stack: "s1", blocks: ["a"] },
      { stack: "empty", blocks: [] },
    ]);
  });
});

// A scheduler that keeps each demand with its blocks and evaluation priority,
// and refuses to set a demand that was released, as core's does.
function played() {
  const demands: { owner: string; priority: number | undefined; blocks: unknown; released: boolean }[] = [];
  const scheduler = {
    demand(owner: string, blocks: unknown, options?: { priority?: number }) {
      const demand = { owner, priority: options?.priority, blocks, released: false };
      demands.push(demand);
      return {
        set(next: unknown) {
          if (demand.released) throw new Error(`The demand of ${owner} was released, so it can no longer be set.`);
          demand.blocks = next;
        },
        release() {
          demand.released = true;
        },
      };
    },
  };
  return { core: { scheduler } as unknown as Pick<CoreApi, "scheduler">, demands };
}

// A page of 800 by 600 pixels whose observer the test plays.
function page() {
  let report: IntersectionObserverCallback = () => undefined;
  const observed = new Set<Element>();
  let disconnected = false;
  class Observer {
    constructor(callback: IntersectionObserverCallback) {
      report = callback;
    }
    observe(el: Element) {
      observed.add(el);
    }
    unobserve(el: Element) {
      observed.delete(el);
    }
    disconnect() {
      disconnected = true;
    }
  }
  const view: View = { innerWidth: 800, innerHeight: 600, IntersectionObserver: Observer as unknown as typeof IntersectionObserver };
  return {
    view,
    observed,
    disconnected: () => disconnected,
    report(entries: { target: Element; isIntersecting: boolean }[]) {
      report(entries as IntersectionObserverEntry[], {} as IntersectionObserver);
    },
  };
}

// An element that lays out at the rectangle given.
function laidOut(top: number, height = 100): HTMLElement {
  const el = document.createElement("article");
  el.getBoundingClientRect = () => ({ top, bottom: top + height, left: 0, right: 400, width: 400, height, x: 0, y: top, toJSON: () => ({}) });
  return el;
}

describe("what the frontend demands", () => {
  test("is every block above the default evaluation priority, and the blocks in view higher still", () => {
    const { core, demands } = played();
    const { view } = page();
    demandShown(core, view);

    expect(demands.map(({ owner, priority }) => [owner, priority])).toEqual([
      [OWNER, SHOWN],
      [OWNER, IN_VIEW],
    ]);
    expect(SHOWN).toBeGreaterThan(0);
    expect(IN_VIEW).toBeGreaterThan(SHOWN);
    const every = demands[0]?.blocks as (board: { blocks: object }) => string[];
    expect(every({ blocks: { a: {}, b: {} } })).toEqual(["a", "b"]);
    expect(demands[1]?.blocks).toEqual([]);
  });

  test("of the blocks in view is measured as the page lays them out, and follows what the page reports", () => {
    const { core, demands } = played();
    const p = page();
    const shown = demandShown(core, p.view);
    const [a, b, c] = [laidOut(0), laidOut(400), laidOut(900)];
    shown.watch("a", a);
    shown.watch("b", b);
    const unwatch = shown.watch("c", c);

    shown.measure();
    expect(demands[1]?.blocks).toEqual(["a", "b"]);

    p.report([
      { target: a, isIntersecting: false },
      { target: c, isIntersecting: true },
    ]);
    expect(demands[1]?.blocks).toEqual(["b", "c"]);

    unwatch();
    expect(demands[1]?.blocks).toEqual(["b"]);
    expect(p.observed.has(c)).toBe(false);

    const unwatchD = shown.watch("d", laidOut(200));
    shown.measure();
    expect(demands[1]?.blocks).toEqual(["a", "b", "d"]);

    shown.release();
    expect(demands.map(({ released }) => released)).toEqual([true, true]);
    expect(p.disconnected()).toBe(true);

    // A card in view that goes after the release sets nothing, as the cards
    // go once the frontend has ended.
    unwatchD();
    shown.measure();
    expect(demands[1]?.blocks).toEqual(["a", "b", "d"]);
  });

  test("of the blocks in view is measured alone where the page reports nothing", () => {
    const { core, demands } = played();
    const shown = demandShown(core, { innerWidth: 800, innerHeight: 600 });
    shown.watch("a", laidOut(100));
    shown.watch("b", laidOut(700));
    shown.measure();
    expect(demands[1]?.blocks).toEqual(["a"]);
  });
});

describe("the severities that show_conditions names", () => {
  test("are the board option's value, and a value that is no array of severities fails", () => {
    expect(asSeverities(["warning", "error"])).toEqual(["warning", "error"]);
    expect(asSeverities([])).toEqual([]);
    for (const value of [undefined, null, "error", ["fatal"]]) {
      expect(() => asSeverities(value)).toThrow(
        `The board option show_conditions is ${JSON.stringify(value)}, where the board takes an array of "message", "warning" and "error".`,
      );
    }
  });
});

describe("what the frontend tells of what the store dropped", () => {
  test("is nothing where it dropped nothing, and names a class alone where no package defines it", () => {
    expect(droppedNote([])).toBeUndefined();
    expect(
      droppedNote([
        { block: "x", registry_id: "filter_block", package: null },
        { plugin: "p", class: "notes_plugin", package: null },
      ]),
    ).toEqual({
      message: 'The board opened without what the app cannot rebuild:\n- the block "x" (filter_block)\n- the plugin "p" (notes_plugin)',
      severity: "warning",
    });
  });
});

// The parts of core's basic frontend that need no page: where it draws each
// block, what it demands and has on screen, and how a menu shows a shortcut.

import { describe, expect, test } from "vitest";
import type { Stack } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { onMac, shortcutLabel } from "../../util/index.ts";
import { demandShown, IN_VIEW, OWNER, SHOWN } from "./demand.ts";
import type { View } from "./demand.ts";
import { layoutItems } from "./layout.ts";

describe("where each block goes", () => {
  const block = { registry_id: "blockr.core-head_block", state: {}, name: "Head", locked: null };
  const stack = (blocks: string[]): Stack => ({ name: "Stack", blocks, color: null });

  test("is the board's order, each stack at the place of its first block, with its blocks in its own order", () => {
    const blocks = { a: block, b: block, c: block, d: block };
    expect(layoutItems({ blocks, stacks: { s1: stack(["d", "b"]) } })).toEqual([
      { block: "a" },
      { stack: "s1", blocks: ["d", "b"] },
      { block: "c" },
    ]);
  });

  test("puts a stack without blocks last", () => {
    expect(layoutItems({ blocks: { a: block }, stacks: { empty: stack([]), s1: stack(["a"]) } })).toEqual([
      { stack: "s1", blocks: ["a"] },
      { stack: "empty", blocks: [] },
    ]);
  });
});

// A scheduler that keeps each demand with its blocks and evaluation priority,
// and refuses to set a demand that was released, as core's does, and what the
// frontend sets as on screen, each time.
function played() {
  const demands: { owner: string; priority: number | undefined; blocks: unknown; released: boolean }[] = [];
  const screens: unknown[] = [];
  const onScreen = {
    set(shown: unknown) {
      screens.push(shown);
    },
  };
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
  return { core: { scheduler, onScreen } as unknown as Pick<CoreApi, "scheduler" | "onScreen">, demands, screens };
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
    shown.watch({ block: "a" }, a);
    shown.watch({ block: "b" }, b);
    const unwatch = shown.watch({ block: "c" }, c);

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

    const unwatchD = shown.watch({ block: "d" }, laidOut(200));
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
    shown.watch({ block: "a" }, laidOut(100));
    shown.watch({ block: "b" }, laidOut(700));
    shown.measure();
    expect(demands[1]?.blocks).toEqual(["a"]);
  });
});

describe("what the frontend has on screen", () => {
  test("is the blocks whose cards and the plugins whose sections are in view, and only the blocks are demanded", () => {
    const { core, demands, screens } = played();
    const p = page();
    const shown = demandShown(core, p.view);
    const a = laidOut(0);
    const md = laidOut(100);
    const b = laidOut(700);
    shown.watch({ block: "a" }, a);
    shown.watch({ plugin: "blockr.md-md_plugin" }, md);
    shown.watch({ block: "b" }, b);

    shown.measure();
    expect(screens.at(-1)).toEqual({ blocks: ["a"], plugins: ["blockr.md-md_plugin"] });
    expect(demands[1]?.blocks).toEqual(["a"]);

    p.report([
      { target: md, isIntersecting: false },
      { target: b, isIntersecting: true },
    ]);
    expect(screens.at(-1)).toEqual({ blocks: ["a", "b"], plugins: [] });
    expect(demands[1]?.blocks).toEqual(["a", "b"]);
  });

  test("finds the element that a block or a plugin is watched by", () => {
    const { core } = played();
    const shown = demandShown(core, page().view);
    const a = laidOut(0);
    const md = laidOut(100);
    shown.watch({ block: "a" }, a);
    const unwatch = shown.watch({ plugin: "blockr.md-md_plugin" }, md);

    expect(shown.element({ block: "a" })).toBe(a);
    expect(shown.element({ plugin: "blockr.md-md_plugin" })).toBe(md);
    expect(shown.element({ block: "blockr.md-md_plugin" })).toBeUndefined();
    unwatch();
    expect(shown.element({ plugin: "blockr.md-md_plugin" })).toBeUndefined();
  });
});

describe("a shortcut as a menu shows it", () => {
  test("is glyphs run together on a Mac, and words joined by + elsewhere, with Mod as Command or Ctrl", () => {
    const shown = (text: string) => [shortcutLabel(text, false), shortcutLabel(text, true)];
    expect(shown("Mod+s")).toEqual(["Ctrl+S", "⌘S"]);
    expect(shown("Mod+Shift+Z")).toEqual(["Ctrl+Shift+Z", "⌘⇧Z"]);
    expect(shown("Ctrl+Alt+Backspace")).toEqual(["Ctrl+Alt+Backspace", "⌃⌥Backspace"]);
    expect(shown("Meta+K")).toEqual(["Meta+K", "⌘K"]);
  });

  test("is nothing for a shortcut that the frontend cannot read, or none", () => {
    expect([shortcutLabel("Mod+Ctrl+S", false), shortcutLabel("Hyper+S", true), shortcutLabel(undefined, false)]).toEqual([
      undefined,
      undefined,
      undefined,
    ]);
  });

  test("takes the page for one on a Mac by the platform that the browser names, its client hints first", () => {
    const page = (navigator: object) => ({ navigator }) as unknown as Window;
    expect(onMac(page({ platform: "MacIntel" }))).toBe(true);
    expect(onMac(page({ platform: "Linux x86_64" }))).toBe(false);
    expect(onMac(page({ platform: "MacIntel", userAgentData: { platform: "Windows" } }))).toBe(false);
  });
});

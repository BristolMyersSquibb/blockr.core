// What core measures of an output's element for the colour that R draws a plot
// on: the background behind the element as the page shows it, which blends a
// translucent background over those behind it.

import { afterEach, describe, expect, test, vi } from "vitest";
import { domMeasure, hexOf } from "./measure.ts";

// An element that lays out at 100 by 50 inside elements with the backgrounds
// given, from the outermost in, the last of which is its own.
function inside(...backgrounds: string[]): HTMLElement {
  let el = document.body;
  for (const background of backgrounds) {
    const child = document.createElement("div");
    child.style.backgroundColor = background;
    el.append(child);
    el = child;
  }
  el.getBoundingClientRect = () => ({ top: 0, bottom: 50, left: 0, right: 100, width: 100, height: 50, x: 0, y: 0, toJSON: () => ({}) });
  return el;
}

function bg(el: HTMLElement): string | undefined {
  return domMeasure().info(el)?.bg;
}

// Has the browser draw the colour given behind the page, as Chromium resolves
// the system colour Canvas under a dark theme, which happy-dom drops. Core
// reads that colour from an element that it adds to the page's root.
function behindPage(colour: string): void {
  const computed = getComputedStyle;
  vi.stubGlobal("getComputedStyle", (at: Element) =>
    at.parentElement === document.documentElement && at !== document.head && at !== document.body
      ? ({ backgroundColor: colour } as CSSStyleDeclaration)
      : computed(at),
  );
}

afterEach(() => {
  document.body.replaceChildren();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

describe("the colour behind an output", () => {
  test("is the nearest opaque background, past transparent ones", () => {
    expect(bg(inside("rgb(255, 0, 0)", "rgb(0, 0, 255)", "", "rgba(0, 0, 0, 0)", "transparent"))).toBe("#0000ff");
  });

  test("blends a translucent background over the opaque one behind it", () => {
    expect(bg(inside("rgb(0, 0, 255)", "rgba(255, 0, 0, 0.2)"))).toBe("#3300cc");
  });

  test("blends each translucent background over those behind it", () => {
    expect(bg(inside("rgb(255, 255, 255)", "rgba(0, 0, 0, 0.5)", "rgb(255 0 0 / 50%)"))).toBe("#bf4040");
  });

  test("blends over the colour behind the page where no background is opaque, as a dark theme makes it", () => {
    behindPage("rgb(18, 18, 18)");

    expect(bg(inside("rgba(255, 255, 255, 0.2)"))).toBe("#414141");
    expect([...document.documentElement.children]).toEqual([document.head, document.body]);
  });

  test("takes white for the colour behind the page where it cannot read it", () => {
    expect(bg(inside("rgba(0, 0, 0, 0.5)"))).toBe("#808080");
  });

  test("keeps the alpha of a background that it reads by drawing it", () => {
    // Core's subtle background on a card under the text colour gray-900, as
    // Chrome computes it, and the pixel that Chrome reads back from a canvas
    // drawn with it.
    const subtle = "oklab(0.21 -0.00316127 -0.0338527 / 0.05)";
    const el = inside("", "");
    const card = el.parentElement;
    const computed = getComputedStyle;
    vi.stubGlobal("getComputedStyle", (at: Element) => (at === card ? ({ backgroundColor: subtle } as CSSStyleDeclaration) : computed(at)));
    const context = { fillStyle: "", fillRect: () => undefined, getImageData: () => ({ data: Uint8ClampedArray.of(20, 20, 39, 13) }) };
    vi.spyOn(HTMLCanvasElement.prototype, "getContext").mockReturnValue(context as unknown as CanvasRenderingContext2D);

    expect(bg(el)).toBe("#f3f3f4");
    expect(context.fillStyle).toBe(subtle);
  });
});

describe("hexOf()", () => {
  test("reads #rgb, #rrggbb, rgb() and rgba() without drawing them, and drops their alpha", () => {
    const colours = ["#AbC", "#aabbcc", "rgb(170, 187, 204)", "rgba(170, 187, 204, 0.5)", "rgb(170 187 204 / 50%)"];
    expect(colours.map(hexOf)).toEqual(colours.map(() => "#aabbcc"));
  });

  test("is null for an empty colour, and for one in another notation where the page cannot draw", () => {
    expect(["", "oklch(0.5 0.1 200)"].map(hexOf)).toEqual([null, null]);
  });
});

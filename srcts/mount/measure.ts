// What core measures of an output's element for the output's `info`: its size
// in CSS pixels, the display's pixel ratio, its colours and its font, so that a
// plot is drawn again at a new size or on a display of another density, and a
// switch between light and dark redraws every plot on screen
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// An element that has left the page measures 0 by 0, and core asks R for no
// output while it measures nothing.

import type { OutputInfo } from "../protocol/index.ts";

export interface Measure {
  // What the element measures, or null where it measures 0 by 0.
  info(el: HTMLElement): OutputInfo | null;
  // Calls `fn` whenever what the element measures may have changed: its size,
  // the display's pixel ratio or the page's theme. It returns what stops it.
  watch(el: HTMLElement, fn: () => void): () => void;
}

// The colours that the element takes where the page gives it none, as on a
// page without a stylesheet: black text on white, and as the accent the blue
// that core's theme takes as its accent, Tailwind's blue-600.
const FALLBACK = { bg: "#ffffff", fg: "#000000", accent: "#155dfc" } as const;

// The variable of core's theme that holds its accent, which a front-end or a
// UI kit may set to restyle core.
const ACCENT = "--bc-color-bg-accent";

// Measures elements on the page, through the DOM.
export function domMeasure(): Measure {
  return {
    info(el) {
      const rect = el.getBoundingClientRect();
      const width = Math.round(rect.width);
      const height = Math.round(rect.height);
      if (width <= 0 || height <= 0) return null;
      const style = getComputedStyle(el);
      return {
        width,
        height,
        pixel_ratio: window.devicePixelRatio > 0 ? window.devicePixelRatio : 1,
        bg: backgroundOf(el) ?? FALLBACK.bg,
        fg: hexOf(style.color) ?? FALLBACK.fg,
        accent: hexOf(style.getPropertyValue(ACCENT).trim()) ?? FALLBACK.accent,
        font: style.fontFamily,
      };
    },
    watch(el, fn) {
      const stops: (() => void)[] = [];

      if (typeof ResizeObserver === "function") {
        const resized = new ResizeObserver(() => {
          fn();
        });
        resized.observe(el);
        stops.push(() => {
          resized.disconnect();
        });
      }

      // A theme switches through the attributes of the page's root element,
      // as the board option dark_mode sets data-bs-theme.
      const themed = new MutationObserver(() => {
        fn();
      });
      themed.observe(document.documentElement, {
        attributes: true,
        attributeFilter: ["data-bs-theme", "class", "style"],
      });
      stops.push(() => {
        themed.disconnect();
      });

      stops.push(watchRatio(fn));

      return () => {
        for (const stop of stops) stop();
      };
    },
  };
}

// Calls `fn` whenever the display's pixel ratio changes, as when the window
// moves to another display or the page zooms, for which the media query of the
// ratio it had stops matching.
function watchRatio(fn: () => void): () => void {
  if (typeof matchMedia !== "function") return () => undefined;
  let query: MediaQueryList | undefined;
  const changed = (): void => {
    listen();
    fn();
  };
  function listen(): void {
    query?.removeEventListener("change", changed);
    query = matchMedia(`(resolution: ${String(window.devicePixelRatio)}dppx)`);
    query.addEventListener("change", changed);
  }
  listen();
  return () => {
    query?.removeEventListener("change", changed);
  };
}

// A colour as its red, green and blue, from 0 to 255, and its alpha, from 0
// to 1.
type Rgba = readonly [number, number, number, number];

// The colour behind the page where core cannot read it, as in a test runner:
// white, as in FALLBACK.
const WHITE: Rgba = [255, 255, 255, 1];

// The colour behind the element as the page shows it. It blends the element's
// own background over those of the elements around it, down to the nearest
// opaque one. Where none is opaque, it blends them over the colour behind the
// page. It is null where a background is a colour that core cannot read.
function backgroundOf(el: HTMLElement): string | null {
  const layers: Rgba[] = [];
  for (let at: Element | null = el; at !== null; at = at.parentElement) {
    const value = getComputedStyle(at).backgroundColor;
    // A test runner gives an element without a background an empty one.
    if (value === "") continue;
    const colour = rgbaOf(value);
    if (colour === null) return null;
    if (colour[3] >= 1) return hex(layers.reduceRight(over, colour));
    if (colour[3] > 0) layers.push(colour);
  }
  return hex(layers.reduceRight(over, behindPage()));
}

// The colour that the browser draws behind the page, and so behind every
// element. It is the system colour Canvas in the colour scheme of the page's
// root, so a dark theme makes it dark. Core reads it from an element that it
// adds to the root for the moment.
function behindPage(): Rgba {
  const probe = document.createElement("div");
  probe.style.cssText = "display: none; background-color: Canvas";
  document.documentElement.append(probe);
  const colour = rgbaOf(getComputedStyle(probe).backgroundColor);
  probe.remove();
  return colour !== null && colour[3] >= 1 ? colour : WHITE;
}

// The colour that a layer shows over an opaque colour behind it, blended in
// sRGB, as the browser blends them.
function over(behind: Rgba, [r, g, b, alpha]: Rgba): Rgba {
  const mix = (front: number, back: number): number => front * alpha + back * (1 - alpha);
  return [mix(r, behind[0]), mix(g, behind[1]), mix(b, behind[2]), 1];
}

// A CSS colour as R reads one, as #rrggbb, or null where it is none that core
// can read. It drops the alpha of a translucent colour.
export function hexOf(colour: string): string | null {
  const rgba = rgbaOf(colour);
  return rgba === null ? null : hex(rgba);
}

// The notation rgb() and rgba(), with commas or spaces between the channels,
// and an alpha, if any, after a comma or a slash, as a number or a percentage.
const RGB = /^rgba?\(\s*(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)(?:\s*[,/]\s*(\d+(?:\.\d+)?)(%?))?\s*\)$/;

// A CSS colour as its channels, or null where it is none that core can read.
// A colour in another notation, such as oklch(), which Tailwind's theme holds,
// is converted by drawing it, where the page can draw.
function rgbaOf(colour: string): Rgba | null {
  const text = colour.trim();
  if (text === "") return null;
  if (text === "transparent") return [0, 0, 0, 0];
  // The notation #rgb stands for #rrggbb.
  const long = /^#[0-9a-f]{3}$/i.test(text) ? text.replace(/[0-9a-f]/gi, "$&$&") : text;
  if (/^#[0-9a-f]{6}$/i.test(long)) {
    const [r = 0, g = 0, b = 0] = [1, 3, 5].map((at) => parseInt(long.slice(at, at + 2), 16));
    return [r, g, b, 1];
  }
  const rgb = RGB.exec(text);
  if (rgb !== null) {
    const [, r = "0", g = "0", b = "0", alpha = "1", percent] = rgb;
    return [Number(r), Number(g), Number(b), Math.min(1, Number(alpha) / (percent === "%" ? 100 : 1))];
  }
  return drawn(text);
}

// The red, green and blue of a colour as #rrggbb.
function hex([r, g, b]: Rgba): string {
  return `#${[r, g, b].map((c) => Math.round(Math.min(255, Math.max(0, c))).toString(16).padStart(2, "0")).join("")}`;
}

// The colour as the browser draws it into a canvas, in sRGB, or null where the
// page cannot draw, as in a test runner.
function drawn(colour: string): Rgba | null {
  try {
    const context = document.createElement("canvas").getContext("2d", { willReadFrequently: true });
    if (context === null) return null;
    context.fillStyle = "#000000";
    context.fillStyle = colour;
    context.fillRect(0, 0, 1, 1);
    const [r = 0, g = 0, b = 0, alpha = 0] = context.getImageData(0, 0, 1, 1).data;
    return [r, g, b, alpha / 255];
  } catch {
    return null;
  }
}

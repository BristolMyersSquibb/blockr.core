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

// The colour behind the element: its own background or that of the nearest
// element around it whose background is not transparent.
function backgroundOf(el: HTMLElement): string | null {
  for (let at: Element | null = el; at !== null; at = at.parentElement) {
    const colour = getComputedStyle(at).backgroundColor;
    if (!transparent(colour)) return hexOf(colour);
  }
  return null;
}

function transparent(colour: string): boolean {
  return colour === "" || colour === "transparent" || /^rgba\(.*,\s*0\)$/.test(colour);
}

// A CSS colour as R reads one, as #rrggbb, or null where it is none that core
// can read. A colour in another notation, such as oklch(), which Tailwind's
// theme holds, is converted by drawing it, where the page can draw.
export function hexOf(colour: string): string | null {
  const text = colour.trim();
  if (/^#[0-9a-f]{6}$/i.test(text)) return text.toLowerCase();
  if (/^#[0-9a-f]{3}$/i.test(text)) {
    const [r, g, b] = [text.charAt(1), text.charAt(2), text.charAt(3)];
    return `#${r}${r}${g}${g}${b}${b}`.toLowerCase();
  }
  const rgb = /^rgba?\(\s*(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)/.exec(text);
  if (rgb !== null) return hex(rgb.slice(1, 4).map(Number));
  return text === "" ? null : drawn(text);
}

function hex(channels: readonly number[]): string {
  return `#${channels.map((c) => Math.round(Math.min(255, Math.max(0, c))).toString(16).padStart(2, "0")).join("")}`;
}

// The colour as the browser draws it into a canvas, in sRGB, or null where the
// page cannot draw, as in a test runner.
function drawn(colour: string): string | null {
  try {
    const context = document.createElement("canvas").getContext("2d", { willReadFrequently: true });
    if (context === null) return null;
    context.fillStyle = "#000000";
    context.fillStyle = colour;
    context.fillRect(0, 0, 1, 1);
    const [r = 0, g = 0, b = 0] = context.getImageData(0, 0, 1, 1).data;
    return hex([r, g, b]);
  } catch {
    return null;
  }
}

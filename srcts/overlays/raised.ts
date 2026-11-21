// What rises above the page, such as a menu, a dialog or a toast, draws on
// bg-raised, a step lighter than the colour behind the element that it rises
// from, in that colour's hue (../runtime/index.css).
// So it fits whatever frontend draws the page, with no palette that frontends
// share: a menu that opens over a blue-grey card is blue-grey, and one that
// opens over a neutral grey card is neutral.
// CSS cannot read the colour behind an element, so core measures it, as it
// measures the colour behind an output (../mount/measure.ts), and sets it on
// the raised element as --bc-color-behind.
// It finds the element that the raised element rises from as the raised
// element mounts, and measures the colour behind it then and again as the
// board option dark_mode switches the page's theme, which restyles what lies
// below.
// It keeps the element that it found, since a modal makes the rest of the page
// inert, which the browser then finds at no point of the window.
// An element rises from the element that opened it, as a popover rises from
// its trigger, from the middle of the window, as a modal does, or from its
// parent, as a toast does.

import { useCallback, useRef } from "react";
import type { RefObject } from "react";
import { visibleBackground } from "../mount/index.ts";

export type RaisedFrom = RefObject<Element | null> | "parent" | "middle";

// A ref for the raised element, which measures the colour behind the element
// that it rises from while it is mounted.
export function useRaised(from: RaisedFrom): (el: HTMLElement | null) => void {
  const stop = useRef<(() => void) | null>(null);
  return useCallback(
    (el: HTMLElement | null) => {
      stop.current?.();
      stop.current = null;
      if (el === null) return;
      const below = origin(el, from);
      const measure = (): void => {
        const colour = below?.isConnected === true ? visibleBackground(below) : null;
        if (colour === null) el.style.removeProperty(BEHIND);
        else el.style.setProperty(BEHIND, colour);
      };
      measure();
      const observer = new MutationObserver(measure);
      observer.observe(document.documentElement, { attributes: true, attributeFilter: ["data-bc-theme"] });
      stop.current = () => {
        observer.disconnect();
      };
    },
    [from],
  );
}

const BEHIND = "--bc-color-behind";

// What marks the root of an overlay of core's, a popover's or a modal's, which
// a modal looks through as it looks for the page in the middle of the window.
const OVERLAY = "[data-bc-overlay]";

// The element that `el` rises from.
function origin(el: HTMLElement, from: RaisedFrom): HTMLElement | null {
  if (from === "parent") return el.parentElement;
  if (from === "middle") return middle();
  const at = from.current;
  return at instanceof HTMLElement ? at : null;
}

// The element of the page in the middle of the window, below every overlay of
// core's, or the page's body where the page cannot tell, as in a test runner.
function middle(): HTMLElement | null {
  const stack = typeof document.elementsFromPoint === "function" ? document.elementsFromPoint(window.innerWidth / 2, window.innerHeight / 2) : [];
  const page = stack.find((at) => at.closest(OVERLAY) === null);
  return page instanceof HTMLElement ? page : document.body;
}

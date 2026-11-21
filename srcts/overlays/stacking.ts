// Where the overlays of core's React SDK stack: at the z-index of the
// overlays of core's theme, --bc-z-index-overlay (../runtime/index.css).
// React Aria writes a z-index of its own on a popover and a tooltip, in their
// style, which the SDK's popovers, its tooltip and the select's list take in
// place of it.
// It lives with the overlays rather than with the classes of ../styles, which
// core's controls for modules without React ship too, since it reads React's
// types.

import type { CSSProperties } from "react";
import { composeRenderProps } from "../aria/index.ts";

export const OVERLAY_Z_INDEX = "var(--bc-z-index-overlay)";

// The style of an overlay of React Aria, which React Aria merges over its own,
// with the overlays' z-index in place of any that it holds.
export function overlayStyle<V>(
  style: CSSProperties | ((values: V) => CSSProperties | undefined) | undefined,
): (values: V) => CSSProperties | undefined {
  return composeRenderProps(style, (given: CSSProperties | undefined) => ({ ...given, zIndex: OVERLAY_Z_INDEX }));
}

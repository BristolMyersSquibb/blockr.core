import type { CSSProperties } from "react";
export declare const OVERLAY_Z_INDEX = "var(--bc-z-index-overlay)";
export declare function overlayStyle<V>(style: CSSProperties | ((values: V) => CSSProperties | undefined) | undefined): (values: V) => CSSProperties | undefined;

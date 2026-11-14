import type { FrontEnd } from "./context.ts";
export declare function bindShortcuts(doc: Document, front: Pick<FrontEnd, "core" | "plugin">): () => void;
export declare function matchesShortcut(text: string, event: KeyboardEvent): boolean;
export declare function ariaShortcuts(text: string | undefined): string | undefined;

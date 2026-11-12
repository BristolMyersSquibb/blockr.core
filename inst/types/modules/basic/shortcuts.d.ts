import type { CoreApi } from "../../types/core-api.ts";
export declare function bindShortcuts(doc: Document, core: CoreApi, failed: (error: unknown) => void): () => void;
export declare function matchesShortcut(text: string, event: KeyboardEvent): boolean;
export declare function ariaShortcuts(text: string | undefined): string | undefined;

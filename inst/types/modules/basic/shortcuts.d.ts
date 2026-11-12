import type { CoreApi } from "../../types/core-api.ts";
export declare function bindShortcuts(doc: Document, core: Pick<CoreApi, "store">, refused: (error: unknown) => void): () => void;

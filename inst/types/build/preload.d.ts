import type { Metafile } from "esbuild";
export declare function staticImports(metafiles: readonly (Metafile | undefined)[]): Record<string, string[]>;

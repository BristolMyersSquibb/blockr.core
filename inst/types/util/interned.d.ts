import type { Id } from "../types/board.ts";
export declare function interned<P extends string | null>(pkg: P, id: string): Id<P>;

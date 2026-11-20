import type { RefObject } from "react";
export interface Items {
    add(value: string, element: () => Element | null): () => void;
}
export declare const ItemsContext: import("react").Context<Items | null>;
export declare function useItems(): {
    readonly known: readonly string[] | null;
    readonly items: Items;
};
export declare function useItem(value: string | undefined, ref: RefObject<Element | null>): void;
export declare function inOrder(values: readonly string[], order: readonly string[]): string[];
export declare function merged(now: readonly string[], before: readonly string[], picks: readonly string[]): string[] | null;
export declare function lacking(values: readonly string[], known: readonly string[] | null): string[];
export declare function held(values: readonly string[], known: readonly string[] | null): string[];

import type { RefObject } from "react";
export type RaisedFrom = RefObject<Element | null> | "parent" | "middle";
export declare function useRaised(from: RaisedFrom): (el: HTMLElement | null) => void;

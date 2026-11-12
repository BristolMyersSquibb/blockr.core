import type { ReactElement, ReactNode } from "react";
import type { Condition } from "../../protocol/index.ts";
import type { Block, BlockId, Stack } from "../../types/board.ts";
import type { AddMode } from "../../picker/index.ts";
type Severity = Condition["severity"];
export interface CardProps {
    readonly id: BlockId;
    readonly block: Block;
    readonly severities: readonly Severity[];
    readonly prependable: boolean;
    readonly add: (mode: AddMode) => void;
}
export declare const BlockCard: import("react").MemoExoticComponent<({ id, block, severities, prependable, add, }: CardProps) => ReactElement>;
export declare function StackGroup({ stack, children }: {
    stack: Stack;
    children: ReactNode;
}): ReactElement;
export {};

import type { ReactElement, ReactNode } from "react";
import type { Block, BlockId, Stack } from "../../types/board.ts";
export interface CardProps {
    readonly id: BlockId;
    readonly block: Block;
}
export declare const BlockCard: import("react").MemoExoticComponent<({ id, block }: CardProps) => ReactElement>;
export declare function StackGroup({ stack, children }: {
    stack: Stack;
    children: ReactNode;
}): ReactElement;

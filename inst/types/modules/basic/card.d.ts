import type { ReactElement, ReactNode } from "react";
import type { Block, BlockId, Stack, StackId } from "../../types/board.ts";
import type { AddMode } from "../../picker/index.ts";
export interface CardProps {
    readonly id: BlockId;
    readonly block: Block;
    readonly prependable: boolean;
    readonly add: (mode: AddMode) => void;
}
export declare const BlockCard: import("react").MemoExoticComponent<({ id, block, prependable, add }: CardProps) => ReactElement>;
export declare function StackGroup({ id, stack, children }: {
    id: StackId;
    stack: Stack;
    children: ReactNode;
}): ReactElement;

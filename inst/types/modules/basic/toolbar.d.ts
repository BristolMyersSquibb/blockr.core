import type { ReactElement } from "react";
import type { Board } from "../../types/board.ts";
export interface ToolbarProps {
    readonly board: Board;
    readonly showOptions: () => void;
}
export declare function Toolbar({ board, showOptions }: ToolbarProps): ReactElement;

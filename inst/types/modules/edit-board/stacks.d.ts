import type { ReactElement } from "react";
import type { Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
export declare function StacksSection({ core, board, locked }: {
    core: CoreApi;
    board: Board;
    locked: boolean;
}): ReactElement;

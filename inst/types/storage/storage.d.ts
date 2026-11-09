import type { Storage } from "../store/index.ts";
import type { Board } from "../types/board.ts";
export declare function coreStorage(doc?: Document): Storage;
export declare function fileName(board: Pick<Board, "id">, now?: Date): string;
export declare function download(text: string, name: string, doc?: Document): void;
export declare function pickFile(doc?: Document): Promise<string | null>;

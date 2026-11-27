import type { OptionId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
export type LocalStorage = Pick<WindowLocalStorage["localStorage"], "getItem" | "setItem" | "removeItem">;
export interface UserPicks {
    get(id: OptionId): Json | undefined;
    set(id: OptionId, value: Json): void;
    remove(id: OptionId): void;
}
export declare function pickKey(id: OptionId): string;
export declare function createUserPicks(storage: LocalStorage | undefined): UserPicks;
export declare function pageLocalStorage(): LocalStorage | undefined;
export declare function memoryStorage(items?: Readonly<Record<string, string>>): LocalStorage;

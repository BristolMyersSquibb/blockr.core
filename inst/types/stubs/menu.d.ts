import type { ShowMenu, ShownLevel } from "../actions/index.ts";
export interface StubMenu {
    level(): Omit<ShownLevel, "id"> | undefined;
    closed(): boolean;
    pick(label?: string): Promise<void>;
    tick(row: string, choice: string): void;
    type(value: string, row?: string): void;
    escape(): void;
    close(): Promise<void>;
}
export declare function stubMenus(): {
    readonly show: ShowMenu;
    readonly open: () => StubMenu | undefined;
};

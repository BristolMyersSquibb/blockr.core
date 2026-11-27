import type { Loader } from "../mount/index.ts";
import type { Json } from "../types/json.ts";
import type { ModuleId } from "../types/board.ts";
import type { ShowMenu } from "./actions.ts";
import type { OpenMenu } from "./quick.ts";
export interface MenuModule {
    render(args: {
        readonly menu: OpenMenu;
        readonly el: HTMLElement;
        readonly anchor: DOMRect;
    }): () => void;
}
export declare const MENU_OPTION = "board_action_menu";
export declare const CORE_MENU: ModuleId;
export declare function menuModule(loader: Pick<Loader, "module">, value: Json | undefined): Promise<ShowMenu>;
export declare function drawWith(module: MenuModule, id?: string): ShowMenu;

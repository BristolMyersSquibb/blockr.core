import type { Commands } from "../commands/index.ts";
import type { Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { OptionId } from "../types/board.ts";
import type { Loader } from "./load.ts";
export interface OptionDeps {
    readonly store: Pick<Store, "board" | "options" | "subscribe">;
    readonly commands: Pick<Commands, "run">;
    readonly scheduler: Pick<Scheduler, "request">;
    readonly loader: Loader;
    readonly renderers: () => Readonly<Record<string, Renderer>>;
    readonly replaced: AbortSignal;
    readonly keepOptions: (ids: readonly OptionId[], by: string) => {
        readonly added: Promise<readonly string[]>;
        readonly stop: () => void;
    };
}
export declare function mountOption(deps: OptionDeps, el: HTMLElement, option: OptionId): () => void;

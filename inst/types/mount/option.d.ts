import type { Commands } from "../commands/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { OptionId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Notify, Raise, Refused, Replied } from "../util/index.ts";
export interface OptionDeps {
    readonly store: Pick<Store, "board" | "options" | "subscribe">;
    readonly commands: Pick<Commands, "run">;
    readonly scheduler: Pick<Scheduler, "request">;
    readonly replaced: AbortSignal;
    readonly keepOptions: (ids: readonly OptionId[], by: string, failed: (failures: readonly string[]) => void, user: boolean) => {
        readonly added: Promise<readonly string[]>;
        readonly stop: () => void;
    };
    readonly value: (option: OptionId) => Json | undefined;
    readonly pick: (option: OptionId, value: Json) => void;
    readonly onPick: (fn: (option: OptionId, value: Json) => void) => () => void;
    readonly raise: Raise;
    readonly replied: Replied;
    readonly notify: Notify;
    readonly refused: Refused;
    readonly optionPackage: (option: OptionId) => string | undefined;
}
export declare function mountOption(deps: OptionDeps, el: HTMLElement, option: OptionId): () => void;

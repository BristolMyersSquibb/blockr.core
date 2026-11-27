import type { Delta, Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi, FrontendModule, Notification, Notifier } from "../types/core-api.ts";
import type { Rendered } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
export interface AdvancedModule {
    render(args: {
        el: HTMLElement;
        core: CoreApi;
        target: Target;
        picked?: Json;
        filter?: string;
        signal: AbortSignal;
        done: (calls?: readonly CommandCall[], opts?: {
            notifier?(notifications: readonly Notification[]): void;
        }) => Promise<void>;
    }): Rendered;
}
export type Place = NonNullable<FrontendModule["advanced"]>;
export type PlaceArgs = Parameters<Place>[0];
export type ShowDialog = (args: PlaceArgs, at: {
    readonly el: HTMLElement;
    readonly signal?: AbortSignal;
}) => (deltas?: readonly Delta[]) => void;
export interface Batching {
    readonly call?: string;
    readonly extend?: ((deltas: readonly Delta[]) => unknown) | undefined;
    readonly notifier?: Notifier | undefined;
}
export interface AdvancedParts {
    readonly store: Pick<Store, "board" | "hasBoard" | "subscribe">;
    readonly core: () => CoreApi;
    readonly applies: (action: BoardAction, target: Target) => boolean;
    readonly run: (batch: readonly CommandCall[], batching: Batching) => Promise<readonly Delta[]>;
    readonly frontend: () => Place | undefined;
    readonly dialog: ShowDialog;
    readonly raise: (notification: Omit<Notification, "id">) => void;
    readonly classOf: (action: BoardAction) => PluginClassId | undefined;
    readonly signal?: AbortSignal;
}
export interface Opening {
    readonly el: HTMLElement;
    readonly place?: {
        readonly show: Place;
        readonly key: unknown;
    } | undefined;
    readonly extend?: ((deltas: readonly Delta[]) => unknown) | undefined;
    readonly call?: string;
    readonly notifier?: Notifier | undefined;
    readonly picked?: Json;
    readonly filter?: string;
}
export interface Advanced {
    open(action: BoardAction, target: Target, opening: Opening): Promise<void>;
    mount(el: HTMLElement, action: BoardAction, target: Target, picked: Json | undefined): () => void;
}
export declare function createAdvanced(parts: AdvancedParts): Advanced;

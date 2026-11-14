import type { Condition } from "../protocol/index.ts";
import type { BoardObject, CoreCode, Notification, Raised, RRequest, SourceModule } from "../types/core-api.ts";
export type Raise = (notification: Omit<Notification, "id">) => () => void;
export type Replied = (module: SourceModule, request: Extract<RRequest, {
    readonly name: string;
}>, conditions: readonly Condition[]) => void;
export type Notify = (module: SourceModule, notification: Raised) => () => void;
export declare const raiseNothing: Raise;
export declare const repliedNothing: Replied;
export declare const notifyNothing: Notify;
export declare function sourceModule(object: SourceModule["object"], pkg: string | null | undefined, renderer?: string): SourceModule;
export interface Concerns {
    readonly about?: BoardObject;
    readonly module?: SourceModule;
}
export declare function coreNotification(code: CoreCode, severity: "warning" | "error", message: string, concerns?: Concerns): Omit<Notification, "id">;
export declare function isBoardObject(value: unknown): value is BoardObject;
export declare function checkRaised(notification: Raised): Raised;

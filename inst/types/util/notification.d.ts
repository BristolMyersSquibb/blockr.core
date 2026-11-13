import type { Condition } from "../protocol/index.ts";
import type { CoreCode, Notification, Owner, Part, Raised, RRequest } from "../types/core-api.ts";
export type Raise = (notification: Omit<Notification, "id">) => () => void;
export type Replied = (about: Part, request: Extract<RRequest, {
    readonly name: string;
}>, conditions: readonly Condition[]) => void;
export type Notify = (owner: Owner, pkg: string | null, notification: Raised) => () => void;
export declare const raiseNothing: Raise;
export declare const repliedNothing: Replied;
export declare const notifyNothing: Notify;
export interface Concerns {
    readonly about?: Part;
    readonly package?: string | null;
}
export declare function coreNotification(code: CoreCode, severity: "warning" | "error", message: string, concerns?: Concerns): Omit<Notification, "id">;
export declare function isPart(value: unknown): value is Part;
export declare function checkRaised(notification: Raised): Raised;

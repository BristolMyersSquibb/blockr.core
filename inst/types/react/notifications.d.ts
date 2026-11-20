import type { ReactElement } from "react";
import type { Notification } from "../types/core-api.ts";
export interface NotificationListProps {
    readonly notifications: readonly Notification[];
    readonly "aria-label"?: string;
}
export declare function NotificationList({ notifications, "aria-label": label }: NotificationListProps): ReactElement | null;

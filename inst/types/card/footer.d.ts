import type { ReactElement } from "react";
import type { BlockStatus } from "../scheduler/index.ts";
import type { Notification, Severity } from "../types/core-api.ts";
export interface FooterProps {
    readonly status: BlockStatus | undefined;
    readonly shown: BlockStatus | undefined;
    readonly notifications: readonly Notification[];
    readonly severities: readonly Severity[];
    readonly open: ReadonlySet<Severity>;
    readonly toggle: (severity: Severity) => void;
    readonly panel: string;
}
export declare function useFooter(status: BlockStatus | undefined, notifications: readonly Notification[], severities?: readonly Severity[]): FooterProps;
export declare function Footer({ status, shown }: FooterProps): ReactElement;
export declare function Pills({ status, shown, notifications, severities, open, toggle, panel }: FooterProps): ReactElement | null;
export declare function Banners({ shown, notifications, severities, open, panel }: FooterProps): ReactElement | null;

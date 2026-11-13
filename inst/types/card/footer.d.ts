import type { ReactElement } from "react";
import type { Condition } from "../protocol/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";
type Severity = Condition["severity"];
type Pill = "reason" | Severity;
export interface FooterProps {
    readonly status: BlockStatus | undefined;
    readonly shown: BlockStatus | undefined;
    readonly severities: readonly Severity[];
    readonly open: ReadonlySet<Pill>;
    readonly toggle: (pill: Pill) => void;
    readonly panel: string;
}
export declare function useFooter(status: BlockStatus | undefined, severities?: readonly Severity[]): FooterProps;
export declare function Footer({ status, shown }: FooterProps): ReactElement;
export declare function Pills({ status, shown, severities, open, toggle, panel }: FooterProps): ReactElement | null;
export declare function Banners({ shown, severities, open, panel }: FooterProps): ReactElement | null;
export {};

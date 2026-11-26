import type { ReactElement, ReactNode, RefObject } from "react";
import type { Refused } from "./edits.ts";
export declare const classes: {
    readonly button: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-inherit bc:cursor-pointer bc:hover:bg-accent bc:hover:text-accent-foreground bc:disabled:cursor-default bc:disabled:opacity-50";
    readonly primary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-primary bc:bg-primary bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-primary-foreground bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50";
    readonly quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-muted-foreground bc:cursor-pointer bc:hover:bg-accent bc:hover:text-accent-foreground";
    readonly page: "bc:bg-background bc:text-foreground";
    readonly surface: "bc:bg-popover bc:text-popover-foreground";
    readonly card: "bc:bg-card bc:text-card-foreground";
    readonly heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold";
    readonly list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0";
};
export declare function NameField({ label, name, onSubmit, onDone, }: {
    label: string;
    name: string;
    onSubmit: (name: string) => void;
    onDone: () => void;
}): ReactElement;
export declare function ActionsButton({ ref, label, onPress, }: {
    ref: RefObject<HTMLButtonElement | null>;
    label: string;
    onPress: () => void;
}): ReactElement;
export declare function drawForm(el: HTMLElement, form: ReactNode): () => void;
export declare const identifier: {
    readonly spellCheck: "false";
    readonly autoCapitalize: "none";
    readonly autoCorrect: "off";
};
export declare function Refusal({ refusal }: {
    refusal: Refused | null;
}): ReactElement | null;

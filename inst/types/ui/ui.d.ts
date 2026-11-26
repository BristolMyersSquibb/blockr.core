import type { ReactElement, ReactNode, RefObject } from "react";
import type { Refused } from "./edits.ts";
export declare const classes: {
    readonly page: "bc:bg-background bc:text-foreground";
    readonly surface: "bc:bg-popover bc:text-popover-foreground";
    readonly card: "bc:bg-card bc:text-card-foreground";
    readonly heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold";
    readonly list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0";
    readonly hint: "bc:m-0 bc:text-sm bc:text-muted-foreground";
    readonly alert: "bc:m-0 bc:text-sm bc:text-destructive-text bc:whitespace-pre-wrap";
    readonly fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans";
    readonly legend: "bc:mb-1 bc:p-0 bc:text-sm bc:font-medium";
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

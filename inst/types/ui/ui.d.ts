import type { ReactElement } from "react";
export declare const classes: {
    readonly button: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50";
    readonly primary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-accent bc:bg-bg-accent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-on-accent bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50";
    readonly quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle";
    readonly page: "bc:text-gray-900 bc:dark:text-gray-100";
    readonly surface: "bc:bg-bg-raised bc:text-gray-900 bc:dark:text-gray-100";
    readonly card: "bc:bg-bg-subtle";
    readonly heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold";
    readonly list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0";
};
export declare function NameField({ label, name, onSubmit, onDone, }: {
    label: string;
    name: string;
    onSubmit: (name: string) => void;
    onDone: () => void;
}): ReactElement;

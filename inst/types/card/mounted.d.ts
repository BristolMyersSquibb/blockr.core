import type { ReactElement } from "react";
export declare function Mounted({ mount, label, className, data, }: {
    mount: (el: HTMLElement) => () => void;
    label?: string;
    className?: string;
    data?: Readonly<Record<`data-${string}`, string>>;
}): ReactElement;

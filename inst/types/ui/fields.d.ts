import type { ReactElement } from "react";
export declare function TextField({ label, value, hint, error, placeholder, identifier, onChange, onLeave, }: {
    label: string;
    value: string;
    hint?: string;
    error?: string;
    placeholder?: string;
    identifier?: boolean;
    onChange: (value: string) => void;
    onLeave?: () => void;
}): ReactElement;

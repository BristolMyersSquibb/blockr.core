export interface Search<T> {
    readonly items: readonly T[];
    readonly onInputChange: (text: string) => void;
    readonly onOpenChange: (isOpen: boolean, trigger?: string) => void;
    readonly renderEmptyState: () => string;
}
export declare function useSearch<T>(all: readonly T[], texts: (item: T) => readonly string[], none?: string): Search<T>;

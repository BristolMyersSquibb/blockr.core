import type { Id } from "../types/board.ts";
export declare function isIdRecord(x: unknown, nullable?: false): x is {
    readonly package: string;
    readonly id: string;
};
export declare function isIdRecord(x: unknown, nullable: true): x is {
    readonly package: string | null;
    readonly id: string;
};
export declare function internedRecord<P extends string | null>(record: {
    readonly package: P;
    readonly id: string;
}): Id<P>;
export declare function idText(x: {
    readonly package: string | null;
    readonly id: string;
}): string;
export declare function idNumber(x: Id<string | null>): number;
export declare function nestedEntries<T>(record: Readonly<Record<string, Readonly<Record<string, T>>>>): [Id, T][];
export declare function nestedValue<T>(record: Readonly<Record<string, Readonly<Record<string, T>>>>, id: Id): T | undefined;
export declare function nestedRecord<T>(entries: Iterable<readonly [Id, T]>): Record<string, Record<string, T>>;
export declare function idAttributes(of: string, x: {
    readonly package: string | null;
    readonly id: string;
}): Record<string, string>;
export declare function givenPluginClassId(given: unknown, call: string): Id;
export declare function givenModuleId(given: unknown): Id;

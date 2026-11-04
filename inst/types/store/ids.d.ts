export declare const RAND_IDS: readonly ["adjective_animal", "adjective_adjective_animal", "letters_6", "letters_8", "letters_12", "bytes_4", "bytes_8", "uuid"];
export type RandId = (typeof RAND_IDS)[number];
export declare function isRandId(value: unknown): value is RandId;
export declare function isId(id: unknown): boolean;
export declare function mintId(taken: (id: string) => boolean, randId?: RandId): string;
export declare function sentenceCase(id: string): string;

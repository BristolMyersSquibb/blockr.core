export type Json = null | boolean | number | string | readonly Json[] | {
    readonly [key: string]: Json;
};
export type JsonSchema = Readonly<Record<string, Json>>;

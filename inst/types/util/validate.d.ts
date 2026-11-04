import type { JsonSchema } from "../types/json.ts";
export type Validate = (value: unknown) => readonly string[];
export declare function compile(what: string, schema: JsonSchema, fails?: string): Validate;

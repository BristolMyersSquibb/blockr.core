import type { OptionId, RegistryId } from "./board.ts";
import type { Json, JsonSchema } from "./json.ts";
import type { OutputRenderer } from "./outputs.ts";
export interface RegistryEntry {
    readonly id: RegistryId;
    readonly name: string;
    readonly description: string;
    readonly details?: string;
    readonly url?: string;
    readonly guidance?: string;
    readonly keywords: readonly string[];
    readonly category: string;
    readonly icon: string;
    readonly inputs: readonly string[];
    readonly module: string;
    readonly css?: string;
    readonly version: number;
    readonly schema: JsonSchema;
    readonly state: Readonly<Record<string, Json>>;
    readonly arguments: Readonly<Record<string, ArgSpec>>;
    readonly requests: Readonly<Record<string, {
        readonly inputs: readonly string[];
        readonly state_args: readonly string[];
        readonly options: readonly OptionId[];
        readonly volatile?: true;
        readonly action?: true;
    }>>;
    readonly renderer?: OutputRenderer;
}
export interface ArgSpec {
    readonly description?: string;
    readonly example?: Json;
    readonly type?: JsonSchema;
}

import type { Choice } from "../binding/index.ts";
import type { Json } from "../types/json.ts";
import type { OptionModule } from "../types/hosts.ts";
export declare function numberUi(label: string, min: number): OptionModule<number>;
export declare function checklistUi(legend: string, choices: readonly string[]): OptionModule<string[]>;
export declare function selectUi<V extends Json = string>(label: string, placeholder: string, choices: readonly Choice[]): OptionModule<V>;

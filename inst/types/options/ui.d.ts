import type { Json } from "../types/json.ts";
import type { OptionModule } from "../types/hosts.ts";
export declare function numberUi(label: string, min: number): OptionModule<number>;
export declare function selectUi<V extends Json = string>(label: string, placeholder: string, items: readonly {
    readonly id: string;
    readonly name: string;
}[]): OptionModule<V>;

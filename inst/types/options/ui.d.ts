import type { Json } from "../types/json.ts";
import type { OptionModule } from "../types/hosts.ts";
export declare function numberUi(label: string, min: number): OptionModule<number>;
export declare function toggleButtonGroupUi<V extends Json = string>(label: string, items: readonly {
    readonly id: string;
    readonly name: string;
}[]): OptionModule<V>;

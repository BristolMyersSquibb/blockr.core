import type { Client } from "../protocol/index.ts";
import type { Json } from "../types/json.ts";
export interface BlockrOptions {
    read(name: string): Promise<Json | undefined>;
}
export declare function createBlockrOptions(client: Pick<Client, "request">): BlockrOptions;

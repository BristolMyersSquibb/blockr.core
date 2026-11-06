import type { Client } from "../protocol/index.ts";
import type { Json } from "../types/json.ts";
export interface BlockrOptions {
    read<T extends Json>(name: string, fallback: T): Promise<T>;
}
export declare function createBlockrOptions(client: Pick<Client, "request">): BlockrOptions;

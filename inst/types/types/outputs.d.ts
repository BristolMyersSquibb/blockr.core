import type { Json } from "./json.ts";
export interface BlockOutput {
    renderer: {
        id: string;
        module: string;
        css?: string;
    };
}
export type OutputRenderer = BlockOutput["renderer"];
export interface TableOutput extends BlockOutput {
    columns: TableColumn[];
    rows: Json[][];
    total: number | null;
}
export interface TableColumn {
    name: string;
    class: string;
}
export interface TableParams {
    page?: number;
    page_size?: number;
    sort?: {
        column: string;
        desc?: boolean;
    };
}
export interface TextOutput extends BlockOutput {
    text: string;
}
export interface PlotOutput extends BlockOutput {
    width: number;
    height: number;
}
export interface OutputFixtures<T extends BlockOutput = BlockOutput> {
    description: string;
    outputs: OutputFixture<T>[];
}
export interface OutputFixture<T extends BlockOutput = BlockOutput> {
    description: string;
    options: Record<string, Json>;
    params: Record<string, Json>;
    output: T;
    body?: {
        type: string;
        name?: string;
    };
    body_file?: string;
}

export declare const API = 1;
export interface Source {
    readonly module: string;
    readonly css?: string;
}
export interface Loader {
    load(source: Source, pkg: string | null): Promise<unknown>;
    module(id: string): Promise<unknown>;
}
export interface LoaderOptions {
    readonly modules?: () => Readonly<Record<string, Source>>;
    readonly importer?: (url: string) => Promise<unknown>;
    readonly stylesheet?: (url: string) => Promise<void>;
}
export declare class ModuleError extends Error {
    readonly module: string;
    readonly package: string | null;
    constructor(module: string, pkg: string | null, what: string, options?: ErrorOptions);
}
export declare function createLoader(options?: LoaderOptions): Loader;

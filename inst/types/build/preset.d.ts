import type { Plugin } from "esbuild";
export declare const shared: Plugin;
export declare const preset: {
    bundle: true;
    format: "esm";
    platform: "browser";
    target: string;
    jsx: "automatic";
    define: {
        "process.env.NODE_ENV": string;
    };
    plugins: Plugin[];
};

import type { Plugin } from "esbuild";
export declare const preset: {
    plugins: Plugin[];
    bundle: true;
    format: "esm";
    platform: "browser";
    target: string;
    jsx: "automatic";
    define: {
        "process.env.NODE_ENV": string;
    };
};

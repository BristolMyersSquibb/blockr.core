export declare const REACT_ENTRIES: readonly ["react", "react/jsx-runtime", "react-dom", "react-dom/client"];
export declare const OTEL_API = "@opentelemetry/api";
export declare const IMPORTS: readonly ["react", "react/jsx-runtime", "react-dom", "react-dom/client", "@blockr/core/react", "@blockr/core/binding", "@blockr/core/controls", "@blockr/core/otel", "@opentelemetry/api"];
export type Import = (typeof IMPORTS)[number];
export declare function importFile(name: Import): string;

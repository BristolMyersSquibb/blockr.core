import type { Context } from "@opentelemetry/api";
export interface Steps {
    step<T>(name: string, fn: () => T | Promise<T>): Promise<T>;
    noted(name: string, start: number, end: number): void;
    record(ready: Promise<unknown>, failure?: unknown): void;
}
export declare function steps(name: string, start?: number, parent?: Context): Steps;

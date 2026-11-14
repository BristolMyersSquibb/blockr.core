import type { Log, LogLine } from "../types/log.ts";
export declare function writingLog(pkg: string | null, write: (line: LogLine) => void): Log;

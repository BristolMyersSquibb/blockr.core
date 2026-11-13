// The scheduler. It tracks revisions, demand and statuses, and has R evaluate
// what is demanded. Both `CoreApi` and the hosts build on it.

export { createScheduler } from "./scheduler.ts";
export type { Demand, Scheduler, SchedulerOptions } from "./scheduler.ts";
export { scheduledCode, unevaluated } from "./status.ts";
export type { BlockStatus, Status } from "./status.ts";
export type { Input, Inputs } from "./recipes.ts";
export { counter } from "./revisions.ts";

// What the store throws where it refuses a delta, an undo or redo, or a board
// to open, with each failure in a sentence.
export class StoreError extends Error {
  readonly failures: readonly string[];

  constructor(message: string, failures: readonly string[]) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"));
    this.name = "StoreError";
    this.failures = failures;
  }
}

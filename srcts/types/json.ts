// The plain JSON values that the client and R exchange, and the JSON Schemas
// that describe some of them, such as the schema of a block's state that its UI
// module ships. Both are read-only, so that nothing changes a value that the
// board holds in place, as copy-on-modify kept R from doing.

export type Json =
  | null
  | boolean
  | number
  | string
  | readonly Json[]
  | { readonly [key: string]: Json };

export type JsonSchema = Readonly<Record<string, Json>>;

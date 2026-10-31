// The plain JSON values that the client and R exchange, and the JSON Schemas
// that describe some of them, such as the schema of a block's state that its UI
// module ships.

export type Json =
  | null
  | boolean
  | number
  | string
  | Json[]
  | { [key: string]: Json };

export type JsonSchema = Record<string, Json>;

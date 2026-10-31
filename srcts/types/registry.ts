// The block registry's entries, which R sends the client with its reply to
// `hello` and to every `open`: each block class by registry id, with what the
// client cannot read off a class that stays in R.

import type { OptionId, RegistryId } from "./board.ts";
import type { Json, JsonSchema } from "./json.ts";

export interface RegistryEntry {
  id: RegistryId;
  // The metadata that registration gives, as today: name, description,
  // details, url (the link that `register_block()` takes), guidance, keywords,
  // category and icon, the icon as SVG markup.
  name: string;
  description: string;
  details?: string;
  url?: string;
  guidance?: string;
  keywords: string[];
  category: string;
  icon: string;
  // Its data inputs, as ["data"], ["x", "y"], or ["..."] for a variadic block.
  inputs: string[];
  // The URL that R serves the class's UI module at, and that of the stylesheet
  // beside it, if it has one.
  module: string;
  css?: string;
  // The schema of the block's state, as its module ships it.
  schema: JsonSchema;
  // What a new block starts with, from `expr` called without args.
  state: Record<string, Json>;
  // The arg specs, which describe each state arg for models and tools.
  arguments: Record<string, ArgSpec>;
  requests: Record<
    string,
    {
      // The data inputs, state args and board options that its function
      // names, which R binds, as the host fills in the state args.
      inputs: string[];
      state_args: string[];
      options: OptionId[];
      // Never kept by the client.
      volatile?: true;
      // Acts rather than reads, and runs once per call.
      action?: true;
    }
  >;
}

// An arg spec, as `new_arg_spec()` makes it, without the fields it leaves
// unset.
export interface ArgSpec {
  description?: string;
  example?: Json;
  type?: JsonSchema;
}

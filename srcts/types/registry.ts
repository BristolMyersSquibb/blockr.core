// The block registry's entries, which R sends the client with its reply to
// `hello`: each block class by registry id, with what the client cannot read
// off a class that stays in R.
// They are read-only, for neither side to edit.

import type { OptionId, RegistryId } from "./board.ts";
import type { Json, JsonSchema } from "./json.ts";
import type { OutputRenderer } from "./outputs.ts";

export interface RegistryEntry {
  readonly id: RegistryId;
  // The metadata that registration gives, as today: name, description,
  // details, url (the link that `register_block()` takes), guidance, keywords,
  // category and icon, the icon as SVG markup.
  readonly name: string;
  readonly description: string;
  readonly details?: string;
  readonly url?: string;
  readonly guidance?: string;
  readonly keywords: readonly string[];
  readonly category: string;
  readonly icon: string;
  // Its data inputs, as ["data"], ["x", "y"], or ["..."] for a variadic block.
  readonly inputs: readonly string[];
  // The URL that R serves the class's UI module at, and that of the stylesheet
  // beside it, if it has one.
  readonly module: string;
  readonly css?: string;
  // The version of the shape of the block's state, which a package raises as
  // it changes that shape, and which a JSON board records with each block.
  readonly version: number;
  // The schema of the block's state, as its module ships it.
  readonly schema: JsonSchema;
  // What a new block starts with, from `expr` called without args.
  readonly state: Readonly<Record<string, Json>>;
  // The arg specs, which describe each state arg for models and tools.
  readonly arguments: Readonly<Record<string, ArgSpec>>;
  readonly requests: Readonly<
    Record<
      string,
      {
        // The data inputs and state args that its function names and the board
        // options it reads, which R binds, as the host fills in the state args.
        readonly inputs: readonly string[];
        readonly state_args: readonly string[];
        readonly options: readonly OptionId[];
        // Never kept by the client.
        readonly volatile?: true;
        // Acts rather than reads, and runs once per call.
        readonly action?: true;
      }
    >
  >;
  // The renderer of the class's outputs: that of the output class whose
  // coercion the class names, which R knows where the output class gives its
  // coercion through its method of `output_coercion()`. Core loads it, and
  // adds the board options that it names to the board, before it asks for a
  // block's first output, so that the request carries the board options that
  // the coercion reads. An output still names the renderer that draws it.
  readonly renderer?: OutputRenderer;
}

// An arg spec, as `new_arg_spec()` makes it, without the properties it
// leaves unset.
export interface ArgSpec {
  readonly description?: string;
  readonly example?: Json;
  readonly type?: JsonSchema;
}

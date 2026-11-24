// The block registry's entries, which R sends the client with its reply to
// `hello`: each block class by registry id, with what the client cannot read
// off a class that stays in R.
// They are read-only, for neither side to edit.

import type { OptionId, RegistryId } from "./board.ts";
import type { Json, JsonSchema } from "./json.ts";
import type { OutputRenderer } from "./outputs.ts";

export interface RegistryEntry {
  readonly id: RegistryId;
  // Whether the app offers the class, which R says only where it does: for
  // the classes of `serve()`'s `blocks` and those that the app script
  // registered. A class that R registered only for a block of the opening
  // board, which the app does not offer, leaves it out, and a picker lists no
  // such class.
  readonly offered?: true;
  // The metadata that registration gives: name, description, url (the link
  // that `register_block()` takes), guidance, keywords, category and icon, the
  // icon as SVG markup.
  readonly name: string;
  readonly description: string;
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
  // The schema of the block's state, as its module ships it, which gives each
  // state arg's type, and may give its description, for forms and models
  // alike.
  readonly schema: JsonSchema;
  // The name R gives a new block whose constructor gets no block_name.
  readonly block_name: string;
  // What a new block starts with, from `expr` called without args.
  readonly state: Readonly<Record<string, Json>>;
  // The board options that the class's UI module reads itself, by id, whose
  // constructors the class names in `options`, which R names in every entry.
  // Core puts them on the board, with those that the class's requests read,
  // before it calls the module's render(), and the module reads all of them
  // through its host's context.
  readonly options?: readonly OptionId[];
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
        // Takes a file that the client sends, in the argument that its
        // `new_block_request()` names. The client sends no file with any
        // other request.
        readonly file?: true;
      }
    >
  >;
  // The board options of the class's outputs, by id, whose constructors the
  // output family that the class names holds, with those of the families that
  // it hands its results to, which R names in every entry: those that the
  // families read, and those that the renderers of their outputs read, since
  // no renderer names any of its own. Core puts them on the board before it
  // asks for a block's first output, sends their values with every output
  // request, and gives them to the renderer through its host's option().
  readonly output_options?: readonly OptionId[];
  // The renderers of the class's outputs: that of the output family that the
  // class names, and of each family that it hands its results to, each the
  // one that the deployment names for the family in the blockr option
  // `renderers`, or else the family's own. Core loads them all before it asks
  // for a block's first output. Each output names the one that draws it.
  readonly renderers?: readonly OutputRenderer[];
}

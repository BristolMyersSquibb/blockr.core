// The schemas of what every block, link or stack shares, such as a block's id
// and name, which core gives and builds the args of its commands from. A
// block's state is not among them. Each block class gives the schema of its
// state, which its UI ships, and stateSchema() describes it by the class's arg
// specs. That schema is the same for every block of the class. So for one
// class, the args of a command that takes a block's state are the command's
// `args` with that schema in place of `state`, which is what a form or a tool
// of the assistant's for that class takes.

import type { Json, JsonSchema } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { isObject, own } from "../util/index.ts";

// An id is any string that is neither empty nor made of digits alone, since a
// JS object puts keys that look like integers first.
const id = { type: "string", pattern: "\\D" };

export const blockId: JsonSchema = {
  ...id,
  description: "The id of a block, unique among the board's blocks.",
};

export const linkId: JsonSchema = {
  ...id,
  description: "The id of a link, unique among the board's links.",
};

export const stackId: JsonSchema = {
  ...id,
  description: "The id of a stack, unique among the board's stacks.",
};

export const registryId: JsonSchema = {
  type: "string",
  minLength: 1,
  description: 'A block class\'s id in the block registry, such as "blockr.core-head_block".',
};

export const name: JsonSchema = {
  type: "string",
  description: "A name that the user sees and may change, which nothing refers to.",
};

export const input: JsonSchema = {
  type: "string",
  description:
    'An input of the block a link goes to, such as "data", or empty for a variadic block, which takes such links in their order.',
};

export const state: JsonSchema = {
  type: "object",
  description: "Values of a block's state args, which the schema of its class types.",
};

export const stateArgs: JsonSchema = {
  type: "array",
  items: { type: "string" },
  uniqueItems: true,
  description: "Names of a block's state args.",
};

export const blockIds: JsonSchema = {
  type: "array",
  items: blockId,
  uniqueItems: true,
  description: "Ids of blocks, in order.",
};

export const optionId: JsonSchema = {
  type: "string",
  minLength: 1,
  description: 'The id of a board option, such as "n_rows".',
};

export const value: JsonSchema = {
  description: "A JSON value, which the schema of what it is set on types.",
};

const described = new WeakMap<RegistryEntry, JsonSchema>();

// The schema of a block class's state as its UI ships it, with each state arg
// that the schema leaves undescribed described by its arg spec, its
// description and its example, for forms and the assistant's tools. It is the
// same for every block of the class, so it is made once per class.
export function stateSchema(entry: RegistryEntry): JsonSchema {
  const known = described.get(entry);
  if (known !== undefined) return known;

  const { properties } = entry.schema;
  let schema = entry.schema;
  if (isRecord(properties)) {
    schema = {
      ...schema,
      properties: Object.fromEntries(
        Object.entries(properties).map(([arg, property]) => {
          const spec = own(entry.arguments, arg);
          if (spec === undefined || !isRecord(property)) return [arg, property];
          return [
            arg,
            {
              ...(spec.description === undefined ? {} : { description: spec.description }),
              ...(spec.example === undefined ? {} : { examples: [spec.example] }),
              ...property,
            },
          ];
        }),
      ),
    };
  }

  described.set(entry, schema);
  return schema;
}

function isRecord(x: Json | undefined): x is Readonly<Record<string, Json>> {
  return isObject(x);
}

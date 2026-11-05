// The checks a board passes after every delta and as it opens,
// each failure in a sentence: the invariants of core's objects,
// as R's validate_links() and the board's checks did,
// and the schemas of the board options and properties of the board's class.
// Where a delta changed only some objects, their shapes and schemas
// are checked alone, since nothing else can have broken them,
// while the invariants run on the whole board every time.

import { graphOf } from "../graph/index.ts";
import type { Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { isObject, own, quoted } from "../util/index.ts";
import type { Changed } from "./commit.ts";
import { isId } from "./ids.ts";
import { recordOf } from "./registrations.ts";
import type { Built, Registrations } from "./registrations.ts";

// A stack's colour: a hex colour, such as "#66c2a5", or "#fc8" in short,
// as blockr.dock took one until now.
// The JSON Schema of a JSON board, which the build generates from core's types,
// holds the same pattern.
export const HEX_COLOR = "^#([0-9a-fA-F]{3}){1,2}$";

const hexColor = new RegExp(HEX_COLOR);

export function check(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  registrations: Registrations,
  built: Built,
  changed?: Changed,
): string[] {
  const failures: string[] = [];

  if (!isId(board.id)) {
    failures.push(`The board's id "${board.id}" is empty or made of digits alone.`);
  }
  if (typeof board.name !== "string") {
    failures.push("The board's name is no string.");
  }
  if (!isClass(board.class)) {
    failures.push("The board's class is no array of class names, as R's class vector of a board is.");
  }

  checkBlocks(board, registry, registrations, changed, failures);
  checkLinks(board, registry, changed, failures);
  checkStacks(board, changed, failures);
  checkValues(board, registrations, changed, failures);
  checkProperties(board, built, changed, failures);

  return failures;
}

// Whether a board's class is a class vector: an array of names,
// which are not empty.
export function isClass(x: unknown): x is readonly string[] {
  return Array.isArray(x) && x.length > 0 && x.every((cls) => typeof cls === "string" && cls !== "");
}

function ids(
  what: string,
  record: Readonly<Record<string, unknown>>,
  failures: string[],
): void {
  for (const id of Object.keys(record)) {
    if (!isId(id)) failures.push(`The ${what} id "${id}" is empty or made of digits alone.`);
  }
}

function checkBlocks(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  registrations: Registrations,
  changed: Changed | undefined,
  failures: string[],
): void {
  ids("block", board.blocks, failures);

  for (const [id, block] of Object.entries(board.blocks)) {
    if (changed !== undefined && !changed.blocks.has(id)) continue;
    const fields = shape(block, { registry_id: "string", state: "object", name: "string" });
    if (fields.length > 0) {
      failures.push(`The block "${id}" ${fields.join(", ")}.`);
      continue;
    }
    if (own(registry, block.registry_id) === undefined) {
      failures.push(
        `The block "${id}" is of the class "${block.registry_id}", which the block registry lacks.`,
      );
    }
  }
}

function checkLinks(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  changed: Changed | undefined,
  failures: string[],
): void {
  ids("link", board.links, failures);

  const fed = new Map<string, string[]>();

  for (const [id, link] of Object.entries(board.links)) {
    if (changed === undefined || changed.links.has(id)) {
      const fields = shape(link, { from: "string", to: "string", input: "string" });
      if (fields.length > 0) {
        failures.push(`The link "${id}" ${fields.join(", ")}.`);
        continue;
      }
    }

    const from = own(board.blocks, link.from);
    const to = own(board.blocks, link.to);
    if (from === undefined) {
      failures.push(`The link "${id}" comes from block "${link.from}", which the board lacks.`);
    }
    if (to === undefined) {
      failures.push(`The link "${id}" goes to block "${link.to}", which the board lacks.`);
      continue;
    }

    const inputs = own(registry, to.registry_id)?.inputs;
    if (inputs === undefined || inputs.includes("...")) continue;
    if (!inputs.includes(link.input)) {
      failures.push(
        `The link "${id}" feeds the input "${link.input}" of block "${link.to}", whose class has no such input.`,
      );
      continue;
    }
    const at = JSON.stringify([link.to, link.input]);
    fed.set(at, [...(fed.get(at) ?? []), id]);
  }

  for (const [at, links] of fed) {
    if (links.length < 2) continue;
    const [to, input] = JSON.parse(at) as [string, string];
    failures.push(
      `The input "${input}" of block "${to}" takes the links ${quoted(links)}, where it takes one.`,
    );
  }

  const cycle = graphOf(board).cycle();
  if (cycle !== undefined) {
    failures.push(`The links form a cycle through the blocks ${quoted(cycle)}.`);
  }
}

function checkStacks(
  board: Board,
  changed: Changed | undefined,
  failures: string[],
): void {
  ids("stack", board.stacks, failures);

  const holder = new Map<string, string>();
  for (const [id, stack] of Object.entries(board.stacks)) {
    if (changed === undefined || changed.stacks.has(id)) {
      const fields = shape(stack, { name: "string", blocks: "array" }, { color: "string" });
      if (fields.length > 0) {
        failures.push(`The stack "${id}" ${fields.join(", ")}.`);
        continue;
      }
      if (stack.color !== undefined && !hexColor.test(stack.color)) {
        failures.push(
          `The stack "${id}" has the colour "${stack.color}", which is no hex colour, such as "#66c2a5".`,
        );
      }
    }
    for (const block of stack.blocks) {
      if (own(board.blocks, block) === undefined) {
        failures.push(`The stack "${id}" holds block "${block}", which the board lacks.`);
        continue;
      }
      const other = holder.get(block);
      if (other === id) {
        failures.push(`The stack "${id}" holds block "${block}" twice.`);
      } else if (other !== undefined) {
        failures.push(`The block "${block}" sits in the stacks "${other}" and "${id}", where it sits in one.`);
      }
      holder.set(block, id);
    }
  }
}

function checkValues(
  board: Board,
  registrations: Registrations,
  changed: Changed | undefined,
  failures: string[],
): void {
  for (const [id, value] of Object.entries(board.options)) {
    if (changed !== undefined && !changed.options.has(id)) continue;
    const option = registrations.option(id);
    if (option === undefined) {
      failures.push(`The board holds a value of the board option "${id}", which no package registered.`);
      continue;
    }
    for (const failure of option.validate(value)) {
      failures.push(`The value of the board option "${id}" ${failure}`);
    }
  }
}

// Each property of the board's class against its schema, as a whole:
// those that a patch changed, or every one as the board opens.
function checkProperties(
  board: Board,
  built: Built,
  changed: Changed | undefined,
  failures: string[],
): void {
  const record = recordOf(board);
  for (const [name, { validate }] of built.properties) {
    if (changed !== undefined && !changed.properties.has(name)) continue;
    const value = own(record, name);
    if (value === undefined) {
      failures.push(`The board holds no value of the property "${name}" of its class.`);
      continue;
    }
    for (const failure of validate(value)) failures.push(`The property "${name}" ${failure}`);
  }
}

type Kind = "string" | "object" | "array";

// What an object of core's lacks or holds beyond its fields, which a delta
// built in plain JS or by a model may get wrong.
// It may lack a field of `optional`, which holds its kind where it is there.
function shape(
  value: unknown,
  fields: Readonly<Record<string, Kind>>,
  optional: Readonly<Record<string, Kind>> = {},
): string[] {
  if (!isObject(value)) return ["is no object"];
  const problems: string[] = [];
  const kinds = { ...fields, ...optional };
  for (const [field, kind] of Object.entries(kinds)) {
    if (Object.hasOwn(optional, field) && !Object.hasOwn(value, field)) continue;
    const x = value[field];
    const ok =
      kind === "array" ? Array.isArray(x) : kind === "object" ? isObject(x) : typeof x === kind;
    if (!ok) problems.push(`has no ${kind} as its ${field}`);
  }
  for (const field of Object.keys(value)) {
    if (!Object.hasOwn(kinds, field)) problems.push(`holds "${field}", which core does not keep`);
  }
  return problems;
}

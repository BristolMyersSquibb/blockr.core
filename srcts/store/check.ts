// The checks a board passes after every delta and as it opens, each failure in
// a sentence: the invariants of core's objects, as R's validate_links() and
// the board's checks did, the schemas of the attributes, board options and
// keys of the front-end's state, and each key's own check. Where a delta
// changed only some objects, their shapes and schemas are checked alone, since
// nothing else can have broken them, while the invariants and the keys' checks
// run on the whole board every time.

import { graphOf } from "../graph/index.ts";
import type { Board, RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Changed } from "./commit.ts";
import { isId } from "./ids.ts";
import type { Registrations } from "./registrations.ts";
import { getAt, isObject, messageOf, own, pathText, quoted } from "./util.ts";

export function check(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  registrations: Registrations,
  changed?: Changed,
): string[] {
  const failures: string[] = [];

  if (!isId(board.id)) {
    failures.push(`The board's id "${board.id}" is empty or made of digits alone.`);
  }
  if (typeof board.name !== "string") {
    failures.push("The board's name is no string.");
  }
  if (typeof board.front_end.id !== "string") {
    failures.push("The board names no front-end by its id.");
  }

  checkBlocks(board, registry, registrations, changed, failures);
  checkLinks(board, registry, changed, failures);
  checkStacks(board, changed, failures);
  checkValues(board, registrations, changed, failures);
  checkKeys(board, registrations, changed, failures);

  return failures;
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
    const fields = shape(block, { registry_id: "string", state: "object", name: "string", attrs: "object" });
    if (fields.length > 0) {
      failures.push(`The block "${id}" ${fields.join(", ")}.`);
      continue;
    }
    if (own(registry, block.registry_id) === undefined) {
      failures.push(
        `The block "${id}" is of the class "${block.registry_id}", which the block registry lacks.`,
      );
    }
    for (const [name, value] of Object.entries(block.attrs)) {
      const attr = registrations.attribute("block", name);
      if (attr === undefined) {
        failures.push(`The block "${id}" has the attribute "${name}", which no package registered.`);
        continue;
      }
      for (const failure of attr.validate(value)) {
        failures.push(`The attribute "${name}" of block "${id}" ${failure}`);
      }
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
      const fields = shape(stack, { name: "string", blocks: "array" });
      if (fields.length > 0) {
        failures.push(`The stack "${id}" ${fields.join(", ")}.`);
        continue;
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

  for (const [name, value] of Object.entries(board.front_end.attrs)) {
    if (changed !== undefined && !changed.attrs.has(name)) continue;
    const attr = registrations.attribute("front_end", name);
    if (attr === undefined) {
      failures.push(`The front-end has the attribute "${name}", which no package registered.`);
      continue;
    }
    for (const failure of attr.validate(value)) {
      failures.push(`The front-end's attribute "${name}" ${failure}`);
    }
  }
}

function checkKeys(
  board: Board,
  registrations: Registrations,
  changed: Changed | undefined,
  failures: string[],
): void {
  for (const { def, validate } of registrations.keys()) {
    const at = JSON.stringify(def.path);
    const where = pathText(def.path);
    const value = getAt(board.front_end.state, def.path) as Json | undefined;

    if (def.kind === "value") {
      if (value !== undefined && (changed === undefined || changed.values.has(at))) {
        for (const failure of validate(value)) failures.push(`The value of ${where} ${failure}`);
      }
    } else if (value !== undefined) {
      if (!isObject(value)) {
        failures.push(`The key ${where} holds no collection.`);
        continue;
      }
      const entries = changed === undefined ? Object.keys(value) : (changed.entries.get(at) ?? []);
      for (const id of entries) {
        for (const failure of validate(own(value, id))) {
          failures.push(`The entry "${id}" of ${where} ${failure}`);
        }
      }
    }

    if (def.check === undefined || value === undefined) continue;
    try {
      for (const failure of def.check(value, board)) failures.push(`${where}: ${failure}`);
    } catch (error) {
      failures.push(`The key ${where} failed to check the board: ${messageOf(error)}`);
    }
  }
}

type Kind = "string" | "object" | "array";

// What an object of core's lacks or holds beyond its fields, which a delta
// built in plain JS or by a model may get wrong.
function shape(value: unknown, fields: Readonly<Record<string, Kind>>): string[] {
  if (!isObject(value)) return ["is no object"];
  const problems: string[] = [];
  for (const [field, kind] of Object.entries(fields)) {
    const x = value[field];
    const ok =
      kind === "array" ? Array.isArray(x) : kind === "object" ? isObject(x) : typeof x === kind;
    if (!ok) problems.push(`has no ${kind} as its ${field}`);
  }
  for (const field of Object.keys(value)) {
    if (!Object.hasOwn(fields, field)) problems.push(`holds "${field}", which core does not keep`);
  }
  return problems;
}

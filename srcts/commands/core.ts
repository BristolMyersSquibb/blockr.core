// Core's commands, which cover what core's plugins and the dock's actions did:
// adding, removing and renaming a block, setting and unsetting its state args,
// appending, prepending and inserting one on a link, adding, changing and
// removing links, adding, changing, renaming and removing stacks, renaming the
// board and setting a board option, adding, changing and removing the entries
// of a key of the front-end's state or setting its value, and setting an
// attribute of a block or of the front-end.
//
// Each reads the board and returns its deltas, leaving the checks of the board
// to the store, but for a block's state, which the store does not check, and
// which these commands check against the schema of the block's class. An edit
// that changes nothing is the store's to drop, which makes no step of it.

import { mintId, sentenceCase } from "../store/index.ts";
import type { Attribute, Entry, StateKey, Store } from "../store/index.ts";
import type {
  Block,
  BlockId,
  Board,
  LinkId,
  RegistryId,
  Stack,
  StackId,
} from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import {
  compile,
  getAt,
  isObject,
  jsonEqual,
  own,
  packageOf,
  quoted,
  without,
} from "../util/index.ts";
import type { Validate } from "../util/index.ts";
import { refusal } from "./commands.ts";
import type { Command } from "./commands.ts";
import * as schemas from "./schemas.ts";

type State = Readonly<Record<string, Json>>;

// A block that a command adds, of a class from the block registry, with the
// state its class starts a block with, changed by the state args given.
type NewBlock = Readonly<{
  registry_id: RegistryId;
  id?: BlockId;
  name?: string;
  state?: State;
}>;

const newBlock = {
  registry_id: { ...schemas.registryId, description: "The class of the new block." },
  id: {
    ...schemas.blockId,
    description: "The new block's id, which is minted where it is left out.",
  },
  name: {
    ...schemas.name,
    description: "The new block's name, which its class gives where it is left out.",
  },
  state: {
    ...schemas.state,
    description:
      "Values of the new block's state args, which the schema of its class types, in place of those its class starts a block with.",
  },
};

export function coreCommands(store: Store): Command<Json, unknown>[] {
  const validators = new WeakMap<RegistryEntry, Validate>();

  // Each way a block's state fails the schema of its class, in a clause.
  function stateFailures(entry: RegistryEntry, state: State): readonly string[] {
    let validate = validators.get(entry);
    if (validate === undefined) {
      validate = compile(`the state of the block class "${entry.id}"`, schemas.stateSchema(entry));
      validators.set(entry, validate);
    }
    return validate(state);
  }

  function classOf(command: string, registryId: RegistryId): RegistryEntry {
    const entry = own(store.blockRegistry(), registryId);
    if (entry === undefined) {
      throw refusal(command, [`The block registry has no block class "${registryId}".`]);
    }
    return entry;
  }

  // The id and the block that a command adds, its id minted where it is left
  // out, with its class.
  function added(
    command: string,
    board: Board,
    args: NewBlock,
  ): [BlockId, Block, RegistryEntry] {
    const entry = classOf(command, args.registry_id);
    const state = { ...entry.state, ...args.state };
    const failures = stateFailures(entry, state);
    if (failures.length > 0) {
      throw refusal(command, failures.map((failure) => `The state of the new block ${failure}`));
    }
    const id = args.id ?? mintId((id) => Object.hasOwn(board.blocks, id));
    return [
      id,
      { registry_id: entry.id, state, name: args.name ?? blockName(entry.id), attrs: {} },
      entry,
    ];
  }

  // The inputs of block `to` that no link feeds, or undefined where the board
  // lacks the block or the block registry its class. A variadic block's is an
  // empty input, which takes its link in link order. The link `except`, which
  // is about to change, feeds nothing.
  function freeInputs(board: Board, to: BlockId, except?: LinkId): readonly string[] | undefined {
    const block = own(board.blocks, to);
    if (block === undefined) return undefined;
    const inputs = own(store.blockRegistry(), block.registry_id)?.inputs;
    if (inputs === undefined) return undefined;
    if (inputs.includes("...")) return [""];
    const fed = new Set(
      Object.entries(board.links)
        .filter(([id, link]) => id !== except && link.to === to)
        .map(([, link]) => link.input),
    );
    return inputs.filter((input) => !fed.has(input));
  }

  // Whether a new link can go to block `to`, into the input given or a free
  // one.
  function linkable(board: Board, to: BlockId, given?: string): boolean {
    if (!Object.hasOwn(board.blocks, to)) return false;
    return named(given) || (freeInputs(board, to)?.length ?? 0) > 0;
  }

  // The input of block `to` that a new link goes to: the one given, and
  // otherwise the only free one, which for a unary block is its only input, or
  // an empty input for a variadic block. Where several inputs are free, the
  // args have to name one, since the input a link feeds changes what the
  // block computes, as the left and the right table of a merge.
  function linkInput(
    command: string,
    board: Board,
    to: BlockId,
    given?: string,
    except?: LinkId,
  ): string {
    if (named(given)) return given;
    const free = freeInputs(board, to, except) ?? [];
    return one(
      command,
      free,
      `The block "${to}" has no input that no link feeds.`,
      `The block "${to}" has the free inputs ${quoted(free)}, so the args have to name the one to use.`,
    );
  }

  // The input of a new block of the class that takes a link: the one given,
  // and otherwise its only input, or an empty input for a variadic class.
  // Where the class has several inputs, the args have to name one.
  function newInput(command: string, entry: RegistryEntry, given?: string): string {
    if (named(given)) return given;
    if (entry.inputs.includes("...")) return "";
    return one(
      command,
      entry.inputs,
      `The block class "${entry.id}" has no input to take a link.`,
      `The block class "${entry.id}" has the inputs ${quoted(entry.inputs)}, so the args have to name the one to use.`,
    );
  }

  function keyAt(path: readonly string[], kind: StateKey["kind"]): StateKey | undefined {
    const key = store
      .registered()
      .find((def): def is StateKey => "path" in def && jsonEqual(def.path, path));
    return key?.kind === kind ? key : undefined;
  }

  function attributeOf(on: Attribute["on"], name: string): Attribute | undefined {
    return store
      .registered()
      .find((def): def is Attribute => "on" in def && def.on === on && def.name === name);
  }

  const blocks = [
    command<NewBlock>({
      id: "blockr.core-add_block",
      label: "Add block",
      description:
        "Adds a block of a class from the block registry, with the state its class starts a block with, changed by the state args given.",
      args: object(newBlock, ["registry_id"]),
      run(board, args) {
        const [id, block] = added(this.id, board, args);
        return { blocks: { add: { [id]: block } } };
      },
    }),
    command<{ readonly id: BlockId }>({
      id: "blockr.core-remove_block",
      label: "Remove block",
      description: "Removes a block, with its links and its place in its stack.",
      args: object({ id: schemas.blockId }, ["id"]),
      enabled: (board, { id }) => Object.hasOwn(board.blocks, id),
      run: (_board, { id }) => ({ blocks: { rm: [id] } }),
    }),
    command<{ readonly id: BlockId; readonly name: string }>({
      id: "blockr.core-rename_block",
      label: "Rename block",
      description: "Gives a block a new name.",
      args: object({ id: schemas.blockId, name: schemas.name }, ["id", "name"]),
      enabled: (board, { id }) => Object.hasOwn(board.blocks, id),
      run: (_board, { id, name }) => ({ blocks: { mod: { [id]: { name } } } }),
    }),
    command<{ readonly id: BlockId; readonly state?: State; readonly unset?: readonly string[] }>({
      id: "blockr.core-set_block",
      label: "Set block state",
      description:
        "Sets values of a block's state args, which the schema of its class types, and unsets the state args named, as a block's UI submits an edit.",
      args: object(
        {
          id: schemas.blockId,
          state: { ...schemas.state, description: "Values of the block's state args to set." },
          unset: { ...schemas.stateArgs, description: "Names of the block's state args to unset." },
        },
        ["id"],
      ),
      enabled: (board, { id }) => Object.hasOwn(board.blocks, id),
      run(board, { id, state, unset }) {
        const block = own(board.blocks, id);
        if (block === undefined) return [];
        // A change may leave the state failing the schema where it failed
        // already, so that a state that an older file left failing can be
        // repaired one arg at a time, but it may not make it fail anew.
        const entry = classOf(this.id, block.registry_id);
        const before = new Set(stateFailures(entry, block.state));
        const failures = stateFailures(entry, {
          ...without(block.state, unset ?? []),
          ...state,
        }).filter((failure) => !before.has(failure));
        if (failures.length > 0) {
          throw refusal(this.id, failures.map((failure) => `The state of block "${id}" ${failure}`));
        }
        return {
          blocks: {
            mod: {
              [id]: { ...(state === undefined ? {} : { state }), ...(unset === undefined ? {} : { unset }) },
            },
          },
        };
      },
    }),
    command<NewBlock & { readonly from: BlockId; readonly input?: string }>({
      id: "blockr.core-append_block",
      label: "Append block",
      description:
        "Adds a block that takes the result of another, through a new link from that block into an input of the new block.",
      args: object(
        {
          from: { ...schemas.blockId, description: "The block whose result the new block takes." },
          ...newBlock,
          input: {
            ...schemas.input,
            description:
              "The input of the new block that takes the result, which may be left out where the new block has one input or is variadic.",
          },
        },
        ["from", "registry_id"],
      ),
      enabled: (board, { from }) => Object.hasOwn(board.blocks, from),
      run(board, args) {
        const [id, block, entry] = added(this.id, board, args);
        const input = newInput(this.id, entry, args.input);
        const link = mintId((id) => Object.hasOwn(board.links, id));
        return {
          blocks: { add: { [id]: block } },
          links: { add: { [link]: { from: args.from, to: id, input } } },
        };
      },
    }),
    command<NewBlock & { readonly to: BlockId; readonly input?: string }>({
      id: "blockr.core-prepend_block",
      label: "Prepend block",
      description:
        "Adds a block whose result another takes, through a new link from the new block into an input of that block.",
      args: object(
        {
          to: { ...schemas.blockId, description: "The block that takes the new block's result." },
          ...newBlock,
          input: {
            ...schemas.input,
            description:
              "The input of that block that takes the result, which may be left out where only one of its inputs is free or it is variadic.",
          },
        },
        ["to", "registry_id"],
      ),
      enabled: (board, { to, input }) => linkable(board, to, input),
      run(board, args) {
        const input = linkInput(this.id, board, args.to, args.input);
        const [id, block] = added(this.id, board, args);
        const link = mintId((id) => Object.hasOwn(board.links, id));
        return {
          blocks: { add: { [id]: block } },
          links: { add: { [link]: { from: id, to: args.to, input } } },
        };
      },
    }),
    command<NewBlock & { readonly link: LinkId; readonly input?: string }>({
      id: "blockr.core-insert_block",
      label: "Insert block",
      description:
        "Adds a block on a link, which it replaces with a link from the link's source into an input of the new block and one from the new block into the input that the link fed.",
      args: object(
        {
          link: { ...schemas.linkId, description: "The link that the new block goes on." },
          ...newBlock,
          input: {
            ...schemas.input,
            description:
              "The input of the new block that takes the result of the link's source, which may be left out where the new block has one input or is variadic.",
          },
        },
        ["link", "registry_id"],
      ),
      enabled: (board, { link }) => Object.hasOwn(board.links, link),
      run(board, args) {
        const split = own(board.links, args.link);
        if (split === undefined) return [];
        const [id, block, entry] = added(this.id, board, args);
        const input = newInput(this.id, entry, args.input);
        const near = mintId((id) => Object.hasOwn(board.links, id));
        const far = mintId((id) => Object.hasOwn(board.links, id) || id === near);

        // The link into the input that the split link fed takes its place
        // among the links, since a variadic block takes its links in order.
        return {
          blocks: { add: { [id]: block } },
          links: {
            rm: [args.link],
            add: {
              [near]: { from: split.from, to: id, input },
              [far]: { from: id, to: split.to, input: split.input },
            },
            before: { [far]: args.link },
          },
        };
      },
    }),
  ];

  const links = [
    command<{
      readonly from: BlockId;
      readonly to: BlockId;
      readonly input?: string;
      readonly id?: LinkId;
    }>({
      id: "blockr.core-add_link",
      label: "Add link",
      description:
        "Adds a link, through which a block takes the result of another as one of its inputs.",
      args: object(
        {
          from: { ...schemas.blockId, description: "The block whose result the link carries." },
          to: { ...schemas.blockId, description: "The block that takes the result." },
          input: {
            ...schemas.input,
            description:
              "The input of the block that takes the result, which may be left out where only one of its inputs is free or it is variadic.",
          },
          id: {
            ...schemas.linkId,
            description: "The new link's id, which is minted where it is left out.",
          },
        },
        ["from", "to"],
      ),
      enabled: (board, { from, to, input }) =>
        Object.hasOwn(board.blocks, from) && linkable(board, to, input),
      run(board, { from, to, input, id }) {
        const link = id ?? mintId((id) => Object.hasOwn(board.links, id));
        const resolved = linkInput(this.id, board, to, input);
        return { links: { add: { [link]: { from, to, input: resolved } } } };
      },
    }),
    command<{
      readonly id: LinkId;
      readonly from?: BlockId;
      readonly to?: BlockId;
      readonly input?: string;
    }>({
      id: "blockr.core-change_link",
      label: "Change link",
      description:
        "Changes the block a link comes from, the block it goes to or the input it feeds, keeping its id.",
      args: object(
        {
          id: schemas.linkId,
          from: { ...schemas.blockId, description: "The block whose result the link carries." },
          to: { ...schemas.blockId, description: "The block that takes the result." },
          input: {
            ...schemas.input,
            description:
              "The input of the block that takes the result, which the link keeps where it is left out, unless the link goes to another block, where it may be left out only if one input of that block is free or the block is variadic.",
          },
        },
        ["id"],
      ),
      enabled: (board, { id }) => Object.hasOwn(board.links, id),
      run(board, args) {
        const link = own(board.links, args.id);
        if (link === undefined) return [];
        const to = args.to ?? link.to;
        const input =
          args.input === undefined && to === link.to
            ? link.input
            : linkInput(this.id, board, to, args.input, args.id);
        return { links: { mod: { [args.id]: { from: args.from ?? link.from, to, input } } } };
      },
    }),
    command<{ readonly id: LinkId }>({
      id: "blockr.core-remove_link",
      label: "Remove link",
      description: "Removes a link.",
      args: object({ id: schemas.linkId }, ["id"]),
      enabled: (board, { id }) => Object.hasOwn(board.links, id),
      run: (_board, { id }) => ({ links: { rm: [id] } }),
    }),
  ];

  const stacks = [
    command<{
      readonly id?: StackId;
      readonly name?: string;
      readonly blocks?: readonly BlockId[];
    }>({
      id: "blockr.core-add_stack",
      label: "Add stack",
      description:
        "Adds a stack of blocks, taking each out of the stack it is in, since a block sits in one stack at most.",
      args: object({
        id: {
          ...schemas.stackId,
          description: "The new stack's id, which is minted where it is left out.",
        },
        name: { ...schemas.name, description: 'The new stack\'s name, "Stack" where it is left out.' },
        blocks: { ...schemas.blockIds, description: "The blocks of the new stack, in order." },
      }),
      run(board, { id, name = "Stack", blocks = [] }) {
        const stack = id ?? mintId((id) => Object.hasOwn(board.stacks, id));
        const left = leave(board, stack, blocks);
        return {
          stacks: {
            add: { [stack]: { name, blocks } },
            ...(Object.keys(left).length === 0 ? {} : { mod: left }),
          },
        };
      },
    }),
    command<{ readonly id: StackId; readonly blocks: readonly BlockId[] }>({
      id: "blockr.core-change_stack",
      label: "Change stack",
      description:
        "Sets the blocks of a stack, taking each out of the stack it is in, since a block sits in one stack at most.",
      args: object(
        {
          id: schemas.stackId,
          blocks: { ...schemas.blockIds, description: "The blocks of the stack, in order." },
        },
        ["id", "blocks"],
      ),
      enabled: (board, { id }) => Object.hasOwn(board.stacks, id),
      run: (board, { id, blocks }) => ({
        stacks: { mod: { ...leave(board, id, blocks), [id]: { blocks } } },
      }),
    }),
    command<{ readonly id: StackId; readonly name: string }>({
      id: "blockr.core-rename_stack",
      label: "Rename stack",
      description: "Gives a stack a new name.",
      args: object({ id: schemas.stackId, name: schemas.name }, ["id", "name"]),
      enabled: (board, { id }) => Object.hasOwn(board.stacks, id),
      run: (_board, { id, name }) => ({ stacks: { mod: { [id]: { name } } } }),
    }),
    command<{ readonly id: StackId }>({
      id: "blockr.core-remove_stack",
      label: "Remove stack",
      description: "Removes a stack, leaving its blocks on the board.",
      args: object({ id: schemas.stackId }, ["id"]),
      enabled: (board, { id }) => Object.hasOwn(board.stacks, id),
      run: (_board, { id }) => ({ stacks: { rm: [id] } }),
    }),
  ];

  const board = [
    command<{ readonly name: string }>({
      id: "blockr.core-rename_board",
      label: "Rename board",
      description: "Gives the board a new name.",
      args: object({ name: schemas.name }, ["name"]),
      run: (_board, { name }) => ({ name }),
    }),
    command<{ readonly id: string; readonly value: Json }>({
      id: "blockr.core-set_option",
      label: "Set board option",
      description: "Sets the value of a board option, which the board option's schema types.",
      args: object({ id: schemas.optionId, value: schemas.value }, ["id", "value"]),
      enabled: (_board, { id }) => own(store.options.registered(), id) !== undefined,
      run: (_board, { id, value }) => ({ options: { set: { [id]: value } } }),
    }),
  ];

  const frontEnd = [
    command<{ readonly path: readonly string[]; readonly id?: string; readonly entry: Entry }>({
      id: "blockr.core-add_front_end_entry",
      label: "Add front-end entry",
      description:
        "Adds an entry to a key of the front-end's state that holds a collection, which the key's schema types.",
      args: object(
        {
          path: schemas.path,
          id: {
            ...schemas.entryId,
            description: "The new entry's id, which is minted where it is left out.",
          },
          entry: { type: "object", description: "The new entry, which the key's schema types." },
        },
        ["path", "entry"],
      ),
      enabled: (_board, { path }) => keyAt(path, "collection") !== undefined,
      run(board, { path, id, entry }) {
        const entries = entriesAt(board, path);
        const key = id ?? mintId((id) => Object.hasOwn(entries, id));
        return { front_end: { state: [{ path, change: { add: { [key]: entry } } }] } };
      },
    }),
    command<{
      readonly path: readonly string[];
      readonly id: string;
      readonly set?: Entry;
      readonly unset?: readonly string[];
    }>({
      id: "blockr.core-change_front_end_entry",
      label: "Change front-end entry",
      description:
        "Sets and unsets fields of an entry of a key of the front-end's state that holds a collection, which the key's schema types.",
      args: object(
        {
          path: schemas.path,
          id: schemas.entryId,
          set: { type: "object", description: "Values of the entry's fields to set." },
          unset: {
            type: "array",
            items: { type: "string" },
            uniqueItems: true,
            description: "Names of the entry's fields to unset.",
          },
        },
        ["path", "id"],
      ),
      enabled: (board, { path, id }) =>
        keyAt(path, "collection") !== undefined && Object.hasOwn(entriesAt(board, path), id),
      run: (_board, { path, id, set, unset }) => ({
        front_end: {
          state: [
            {
              path,
              change: {
                mod: {
                  [id]: { ...(set === undefined ? {} : { set }), ...(unset === undefined ? {} : { unset }) },
                },
              },
            },
          ],
        },
      }),
    }),
    command<{ readonly path: readonly string[]; readonly id: string }>({
      id: "blockr.core-remove_front_end_entry",
      label: "Remove front-end entry",
      description: "Removes an entry of a key of the front-end's state that holds a collection.",
      args: object({ path: schemas.path, id: schemas.entryId }, ["path", "id"]),
      enabled: (board, { path, id }) =>
        keyAt(path, "collection") !== undefined && Object.hasOwn(entriesAt(board, path), id),
      run: (_board, { path, id }) => ({ front_end: { state: [{ path, change: { rm: [id] } }] } }),
    }),
    command<{ readonly path: readonly string[]; readonly value: Json }>({
      id: "blockr.core-set_front_end_value",
      label: "Set front-end value",
      description:
        "Sets the value of a key of the front-end's state that holds a value, which the key's schema types.",
      args: object({ path: schemas.path, value: schemas.value }, ["path", "value"]),
      enabled: (_board, { path }) => keyAt(path, "value") !== undefined,
      run: (_board, { path, value }) => ({ front_end: { state: [{ path, change: { set: value } }] } }),
    }),
    command<{ readonly id: BlockId; readonly name: string; readonly value: Json }>({
      id: "blockr.core-set_block_attribute",
      label: "Set block attribute",
      description:
        "Sets an attribute of a block, a value that is not its state, such as which of its sections show, which the attribute's schema types.",
      args: object(
        { id: schemas.blockId, name: schemas.attribute, value: schemas.value },
        ["id", "name", "value"],
      ),
      enabled: (board, { id, name }) =>
        Object.hasOwn(board.blocks, id) && attributeOf("block", name) !== undefined,
      run: (_board, { id, name, value }) => ({
        blocks: { mod: { [id]: { attrs: { set: { [name]: value } } } } },
      }),
    }),
    command<{ readonly name: string; readonly value: Json }>({
      id: "blockr.core-set_front_end_attribute",
      label: "Set front-end attribute",
      description: "Sets an attribute of the front-end, which the attribute's schema types.",
      args: object({ name: schemas.attribute, value: schemas.value }, ["name", "value"]),
      enabled: (_board, { name }) => attributeOf("front_end", name) !== undefined,
      run: (_board, { name, value }) => ({ front_end: { attrs: { set: { [name]: value } } } }),
    }),
  ];

  return [...blocks, ...links, ...stacks, ...board, ...frontEnd];
}

// The name of a new block, made as R makes one from its class, from the name
// in its registry id: "Head" for "blockr.core-head_block".
export function blockName(registryId: RegistryId): string {
  const pkg = packageOf(registryId);
  const name = pkg === null ? registryId : registryId.slice(pkg.length + 1);
  return sentenceCase(name.replace(/_block$/, ""));
}

// The changes that take the blocks out of every stack but `id`, since a block
// sits in one stack at most.
function leave(
  board: Board,
  id: StackId,
  blocks: readonly BlockId[],
): Record<StackId, Partial<Stack>> {
  const moving = new Set(blocks);
  return Object.fromEntries(
    Object.entries(board.stacks)
      .filter(([other, stack]) => other !== id && stack.blocks.some((block) => moving.has(block)))
      .map(([other, stack]) => [
        other,
        { blocks: stack.blocks.filter((block) => !moving.has(block)) },
      ]),
  );
}

function entriesAt(board: Board, path: readonly string[]): Readonly<Record<string, unknown>> {
  const at = getAt(board.front_end.state, path);
  return isObject(at) ? at : {};
}

function object(
  properties: Readonly<Record<string, JsonSchema>>,
  required: readonly string[] = [],
): JsonSchema {
  return { type: "object", properties, required, additionalProperties: false };
}

function named(input: string | undefined): input is string {
  return input !== undefined && input !== "";
}

// The one input of several, or a refusal where there is none or more than one.
function one(command: string, inputs: readonly string[], none: string, several: string): string {
  const [input, ...rest] = inputs;
  if (input === undefined) throw refusal(command, [none]);
  if (rest.length > 0) throw refusal(command, [several]);
  return input;
}

// One of core's commands, with its args typed as their schema types them, which
// the command registry checks them against before it calls the command. Its
// methods are bound to it, so that they read its id as `this.id` however they
// are called, such as once a caller has taken `run` off the command.
function command<A extends Json>(
  def: Command<A> & ThisType<Command<A>>,
): Command<Json, unknown> {
  return {
    ...def,
    run: def.run.bind(def),
    ...(def.enabled === undefined ? {} : { enabled: def.enabled.bind(def) }),
  };
}


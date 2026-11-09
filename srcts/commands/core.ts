// Core's commands, which cover what core's plugins of today and the dock's
// actions did: adding, removing and renaming a block, setting and unsetting its
// state args, appending, prepending and inserting one on a link, adding,
// changing and removing links, adding, changing, renaming and removing stacks,
// renaming the board and setting a board option, and setting a plugin's
// state, while the app's opening board sets which plugins a board holds.
// Besides core's command for any plugin's state, a plugin class's package
// registers commands of its own for what its plugins keep.
//
// Each reads the board and returns its deltas, leaving the checks of the board
// to the store, but for a block's state, which the store checks only as it
// opens a board, and which these commands check against the schema of the
// block's class. An edit that changes nothing is the store's to drop, which
// makes no step of it.

import { mintId, pathOf, sentenceCase, stateFailures } from "../store/index.ts";
import type { Patch, Store } from "../store/index.ts";
import type {
  Block,
  BlockId,
  Board,
  LinkId,
  PluginId,
  RegistryId,
  Stack,
  StackId,
} from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { own, packageOf, quoted, without } from "../util/index.ts";
import { refusal } from "./commands.ts";
import type { Command } from "./commands.ts";
import * as schemas from "./schemas.ts";

type State = Readonly<Record<string, Json>>;

// A change of a plugin's state, at a path within the state, as core's command
// for a plugin's state takes it, declared with `type`, as args are Json.
// eslint-disable-next-line @typescript-eslint/consistent-type-definitions -- an interface would not be assignable to Json
type StatePatch = {
  readonly op: Patch["op"];
  readonly path: string;
  readonly value?: Json;
};

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

export interface CoreCommandsOptions {
  // Whether R's reply to `hello` locks the board, which leaves core's command
  // for a plugin's state not enabled.
  readonly locked?: boolean;
}

export function coreCommands(store: Store, options: CoreCommandsOptions = {}): Command<Json, unknown>[] {
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
      { registry_id: entry.id, state, name: args.name ?? blockName(entry.id) },
      entry,
    ];
  }

  // Whether a new link can go to block `to`, into the input given or a free
  // one.
  function linkable(board: Board, to: BlockId, given?: string): boolean {
    if (!Object.hasOwn(board.blocks, to)) return false;
    return named(given) || (freeInputs(board, store.blockRegistry(), to)?.length ?? 0) > 0;
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
    const free = freeInputs(board, store.blockRegistry(), to, except) ?? [];
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
        // already, as a delta applied without a command may leave it, so that
        // such a state can be repaired one arg at a time, but it may not make
        // it fail anew.
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
      readonly color?: string | null;
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
        color: {
          ...schemas.color,
          description:
            "The new stack's colour, which front-ends draw it in, or null for none. The stack gets null where this is left out, and a front-end draws a stack without a colour in a default of its own.",
        },
      }),
      run(board, { id, name = "Stack", blocks = [], color = null }) {
        const stack = id ?? mintId((id) => Object.hasOwn(board.stacks, id));
        const left = leave(board, stack, blocks);
        return {
          stacks: {
            add: { [stack]: { name, blocks, color } },
            ...(Object.keys(left).length === 0 ? {} : { mod: left }),
          },
        };
      },
    }),
    command<{
      readonly id: StackId;
      readonly blocks?: readonly BlockId[];
      readonly color?: string | null;
    }>({
      id: "blockr.core-change_stack",
      label: "Change stack",
      description:
        "Changes the blocks of a stack or its colour, taking each block out of the stack it is in, since a block sits in one stack at most.",
      args: object(
        {
          id: schemas.stackId,
          blocks: {
            ...schemas.blockIds,
            description: "The blocks of the stack, in order, which it keeps where they are left out.",
          },
          color: {
            ...schemas.color,
            description:
              "The stack's colour, which front-ends draw it in, or null to clear it. A front-end draws a stack without a colour in a default of its own. The stack keeps its colour where this is left out.",
          },
        },
        ["id"],
      ),
      enabled: (board, { id }) => Object.hasOwn(board.stacks, id),
      run: (board, { id, blocks, color }) => ({
        stacks: {
          mod: {
            ...(blocks === undefined ? {} : leave(board, id, blocks)),
            [id]: {
              ...(blocks === undefined ? {} : { blocks }),
              ...(color === undefined ? {} : { color }),
            },
          },
        },
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

  const plugins = [
    command<{ readonly plugin: PluginId; readonly patches: readonly StatePatch[] }>({
      id: "blockr.core-set_plugin_state",
      label: "Set plugin state",
      description:
        "Changes a plugin's state by patches at paths within that state, as JSON Patch has them, which the schema of the plugin's class types. It is not enabled on a locked board, where only the commands of a plugin's class know what the plugin locks.",
      args: object(
        {
          plugin: { ...schemas.pluginId, description: "The plugin whose state the patches change." },
          patches: schemas.statePatches,
        },
        ["plugin", "patches"],
      ),
      enabled: (board, { plugin }) => options.locked !== true && Object.hasOwn(board.plugins, plugin),
      run: (_board, { plugin, patches }) => ({
        patches: patches.map((patch) => ({ ...patch, path: `${pathOf(["plugins", plugin, "state"])}${patch.path}` })),
      }),
    }),
  ];

  return [...blocks, ...links, ...stacks, ...board, ...plugins];
}

// The inputs of block `to` that no link feeds, by which the commands find the
// input that a new link goes to, and a front-end the inputs that it offers.
// It is undefined where the board lacks the block or the block registry its
// class. A variadic block's is an empty input, which takes its link in link
// order. The link `except`, which is about to change, feeds nothing.
export function freeInputs(
  board: Pick<Board, "blocks" | "links">,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  to: BlockId,
  except?: LinkId,
): readonly string[] | undefined {
  const block = own(board.blocks, to);
  if (block === undefined) return undefined;
  const inputs = own(registry, block.registry_id)?.inputs;
  if (inputs === undefined) return undefined;
  if (inputs.includes("...")) return [""];
  const fed = new Set(
    Object.entries(board.links)
      .filter(([id, link]) => id !== except && link.to === to)
      .map(([, link]) => link.input),
  );
  return inputs.filter((input) => !fed.has(input));
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


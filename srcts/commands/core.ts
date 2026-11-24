// Core's commands, which cover what core's plugins of today and the dock's
// actions did: adding, changing and removing blocks, links and stacks, changing
// the board and setting a board option, and setting a plugin's state, while
// the app's opening board sets which plugins a board holds. A block that a
// command adds starts with a lock of null, which follows the board lock.
// Appending, prepending and inserting a block are batches of these, so that
// each rule about links lives in add_link alone
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#commands):
// - Appending block n after block a runs add_block for n, then add_link from a
//   to n.
// - Prepending block n before block b runs add_block for n, then add_link from
//   n to b.
// - Inserting block n on link l runs add_block for n, then add_link from the
//   source of l to n, then change_link, which has l come from n. So l keeps
//   its id, its input and its place among the links.
// A batch hands no value from one command to the next, so the caller fixes the
// id of n first, and mints it with CoreApi's mintId() where it has none.
// Besides core's command for any plugin's state, a plugin class's package
// registers commands of its own for what its plugins keep.
//
// Each reads the board and returns its deltas, leaving the checks of the board
// to the store. A command that sets a block's state checks it against the
// schema of the block's class before the store does, so that the error names
// the command. The preflights of the commands that add or change a link check
// the link before the store does, too. A link fails them where it would join a
// block to itself, close a cycle or feed an input that another link feeds, and
// the refusal says which, in a sentence for each failure. So a menu that offers
// only the links whose preflight passes offers none that the store refuses.
// An edit that changes nothing is the store's to drop, which makes no step of
// it.
// They read the board lock and mint ids through the context that the command
// registry hands every command, as a plugin class's commands do. So the ids
// that they mint for what they add take the shape that the blockr option
// rand_id names, as R's rand_names() draws them.

import { createGraph } from "../graph/index.ts";
import { pathFromSegments, stateFailures } from "../store/index.ts";
import type { BlockMod, Patch, Store } from "../store/index.ts";
import type {
  Block,
  BlockId,
  Board,
  LinkId,
  PluginClassId,
  RegistryId,
  Stack,
  StackId,
} from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { own, quoted, without } from "../util/index.ts";
import { refusal } from "./commands.ts";
import type { Command, CommandContext } from "./commands.ts";
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

export function coreCommands(store: Store): Command<Json, unknown>[] {
  function registryEntry(command: string, registryId: RegistryId): RegistryEntry {
    const entry = own(store.blockRegistry(), registryId);
    if (entry === undefined) {
      throw refusal(command, [`The block registry has no block class "${registryId}".`]);
    }
    return entry;
  }

  // The id and the block that a command adds, its id minted where it is left
  // out.
  function added(
    command: string,
    board: Board,
    args: NewBlock,
    mintId: CommandContext["mintId"],
  ): [BlockId, Block] {
    const entry = registryEntry(command, args.registry_id);
    const state = { ...entry.state, ...args.state };
    const failures = stateFailures(entry, state);
    if (failures.length > 0) {
      throw refusal(command, failures.map((failure) => `The state of the new block ${failure}`));
    }
    const id = args.id ?? mintId((id) => Object.hasOwn(board.blocks, id));
    return [id, { registry_id: entry.id, state, name: args.name ?? entry.block_name, locked: null }];
  }

  // Why block `to` cannot take one more link, into the input given or else a
  // free one, with a sentence for each failure: another link feeds the input
  // given, or, where none is given, the block has no free input. The sentence
  // for that comes from noFreeInput(). A variadic block takes any number of
  // links, under any name. The caller checks that the board holds the block.
  function inputFailures(board: Board, to: BlockId, given?: string): string[] {
    const registry = store.blockRegistry();
    if (!named(given)) {
      const free = freeInputs(board, registry, to) ?? [];
      return free.length > 0 ? [] : [noFreeInput(board, to)];
    }
    const other = linkFeeding(board, registry, to, given);
    return other === undefined ? [] : [`The link "${other}" feeds the input "${given}" of block "${to}" already.`];
  }

  // Why no new link can go to block `to`, which has no free input, in a
  // sentence: the board lacks the block, the block registry lacks its class,
  // its class takes no input, or links feed every input that it has.
  function noFreeInput(board: Board, to: BlockId): string {
    const block = own(board.blocks, to);
    if (block === undefined) return `The board holds no block "${to}".`;
    const entry = own(store.blockRegistry(), block.registry_id);
    if (entry === undefined) return `The block registry has no block class "${block.registry_id}".`;
    if (entry.inputs.length === 0) {
      return `The block "${to}" takes no input, since its class "${entry.id}" has none.`;
    }
    return `Every input of the block "${to}" takes a link already.`;
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
      noFreeInput(board, to),
      `The block "${to}" has the free inputs ${quoted(free)}, so the args have to name the one to use.`,
    );
  }

  const blocks = [
    command<NewBlock>({
      id: "blockr.core-add_block",
      label: "Add block",
      description:
        "Adds a block of a class from the block registry, with the state its class starts a block with, changed by the state args given. To add a block that reads from another, give the new block an id, and link it with blockr.core-add_link in the same batch, after this command.",
      args: object(newBlock, ["registry_id"]),
      run(board, args, { mintId }) {
        const [id, block] = added(this.id, board, args, mintId);
        return { blocks: { add: { [id]: block } } };
      },
    }),
    command<{ readonly id: BlockId }>({
      id: "blockr.core-remove_block",
      label: "Remove block",
      description: "Removes a block, with its links and its place in its stack.",
      args: object({ id: schemas.blockId }, ["id"]),
      preflight: (board, { id }) => Object.hasOwn(board.blocks, id),
      run: (_board, { id }) => ({ blocks: { rm: [id] } }),
    }),
    command<{ readonly id: BlockId } & Omit<BlockMod, "edit">>({
      id: "blockr.core-change_block",
      label: "Change block",
      description:
        "Changes a block's name, its lock or its state. It sets the state args given, which the schema of the block's class types, and unsets those named, as a block's UI submits an edit. The block keeps what the args leave out.",
      args: object(
        {
          id: schemas.blockId,
          name: { ...schemas.name, description: "The block's name, which it keeps where this is left out." },
          locked: {
            ...schemas.locked,
            description:
              "The block's lock, which fixes its state where the board lock is on: true locks the block, false leaves it open to readers, and null has it follow the board lock. The block keeps its lock where this is left out.",
          },
          state: { ...schemas.state, description: "Values of the block's state args to set." },
          unset: { ...schemas.stateArgs, description: "Names of the block's state args to unset." },
        },
        ["id"],
      ),
      preflight: (board, { id }) => Object.hasOwn(board.blocks, id),
      // The args but the id are the block's change, as the schema admits no
      // other args.
      run(board, { id, ...change }) {
        const block = own(board.blocks, id);
        if (block === undefined) return [];
        const { state, unset } = change;
        if (state !== undefined || unset !== undefined) {
          const entry = registryEntry(this.id, block.registry_id);
          const failures = stateFailures(entry, { ...without(block.state, unset ?? []), ...state });
          if (failures.length > 0) {
            throw refusal(this.id, failures.map((failure) => `The state of block "${id}" ${failure}`));
          }
        }
        return { blocks: { mod: { [id]: change } } };
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
      preflight: (board, { from, to, input }, { refusal }) =>
        Object.hasOwn(board.blocks, from) &&
        Object.hasOwn(board.blocks, to) &&
        passes([...cycleFailures(board, from, to), ...inputFailures(board, to, input)], refusal),
      run(board, { from, to, input, id }, { mintId }) {
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
      readonly before?: LinkId | null;
    }>({
      id: "blockr.core-change_link",
      label: "Change link",
      description:
        "Changes the block a link comes from, the block it goes to, the input it feeds or its place among the links into its block, keeping its id. Where the link keeps its block and another link feeds the input given, that other link takes the link's old input, so that the two links swap their inputs, as the links into a merge block's x and y. A variadic block takes its links in their order on the board, so an rbind block binds first the rows of the block that its first link comes from.",
      args: object(
        {
          id: schemas.linkId,
          from: { ...schemas.blockId, description: "The block whose result the link carries." },
          to: { ...schemas.blockId, description: "The block that takes the result." },
          input: {
            ...schemas.input,
            description:
              "The input of the block that takes the result, which the link keeps where it is left out, unless the link goes to another block, where it may be left out only if one input of that block is free or the block is variadic. Where the link keeps its block, an input that another link feeds swaps the inputs of the two links.",
          },
          before: {
            ...schemas.linkId,
            type: ["string", "null"],
            description:
              "The link that the link goes before, among the links into the block that it goes to, or null to put it last among them. The link keeps its place where this is left out. A block that is not variadic takes each input by its name, so the command refuses this for a link into one.",
          },
        },
        ["id"],
      ),
      preflight(board, args, { refusal }) {
        const link = own(board.links, args.id);
        if (link === undefined) return false;
        const from = args.from ?? link.from;
        const to = args.to ?? link.to;
        if (!Object.hasOwn(board.blocks, from) || !Object.hasOwn(board.blocks, to)) return false;
        // Where the link keeps its block, an input that another link feeds
        // swaps the inputs of the two links, as run() has it.
        const inputs = to === link.to ? [] : inputFailures(board, to, args.input);
        return passes([...cycleFailures(board, from, to, args.id), ...inputs], refusal);
      },
      run(board, args) {
        const link = own(board.links, args.id);
        if (link === undefined) return [];
        const to = args.to ?? link.to;
        const input =
          args.input === undefined && to === link.to
            ? link.input
            : linkInput(this.id, board, to, args.input, args.id);
        const changed = { [args.id]: { from: args.from ?? link.from, to, input } };
        // Where another link into the block feeds the input, that link takes
        // the link's old input in the same delta. The store checks the board
        // that the delta leaves, so it takes the swap, and one undo takes it
        // back.
        const other = to === link.to ? linkFeeding(board, store.blockRegistry(), to, input, args.id) : undefined;
        const mod = other === undefined ? changed : { ...changed, [other]: { input: link.input } };
        const { before } = args;
        if (before === undefined) return { links: { mod } };
        const block = own(board.blocks, to);
        if (block !== undefined && !registryEntry(this.id, block.registry_id).inputs.includes("...")) {
          throw refusal(this.id, [
            `The block "${to}" is not variadic and takes each input by its name, so the order of its links changes nothing that it computes. The arg "input" changes the input that a link into it feeds, and "before" has to be left out.`,
          ]);
        }
        if (before !== null) {
          if (before === args.id) {
            throw refusal(this.id, [`The link "${args.id}" cannot go before itself.`]);
          }
          const next = own(board.links, before);
          if (next !== undefined && next.to !== to) {
            throw refusal(this.id, [
              `The link "${args.id}" goes into block "${to}" and the link "${before}" into block "${next.to}", so the args have to name a link into block "${to}".`,
            ]);
          }
        }
        // The store moves the link before it applies the change, so the link
        // takes its place among the links into the block that it goes to after
        // the change.
        const place = before ?? lastPlace(board, to, args.id);
        return { links: { mod, ...(place === undefined ? {} : { before: { [args.id]: place } }) } };
      },
    }),
    command<{ readonly id: LinkId }>({
      id: "blockr.core-remove_link",
      label: "Remove link",
      description: "Removes a link.",
      args: object({ id: schemas.linkId }, ["id"]),
      preflight: (board, { id }) => Object.hasOwn(board.links, id),
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
        name: {
          ...schemas.name,
          description: 'The new stack\'s name, the first free "Stack n" where it is left out.',
        },
        blocks: { ...schemas.blockIds, description: "The blocks of the new stack, in order." },
        color: {
          ...schemas.color,
          description:
            "The new stack's colour, which frontends draw it in, or null for none. The stack gets null where this is left out, and a frontend draws a stack without a colour in a default of its own.",
        },
      }),
      run(board, { id, name, blocks = [], color = null }, { mintId }) {
        const stack = id ?? mintId((id) => Object.hasOwn(board.stacks, id));
        const left = leave(board, stack, blocks);
        return {
          stacks: {
            add: {
              [stack]: { name: name ?? suggestedName(Object.values(board.stacks)), blocks, color },
            },
            ...(Object.keys(left).length === 0 ? {} : { mod: left }),
          },
        };
      },
    }),
    command<{ readonly id: StackId } & Partial<Stack>>({
      id: "blockr.core-change_stack",
      label: "Change stack",
      description:
        "Changes the name of a stack, its blocks or its colour, taking each block out of the stack it is in, since a block sits in one stack at most.",
      args: object(
        {
          id: schemas.stackId,
          name: { ...schemas.name, description: "The stack's name, which it keeps where this is left out." },
          blocks: {
            ...schemas.blockIds,
            description: "The blocks of the stack, in order, which it keeps where they are left out.",
          },
          color: {
            ...schemas.color,
            description:
              "The stack's colour, which frontends draw it in, or null to clear it. A frontend draws a stack without a colour in a default of its own. The stack keeps its colour where this is left out.",
          },
        },
        ["id"],
      ),
      preflight: (board, { id }) => Object.hasOwn(board.stacks, id),
      // The args but the id are the stack's change, as for change_block.
      run: (board, { id, ...change }) => ({
        stacks: { mod: { ...(change.blocks === undefined ? {} : leave(board, id, change.blocks)), [id]: change } },
      }),
    }),
    command<{ readonly id: StackId }>({
      id: "blockr.core-remove_stack",
      label: "Remove stack",
      description: "Removes a stack, leaving its blocks on the board.",
      args: object({ id: schemas.stackId }, ["id"]),
      preflight: (board, { id }) => Object.hasOwn(board.stacks, id),
      run: (_board, { id }) => ({ stacks: { rm: [id] } }),
    }),
  ];

  const board = [
    command<{ readonly name?: string }>({
      id: "blockr.core-change_board",
      label: "Change board",
      description: "Changes the board's name. The board keeps what the args leave out.",
      args: object({ name: { ...schemas.name, description: "The board's name, which it keeps where this is left out." } }),
      run: (_board, change) => change,
    }),
    command<{ readonly id: string; readonly value: Json }>({
      id: "blockr.core-set_option",
      label: "Set board option",
      description: "Sets the value of a board option, which the board option's schema types.",
      args: object({ id: schemas.optionId, value: schemas.value }, ["id", "value"]),
      preflight: (_board, { id }) => own(store.options.registered(), id) !== undefined,
      run: (_board, { id, value }) => ({ options: { set: { [id]: value } } }),
    }),
  ];

  const plugins = [
    command<{ readonly plugin: PluginClassId; readonly patches: readonly StatePatch[] }>({
      id: "blockr.core-set_plugin_state",
      label: "Set plugin state",
      description:
        "Changes a plugin's state by patches at paths within that state, as JSON Patch has them, which the schema of the plugin's class types. It fails its preflight on a locked board, where only the commands of a plugin's class know what the plugin locks.",
      args: object(
        {
          plugin: {
            ...schemas.pluginClassId,
            description: "The id of the class of the plugin whose state the patches change.",
          },
          patches: schemas.statePatches,
        },
        ["plugin", "patches"],
      ),
      preflight: (board, { plugin }, { lock }) => !lock.board && Object.hasOwn(board.plugins, plugin),
      run: (_board, { plugin, patches }) => ({
        patches: patches.map((patch) => ({ ...patch, path: `${pathFromSegments(["plugins", plugin])}${patch.path}` })),
      }),
    }),
  ];

  return [...blocks, ...links, ...stacks, ...board, ...plugins];
}

// The inputs of block `to` that no link feeds, by which the commands find the
// input that a new link goes to, and a frontend the inputs that it offers.
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

// Whether block `to` takes a new link: a variadic block always, and another
// block where one of its inputs is free. A block without inputs takes none,
// and neither does one that the board or the block registry lacks.
export function takesLink(
  board: Pick<Board, "blocks" | "links">,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  to: BlockId,
): boolean {
  return (freeInputs(board, registry, to)?.length ?? 0) > 0;
}

// The link into block `to` that feeds the input given, other than the link
// `except`. By it, change_link finds the link to swap inputs with, and a
// frontend the link that a change would swap with. It is undefined where no
// other link feeds the input. It is undefined for a variadic block as well,
// since its links give their inputs names, or none, rather than feed the
// inputs of its class.
export function linkFeeding(
  board: Pick<Board, "blocks" | "links">,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  to: BlockId,
  input: string,
  except?: LinkId,
): LinkId | undefined {
  const block = own(board.blocks, to);
  const inputs = block === undefined ? undefined : own(registry, block.registry_id)?.inputs;
  if (inputs === undefined || inputs.includes("...")) return undefined;
  return Object.entries(board.links).find(([id, link]) => id !== except && link.to === to && link.input === input)?.[0];
}

// "Stack 1", or the first "Stack n" that no stack has.
export function suggestedName(stacks: readonly Stack[]): string {
  const names = new Set(stacks.map((stack) => stack.name));
  let n = 1;
  while (names.has(`Stack ${String(n)}`)) n++;
  return `Stack ${String(n)}`;
}

// Why a link from block `from` to block `to` would make the board's links form
// a cycle, with a sentence for the failure: the link joins a block to itself,
// or a path of other links leads from block `to` back to block `from`. The
// link `except`, which a command is about to change, is no part of a path.
function cycleFailures(board: Board, from: BlockId, to: BlockId, except?: LinkId): string[] {
  if (from === to) return [`The link would join block "${to}" to itself.`];
  const links = except === undefined ? board.links : without(board.links, [except]);
  const cycle = createGraph({ blocks: board.blocks, links }).path(to, from);
  return cycle === undefined ? [] : [`The link would close a cycle through the blocks ${quoted(cycle)}.`];
}

// Passes a preflight where nothing fails, and otherwise refuses the command
// with a sentence for each failure, so that the refusal says why.
function passes(failures: readonly string[], refusal: CommandContext["refusal"]): true {
  if (failures.length > 0) throw refusal(failures);
  return true;
}

// Where the link `id` goes to be the last of the links into block `to`: before
// the link that follows the block's last other link on the board, or null
// where none follows it. So the link moves no further on the board than it
// has to. It is undefined where no other link goes into the block.
function lastPlace(board: Pick<Board, "links">, to: BlockId, id: LinkId): LinkId | null | undefined {
  const others = Object.keys(board.links).filter((other) => other !== id);
  const last = others.filter((other) => own(board.links, other)?.to === to).at(-1);
  if (last === undefined) return undefined;
  return others[others.indexOf(last) + 1] ?? null;
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
    ...(def.preflight === undefined ? {} : { preflight: def.preflight.bind(def) }),
  };
}


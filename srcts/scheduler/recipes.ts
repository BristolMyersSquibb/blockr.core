// What a block's links make of the inputs its class declares: the inputs of its
// recipe, and the inputs that keep it waiting.

import type { Graph } from "../graph/index.ts";
import type { BlockId, Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { own } from "../util/index.ts";

// An input of a block's recipe, named by the input it feeds. It names the
// upstream block whose result the input binds. For a block that `serve()`
// serves alone, R binds an input itself, and the input comes without a block.
export interface Input {
  readonly input: string;
  readonly block?: BlockId;
}

export interface Inputs {
  // The inputs of the block's recipe, in the order of its class's inputs. A
  // variadic block's come in link order.
  readonly recipe: readonly Input[];
  // The inputs that no link feeds and R does not bind. They keep the block
  // waiting. A variadic block that has no link lacks its input "...".
  readonly unlinked: readonly string[];
}

// The inputs of every block of the board, by block id.
export function inputsOf(
  board: Pick<Board, "blocks" | "links">,
  graph: Graph,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  bound: ReadonlySet<string>,
): Map<BlockId, Inputs> {
  const inputs = new Map<BlockId, Inputs>();
  for (const [id, block] of Object.entries(board.blocks)) {
    const declared = own(registry, block.registry_id)?.inputs ?? [];
    const links = graph.linksInto(id).flatMap((link) => own(board.links, link) ?? []);
    inputs.set(
      id,
      declared.includes("...")
        ? variadic(declared, links, bound)
        : fixed(declared, links, bound),
    );
  }
  return inputs;
}

// A class with inputs of fixed names takes each from the link that feeds it,
// or from R where R binds it.
function fixed(
  declared: readonly string[],
  links: readonly { readonly from: BlockId; readonly input: string }[],
  bound: ReadonlySet<string>,
): Inputs {
  const recipe: Input[] = [];
  const unlinked: string[] = [];
  for (const name of declared) {
    const link = links.find((candidate) => candidate.input === name);
    if (link !== undefined) recipe.push({ input: name, block: link.from });
    else if (bound.has(name)) recipe.push({ input: name });
    else unlinked.push(name);
  }
  return { recipe, unlinked };
}

// A variadic class takes every link in link order, and then the inputs that R
// binds itself. Its inputs of fixed names still need a link or R.
function variadic(
  declared: readonly string[],
  links: readonly { readonly from: BlockId; readonly input: string }[],
  bound: ReadonlySet<string>,
): Inputs {
  const recipe: Input[] = links.map((link) => ({ input: link.input, block: link.from }));
  for (const name of bound) {
    if (!links.some((link) => link.input === name)) recipe.push({ input: name });
  }
  const unlinked = recipe.length === 0 ? ["..."] : [];
  for (const name of declared) {
    if (name === "...") continue;
    if (!links.some((link) => link.input === name) && !bound.has(name)) unlinked.push(name);
  }
  return { recipe, unlinked };
}

// Whether two recipes take the same inputs from the same blocks.
export function sameInputs(a: readonly Input[], b: readonly Input[]): boolean {
  return (
    a.length === b.length &&
    a.every((input, i) => input.input === b[i]?.input && input.block === b[i].block)
  );
}

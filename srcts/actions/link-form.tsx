// The form of a link, the advanced action of Create link and of Edit link
// (./links.ts), as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action).
// It offers only links that the store takes:
// - As a link's target, a block with an input that no other link feeds, or a
//   variadic block, which takes any number of links. A block without inputs
//   takes none.
// - Neither end where the link would close a cycle, such as a link from a
//   block into a block upstream of it, nor a link from a block into itself.
// - Where the target's inputs are fixed, one that no other link feeds, which
//   the user picks where there are several. The form that edits a link offers
//   every input of the block that the link goes to, since change_link swaps
//   the inputs of the link and of the link that feeds the input picked, and
//   says so below the input.
// - Where the target is variadic, a name for the input, which the user may
//   give, as `b2` in `rbind(a, b2 = b)`, or leave empty for an input without a
//   name. A name that another link into the block gives already does not do.
//   Such a block takes its links in their order on the board, and the form
//   that edits a link offers the link's place among the block's links too.
// Each form runs nothing before the user submits it, through its Add link
// button or the Apply button of the form that edits, and then hands done()
// the batch, which runs as one step of the user:
// - The form that adds a link, Create link's, starts from the block that its
//   target names, as the link's source, or from no block on the board, or
//   from the blocks and the input that the open icon of the quick action
//   hands it. It mints the link's ID through CoreApi's mintId() and adds the
//   link with add_link.
//   On a block, the link starts or ends at that block, as add_link_role has
//   it (./roles.ts). So the form names the block in a line above its fields, as
//   "A link from "Scatter".", in place of the select of that end, and offers a
//   select of the other end alone. The block is the link's target where the
//   open icon of a "Take from" row handed it as the target, as "A link into
//   "Merge".", and its source otherwise.
// - The form that edits a link, Edit link's, changes it with change_link,
//   which keeps its ID. Where the user picks another place among the links
//   into a variadic block, the call gives that place too, as its `before`.
// Where the store refuses the batch, the form says why below its button, and
// keeps what the user gave. It reads the board as it is, through the store.
// It takes from the board only what it shows, such as the blocks that its
// selects offer, by their names. So it draws again only where that changes,
// and not for a change of a block's state.

import { useState } from "react";
import type { ReactElement, SyntheticEvent } from "react";
import { flushSync } from "react-dom";
import { freeInputs, linkFeeding, takesLink } from "../commands/index.ts";
import { createGraph } from "../graph/index.ts";
import { focusFirstInvalid, inputNameError, isVariadic, namesInto, without } from "../picker/index.ts";
import { ComboBox, Form, Select, SelectItem, TextField, useBoard } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockId, Board, Link, LinkId } from "../types/board.ts";
import type { BlockRegistry, CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import {
  blockItem,
  blockName,
  blockRow,
  blockTexts,
  classes,
  classInputs,
  drawForm,
  identifier,
  isVariadicBlock,
  Refusal,
  useEdits,
  useSearch,
} from "../ui/index.ts";
import type { BlockItem, Done } from "../ui/index.ts";
import { isObject, own } from "../util/index.ts";
import type { AdvancedModule } from "./advanced.ts";
import type { CommandCall } from "./actions.ts";

const ADD_LINK = "blockr.core-add_link";
const CHANGE_LINK = "blockr.core-change_link";

// What the form that adds a link starts from: its source, its target and the
// input, each where given.
interface Start {
  readonly from?: BlockId;
  readonly to?: BlockId;
  readonly input?: string;
}

// Create link's advanced action, which keeps the block that its target names
// at one end of the link: its target where the open icon handed the block as
// the target, and else its source.
export const createLinkForm: AdvancedModule = {
  render({ el, core, target, picked, done }) {
    const given = picked === undefined ? {} : startOf(picked);
    if (!("block" in target)) return drawForm(el, <LinkForm core={core} start={given} done={done} />);
    const { block } = target;
    const fixed = given.to === block ? "to" : "from";
    const start = fixed === "to" ? { ...given, to: block } : { ...given, from: block };
    return drawForm(el, <LinkForm core={core} start={start} fixed={fixed} done={done} />);
  },
};

// Edit link's advanced action, which core hands a link alone.
export const editLinkForm: AdvancedModule = {
  render({ el, core, target, done }) {
    if (!("link" in target)) return undefined;
    return drawForm(el, <EditLink core={core} id={target.link} done={done} />);
  },
};

// What the open icon of Create link's quick action hands its advanced action,
// the blocks of the link and its input, checked, since a caller of
// mountAdvanced() may hand it anything.
function startOf(picked: Json): Start {
  if (!isObject(picked)) return {};
  const { from, to, input } = picked;
  return {
    ...(typeof from === "string" ? { from } : {}),
    ...(typeof to === "string" ? { to } : {}),
    ...(typeof input === "string" ? { input } : {}),
  };
}

// An end of a link, which the target of Create link may fix.
type End = "from" | "to";

// The line that names the block at the end of the link that the target
// fixes.
function fixedEnd(board: Board, end: End, block: BlockId | undefined): string {
  const name = block === undefined ? "" : blockName(board, block);
  return end === "from" ? `A link from "${name}".` : `A link into "${name}".`;
}

// The form that edits the link, which draws nothing once the board no longer
// holds it, since core then ends it.
function EditLink({ core, id, done }: { core: CoreApi; id: LinkId; done: Done }): ReactElement | null {
  const link = useBoard(core, (board) => own(board.links, id));
  return link === undefined ? null : <LinkForm core={core} editing={{ id, link }} done={done} />;
}

type Errors = Partial<Record<"from" | "to" | "input", string>>;

// The form that adds a link, or that changes the link given, which offers
// only links that the store takes (above). Where it changes a link into a
// variadic block, it offers the link's place among the block's links as
// well. Where it adds a link with an end `fixed`, it names the block of that
// end in a line in place of the end's select.
function LinkForm({
  core,
  start = {},
  fixed,
  editing,
  done,
}: {
  core: CoreApi;
  start?: Start;
  fixed?: End;
  editing?: { readonly id: LinkId; readonly link: Link };
  done: Done;
}): ReactElement {
  const except = editing?.id;
  const [from, setFrom] = useState<BlockId | undefined>(editing?.link.from ?? start.from);
  const [to, setTo] = useState<BlockId | undefined>(editing?.link.to ?? start.to);
  const [input, setInput] = useState<string | undefined>(editing?.link.input ?? start.input);
  const [name, setName] = useState(() =>
    editing !== undefined && isVariadicBlock(core.store.board(), core.blockRegistry(), editing.link.to) ? editing.link.input : "",
  );
  // The place among the links into a variadic target that the user picks.
  const [place, setPlace] = useState<number | undefined>(undefined);
  const [errors, setErrors] = useState<Errors>({});
  const [busy, setBusy] = useState(false);
  const edits = useEdits(core);

  // What the form shows of the board, for the blocks and the input that the
  // user picked.
  const { source, target, fromOptions, toOptions, variadic, offered, chosen, hint, taken, among, fixedLine, targetName, few } = useBoard(
    core,
    (board) => {
      const registry = core.blockRegistry();
      // The blocks that the form offers, with the links of the board but the
      // one that the form changes, which is about to move.
      const others = withoutLink(board, except);
      const held = (block: BlockId | undefined): block is BlockId => block !== undefined && own(board.blocks, block) !== undefined;
      const source = held(from) ? from : undefined;
      const target = held(to) ? to : undefined;
      const free = target === undefined ? [] : (freeInputs(others, registry, target) ?? []);
      const variadic = isVariadic(free);
      // Where the form changes a link and keeps its block, every input of the
      // block, and otherwise the free inputs.
      const offered = !variadic && editing !== undefined && target === editing.link.to ? classInputs(board, registry, target) : free;
      const chosen = variadic ? undefined : input !== undefined && offered.includes(input) ? input : free[0];
      return {
        source,
        target,
        ...ends(others, registry, source, target),
        variadic,
        offered,
        chosen,
        // What the form says below the input where another link feeds the one
        // picked, so that applying the form swaps the inputs of the two links.
        hint: editing !== undefined && target === editing.link.to && chosen !== undefined ? swapHint(board, registry, editing, chosen) : undefined,
        taken: target === undefined ? [] : namesInto(board, target, except),
        // Where the form changes a link into a variadic block, the block's
        // other links and the place that the link falls in where nothing moves
        // it, which the form offers until the user picks another.
        among: editing === undefined || target === undefined || !variadic ? undefined : placeAmong(board, editing.id, target),
        fixedLine: fixed === undefined ? undefined : fixedEnd(board, fixed, fixed === "from" ? from : to),
        targetName: target === undefined ? "" : blockName(board, target),
        // Whether the board holds fewer blocks than a link joins.
        few: Object.keys(board.blocks).length < 2,
      };
    },
  );
  const fromSearch = useSearch(fromOptions, blockTexts);
  const toSearch = useSearch(toOptions, blockTexts);
  const position = among === undefined ? undefined : place !== undefined && place <= among.others.length + 1 ? place : among.place;

  function check(): Errors {
    return {
      ...(source === undefined ? { from: "Pick the block that the link comes from." } : {}),
      ...(target === undefined ? { to: "Pick the block that the link goes to." } : {}),
      ...(variadic ? optional("input", inputNameError(name, taken)) : {}),
    };
  }

  // The batch that the form runs: add_link with an ID that CoreApi mints, or
  // change_link. Where the user picked another place among the links into a
  // variadic block, change_link gives it as its `before`: the link at that
  // place, which the link goes before, or null for the last place. The place
  // counts the other links into the block, which change_link leaves where
  // they are.
  function batch(source: BlockId, target: BlockId): CommandCall[] {
    const linkInput = variadic ? name.trim() : chosen;
    const link: Record<string, Json> = { from: source, to: target, ...(linkInput === undefined ? {} : { input: linkInput }) };
    if (editing === undefined) {
      return [{ id: ADD_LINK, args: { id: core.mintId((minted) => Object.hasOwn(core.store.board().links, minted)), ...link } }];
    }
    const moved = among !== undefined && position !== undefined && position !== among.place;
    const before = moved ? (among.others[position - 1] ?? null) : undefined;
    return [{ id: CHANGE_LINK, args: { id: editing.id, ...link, ...(before === undefined ? {} : { before }) } }];
  }

  async function submit(event: SyntheticEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check();
    flushSync(() => {
      setErrors(found);
      edits.clear();
    });
    if (Object.keys(found).length > 0 || source === undefined || target === undefined || busy) {
      focusFirstInvalid(form);
      return;
    }
    // The sentence that names the edit, which names the link as its row does
    // where the form changes one.
    const board = core.store.board();
    const title =
      editing === undefined
        ? "Could not add the link."
        : `Could not change the link from "${blockName(board, editing.link.from)}" to "${blockName(board, editing.link.to)}".`;
    const calls = batch(source, target);
    setBusy(true);
    // Once the batch has run, core ends the form, or renders it afresh in a
    // plugin's room, so that only a refusal leaves it as the user filled it in.
    await edits.run(title, calls, done);
    setBusy(false);
  }

  return (
    <Form
      aria-label={editing === undefined ? "Add link" : "Change link"}
      validationBehavior="aria"
      onSubmit={(event) => {
        void submit(event);
      }}
    >
      {fixedLine === undefined ? null : <p className={styles.hint}>{fixedLine}</p>}
      <div className={fixed === undefined ? "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2" : "bc:grid"}>
        {fixed === "from" ? null : (
          <ComboBox
            label="From"
            {...fromSearch}
            value={source ?? null}
            placeholder="Search blocks"
            isInvalid={errors.from !== undefined}
            errorMessage={errors.from}
            onChange={(key) => {
              setFrom(key === null ? undefined : String(key));
              setErrors((now) => without(now, "from"));
            }}
          >
            {blockRow}
          </ComboBox>
        )}
        {fixed === "to" ? null : (
          <ComboBox
            label="To"
            {...toSearch}
            value={target ?? null}
            placeholder="Search blocks"
            isInvalid={errors.to !== undefined}
            errorMessage={errors.to}
            description={toOptions.length === 0 ? "No block takes a link from this block." : undefined}
            onChange={(key) => {
              setTo(key === null ? undefined : String(key));
              setInput(undefined);
              setPlace(undefined);
              setErrors((now) => without(without(now, "to"), "input"));
            }}
          >
            {blockRow}
          </ComboBox>
        )}
      </div>
      {target === undefined ? null : variadic ? (
        <div className={among === undefined ? "bc:grid" : "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2"}>
          <TextField
            label="Input name"
            value={name}
            description={`Optional. The block "${targetName}" takes any number of links, each by its name or, where it has none, by its position.`}
            errorMessage={errors.input}
            isInvalid={errors.input !== undefined}
            autoComplete="off"
            {...identifier}
            onChange={(value) => {
              setName(value);
              setErrors((now) => without(now, "input"));
            }}
            onBlur={() => {
              setErrors((now) => ({ ...now, ...optional("input", inputNameError(name, taken)) }));
            }}
          />
          {among === undefined ? null : (
            <Select
              label="Position"
              isRequired
              items={Array.from({ length: among.others.length + 1 }, (_, i) => ({ id: String(i + 1) }))}
              value={position === undefined ? null : String(position)}
              isDisabled={among.others.length === 0}
              onChange={(key) => {
                if (key !== null) setPlace(Number(key));
              }}
            >
              {(item) => <SelectItem id={item.id}>{item.id}</SelectItem>}
            </Select>
          )}
        </div>
      ) : (
        <Select
          label="Input"
          isRequired
          items={offered.map((id) => ({ id }))}
          value={chosen ?? null}
          isDisabled={offered.length < 2}
          description={hint}
          onChange={(key) => {
            if (key !== null) setInput(String(key));
          }}
        >
          {(item) => <SelectItem id={item.id}>{item.id}</SelectItem>}
        </Select>
      )}
      {editing === undefined && few ? (
        <p className={styles.hint}>A link joins two blocks, so the board needs two blocks first.</p>
      ) : null}
      <div className="bc:flex bc:gap-2">
        <button type="submit" className={classes.primary} disabled={busy}>
          {editing === undefined ? "Add link" : "Apply"}
        </button>
      </div>
      <Refusal refusal={edits.refusal} />
    </Form>
  );
}

// The blocks that the form offers as a link's source and as its target, on
// the board given: as the source, any block but one downstream of the target,
// and as the target, any block that takes a link but one upstream of the
// source, so that the link closes no cycle.
function ends(
  board: Board,
  registry: BlockRegistry,
  source: BlockId | undefined,
  target: BlockId | undefined,
): { fromOptions: BlockItem[]; toOptions: BlockItem[] } {
  const graph = createGraph(board);
  const upstream = source === undefined ? new Set<BlockId>() : graph.withUpstream([source]);
  const downstream = target === undefined ? new Set<BlockId>() : graph.withDownstream([target]);
  const ids = Object.keys(board.blocks);
  return {
    fromOptions: ids.filter((id) => !downstream.has(id)).map((id) => blockItem(board, registry, id)),
    toOptions: ids.filter((id) => !upstream.has(id) && takesLink(board, registry, id)).map((id) => blockItem(board, registry, id)),
  };
}

// What the form that changes a link says where another link into the same
// block feeds the input picked: which link that is, and the input that it
// takes once the user applies the form. It is undefined where no other link
// feeds the input.
function swapHint(
  board: Board,
  registry: BlockRegistry,
  editing: { readonly id: LinkId; readonly link: Link },
  input: string,
): string | undefined {
  const other = linkFeeding(board, registry, editing.link.to, input, editing.id);
  const from = other === undefined ? undefined : own(board.links, other)?.from;
  if (from === undefined) return undefined;
  return `The link from "${blockName(board, from)}" feeds "${input}". Applying gives that link "${editing.link.input}", so the two links swap their inputs.`;
}

// The links into block `to` but the link `id`, in their order on the board,
// and the place, from 1, that the link takes among them where it goes to the
// block and keeps its place on the board.
function placeAmong(board: Pick<Board, "links">, id: LinkId, to: BlockId): { others: LinkId[]; place: number } {
  const ids = Object.keys(board.links).filter((other) => other === id || own(board.links, other)?.to === to);
  return { others: ids.filter((other) => other !== id), place: ids.indexOf(id) + 1 };
}

// The board without the link given, as one that a form is about to change.
function withoutLink(board: Board, id: LinkId | undefined): Board {
  if (id === undefined || !Object.hasOwn(board.links, id)) return board;
  const links = Object.fromEntries(Object.entries(board.links).filter(([other]) => other !== id));
  return { ...board, links };
}

function optional<K extends string>(key: K, error: string | undefined): Partial<Record<K, string>> {
  return error === undefined ? {} : ({ [key]: error } as Partial<Record<K, string>>);
}

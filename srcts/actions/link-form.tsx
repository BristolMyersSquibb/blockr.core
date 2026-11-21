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
// The form that adds a link, Create link's, runs nothing before the user
// submits it. It starts from the block that its target names, as the link's
// source, or from no block on the board, or from the blocks and the input that
// the open icon of the quick action hands it. Once the user submits it, it
// mints the link's ID through CoreApi's mintId() and hands done() the batch
// that adds the link. Where the store refuses the batch, it says why below its
// button, and keeps what the user gave.
// The form that edits a link, Edit link's, applies each edit as the user makes
// it, through core.commands.run(), as one step of the user, so that it needs
// no button: picking another source or target, or another input, changes the
// link with change_link at once, and so does a name that the user gives the
// input, once the user leaves its field or presses Enter there. A place that
// the user picks among the links into a variadic block moves the link there
// with move_link. A link that goes into another block takes that block's first
// free input, which the user may then change. Where the store refuses an edit,
// the form says why below itself, until the user's next edit there. It shows
// the link as the board holds it, after each edit.
// Both read the board as it is, through the store.

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
  EditField,
  identifier,
  Refusal,
  useEdits,
  useSearch,
} from "../ui/index.ts";
import type { BlockItem } from "../ui/index.ts";
import { isObject, own } from "../util/index.ts";
import type { AdvancedModule } from "./advanced.ts";
import type { CommandCall } from "./actions.ts";

const ADD_LINK = "blockr.core-add_link";
const CHANGE_LINK = "blockr.core-change_link";
const MOVE_LINK = "blockr.core-move_link";

// What the form that adds a link starts from: its source, its target and the
// input, each where given.
interface Start {
  readonly from?: BlockId;
  readonly to?: BlockId;
  readonly input?: string;
}

type Done = Parameters<AdvancedModule["render"]>[0]["done"];

// Create link's advanced action.
export const createLinkForm: AdvancedModule = {
  render({ el, core, target, picked, done }) {
    const start = picked === undefined ? ("block" in target ? { from: target.block } : {}) : startOf(picked);
    return drawForm(el, <CreateLink core={core} start={start} done={done} />);
  },
};

// Edit link's advanced action, which core hands a link alone.
export const editLinkForm: AdvancedModule = {
  render({ el, core, target }) {
    if (!("link" in target)) return undefined;
    return drawForm(el, <EditLink core={core} id={target.link} />);
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

type Errors = Partial<Record<"from" | "to" | "input", string>>;

// The form that adds a link, once the user submits it.
function CreateLink({ core, start, done }: { core: CoreApi; start: Start; done: Done }): ReactElement {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const [from, setFrom] = useState<BlockId | undefined>(start.from);
  const [to, setTo] = useState<BlockId | undefined>(start.to);
  const [input, setInput] = useState<string | undefined>(start.input);
  const [name, setName] = useState("");
  const [errors, setErrors] = useState<Errors>({});
  const [busy, setBusy] = useState(false);
  const edits = useEdits(core);

  const held = (block: BlockId | undefined): block is BlockId => block !== undefined && own(board.blocks, block) !== undefined;
  const source = held(from) ? from : undefined;
  const target = held(to) ? to : undefined;
  const { fromOptions, toOptions } = ends(board, registry, source, target);
  const fromSearch = useSearch(fromOptions, blockTexts);
  const toSearch = useSearch(toOptions, blockTexts);

  const free = target === undefined ? [] : (freeInputs(board, registry, target) ?? []);
  const variadic = isVariadic(free);
  const chosen = variadic ? undefined : input !== undefined && free.includes(input) ? input : free[0];
  const taken = target === undefined ? [] : namesInto(board, target);

  function check(): Errors {
    return {
      ...(source === undefined ? { from: "Pick the block that the link comes from." } : {}),
      ...(target === undefined ? { to: "Pick the block that the link goes to." } : {}),
      ...(variadic ? optional("input", inputNameError(name, taken)) : {}),
    };
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
    const linkInput = variadic ? name.trim() : chosen;
    const id = core.mintId((minted) => Object.hasOwn(core.store.board().links, minted));
    const batch: CommandCall[] = [
      { id: ADD_LINK, args: { id, from: source, to: target, ...(linkInput === undefined ? {} : { input: linkInput }) } },
    ];
    setBusy(true);
    // Once the batch has run, core ends the form, or renders it afresh in a
    // plugin's room, so that only a refusal leaves it as the user filled it in.
    await edits.attempt("Could not add the link.", (opts) => done(batch, opts));
    setBusy(false);
  }

  return (
    <Form
      aria-label="Add link"
      validationBehavior="aria"
      onSubmit={(event) => {
        void submit(event);
      }}
    >
      <div className="bc:grid bc:grid-cols-2 bc:items-start bc:gap-2">
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
            setErrors((now) => without(without(now, "to"), "input"));
          }}
        >
          {blockRow}
        </ComboBox>
      </div>
      {target === undefined ? null : variadic ? (
        <TextField
          label="Input name"
          value={name}
          description={variadicHint(board, target)}
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
      ) : (
        <Select
          label="Input"
          isRequired
          items={free.map((id) => ({ id }))}
          value={chosen ?? null}
          isDisabled={free.length < 2}
          onChange={(key) => {
            if (key !== null) setInput(String(key));
          }}
        >
          {(item) => <SelectItem id={item.id}>{item.id}</SelectItem>}
        </Select>
      )}
      {Object.keys(board.blocks).length < 2 ? <p className={styles.hint}>A link joins two blocks, so the board needs two blocks first.</p> : null}
      <div className="bc:flex bc:gap-2">
        <button type="submit" className={classes.primary} disabled={busy}>
          Add link
        </button>
      </div>
      <Refusal refusal={edits.refusal} />
    </Form>
  );
}

// The form that edits the link, which draws nothing once the board no longer
// holds it, since core then ends it.
function EditLink({ core, id }: { core: CoreApi; id: LinkId }): ReactElement | null {
  const board = useBoard(core);
  const link = own(board.links, id);
  return link === undefined ? null : <LinkEdits core={core} board={board} id={id} link={link} />;
}

function LinkEdits({ core, board, id, link }: { core: CoreApi; board: Board; id: LinkId; link: Link }): ReactElement {
  const registry = core.blockRegistry();
  const edits = useEdits(core);
  // The board without the link, as the link is about to change, and the
  // blocks that the form offers on it.
  const others = withoutLink(board, id);
  const { fromOptions, toOptions } = ends(others, registry, link.from, link.to);
  const fromSearch = useSearch(fromOptions, blockTexts);
  const toSearch = useSearch(toOptions, blockTexts);
  const variadic = isVariadic(freeInputs(others, registry, link.to) ?? []);
  const inputs = variadic ? [] : classInputs(board, registry, link.to);
  // Where the form edits a link into a variadic block, the block's other
  // links and the place that the link holds among them.
  const among = variadic ? placeAmong(board, id, link.to) : undefined;
  // The sentence that names the edit, as the link is now.
  const title = `Could not change the link from "${blockName(board, link.from)}" to "${blockName(board, link.to)}".`;

  function change(args: Readonly<Record<string, Json>>): void {
    void edits.run(title, CHANGE_LINK, { id, ...args });
  }

  return (
    <Form
      aria-label="Change link"
      validationBehavior="aria"
      onSubmit={(event) => {
        event.preventDefault();
      }}
    >
      <div className="bc:grid bc:grid-cols-2 bc:items-start bc:gap-2">
        <ComboBox
          label="From"
          {...fromSearch}
          value={link.from}
          placeholder="Search blocks"
          onChange={(key) => {
            if (key !== null && String(key) !== link.from) change({ from: String(key) });
          }}
        >
          {blockRow}
        </ComboBox>
        <ComboBox
          label="To"
          {...toSearch}
          value={link.to}
          placeholder="Search blocks"
          onChange={(key) => {
            if (key === null || String(key) === link.to) return;
            const to = String(key);
            const [input] = freeInputs(others, registry, to) ?? [];
            change({ to, ...(input === undefined ? {} : { input }) });
          }}
        >
          {blockRow}
        </ComboBox>
      </div>
      {variadic ? (
        <div className={among === undefined || among.others.length === 0 ? "bc:grid" : "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2"}>
          <EditField
            key={`${link.to} ${link.input}`}
            label="Input name"
            value={link.input}
            description={variadicHint(board, link.to)}
            check={(input) => inputNameError(input, namesInto(board, link.to, id))}
            identifier
            onSubmit={(input) => {
              change({ input });
            }}
          />
          {among === undefined || among.others.length === 0 ? null : (
            <Select
              label="Position"
              isRequired
              items={Array.from({ length: among.others.length + 1 }, (_, i) => ({ id: String(i + 1) }))}
              value={String(among.place)}
              onChange={(key) => {
                const position = Number(key);
                if (key === null || position === among.place) return;
                const before = placeAmong(core.store.board(), id, link.to).others[position - 1];
                void edits.run(title, MOVE_LINK, { id, ...(before === undefined ? {} : { before }) });
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
          items={inputs.map((input) => ({ id: input }))}
          value={link.input}
          isDisabled={inputs.length < 2}
          description={swaps(board, registry, id, link, inputs) ? "Picking an input that another link feeds swaps the inputs of the two links." : undefined}
          onChange={(key) => {
            if (key !== null && String(key) !== link.input) change({ input: String(key) });
          }}
        >
          {(item) => <SelectItem id={item.id}>{item.id}</SelectItem>}
        </Select>
      )}
      <Refusal refusal={edits.refusal} />
    </Form>
  );
}

// What the form says of a variadic block below the name of a link's input.
function variadicHint(board: Pick<Board, "blocks">, block: BlockId): string {
  return `Optional. The block "${blockName(board, block)}" takes any number of links, each by its name or, where it has none, by its position.`;
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
    toOptions: ids
      .filter((id) => !upstream.has(id) && takesLink(board, registry, id))
      .map((id) => blockItem(board, registry, id)),
  };
}

// Whether another link into the block that the link goes to feeds one of the
// inputs given, so that picking that input swaps the inputs of the two links.
function swaps(board: Board, registry: BlockRegistry, id: LinkId, link: Link, inputs: readonly string[]): boolean {
  return inputs.some((input) => input !== link.input && linkFeeding(board, registry, link.to, input, id) !== undefined);
}

// The links into block `to` but the link `id`, in their order on the board,
// and the place, from 1, that the link takes among them.
function placeAmong(board: Pick<Board, "links">, id: LinkId, to: BlockId): { others: LinkId[]; place: number } {
  const ids = Object.keys(board.links).filter((other) => other === id || own(board.links, other)?.to === to);
  return { others: ids.filter((other) => other !== id), place: ids.indexOf(id) + 1 };
}

// The board without the link given, as one that a form is about to change.
function withoutLink(board: Board, id: LinkId): Board {
  if (!Object.hasOwn(board.links, id)) return board;
  const links = Object.fromEntries(Object.entries(board.links).filter(([other]) => other !== id));
  return { ...board, links };
}

function optional<K extends string>(key: K, error: string | undefined): Partial<Record<K, string>> {
  return error === undefined ? {} : ({ [key]: error } as Partial<Record<K, string>>);
}

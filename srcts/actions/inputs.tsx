// The advanced action of Edit inputs, one of core's board actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions):
// a block's inputs in their order, each with its source, the block that its
// link comes from, as the actions sidebar of blockr.dock lists them
// (https://github.com/BristolMyersSquibb/blockr.dock/blob/655e323c990649818627ff0ba216717396e00d5c/R/sidebar-inputs.R).
//
// A block that is not variadic takes each input of its class by its name. For
// each input, in the order of the class's inputs, the advanced action shows a
// select of its source and a button that removes the source. The select of an
// input that has a source shows no button that clears it, so that removing a
// source takes the one button.
//
// A variadic block takes its links in their order on the board, and each link
// is one of its inputs. The advanced action shows them in that order, each
// with a button that drags it, a select of its source, a field of its name
// and a button that removes it:
// - The user drags an input to another place among the others, or moves it
//   there from the keyboard, from the button that drags it. A press in the
//   select or the field drags nothing, so that the user selects text there.
// - The user may leave a name empty, for an input without a name. A name that
//   another input of the block gives already does not do. The field says so
//   in place once the user has left it, or has applied the edits, and the
//   edits do not apply while it does.
// A select below the inputs adds an input from the block that the user picks,
// without a name, last among the block's inputs.
//
// The selects offer only the blocks that may feed the block: every block but
// the block itself and those downstream of it, since a link from one of those
// would close a cycle.
//
// The advanced action runs nothing before the user applies it, as Board
// actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action).
// It shows the links into the block as the board holds them, with the user's
// edits on top. So an edit from elsewhere, such as an undo, shows in the
// inputs that the user left alone.
// The Apply button below the inputs hands done() one batch, which runs as one
// step of the user. The batch holds its commands in the order that the role
// edit_inputs_role names, each at most once for each input:
// 1. A remove_link for each input whose source the user removed.
// 2. A change_link for each input whose source or name the user changed.
// 3. An add_link for each input that had no source and got one, and for each
//    input that the user added, with an ID that CoreApi's mintId() mints.
// 4. A move_link for each input of a variadic block that has to move, so that
//    the block takes its inputs in the order that the user dragged them into.
// So the user's edits undo together, and the board never evaluates a state
// that the user only passed through on the way to the one they applied.
// Where the store or the command registry refuses the batch, the advanced
// action says why below its button, in a banner under a sentence that names
// the edit, and keeps what the user gave. It takes the notifications of the
// refusal through the notifier of done(), so that core raises none.
// It draws with core's React SDK on Tailwind CSS, whose theme variables a
// frontend sets to restyle it, as it restyles core's other UIs.

import { useState } from "react";
import type { PointerEvent, ReactElement, ReactNode, SyntheticEvent } from "react";
import { flushSync } from "react-dom";
import { linkFeeding } from "../commands/index.ts";
import { createGraph } from "../graph/index.ts";
import { focusFirstInvalid, inputNameError } from "../picker/index.ts";
import { Button, ComboBox, Form, GridList, GridListItem, TextField, useBoard, useDragAndDrop } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockId, Board, LinkId } from "../types/board.ts";
import type { BlockRegistry, CoreApi } from "../types/core-api.ts";
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
import { own } from "../util/index.ts";
import type { AdvancedModule } from "./advanced.ts";
import type { CommandCall } from "./actions.ts";

const ADD_LINK = "blockr.core-add_link";
const CHANGE_LINK = "blockr.core-change_link";
const MOVE_LINK = "blockr.core-move_link";
const REMOVE_LINK = "blockr.core-remove_link";

// Renders the advanced action for the block that the target names, the one
// kind of target that core hands Edit inputs, and draws it before it returns.
export const editInputs: AdvancedModule = {
  render({ el, core, target, done }) {
    if (!("block" in target)) return undefined;
    return drawForm(el, <EditInputs core={core} block={target.block} done={done} />);
  },
};

// What the inputs of the block draw with: CoreApi, the done() of the advanced
// action, the board, the block registry, the block, and the blocks that the
// selects offer.
interface Parts {
  readonly core: CoreApi;
  readonly done: Done;
  readonly board: Board;
  readonly registry: BlockRegistry;
  readonly block: BlockId;
  readonly sources: readonly BlockItem[];
}

function EditInputs({ core, block, done }: { core: CoreApi; block: BlockId; done: Done }): ReactElement | null {
  const board = useBoard(core);
  // Core ends the advanced action as the block leaves the board, and it draws
  // nothing until then.
  if (own(board.blocks, block) === undefined) return null;
  const registry = core.blockRegistry();
  const parts: Parts = { core, done, board, registry, block, sources: sourcesOf(board, registry, block) };
  return isVariadicBlock(board, registry, block) ? <VariadicInputs {...parts} /> : <FixedInputs {...parts} />;
}

// The blocks that may feed the block, each as a select offers it: every block
// but the block itself and those downstream of it, from which a link would
// close a cycle. The source of every link into the block is among them.
function sourcesOf(board: Board, registry: BlockRegistry, block: BlockId): readonly BlockItem[] {
  const downstream = createGraph(board).withDownstream([block]);
  return Object.keys(board.blocks)
    .filter((id) => !downstream.has(id))
    .map((id) => blockItem(board, registry, id));
}

// The form of the inputs, with the Apply button below them, and below the
// button the banner that says why the store or the command registry refused
// the batch that the button applied.
// On a submit, `prepare` gives the batch, or undefined where the form's own
// checks stop it, which then say what to change. That check is the user's
// next edit there, so the banner of the last batch goes.
function InputsForm({
  core,
  done,
  board,
  block,
  variadic,
  prepare,
  children,
}: Pick<Parts, "core" | "done" | "board" | "block"> & {
  variadic: boolean;
  prepare: () => readonly CommandCall[] | undefined;
  children: ReactNode;
}): ReactElement {
  const edits = useEdits(core);
  const [busy, setBusy] = useState(false);
  const name = blockName(board, block);

  async function submit(event: SyntheticEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    if (busy) return;
    const form = event.currentTarget;
    const calls = prepare();
    if (calls === undefined) {
      edits.clear();
      focusFirstInvalid(form);
      return;
    }
    setBusy(true);
    // Once the batch has run, core ends the advanced action, or renders it
    // afresh in a plugin's room, so that only a refusal leaves it as the user
    // left it.
    await edits.run(`Could not change the inputs of the block "${name}".`, calls, done);
    setBusy(false);
  }

  return (
    <Form
      aria-label="Change inputs"
      validationBehavior="aria"
      className="bc:font-sans bc:text-sm bc:text-text-default"
      onSubmit={(event) => {
        void submit(event);
      }}
    >
      <p className={styles.hint}>{variadic ? `The inputs of the block "${name}", which it takes in this order.` : `The inputs of the block "${name}".`}</p>
      {children}
      <div className="bc:flex bc:gap-2">
        <button type="submit" className={classes.primary} disabled={busy}>
          Apply
        </button>
      </div>
      <Refusal refusal={edits.refusal} />
    </Form>
  );
}

// The sources that the user gave the inputs of a block that is not variadic,
// by the input, each a block, or null where the user removed the source.
type Sources = Readonly<Record<string, BlockId | null>>;

// The inputs of a block that is not variadic, in the order of its class's
// inputs.
function FixedInputs(parts: Parts): ReactElement {
  const { core, board, registry, block, sources } = parts;
  const [given, setGiven] = useState<Sources>({});
  const inputs = classInputs(board, registry, block);
  return (
    <InputsForm {...parts} variadic={false} prepare={() => fixedBatch(core, board, registry, block, given)}>
      <div className="bc:grid bc:gap-2">
        {inputs.map((input) => (
          <FixedInput
            key={input}
            sources={sources}
            input={input}
            source={shownSource(board, registry, block, input, given)}
            onChange={(source) => {
              setGiven((now) => ({ ...now, [input]: source }));
            }}
          />
        ))}
      </div>
    </InputsForm>
  );
}

// The source of an input of a block that is not variadic, as the advanced
// action shows it: the one that the user gave it, a block or none, or else
// the block that the link that feeds it on the board comes from. A block that
// the user gave and that has left the board since counts for nothing.
function shownSource(board: Board, registry: BlockRegistry, block: BlockId, input: string, given: Sources): BlockId | null {
  const source = own(given, input);
  if (source === null || (source !== undefined && Object.hasOwn(board.blocks, source))) return source;
  const id = linkFeeding(board, registry, block, input);
  return (id === undefined ? undefined : own(board.links, id)?.from) ?? null;
}

// The batch that gives the inputs of a block that is not variadic the sources
// that the advanced action shows, against the links that feed them on the
// board: remove_link where the user removed a source, change_link where the
// user picked another block, then add_link where the input had no source,
// each in the order of the class's inputs.
function fixedBatch(core: CoreApi, board: Board, registry: BlockRegistry, block: BlockId, given: Sources): CommandCall[] {
  const removes: CommandCall[] = [];
  const changes: CommandCall[] = [];
  const adds: CommandCall[] = [];
  const minted = new Set<LinkId>();
  for (const input of classInputs(board, registry, block)) {
    const source = shownSource(board, registry, block, input, given);
    const id = linkFeeding(board, registry, block, input);
    const from = id === undefined ? undefined : own(board.links, id)?.from;
    if (id === undefined) {
      if (source !== null) adds.push({ id: ADD_LINK, args: { id: mint(core, minted), from: source, to: block, input } });
    } else if (source === null) {
      removes.push({ id: REMOVE_LINK, args: { id } });
    } else if (source !== from) {
      changes.push({ id: CHANGE_LINK, args: { id, from: source } });
    }
  }
  return [...removes, ...changes, ...adds];
}

// The ID of a new link, which CoreApi mints, that neither the board nor the
// batch holds yet.
function mint(core: CoreApi, minted: Set<LinkId>): LinkId {
  const id = core.mintId((taken) => minted.has(taken) || Object.hasOwn(core.store.board().links, taken));
  minted.add(id);
  return id;
}

// An input of a block that is not variadic: the select of its source, and the
// button that removes the source, where it has one.
function FixedInput({
  sources,
  input,
  source,
  onChange,
}: Pick<Parts, "sources"> & { input: string; source: BlockId | null; onChange: (source: BlockId | null) => void }): ReactElement {
  const search = useSearch(sources, blockTexts, "No block can feed the input.");
  return (
    <div className="bc:flex bc:items-end bc:gap-2">
      <ComboBox
        label={input}
        {...search}
        value={source}
        placeholder="Search blocks"
        // The select of an input that has a source shows no button that clears
        // it, since the button beside it removes the source. Emptying the
        // field removes nothing either, since onChange() passes over the empty
        // pick that it then gets.
        isRequired={source !== null}
        className="bc:min-w-0 bc:flex-1"
        onChange={(key) => {
          if (key !== null) onChange(String(key));
        }}
      >
        {blockRow}
      </ComboBox>
      <div className="bc:flex bc:w-7 bc:shrink-0 bc:justify-center bc:pb-1">
        {source === null ? null : (
          <RemoveButton
            label={`Remove the source of ${input}`}
            onPress={() => {
              onChange(null);
            }}
          />
        )}
      </div>
    </div>
  );
}

// An input of a variadic block as the advanced action shows it: a link into
// the block that the board holds, with what the user changed of it, or an
// input that the user added. Its key is the link's ID, and for an input that
// the user added, a key of digits alone, which no ID is (../store/ids.ts), so
// that the two never meet. Its name is as the user typed it.
interface Input {
  readonly key: string;
  readonly from: BlockId;
  readonly input: string;
}

// What the user changed in the inputs of a variadic block and has not applied
// yet: the source and the name of each link into the block that the user
// changed, by the link's ID, the links that the user removed, the inputs that
// the user added, with the number that keys the next, and the keys of the
// inputs in the order that the user dragged them into, once the user has.
interface Changes {
  readonly links: Readonly<Record<LinkId, { readonly from?: BlockId; readonly input?: string }>>;
  readonly removed: readonly LinkId[];
  readonly added: readonly Input[];
  readonly next: number;
  readonly order?: readonly string[];
}

const UNCHANGED: Changes = { links: {}, removed: [], added: [], next: 1 };

// The inputs of a variadic block, the links into it in their order, in a list
// whose items the user drags into another order, and the select that adds an
// input below it.
function VariadicInputs(parts: Parts): ReactElement {
  const { core, board, block, sources } = parts;
  const [changes, setChanges] = useState<Changes>(UNCHANGED);
  // The inputs whose names the advanced action checks, by their keys: each
  // that the user has left, and every one once the user has applied the edits.
  // A field says why its name does not do from then on, as the user types.
  const [checked, setChecked] = useState<ReadonlySet<string>>(new Set());
  const shown = shownInputs(board, block, changes);
  const { dragAndDropHooks } = useDragAndDrop({
    getItems: (keys) => [...keys].map((key) => ({ "text/plain": String(key) })),
    onReorder(event) {
      const [moved] = [...event.keys];
      if (moved === undefined) return;
      const keys = shown.map(({ key }) => key);
      const order = reordered(keys, String(moved), String(event.target.key), event.target.dropPosition);
      if (order !== null) {
        setChanges((now) => ({ ...now, order }));
      }
    },
  });
  const rows = shown.map((input, i) => ({
    id: input.key,
    input,
    place: i + 1,
    error: checked.has(input.key) ? nameError(board, shown, input.key) : undefined,
  }));

  function prepare(): readonly CommandCall[] | undefined {
    flushSync(() => {
      setChecked(new Set(shown.map(({ key }) => key)));
    });
    return shown.some(({ key }) => nameError(board, shown, key) !== undefined) ? undefined : variadicBatch(core, board, block, shown);
  }

  return (
    <InputsForm {...parts} variadic prepare={prepare}>
      <GridList
        aria-label={`Inputs of ${blockName(board, block)}`}
        items={rows}
        dragAndDropHooks={dragAndDropHooks}
        // Tab moves between the controls of an item, and every other key
        // stays with the control, so that the arrow keys move in a field and
        // open the list of a select, rather than move between items.
        keyboardNavigationBehavior="tab"
        renderEmptyState={() => "No link feeds the block yet."}
        className="bc:gap-1.5"
      >
        {(row) => (
          <GridListItem id={row.id} textValue={inputText(board, row.input, row.place)} className={ROW}>
            <VariadicInput
              sources={sources}
              input={row.input}
              place={row.place}
              error={row.error}
              onChange={(change) => {
                setChanges((now) => withChange(now, row.id, change));
              }}
              onCheck={() => {
                setChecked((now) => new Set([...now, row.id]));
              }}
              onRemove={() => {
                setChanges((now) => withRemoved(now, row.id));
              }}
            />
          </GridListItem>
        )}
      </GridList>
      <AddInput
        sources={sources}
        onAdd={(from) => {
          setChanges((now) => ({ ...now, added: [...now.added, { key: String(now.next), from, input: "" }], next: now.next + 1 }));
        }}
      />
    </InputsForm>
  );
}

// The inputs of a variadic block as the advanced action shows them:
// 1. The links into the block on the board, in their order, without those
//    that the user removed, and with what the user changed of the others.
// 2. After them, the inputs that the user added.
// 3. Once the user has dragged an input, all in the order that the drag left,
//    where an input that came since goes after the others.
// A source that has left the board since the user picked it counts for
// nothing, so that its input shows its source on the board, or goes where the
// user added it.
function shownInputs(board: Board, block: BlockId, changes: Changes): Input[] {
  const removed = new Set(changes.removed);
  const held = (from: BlockId | undefined): from is BlockId => from !== undefined && Object.hasOwn(board.blocks, from);
  const links = createGraph(board)
    .linksInto(block)
    .flatMap((id): Input[] => {
      const link = own(board.links, id);
      if (link === undefined || removed.has(id)) return [];
      const change = own(changes.links, id);
      const from = change?.from;
      return [{ key: id, from: held(from) ? from : link.from, input: change?.input ?? link.input }];
    });
  const inputs = [...links, ...changes.added.filter(({ from }) => held(from))];
  const { order } = changes;
  if (order === undefined) return inputs;
  const place = ({ key }: Input): number => {
    const at = order.indexOf(key);
    return at === -1 ? order.length : at;
  };
  return inputs.sort((a, b) => place(a) - place(b));
}

// The changes with the source or the name of the input given changed.
function withChange(changes: Changes, key: string, change: { readonly from?: BlockId; readonly input?: string }): Changes {
  if (changes.added.some((input) => input.key === key)) {
    return { ...changes, added: changes.added.map((input) => (input.key === key ? { ...input, ...change } : input)) };
  }
  return { ...changes, links: { ...changes.links, [key]: { ...own(changes.links, key), ...change } } };
}

// The changes without the input given, which the user removed.
function withRemoved(changes: Changes, key: string): Changes {
  if (changes.added.some((input) => input.key === key)) {
    return { ...changes, added: changes.added.filter((input) => input.key !== key) };
  }
  return { ...changes, removed: [...changes.removed, key] };
}

// The keys of the inputs in the order that a drop leaves them in, where the
// user drops the input `moved` before or after the input `target`. It is null
// where the input stays in its place, so that the drop changes nothing.
function reordered(keys: readonly string[], moved: string, target: string, position: "before" | "after" | "on"): string[] | null {
  const others = keys.filter((key) => key !== moved);
  const at = others.indexOf(target);
  if (moved === target || position === "on" || at === -1) return null;
  const index = position === "before" ? at : at + 1;
  const next = [...others.slice(0, index), moved, ...others.slice(index)];
  return next.every((key, i) => key === keys[i]) ? null : next;
}

// Why the name that the user gave the input with the key given does not do,
// where it does not: a name that another input of the block gives already,
// as the advanced action shows them. A name that the board holds for the link
// already is no edit of the user's, so it passes.
function nameError(board: Board, shown: readonly Input[], key: string): string | undefined {
  const input = shown.find((other) => other.key === key);
  if (input === undefined || input.input.trim() === own(board.links, key)?.input) return undefined;
  const taken = shown.flatMap((other) => (other.key === key ? [] : [other.input.trim()]));
  return inputNameError(input.input, taken);
}

// The batch that gives a variadic block the inputs that the advanced action
// shows, against the links into it on the board:
// 1. A remove_link for each link into the block that the advanced action no
//    longer shows, in their order on the board.
// 2. A change_link for each link whose source or name the user changed, with
//    the source and the name that changed.
// 3. An add_link for each input that the user added, in the order shown, with
//    an ID that CoreApi mints, so that move_link can name the new link.
// 4. A move_link for each link that has to move to stand in the order shown
//    (moves()).
function variadicBatch(core: CoreApi, board: Board, block: BlockId, shown: readonly Input[]): CommandCall[] {
  const into = createGraph(board).linksInto(block);
  const minted = new Set<LinkId>();
  const order: LinkId[] = [];
  const changes: CommandCall[] = [];
  const adds: CommandCall[] = [];
  for (const { key, from, input } of shown) {
    const name = input.trim();
    const link = own(board.links, key);
    if (link === undefined) {
      const id = mint(core, minted);
      order.push(id);
      adds.push({ id: ADD_LINK, args: { id, from, to: block, ...(name === "" ? {} : { input: name }) } });
      continue;
    }
    order.push(key);
    const args = { ...(from === link.from ? {} : { from }), ...(name === link.input ? {} : { input: name }) };
    if (Object.keys(args).length > 0) changes.push({ id: CHANGE_LINK, args: { id: key, ...args } });
  }
  const kept = into.filter((id) => order.includes(id));
  return [
    ...into.filter((id) => !order.includes(id)).map((id): CommandCall => ({ id: REMOVE_LINK, args: { id } })),
    ...changes,
    ...adds,
    ...moves([...kept, ...minted], order),
  ];
}

// The calls of move_link that put the links into a variadic block, which
// stand in the order `from` on the board, in the order `to`, each link at most
// once. As many links as can stay where they are: the longest run of links
// that stand in the same order in both (longestRun()). Each other link moves
// before the link that follows it in `to`, or last where none follows it. The
// moves run from the last link back to the first, so that the links after each
// one stand in their order already as it moves.
function moves(from: readonly LinkId[], to: readonly LinkId[]): CommandCall[] {
  const staying = longestRun(from, to);
  const calls: CommandCall[] = [];
  for (const [i, id] of [...to.entries()].reverse()) {
    if (staying.has(id)) continue;
    const before = to[i + 1];
    calls.push({ id: MOVE_LINK, args: { id, ...(before === undefined ? {} : { before }) } });
  }
  return calls;
}

// The most links of `to` that stand in the same order in `from`, not
// necessarily next to one another. For each link of `to`, it finds the
// longest run that ends with the link, from the runs of the links before it
// that stand before it in `from` too.
function longestRun(from: readonly LinkId[], to: readonly LinkId[]): ReadonlySet<LinkId> {
  const longest = (runs: readonly (readonly LinkId[])[]): readonly LinkId[] =>
    runs.reduce<readonly LinkId[]>((best, run) => (run.length > best.length ? run : best), []);
  const ends: { readonly place: number; readonly run: readonly LinkId[] }[] = [];
  for (const id of to) {
    const place = from.indexOf(id);
    const before = longest(ends.filter((end) => end.place < place).map(({ run }) => run));
    ends.push({ place, run: [...before, id] });
  }
  return new Set(longest(ends.map(({ run }) => run)));
}

// What an item of the list says to assistive technology, which React Aria
// reads out as the user drags it: its place, its source and its name, if it
// has one.
function inputText(board: Pick<Board, "blocks">, { from, input }: Input, place: number): string {
  const source = blockName(board, from);
  const name = input.trim();
  return name === "" ? `Input ${String(place)}, from ${source}` : `Input ${String(place)}, ${name}, from ${source}`;
}

// An input of a variadic block: the select of its source, the field of its
// name and the button that removes it. The list draws the button that drags
// it before them. The field says in place why its name does not do, and has
// the name checked as the user leaves it.
function VariadicInput({
  sources,
  input,
  place,
  error,
  onChange,
  onCheck,
  onRemove,
}: Pick<Parts, "sources"> & {
  input: Input;
  place: number;
  error: string | undefined;
  onChange: (change: { readonly from?: BlockId; readonly input?: string }) => void;
  onCheck: () => void;
  onRemove: () => void;
}): ReactElement {
  const search = useSearch(sources, blockTexts, "No block can feed the block.");
  const what = `input ${String(place)}`;
  return (
    <div className="bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2" onPointerDownCapture={holdStill}>
      <ComboBox
        aria-label={`Source of ${what}`}
        {...search}
        value={input.from}
        placeholder="Search blocks"
        // An input always has a source, so the select shows no button that
        // clears it, and emptying the field changes nothing, since onChange()
        // passes over the empty pick that it then gets.
        isRequired
        className="bc:min-w-0 bc:flex-1"
        onChange={(key) => {
          if (key !== null && String(key) !== input.from) onChange({ from: String(key) });
        }}
      >
        {blockRow}
      </ComboBox>
      <TextField
        aria-label={`Name of ${what}`}
        value={input.input}
        placeholder="No name"
        isInvalid={error !== undefined}
        errorMessage={error}
        autoComplete="off"
        {...identifier}
        className="bc:w-1/3 bc:shrink-0"
        onChange={(value) => {
          onChange({ input: value });
        }}
        onBlur={onCheck}
      />
      <RemoveButton label={`Remove ${what}`} onPress={onRemove} />
    </div>
  );
}

// Keeps the item of the list still while the user presses in its controls, so
// that the user selects text in a field there with the pointer, as in any
// field. React Aria lets the pointer drag an item from anywhere on it, while
// the actions sidebar of blockr.dock drags a row from its handle alone.
// The browser decides whether a press drags an element once the pointer first
// moves, so a press in the controls goes in these steps:
// 1. As the press starts, the item stops being draggable.
// 2. As the pointer moves, the browser selects text rather than drag the item.
// 3. As the press ends, the item is draggable again.
// So the item drags from its button alone.
function holdStill(event: PointerEvent<HTMLElement>): void {
  const item = event.currentTarget.closest('[role="row"][draggable="true"]');
  if (item === null) return;
  item.setAttribute("draggable", "false");
  const doc = item.ownerDocument;
  const release = (): void => {
    item.setAttribute("draggable", "true");
    doc.removeEventListener("pointerup", release, true);
    doc.removeEventListener("pointercancel", release, true);
  };
  doc.addEventListener("pointerup", release, true);
  doc.addEventListener("pointercancel", release, true);
}

// The select that adds an input to a variadic block: a link from the block
// that the user picks, without a name, which goes last among the block's
// inputs. It shows no pick, so that it is empty again once the input shows in
// the list, ready for the next.
function AddInput({ sources, onAdd }: Pick<Parts, "sources"> & { onAdd: (from: BlockId) => void }): ReactElement {
  const search = useSearch(sources, blockTexts, "No block can feed the block.");
  return (
    <ComboBox
      label="Add input"
      {...search}
      value={null}
      placeholder="Search blocks"
      onChange={(key) => {
        if (key !== null) onAdd(String(key));
      }}
    >
      {blockRow}
    </ComboBox>
  );
}

// The button that removes an input or its source, a cross, whose label names
// what it removes.
function RemoveButton({ label, onPress }: { label: string; onPress: () => void }): ReactElement {
  return (
    <Button variant="quiet" aria-label={label} onPress={onPress} className="bc:size-7 bc:shrink-0 bc:justify-center bc:p-0">
      {/* React Aria's Button takes no title, so the cross carries it. */}
      <span title={label} className="bc:flex bc:size-full bc:items-center bc:justify-center">
        <Cross />
      </span>
    </Button>
  );
}

// The cross of a button that removes, as the SDK's inputs draw it, in the
// colour of the text around it.
function Cross(): ReactElement {
  return (
    <svg width="10" height="10" viewBox="0 0 10 10" fill="none" stroke="currentColor" strokeWidth="1.4" strokeLinecap="round" aria-hidden="true">
      <path d="M2.5 2.5l5 5M7.5 2.5l-5 5" />
    </svg>
  );
}

// An input of a variadic block, framed as the actions sidebar of blockr.dock
// frames it, and faded while the user drags it.
const ROW = "bc:rounded-md bc:border bc:border-border-default bc:bg-bg-subtle bc:py-1 bc:data-dragging:opacity-60";

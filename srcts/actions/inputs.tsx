// The advanced action of Edit inputs, one of core's board actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions):
// a block's inputs in their order, each with its source, the block that its
// link comes from, as the actions sidebar of blockr.dock lists them
// (https://github.com/BristolMyersSquibb/blockr.dock/blob/655e323c990649818627ff0ba216717396e00d5c/R/sidebar-inputs.R).
// It reads the board as it is, so that it shows each edit as it applies,
// wherever the edit comes from.
//
// A block that is not variadic takes each input of its class by its name. For
// each input, in the order of the class's inputs, the advanced action shows a
// select of its source and a button that removes the source:
// - Picking a block in the select of an input that a link feeds has the link
//   come from that block, through change_link.
// - Picking a block in the select of an input that no link feeds adds a link
//   from that block into the input, through add_link.
// - The button removes the link that feeds the input, through remove_link.
//   The select of such an input shows no button that clears it, so that
//   removing a source takes the one button.
//
// A variadic block takes its links in their order on the board, and each link
// is one of its inputs. The advanced action shows them in that order, each
// with a button that drags it, a select of its source, a field of its name
// and a button that removes it:
// - The user drags an input to another place among the others, or moves it
//   there from the keyboard, from the button that drags it. A press in the
//   select or the field drags nothing, so that the user selects text there.
//   Each drop moves the link through move_link.
// - Picking a block in the select has the link come from that block, through
//   change_link.
// - A name goes in through change_link, as the user leaves the field or
//   presses Enter. The user may leave the field empty, for an input without a
//   name. A name that another link into the block gives already does not do,
//   and the field says so in place.
// - The button removes the link, through remove_link.
// A select below the inputs adds an input: a link from the block that the
// user picks, without a name, which goes last among the block's links,
// through add_link.
//
// The selects offer only the blocks that may feed the block: every block but
// the block itself and those downstream of it, since a link from one of those
// would close a cycle.
// Each edit applies as the user makes it, through core.commands.run(), as one
// step of the user that undo takes back. So the advanced action needs no
// button that applies its edits, and each drop is one step
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#each-edit-at-once).
// Where the store or the command registry refuses an edit, the advanced action
// says why below its inputs, in a banner under a sentence that names the edit,
// until the user's next edit. It takes the notifications of the refusal
// through the notifier of run(), so that core raises none.
// It draws with core's React SDK on Tailwind CSS, whose theme variables a
// frontend sets to restyle it, as it restyles core's other UIs.

import { useState } from "react";
import type { PointerEvent, ReactElement } from "react";
import { linkFeeding } from "../commands/index.ts";
import { createGraph } from "../graph/index.ts";
import { inputNameError, namesInto } from "../picker/index.ts";
import { Button, ComboBox, GridList, GridListItem, TextField, useBoard, useDragAndDrop } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockId, Board, Link, LinkId } from "../types/board.ts";
import type { BlockRegistry, CoreApi } from "../types/core-api.ts";
import {
  blockItem,
  blockName,
  blockRow,
  blockTexts,
  classInputs,
  drawForm,
  identifier,
  isVariadicBlock,
  Refusal,
  useEdits,
  useSearch,
} from "../ui/index.ts";
import type { BlockItem, Edits } from "../ui/index.ts";
import { own } from "../util/index.ts";
import type { AdvancedModule } from "./advanced.ts";

// Renders the advanced action for the block that the target names, the one
// kind of target that core hands Edit inputs, and draws it before it returns.
export const editInputs: AdvancedModule = {
  render({ el, core, target }) {
    if (!("block" in target)) return undefined;
    return drawForm(el, <EditInputs core={core} block={target.block} />);
  },
};

// What each input of the block draws with: the edits of the advanced action,
// the board, the block registry, the block, and the blocks that its selects
// offer.
interface Parts {
  readonly edits: Edits;
  readonly board: Board;
  readonly registry: BlockRegistry;
  readonly block: BlockId;
  readonly sources: readonly BlockItem[];
}

function EditInputs({ core, block }: { core: CoreApi; block: BlockId }): ReactElement | null {
  const board = useBoard(core);
  const edits = useEdits(core);
  // Core ends the advanced action as the block leaves the board, and it draws
  // nothing until then.
  if (own(board.blocks, block) === undefined) return null;
  const registry = core.blockRegistry();
  const inputs = classInputs(board, registry, block);
  const parts: Parts = { edits, board, registry, block, sources: sourcesOf(board, registry, block) };
  const variadic = isVariadicBlock(board, registry, block);
  const name = blockName(board, block);
  return (
    <div className="bc:grid bc:gap-3 bc:font-sans bc:text-sm bc:text-text-default">
      <p className={styles.hint}>
        {variadic ? `The inputs of the block "${name}", which it takes in this order.` : `The inputs of the block "${name}".`}
      </p>
      {variadic ? <VariadicInputs {...parts} /> : <FixedInputs {...parts} inputs={inputs} />}
      <Refusal refusal={edits.refusal} />
    </div>
  );
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

// The inputs of a block that is not variadic, in the order of its class's
// inputs.
function FixedInputs({ inputs, ...parts }: Parts & { inputs: readonly string[] }): ReactElement {
  return (
    <div className="bc:grid bc:gap-2">
      {inputs.map((input) => (
        <FixedInput key={input} {...parts} input={input} />
      ))}
    </div>
  );
}

// An input of a block that is not variadic: the select of its source, and the
// button that removes the source, where a link feeds the input.
function FixedInput({ edits, board, registry, block, sources, input }: Parts & { input: string }): ReactElement {
  const search = useSearch(sources, blockTexts, "No block can feed the input.");
  const id = linkFeeding(board, registry, block, input);
  const link = id === undefined ? undefined : own(board.links, id);
  const what = `the input "${input}"`;
  return (
    <div className="bc:flex bc:items-end bc:gap-2">
      <ComboBox
        label={input}
        {...search}
        value={link?.from ?? null}
        placeholder="Search blocks"
        // The select of an input that a link feeds shows no button that
        // clears it, since the button beside it removes the source. Emptying
        // the field removes nothing either, since onChange() passes over the
        // empty pick that it then gets.
        isRequired={link !== undefined}
        className="bc:min-w-0 bc:flex-1"
        onChange={(key) => {
          if (key === null) return;
          const from = String(key);
          const title = `Could not change the source of ${what}.`;
          if (id === undefined || link === undefined) {
            void edits.run(title, "blockr.core-add_link", { from, to: block, input });
          } else if (from !== link.from) {
            void edits.run(title, "blockr.core-change_link", { id, from });
          }
        }}
      >
        {blockRow}
      </ComboBox>
      <div className="bc:flex bc:w-7 bc:shrink-0 bc:justify-center bc:pb-1">
        {id === undefined ? null : (
          <RemoveButton
            label={`Remove the source of ${input}`}
            onPress={() => {
              void edits.run(`Could not remove the source of ${what}.`, "blockr.core-remove_link", { id });
            }}
          />
        )}
      </div>
    </div>
  );
}

// The inputs of a variadic block, the links into it in their order on the
// board, in a list whose items the user drags into another order, and the
// select that adds an input below it.
function VariadicInputs(parts: Parts): ReactElement {
  const { edits, board, block } = parts;
  const order = createGraph(board).linksInto(block);
  const rows = order.flatMap((id, i) => {
    const link = own(board.links, id);
    return link === undefined ? [] : [{ id, link, place: i + 1 }];
  });
  const { dragAndDropHooks } = useDragAndDrop({
    getItems: (keys) => [...keys].map((key) => ({ "text/plain": String(key) })),
    onReorder(event) {
      const [moved] = [...event.keys];
      if (moved === undefined) return;
      const id = String(moved);
      const before = placeBefore(order, id, String(event.target.key), event.target.dropPosition);
      if (before === null) return;
      void edits.run(`Could not move input ${String(order.indexOf(id) + 1)}.`, "blockr.core-move_link", {
        id,
        ...(before === undefined ? {} : { before }),
      });
    },
  });
  return (
    <>
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
          <GridListItem id={row.id} textValue={inputText(board, row.link, row.place)} className={ROW}>
            <VariadicInput {...parts} id={row.id} link={row.link} place={row.place} />
          </GridListItem>
        )}
      </GridList>
      <AddInput {...parts} />
    </>
  );
}

// The link that the link `moved` goes before, as move_link takes it, where
// the user drops it before or after the link `target` among the links into
// its block, which `order` holds in their order. It is undefined where the
// link goes last, and null where the link stays in its place, so that the drop
// moves nothing.
function placeBefore(order: readonly LinkId[], moved: LinkId, target: LinkId, position: "before" | "after" | "on"): LinkId | undefined | null {
  const others = order.filter((id) => id !== moved);
  const at = others.indexOf(target);
  if (moved === target || position === "on" || at === -1) return null;
  const index = position === "before" ? at : at + 1;
  const next = [...others.slice(0, index), moved, ...others.slice(index)];
  if (next.every((id, i) => id === order[i])) return null;
  return others[index];
}

// What an item of the list says to assistive technology, which React Aria
// reads out as the user drags it: its place, its source and its name, if it
// has one.
function inputText(board: Pick<Board, "blocks">, link: Link, place: number): string {
  const from = blockName(board, link.from);
  return link.input === "" ? `Input ${String(place)}, from ${from}` : `Input ${String(place)}, ${link.input}, from ${from}`;
}

// An input of a variadic block, one link into it: the select of its source,
// the field of its name and the button that removes it. The list draws the
// button that drags it before them.
function VariadicInput({ edits, board, block, sources, id, link, place }: Parts & { id: LinkId; link: Link; place: number }): ReactElement {
  const search = useSearch(sources, blockTexts, "No block can feed the block.");
  const what = `input ${String(place)}`;
  return (
    <div className="bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2" onPointerDownCapture={holdStill}>
      <ComboBox
        aria-label={`Source of ${what}`}
        {...search}
        value={link.from}
        placeholder="Search blocks"
        // A link always has a source, so the select shows no button that
        // clears it, and emptying the field changes nothing, since onChange()
        // passes over the empty pick that it then gets.
        isRequired
        className="bc:min-w-0 bc:flex-1"
        onChange={(key) => {
          if (key === null || String(key) === link.from) return;
          void edits.run(`Could not change the source of ${what}.`, "blockr.core-change_link", { id, from: String(key) });
        }}
      >
        {blockRow}
      </ComboBox>
      <InputName edits={edits} board={board} block={block} id={id} link={link} what={what} />
      <RemoveButton
        label={`Remove ${what}`}
        onPress={() => {
          void edits.run(`Could not remove ${what}.`, "blockr.core-remove_link", { id });
        }}
      />
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

// The field of the name of an input of a variadic block, which the user may
// leave empty, for an input without a name. The name goes in as the user
// leaves the field or presses Enter, unless another link into the block gives
// it already, which the field then says in place. The field shows the name
// that the board holds anew wherever that changes, as on an undo.
function InputName({
  edits,
  board,
  block,
  id,
  link,
  what,
}: Pick<Parts, "edits" | "board" | "block"> & { id: LinkId; link: Link; what: string }): ReactElement {
  const [text, setText] = useState(link.input);
  const [error, setError] = useState<string | undefined>(undefined);
  // The name that the field last took from the board.
  const [held, setHeld] = useState(link.input);
  if (held !== link.input) {
    setHeld(link.input);
    setText(link.input);
    setError(undefined);
  }

  function submit(): void {
    const name = text.trim();
    const found = inputNameError(name, namesInto(board, block, id));
    setError(found);
    if (found !== undefined) {
      // The check that stops the edit is the user's next edit there, so the
      // banner of the last one goes.
      edits.clear();
      return;
    }
    if (name !== link.input) void edits.run(`Could not rename ${what}.`, "blockr.core-change_link", { id, input: name });
  }

  return (
    <TextField
      aria-label={`Name of ${what}`}
      value={text}
      placeholder="No name"
      isInvalid={error !== undefined}
      errorMessage={error}
      autoComplete="off"
      {...identifier}
      className="bc:w-1/3 bc:shrink-0"
      onChange={(value) => {
        setText(value);
        setError(undefined);
      }}
      onKeyDown={(event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) submit();
        else event.continuePropagation();
      }}
      onBlur={submit}
    />
  );
}

// The select that adds an input to a variadic block: a link from the block
// that the user picks, without a name, which goes last among the block's
// links. The select shows the pick until the link is added, and is empty
// again then, ready for the next.
function AddInput({ edits, board, block, sources }: Parts): ReactElement {
  const search = useSearch(sources, blockTexts, "No block can feed the block.");
  const [picked, setPicked] = useState<BlockId | null>(null);
  return (
    <ComboBox
      label="Add input"
      {...search}
      value={picked}
      placeholder="Search blocks"
      onChange={(key) => {
        if (key === null) return;
        const from = String(key);
        setPicked(from);
        void edits.run(`Could not add an input from "${blockName(board, from)}".`, "blockr.core-add_link", { from, to: block }).then(() => {
          setPicked(null);
        });
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

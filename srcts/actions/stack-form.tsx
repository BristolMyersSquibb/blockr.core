// The form of a stack, the advanced action of Create stack and of Edit stack
// (./stacks.ts), as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action):
// the stack's name, its colour and its blocks.
// It picks the blocks in a select of several, whose list the user filters, so
// that it stays usable on a board of many blocks.
// A block sits in one stack at most. So the select lists the blocks in no
// stack first, and apart from them those that another stack holds, naming the
// stack, which a block that the user picks leaves.
// It draws a stack's colour with the colours of core's React SDK: a colour
// picker, beside a checkbox that gives the stack a colour. The picker's
// popover holds the palette's colours among its swatches.
// Each form runs nothing before the user submits it, and then hands done()
// the batch, which runs as one step of the user:
// - The form that adds a stack, Create stack's, starts with the blocks that
//   its target names, the block or those of the selection, or with none on
//   the board, and with a name and a colour that no other stack has, such as
//   "Stack 2" and the first colour of a palette that no stack uses, which the
//   user may change or, for the colour, leave out. It mints the stack's ID
//   through CoreApi's mintId() and adds the stack with add_stack.
// - The form that edits a stack, Edit stack's, renames it with rename_stack
//   and sets its colour and its blocks with change_stack, each where it
//   changed. A name that the user leaves empty keeps the stack's name.
// Where the store refuses the batch, the form says why below its button, and
// keeps what the user gave. It reads the board as it is, through the store.

import { useState } from "react";
import type { ReactElement, SyntheticEvent } from "react";
import {
  Checkbox,
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ComboBox,
  ComboBoxSection,
  Form,
  TextField,
  useBoard,
} from "../react/index.ts";
import type { Color } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockId, Board, Stack, StackId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { blockItem, blockRow, blockTexts, classes, drawForm, Refusal, useEdits, useSearch } from "../ui/index.ts";
import type { BlockItem } from "../ui/index.ts";
import { own } from "../util/index.ts";
import type { AdvancedModule } from "./advanced.ts";
import type { CommandCall, Target } from "./actions.ts";

const ADD_STACK = "blockr.core-add_stack";
const CHANGE_STACK = "blockr.core-change_stack";
const RENAME_STACK = "blockr.core-rename_stack";

// The colours that a new stack starts from, ColorBrewer's Set2, of which a new
// stack takes the first that no stack has.
export const PALETTE = ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"] as const;

// The first colour of the palette that no stack has, or else the one that
// follows the number of stacks round the palette.
export function suggestedColor(stacks: readonly Stack[]): string {
  const used = new Set(stacks.map((stack) => stack.color?.toLowerCase()));
  return PALETTE.find((color) => !used.has(color)) ?? PALETTE[stacks.length % PALETTE.length] ?? PALETTE[0];
}

// "Stack 1", or the first "Stack n" that no stack has.
export function suggestedName(stacks: readonly Stack[]): string {
  const names = new Set(stacks.map((stack) => stack.name));
  let n = 1;
  while (names.has(`Stack ${String(n)}`)) n++;
  return `Stack ${String(n)}`;
}

type Done = Parameters<AdvancedModule["render"]>[0]["done"];

// Create stack's advanced action.
export const createStackForm: AdvancedModule = {
  render({ el, core, target, done }) {
    return drawForm(el, <AddStack core={core} given={stackedBlocks(core.store.board(), target)} done={done} />);
  },
};

// Edit stack's advanced action, which core hands a stack alone.
export const editStackForm: AdvancedModule = {
  render({ el, core, target, done }) {
    if (!("stack" in target)) return undefined;
    return drawForm(el, <EditStack core={core} id={target.stack} done={done} />);
  },
};

// The blocks that Create stack puts in a stack: the block that the target
// names, or those of a selection, which the board holds, and none on the
// board.
export function stackedBlocks(board: Board, target: Target): BlockId[] {
  if ("block" in target) return Object.hasOwn(board.blocks, target.block) ? [target.block] : [];
  if (!("selection" in target)) return [];
  return target.selection.flatMap((object) => ("block" in object && Object.hasOwn(board.blocks, object.block) ? [object.block] : []));
}

function AddStack({ core, given, done }: { core: CoreApi; given: readonly BlockId[]; done: Done }): ReactElement {
  const board = useBoard(core);
  return <StackForm core={core} board={board} given={given} done={done} />;
}

// The form that edits the stack, which draws nothing once the board no longer
// holds it, since core then ends it.
function EditStack({ core, id, done }: { core: CoreApi; id: StackId; done: Done }): ReactElement | null {
  const board = useBoard(core);
  const stack = own(board.stacks, id);
  return stack === undefined ? null : <StackForm core={core} board={board} editing={{ id, stack }} done={done} />;
}

// A colour as the form holds it: the hex of a stack's colour or of the
// palette, or the colour that the picker gave.
// The form keeps the colour as the picker gave it, in the colour space of the
// part that the user changed it in, rather than as hex.
// The hex of a grey holds no hue, so the picker's hue stays where the user
// left it.
type Picked = string | Color;

// The hex of a colour that the form holds, as a stack holds it: six digits,
// as React Aria's color.toString("hex") writes them.
function stackColor(color: Picked): string {
  return typeof color === "string" ? color : color.toString("hex");
}

// Whether two colours of stacks are one, which their hex may give in either
// case.
function sameColor(a: string | null, b: string | null): boolean {
  return a === null || b === null ? a === b : a.toLowerCase() === b.toLowerCase();
}

// The form that adds a stack of the blocks given, or that changes the stack
// given.
function StackForm({
  core,
  board,
  given = [],
  editing,
  done,
}: {
  core: CoreApi;
  board: Board;
  given?: readonly BlockId[];
  editing?: { readonly id: StackId; readonly stack: Stack };
  done: Done;
}): ReactElement {
  const registry = core.blockRegistry();
  const stacks = Object.values(board.stacks);
  const [name, setName] = useState(editing?.stack.name ?? "");
  const [color, setColor] = useState<Picked | null>(() => (editing === undefined ? suggestedColor(stacks) : editing.stack.color));
  const [blocks, setBlocks] = useState<readonly BlockId[]>(editing?.stack.blocks ?? given);
  const [busy, setBusy] = useState(false);
  const edits = useEdits(core);
  const fallbackName = editing === undefined ? suggestedName(stacks) : editing.stack.name;

  // The stack that holds each block, but the one that the form changes.
  const holder = new Map<BlockId, string>();
  for (const [id, stack] of Object.entries(board.stacks)) {
    if (id === editing?.id) continue;
    for (const block of stack.blocks) holder.set(block, stack.name);
  }
  // The blocks, those in no stack first, and those in another stack, which
  // say that they leave it.
  const options = Object.keys(board.blocks).map((id): BlockItem & { readonly free: boolean } => {
    const item = blockItem(board, registry, id);
    const other = holder.get(id);
    return other === undefined ? { ...item, free: true } : { ...item, free: false, detail: `${item.detail} · leaves the stack ${other}` };
  });
  const search = useSearch(options, blockTexts, "The board holds no block.");
  const sections = [
    { id: "free", title: "In no stack", items: search.items.filter((item) => item.free) },
    { id: "held", title: "In another stack", items: search.items.filter((item) => !item.free) },
  ].filter((section) => section.items.length > 0);
  // The blocks that the board still holds, in the order that the user picked
  // them.
  const kept = blocks.filter((block) => Object.hasOwn(board.blocks, block));

  // The batch that the form runs: add_stack with an ID that CoreApi mints, or
  // rename_stack and change_stack, each where it changes the stack.
  function batch(): CommandCall[] {
    const trimmed = name.trim();
    const hex = color === null ? null : stackColor(color);
    if (editing === undefined) {
      const id = core.mintId((minted) => Object.hasOwn(core.store.board().stacks, minted));
      return [{ id: ADD_STACK, args: { id, blocks: kept, color: hex, name: trimmed === "" ? fallbackName : trimmed } }];
    }
    const { id, stack } = editing;
    const changed: Record<string, Json> = {
      ...(sameBlocks(kept, stack.blocks) ? {} : { blocks: kept }),
      ...(sameColor(hex, stack.color) ? {} : { color: hex }),
    };
    return [
      ...(trimmed === "" || trimmed === stack.name ? [] : [{ id: RENAME_STACK, args: { id, name: trimmed } }]),
      ...(Object.keys(changed).length === 0 ? [] : [{ id: CHANGE_STACK, args: { id, ...changed } }]),
    ];
  }

  async function submit(event: SyntheticEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    if (busy) return;
    // The sentence that names the edit, which names the stack as its row does
    // where the form changes one.
    const title = editing === undefined ? "Could not add the stack." : `Could not change the stack "${editing.stack.name}".`;
    const calls = batch();
    setBusy(true);
    // Once the batch has run, core ends the form, or renders it afresh in a
    // plugin's room, so that only a refusal leaves it as the user filled it in.
    await edits.attempt(title, (opts) => done(calls, opts));
    setBusy(false);
  }

  return (
    <Form
      aria-label={editing === undefined ? "Add stack" : "Change stack"}
      validationBehavior="aria"
      onSubmit={(event) => {
        void submit(event);
      }}
    >
      <div className="bc:grid bc:grid-cols-2 bc:items-start bc:gap-2">
        <TextField
          label="Name"
          value={name}
          placeholder={fallbackName}
          description={editing === undefined ? `Optional. "${fallbackName}" where left empty.` : undefined}
          autoComplete="off"
          onChange={setName}
        />
        <StackColor color={color} onChange={setColor} />
      </div>
      <ComboBox
        selectionMode="multiple"
        label="Blocks"
        {...search}
        items={sections}
        value={kept}
        placeholder="No blocks"
        description="A block sits in one stack at most."
        onChange={(keys) => {
          setBlocks(keys.map(String));
        }}
      >
        {(section) => (
          <ComboBoxSection id={section.id} title={section.title} items={section.items}>
            {blockRow}
          </ComboBoxSection>
        )}
      </ComboBox>
      <div className="bc:flex bc:gap-2">
        <button type="submit" className={classes.primary} disabled={busy}>
          {editing === undefined ? "Add stack" : "Save"}
        </button>
      </div>
      <Refusal refusal={edits.refusal} />
    </Form>
  );
}

// A stack's colour, or none: the SDK's checkbox, which gives the stack a
// colour, and the SDK's colour picker, which picks it.
// The picker's popover holds the starter kit's parts, an area of the
// saturation and the brightness, a slider of the hue and a field of the hex,
// and below them the palette's colours as swatches, in two rows of four,
// which pick one at a click.
// While the stack has no colour, the picker shows the colour last picked and
// is disabled, and the checkbox gives the stack that colour again.
// The picker's button names the colour by its hex too.
function StackColor({ color, onChange }: { color: Picked | null; onChange: (color: Picked | null) => void }): ReactElement {
  const [last, setLast] = useState<Picked>(color ?? PALETTE[0]);
  const shown = color ?? last;
  return (
    <fieldset className={styles.fieldset}>
      <legend className={styles.legend}>Colour</legend>
      <div className="bc:flex bc:flex-wrap bc:items-center bc:gap-x-3 bc:gap-y-1">
        <Checkbox
          isSelected={color !== null}
          onChange={(selected) => {
            onChange(selected ? last : null);
          }}
        >
          A colour of its own
        </Checkbox>
        <ColorPicker
          label={stackColor(shown).toUpperCase()}
          value={shown}
          isDisabled={color === null}
          onChange={(picked) => {
            setLast(picked);
            onChange(picked);
          }}
        >
          <ColorArea colorSpace="hsb" xChannel="saturation" yChannel="brightness" />
          <ColorSlider colorSpace="hsb" channel="hue" />
          <ColorField label="Hex" />
          <ColorSwatchPicker aria-label="Palette" className="bc:max-w-36">
            {PALETTE.map((hex) => (
              <ColorSwatchPickerItem key={hex} color={hex} />
            ))}
          </ColorSwatchPicker>
        </ColorPicker>
      </div>
    </fieldset>
  );
}

function sameBlocks(a: readonly BlockId[], b: readonly BlockId[]): boolean {
  return a.length === b.length && a.every((block, i) => block === b[i]);
}

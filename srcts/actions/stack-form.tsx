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
// The form that adds a stack, Create stack's, runs nothing before the user
// submits it. It starts with the blocks that its target names, the block or
// those of the selection, or with none on the board, and with a name and a
// colour that no other stack has, such as "Stack 2" and the first colour of
// a palette that no stack uses, which the user may change or, for the colour,
// leave out. Once the user submits it, it mints the stack's ID through
// CoreApi's mintId() and hands done() the batch that adds the stack. Where the
// store refuses the batch, it says why below its button, and keeps what the
// user gave.
// The form that edits a stack, Edit stack's, applies each edit as the user
// makes it, through core.commands.run(), as one step of the user, so that it
// needs no button: a name, once the user leaves its field or presses Enter
// there, with rename_stack, and a colour, as the user lets go of a part of the
// picker or picks a swatch, or the checkbox, and each block that the user
// picks or unpicks, with change_stack. Where the store refuses an edit, the
// form says why below itself, until the user's next edit there. It shows the
// stack as the board holds it, after each edit.
// Both read the board as it is, through the store.

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
import { blockItem, blockRow, blockTexts, classes, drawForm, EditField, Refusal, useEdits, useSearch } from "../ui/index.ts";
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
    return drawForm(el, <CreateStack core={core} given={stackedBlocks(core.store.board(), target)} done={done} />);
  },
};

// Edit stack's advanced action, which core hands a stack alone.
export const editStackForm: AdvancedModule = {
  render({ el, core, target }) {
    if (!("stack" in target)) return undefined;
    return drawForm(el, <EditStack core={core} id={target.stack} />);
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

// The form that adds a stack, once the user submits it.
function CreateStack({ core, given, done }: { core: CoreApi; given: readonly BlockId[]; done: Done }): ReactElement {
  const board = useBoard(core);
  const stacks = Object.values(board.stacks);
  const [name, setName] = useState("");
  const [color, setColor] = useState<Picked | null>(() => suggestedColor(stacks));
  const [blocks, setBlocks] = useState<readonly BlockId[]>(given);
  const [busy, setBusy] = useState(false);
  const edits = useEdits(core);
  const fallbackName = suggestedName(stacks);
  // The blocks that the board still holds, in the order that the user picked
  // them.
  const kept = blocks.filter((block) => Object.hasOwn(board.blocks, block));

  async function submit(event: SyntheticEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    if (busy) return;
    const trimmed = name.trim();
    const id = core.mintId((minted) => Object.hasOwn(core.store.board().stacks, minted));
    const batch: CommandCall[] = [
      { id: ADD_STACK, args: { id, blocks: kept, color: color === null ? null : stackColor(color), name: trimmed === "" ? fallbackName : trimmed } },
    ];
    setBusy(true);
    // Once the batch has run, core ends the form, or renders it afresh in a
    // plugin's room, so that only a refusal leaves it as the user filled it in.
    await edits.attempt("Could not add the stack.", (opts) => done(batch, opts));
    setBusy(false);
  }

  return (
    <Form
      aria-label="Add stack"
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
          description={`Optional. "${fallbackName}" where left empty.`}
          autoComplete="off"
          onChange={setName}
        />
        <StackColor
          color={color}
          onChange={(picked) => {
            setColor(picked);
          }}
        />
      </div>
      <StackBlocks
        board={board}
        core={core}
        blocks={kept}
        onChange={(picked) => {
          setBlocks(picked);
        }}
      />
      <div className="bc:flex bc:gap-2">
        <button type="submit" className={classes.primary} disabled={busy}>
          Add stack
        </button>
      </div>
      <Refusal refusal={edits.refusal} />
    </Form>
  );
}

// The form that edits the stack, which draws nothing once the board no longer
// holds it, since core then ends it.
function EditStack({ core, id }: { core: CoreApi; id: StackId }): ReactElement | null {
  const board = useBoard(core);
  const stack = own(board.stacks, id);
  return stack === undefined ? null : <StackEdits core={core} board={board} id={id} stack={stack} />;
}

function StackEdits({ core, board, id, stack }: { core: CoreApi; board: Board; id: StackId; stack: Stack }): ReactElement {
  const edits = useEdits(core);
  // The colour that the user drags a part of the picker to, which the stack
  // takes as the user lets go, until the board holds the stack's colour again.
  const [dragged, setDragged] = useState<{ readonly color: Picked | null } | undefined>(undefined);
  const kept = stack.blocks.filter((block) => Object.hasOwn(board.blocks, block));
  // The sentence that names the edit, as the stack is now.
  const title = `Could not change the stack "${stack.name}".`;

  function recolor(color: Picked | null): void {
    setDragged(undefined);
    const hex = color === null ? null : stackColor(color);
    if (!sameColor(hex, stack.color)) void edits.run(title, CHANGE_STACK, { id, color: hex });
  }

  return (
    <Form
      aria-label="Change stack"
      validationBehavior="aria"
      onSubmit={(event) => {
        event.preventDefault();
      }}
    >
      <div className="bc:grid bc:grid-cols-2 bc:items-start bc:gap-2">
        <EditField
          key={stack.name}
          label="Name"
          value={stack.name}
          check={(name) => (name === "" ? "A name cannot be empty." : undefined)}
          onSubmit={(name) => {
            void edits.run(title, RENAME_STACK, { id, name });
          }}
        />
        <StackColor
          color={dragged === undefined ? stack.color : dragged.color}
          onChange={(color, ended) => {
            if (ended) recolor(color);
            else setDragged({ color });
          }}
        />
      </div>
      <StackBlocks
        board={board}
        core={core}
        stack={id}
        blocks={kept}
        onChange={(blocks) => {
          if (!sameBlocks(blocks, kept)) void edits.run(title, CHANGE_STACK, { id, blocks: [...blocks] });
        }}
      />
      <Refusal refusal={edits.refusal} />
    </Form>
  );
}

// The select of a stack's blocks, those in no stack first, and those in
// another stack, which say that they leave it. The stack that the form edits,
// if any, holds no block of another.
function StackBlocks({
  core,
  board,
  stack,
  blocks,
  onChange,
}: {
  core: CoreApi;
  board: Board;
  stack?: StackId;
  blocks: readonly BlockId[];
  onChange: (blocks: readonly BlockId[]) => void;
}): ReactElement {
  const registry = core.blockRegistry();
  // The stack that holds each block, but the one that the form edits.
  const holder = new Map<BlockId, string>();
  for (const [id, other] of Object.entries(board.stacks)) {
    if (id === stack) continue;
    for (const block of other.blocks) holder.set(block, other.name);
  }
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
  return (
    <ComboBox
      selectionMode="multiple"
      label="Blocks"
      {...search}
      items={sections}
      value={blocks}
      placeholder="No blocks"
      description="A block sits in one stack at most."
      onChange={(keys) => {
        onChange(keys.map(String));
      }}
    >
      {(section) => (
        <ComboBoxSection id={section.id} title={section.title} items={section.items}>
          {blockRow}
        </ComboBoxSection>
      )}
    </ComboBox>
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
// It calls onChange with each colour that the user changes it to, and says
// whether the user has ended the change: let go of the area or the slider,
// left the field or pressed Enter there, picked a swatch or clicked the
// checkbox.
function StackColor({ color, onChange }: { color: Picked | null; onChange: (color: Picked | null, ended: boolean) => void }): ReactElement {
  const [last, setLast] = useState<Picked>(color ?? PALETTE[0]);
  const shown = color ?? last;
  const ended = (picked: Picked): void => {
    setLast(picked);
    onChange(picked, true);
  };
  return (
    <fieldset className={styles.fieldset}>
      <legend className={styles.legend}>Colour</legend>
      <div className="bc:flex bc:flex-wrap bc:items-center bc:gap-x-3 bc:gap-y-1">
        <Checkbox
          isSelected={color !== null}
          onChange={(selected) => {
            onChange(selected ? last : null, true);
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
            onChange(picked, false);
          }}
        >
          <ColorArea colorSpace="hsb" xChannel="saturation" yChannel="brightness" onChangeEnd={ended} />
          <ColorSlider colorSpace="hsb" channel="hue" onChangeEnd={ended} />
          <ColorField
            label="Hex"
            onChange={(picked) => {
              if (picked !== null) ended(picked);
            }}
          />
          <ColorSwatchPicker aria-label="Palette" className="bc:max-w-36" onChange={ended}>
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

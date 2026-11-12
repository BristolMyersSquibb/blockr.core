// The Edit board plugin's section of stacks. For each stack, it shows these:
// - The stack's name, which rename_stack changes.
// - Its colour and its blocks, which change_stack changes.
// - A button for remove_stack, which leaves the stack's blocks on the board.
// Below the stacks, a button adds an empty stack with add_stack, which asks for
// no id, since the command mints one.
// A block sits in one stack at most, so checking a block in one stack takes
// it out of the stack it was in.
// Under the board lock, it lists each stack alone, with its colour and its
// blocks.

import { useEffect, useRef, useState } from "react";
import type { ReactElement } from "react";
import { styles } from "../../styles/index.ts";
import type { Board, Stack, StackId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { classes, NameField } from "../../ui/index.ts";
import { Section } from "./board.tsx";
import { blockName } from "./links.tsx";
import { runCommand } from "./run.ts";

// The colour that a stack without one shows in its picker.
const NO_COLOR = "#808080";

export function StacksSection({ core, board, locked }: { core: CoreApi; board: Board; locked: boolean }): ReactElement {
  const stacks = Object.entries(board.stacks);

  return (
    <Section title="Stacks">
      {stacks.length === 0 ? <p className={styles.hint}>The board has no stacks yet.</p> : null}
      <ul aria-label="Stacks" className={classes.list}>
        {stacks.map(([id, stack]) =>
          locked ? (
            <StackText key={id} stack={stack} board={board} />
          ) : (
            <StackItem key={id} core={core} id={id} stack={stack} board={board} />
          ),
        )}
      </ul>
      {locked ? null : (
        <div>
          <button
            type="button"
            className={classes.primary}
            onClick={() => {
              void runCommand(core, "blockr.core-add_stack", {});
            }}
          >
            Add stack
          </button>
        </div>
      )}
    </Section>
  );
}

// A stack as the user reads it under the board lock: its name in its colour,
// and its blocks.
function StackText({ stack, board }: { stack: Stack; board: Board }): ReactElement {
  return (
    <li aria-label={stack.name} className="bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:text-sm">
      <span
        aria-hidden="true"
        className="bc:size-3 bc:rounded-full bc:border bc:border-border-default"
        style={stack.color === null ? undefined : { backgroundColor: stack.color }}
      />
      <span className="bc:font-medium">{stack.name}</span>
      <span className="bc:text-text-muted">
        {stack.blocks.length === 0 ? "No blocks" : stack.blocks.map((block) => blockName(board, block)).join(", ")}
      </span>
    </li>
  );
}

function StackItem({ core, id, stack, board }: { core: CoreApi; id: StackId; stack: Stack; board: Board }): ReactElement {
  const [renaming, setRenaming] = useState(false);

  return (
    <li aria-label={stack.name} className="bc:grid bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:p-2">
      <div className="bc:flex bc:flex-wrap bc:items-center bc:gap-2">
        {renaming ? (
          <NameField
            label="Stack name"
            name={stack.name}
            onSubmit={(name) => {
              void runCommand(core, "blockr.core-rename_stack", { id, name });
            }}
            onDone={() => {
              setRenaming(false);
            }}
          />
        ) : (
          <span className="bc:text-sm bc:font-medium">{stack.name}</span>
        )}
        {renaming ? null : (
          <button
            type="button"
            className={classes.quiet}
            aria-label={`Rename the stack ${stack.name}`}
            onClick={() => {
              setRenaming(true);
            }}
          >
            Rename
          </button>
        )}
        <ColorField
          label={`Colour of ${stack.name}`}
          color={stack.color}
          onChange={(color) => {
            void runCommand(core, "blockr.core-change_stack", { id, color });
          }}
        />
        {stack.color === null ? null : (
          <button
            type="button"
            className={classes.quiet}
            onClick={() => {
              void runCommand(core, "blockr.core-change_stack", { id, color: null });
            }}
          >
            No colour
          </button>
        )}
        <button
          type="button"
          className={`${classes.button} bc:ml-auto`}
          aria-label={`Remove the stack ${stack.name}`}
          onClick={() => {
            void runCommand(core, "blockr.core-remove_stack", { id });
          }}
        >
          Remove
        </button>
      </div>
      <fieldset className={styles.fieldset}>
        <legend className={styles.legend}>Blocks</legend>
        {Object.keys(board.blocks).length === 0 ? <p className={styles.hint}>The board holds no blocks.</p> : null}
        <div className={styles.list}>
          {Object.entries(board.blocks).map(([block, { name }]) => (
            <label key={block} className={styles.check}>
              <input
                type="checkbox"
                className={styles.checkbox}
                checked={stack.blocks.includes(block)}
                onChange={(event) => {
                  const blocks = event.target.checked ? [...stack.blocks, block] : stack.blocks.filter((b) => b !== block);
                  void runCommand(core, "blockr.core-change_stack", { id, blocks });
                }}
              />
              {name} ({block})
            </label>
          ))}
        </div>
      </fieldset>
    </li>
  );
}

// A colour input that submits the colour once the user has picked it, on the
// input's change, rather than at every step of dragging through the picker.
function ColorField({
  label,
  color,
  onChange,
}: {
  label: string;
  color: string | null;
  onChange: (color: string) => void;
}): ReactElement {
  const ref = useRef<HTMLInputElement>(null);
  const latest = useRef(onChange);
  useEffect(() => {
    latest.current = onChange;
  });
  useEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const picked = (): void => {
      latest.current(el.value);
    };
    el.addEventListener("change", picked);
    return () => {
      el.removeEventListener("change", picked);
    };
  }, []);
  return (
    <input
      ref={ref}
      type="color"
      aria-label={label}
      defaultValue={color ?? NO_COLOR}
      key={color ?? NO_COLOR}
      className="bc:h-7 bc:w-10 bc:cursor-pointer bc:rounded bc:border bc:border-border-default bc:bg-transparent bc:p-0.5"
    />
  );
}

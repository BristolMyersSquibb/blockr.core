// The toolbar of core's basic frontend. It shows the board's name, which the
// user renames, and the controls for the board: adding a block, editing links
// and stacks, setting the board options, undo and redo, saving the board as a
// file and opening one.
// On a locked board, it draws no control for what the lock fixes. Only undo,
// redo and saving stay, since a locked board opens no other board.

import { useRef, useState } from "react";
import type { ReactElement } from "react";
import type { Board } from "../../types/board.ts";
import { runCommand, useFrontEnd } from "./context.ts";
import { download, fileName } from "./file.ts";
import { useHistory } from "./hooks.ts";
import { classes, NameField } from "./ui.tsx";

// A dialog that the toolbar opens.
export type ToolbarDialog = "add" | "links" | "stacks" | "options";

export function Toolbar({ board, show }: { board: Board; show: (dialog: ToolbarDialog) => void }): ReactElement {
  const front = useFrontEnd();
  const { core, notices } = front;
  const { canUndo, canRedo } = useHistory(core);
  const [renaming, setRenaming] = useState(false);
  const picker = useRef<HTMLInputElement>(null);
  const locked = core.lock !== null;

  function step(back: boolean): void {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch (error) {
      notices.refused(error);
    }
  }

  function save(): void {
    try {
      download(core.store.save(), fileName(board));
    } catch (error) {
      notices.refused(error);
    }
  }

  async function open(input: HTMLInputElement): Promise<void> {
    const file = input.files?.[0];
    input.value = "";
    if (file === undefined) return;
    try {
      await core.store.open(await file.text());
    } catch (error) {
      notices.refused(error);
    }
  }

  return (
    <header className="bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:border-b bc:border-border-default bc:pb-3">
      {renaming ? (
        <NameField
          label="Board name"
          name={board.name}
          onSubmit={(name) => {
            void runCommand(front, "blockr.core-rename_board", { name });
          }}
          onDone={() => {
            setRenaming(false);
          }}
        />
      ) : (
        <h1 className="bc:m-0 bc:font-sans bc:text-lg bc:font-semibold">{board.name}</h1>
      )}
      {locked || renaming ? null : (
        <button
          type="button"
          className={classes.quiet}
          aria-label="Rename board"
          onClick={() => {
            setRenaming(true);
          }}
        >
          Rename
        </button>
      )}
      <div role="toolbar" aria-label="Board" className="bc:ml-auto bc:flex bc:flex-wrap bc:items-center bc:gap-2">
        {locked ? null : (
          <>
            <button
              type="button"
              className={classes.primary}
              onClick={() => {
                show("add");
              }}
            >
              Add block
            </button>
            <button
              type="button"
              className={classes.button}
              onClick={() => {
                show("links");
              }}
            >
              Links
            </button>
            <button
              type="button"
              className={classes.button}
              onClick={() => {
                show("stacks");
              }}
            >
              Stacks
            </button>
            <button
              type="button"
              className={classes.button}
              onClick={() => {
                show("options");
              }}
            >
              Options
            </button>
          </>
        )}
        <button
          type="button"
          className={classes.button}
          disabled={!canUndo}
          aria-keyshortcuts="Control+Z Meta+Z"
          onClick={() => {
            step(true);
          }}
        >
          Undo
        </button>
        <button
          type="button"
          className={classes.button}
          disabled={!canRedo}
          aria-keyshortcuts="Control+Shift+Z Meta+Shift+Z Control+Y"
          onClick={() => {
            step(false);
          }}
        >
          Redo
        </button>
        <button type="button" className={classes.button} onClick={save}>
          Save
        </button>
        {locked ? null : (
          <>
            <button
              type="button"
              className={classes.button}
              onClick={() => {
                picker.current?.click();
              }}
            >
              Open
            </button>
            <input
              ref={picker}
              type="file"
              accept=".json,application/json"
              aria-label="Board file to open"
              hidden
              onChange={(event) => {
                void open(event.currentTarget);
              }}
            />
          </>
        )}
      </div>
    </header>
  );
}

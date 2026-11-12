// The toolbar of core's basic frontend. It shows the board's name, which the
// user renames, and the controls for the board: adding a block, editing links
// and stacks, setting the board options, undo and redo, and saving the board
// and opening another.
// It saves and opens through the storage that core.plugins.storage() gives,
// that of the app's storage plugin, such as core's own, which saves a download
// and opens an upload, and draws neither control where the app runs none.
// Under the board lock, it draws no control for what the lock fixes, so that
// the board options, undo, redo, saving and opening stay. Under
// the load lock, it draws no control for opening a board, since the store
// opens no other board there.

import { useState } from "react";
import type { ReactElement } from "react";
import type { Board } from "../../types/board.ts";
import { runCommand, useFrontEnd } from "./context.ts";
import { useHistory } from "./hooks.ts";
import { classes, NameField } from "./ui.tsx";

// A dialog that the toolbar opens.
export type ToolbarDialog = "add" | "links" | "stacks" | "options";

export interface ToolbarProps {
  readonly board: Board;
  readonly show: (dialog: ToolbarDialog) => void;
}

export function Toolbar({ board, show }: ToolbarProps): ReactElement {
  const front = useFrontEnd();
  const { core, notices } = front;
  const { canUndo, canRedo } = useHistory(core);
  const [renaming, setRenaming] = useState(false);
  const locked = core.lock.board;
  const storage = core.plugins.storage();
  const opens = !core.lock.load && storage !== undefined;

  function step(back: boolean): void {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch (error) {
      notices.failed(error);
    }
  }

  async function save(): Promise<void> {
    try {
      await storage?.save(core.store.save());
    } catch (error) {
      notices.failed(error);
    }
  }

  // Asks the storage for a board as the user clicks, since a browser shows
  // its file picker only then, and opens what the user picks. A frontend that
  // starts again draws the board that opens, and core sends it the note of
  // what the store dropped, so what open() resolves with goes unread here.
  async function open(): Promise<void> {
    try {
      const text = (await storage?.open()) ?? null;
      if (text !== null) await core.store.open(text);
    } catch (error) {
      notices.failed(error);
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
          </>
        )}
        <button
          type="button"
          className={classes.button}
          onClick={() => {
            show("options");
          }}
        >
          Options
        </button>
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
        {storage === undefined ? null : (
          <button
            type="button"
            className={classes.button}
            onClick={() => {
              void save();
            }}
          >
            Save
          </button>
        )}
        {opens ? (
          <button
            type="button"
            className={classes.button}
            onClick={() => {
              void open();
            }}
          >
            Open
          </button>
        ) : null}
      </div>
    </header>
  );
}

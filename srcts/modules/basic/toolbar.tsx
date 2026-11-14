// The toolbar of core's basic frontend. It shows the board's name, which the
// user renames, and the controls for the board: setting the board options,
// undo and redo, and at its end the storage controls of the app's storage
// plugin, such as Save and Open of core's own, which save a download and open
// an upload. Adding a block and editing links and stacks are the Edit board
// plugin's, which the frontend shows beside the board.
// Under the board lock, it draws no control for what the lock fixes, so that
// the board options, undo, redo and the storage controls stay. The storage
// controls leave out what a lock fixes themselves, as core's own leave out
// Open under the load lock, since the store opens no other board there.
//
// The toolbar draws the storage controls that core.plugins.storage() gives
// (./storage.ts), and none where the app runs no storage plugin, each kind in
// its own look:
// - A button is one of the toolbar's buttons, greyed out while it is disabled.
// - A menu is a button that opens a list of what the menu holds. A menu in
//   that list shows as a group of its items under its label.
// - A status is text in the muted colour.
// A control's tooltip shows as the browser's tooltip.
// The toolbar draws the controls again whenever the storage's subscribe()
// calls back, and as the board changes. Where a button's run() rejects, the
// frontend logs it, since the storage plugin or core tells the user why.

import { useEffect, useReducer, useRef, useState } from "react";
import type { ReactElement } from "react";
import type { StorageControl } from "../../store/index.ts";
import type { Board } from "../../types/board.ts";
import { classes, NameField, runCommand, useHistory } from "../../ui/index.ts";
import { report } from "../../util/index.ts";
import { useFrontEnd } from "./context.ts";
import { ariaShortcuts } from "./shortcuts.ts";
import { drawnControls, runButton } from "./storage.ts";
import type { Button } from "./storage.ts";
import { menuClasses, useDismiss } from "./ui.tsx";

export interface ToolbarProps {
  readonly board: Board;
  // Opens the dialog of the board options.
  readonly showOptions: () => void;
}

export function Toolbar({ board, showOptions }: ToolbarProps): ReactElement {
  const { core } = useFrontEnd();
  const { canUndo, canRedo } = useHistory(core);
  const [renaming, setRenaming] = useState(false);
  const locked = core.lock.board;

  function step(back: boolean): void {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch {
      // Core has told the user why the store refused it.
    }
  }

  return (
    <header className="bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:border-b bc:border-border-default bc:pb-3">
      {renaming ? (
        <NameField
          label="Board name"
          name={board.name}
          onSubmit={(name) => {
            void runCommand(core, "blockr.core-rename_board", { name });
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
        <button
          type="button"
          className={classes.button}
          onClick={() => {
            showOptions();
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
        <StorageControls />
      </div>
    </header>
  );
}

// The storage controls, or a warning in their place where controls() failed,
// which tells why in its tooltip.
function StorageControls(): ReactElement | null {
  const { core } = useFrontEnd();
  const storage = core.plugins.storage();
  const [, redraw] = useReducer((n: number) => n + 1, 0);
  useEffect(() => {
    if (storage?.subscribe === undefined) return undefined;
    try {
      return storage.subscribe(redraw);
    } catch (error) {
      report(error);
      return undefined;
    }
  }, [storage]);

  const drawn = drawnControls(core);
  // The page hears of each problem once, rather than as each drawing finds it
  // again.
  const problem = drawn?.problem;
  useEffect(() => {
    if (problem !== undefined) report(new Error(problem));
  }, [problem]);

  if (drawn === undefined) return null;
  if (drawn.failed) {
    return (
      <span role="alert" title={problem} className="bc:font-sans bc:text-sm bc:text-text-danger">
        Storage failed
      </span>
    );
  }
  return (
    <>
      {drawn.controls.map((control) => (
        <Control key={control.id} control={control} />
      ))}
    </>
  );
}

function Control({ control }: { control: StorageControl }): ReactElement {
  switch (control.kind) {
    case "button":
      return <StorageButton button={control} className={classes.button} />;
    case "menu":
      return <StorageMenu label={control.label} items={control.items} />;
    case "status":
      return (
        <span role="status" title={control.tooltip} className="bc:font-sans bc:text-sm bc:text-text-muted">
          {control.text}
        </span>
      );
  }
}

// A button, which runs as the user clicks it, after `before`, as a menu closes
// as one of its buttons runs.
function StorageButton({
  button,
  className,
  role,
  before,
}: {
  button: Button;
  className: string;
  role?: "menuitem";
  before?: () => void;
}): ReactElement {
  return (
    <button
      type="button"
      role={role}
      className={className}
      disabled={button.disabled}
      title={button.tooltip}
      aria-keyshortcuts={ariaShortcuts(button.shortcut)}
      onClick={() => {
        before?.();
        void runButton(button);
      }}
    >
      {button.label}
    </button>
  );
}

// A menu, a button that opens a list of what it holds, which closes as one of
// its buttons runs, on Escape and on a click elsewhere.
function StorageMenu({ label, items }: { label: string; items: readonly StorageControl[] }): ReactElement {
  const [open, setOpen] = useState(false);
  const ref = useRef<HTMLDivElement>(null);
  useDismiss(ref, open, setOpen);
  return (
    <div ref={ref} className="bc:relative">
      <button
        type="button"
        aria-haspopup="menu"
        aria-expanded={open}
        className={classes.button}
        onClick={() => {
          setOpen(!open);
        }}
      >
        {label}
        <span aria-hidden="true">▾</span>
      </button>
      {open ? (
        <ul role="menu" aria-label={label} className={menuClasses.list}>
          <MenuItems
            items={items}
            close={() => {
              setOpen(false);
            }}
          />
        </ul>
      ) : null}
    </div>
  );
}

// What a menu holds, a menu among it as a group of its items under its label.
function MenuItems({ items, close }: { items: readonly StorageControl[]; close: () => void }): ReactElement {
  return (
    <>
      {items.map((item) => {
        switch (item.kind) {
          case "button":
            return (
              <li key={item.id} role="none">
                <StorageButton button={item} className={menuClasses.item} role="menuitem" before={close} />
              </li>
            );
          case "menu":
            return (
              <li key={item.id} role="none">
                <div role="group" aria-label={item.label}>
                  <span aria-hidden="true" className="bc:block bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted">
                    {item.label}
                  </span>
                  <ul role="none" className="bc:m-0 bc:grid bc:list-none bc:gap-0.5 bc:p-0 bc:pl-2">
                    <MenuItems items={item.items} close={close} />
                  </ul>
                </div>
              </li>
            );
          case "status":
            return (
              <li key={item.id} role="none" title={item.tooltip} className="bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-muted">
                {item.text}
              </li>
            );
        }
      })}
    </>
  );
}

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
// - A menu is a button that opens a list of what the menu holds, the SDK's
//   menu. A menu in that list shows as a group of its items under its label,
//   and a menu inside that group as a group of its own after it, since React
//   Aria's menus hold no group inside a group.
// - A status is text in the muted colour, and in a menu an item that the user
//   cannot pick.
// A control's tooltip shows as the browser's tooltip in the toolbar, and in a
// menu as the item's description, a muted line below it, since React Aria's
// menu items take no title.
// A button's shortcut is named to assistive technology in the toolbar, and in
// a menu shows at the item's right, which React Aria names as well.
// The toolbar draws the controls again whenever the storage's subscribe()
// calls back, and as the board changes. Where a button's run() rejects, the
// frontend logs it, since the storage plugin or core tells the user why.

import { useEffect, useReducer, useState } from "react";
import type { ReactElement } from "react";
import { Button as SdkButton, Keyboard, Menu, MenuItem, MenuSection, MenuTrigger, Text } from "../../react/index.ts";
import type { StorageControl } from "../../store/index.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { classes, NameField, runCommand, useHistory } from "../../ui/index.ts";
import { ariaShortcuts, asError, onMac, shortcutLabel } from "../../util/index.ts";
import { useFrontEnd } from "./context.ts";
import { drawnControls, runButton } from "./storage.ts";
import type { Button } from "./storage.ts";

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
      core.log("blockr.core").error(asError(error));
      return undefined;
    }
  }, [core, storage]);

  const drawn = drawnControls(core);
  // Core's log gets each problem once, rather than as each drawing finds it
  // again.
  const problem = drawn?.problem;
  useEffect(() => {
    if (problem !== undefined) core.log("blockr.core").error(problem);
  }, [core, problem]);

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

// A button, which runs as the user clicks it.
function StorageButton({ button, className }: { button: Button; className: string }): ReactElement {
  const { core } = useFrontEnd();
  return (
    <button
      type="button"
      className={className}
      disabled={button.disabled}
      title={button.tooltip}
      aria-keyshortcuts={ariaShortcuts(button.shortcut)}
      onClick={() => {
        void runButton(core, button);
      }}
    >
      {button.label}
    </button>
  );
}

// A menu, a button that opens a list of what it holds, which closes as one of
// its buttons runs, on Escape and on a press elsewhere.
function StorageMenu({ label, items }: { label: string; items: readonly StorageControl[] }): ReactElement {
  const { core } = useFrontEnd();
  const mac = onMac(window);
  return (
    <MenuTrigger placement="bottom end">
      <SdkButton variant="secondary">
        {label}
        <span aria-hidden="true">▾</span>
      </SdkButton>
      <Menu aria-label={label}>
        {groups(label, items).flatMap((group) =>
          group.title === undefined
            ? entries(group.items, core, mac)
            : [
                <MenuSection key={group.id} id={group.id} title={group.title}>
                  {entries(group.items, core, mac)}
                </MenuSection>,
              ],
        )}
      </Menu>
    </MenuTrigger>
  );
}

// The buttons and statuses of a menu, or of one of its groups, as the items of
// the SDK's menu, each with its tooltip as its description and a button with
// its shortcut. A status is an item that the user cannot pick.
function entries(items: readonly Entry[], core: CoreApi, mac: boolean): ReactElement[] {
  return items.map((item) => {
    const label = item.kind === "button" ? item.label : item.text;
    const shortcut = item.kind === "button" ? shortcutLabel(item.shortcut, mac) : undefined;
    return (
      <MenuItem
        key={item.id}
        id={item.id}
        textValue={label}
        isDisabled={item.kind === "status" || item.disabled}
        onAction={
          item.kind === "button"
            ? () => {
                void runButton(core, item);
              }
            : undefined
        }
      >
        <Text slot="label">{label}</Text>
        {item.tooltip === undefined ? null : <Text slot="description">{item.tooltip}</Text>}
        {shortcut === undefined ? null : <Keyboard>{shortcut}</Keyboard>}
      </MenuItem>
    );
  });
}

// What a menu holds, as groups of buttons and statuses in the order in which
// the menu holds them: first the menu's own, which the list shows without a
// title, then each menu inside it, under its label, and each menu inside that
// one after it.
type Entry = Exclude<StorageControl, { kind: "menu" }>;

interface Group {
  readonly id: string;
  readonly title?: string;
  readonly items: readonly Entry[];
}

function groups(id: string, items: readonly StorageControl[], title?: string): Group[] {
  const own = items.filter((item): item is Entry => item.kind !== "menu");
  const inner = items.flatMap((item) => (item.kind === "menu" ? groups(`${id}/${item.id}`, item.items, item.label) : []));
  return [...(own.length === 0 && title === undefined ? [] : [{ id, title, items: own }]), ...inner];
}

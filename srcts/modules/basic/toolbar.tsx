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
// The controls sit in blockr.ui's Toolbar, in three groups with a Separator of
// blockr.ui between each two: the board options, undo and redo, and the storage
// controls. Tab moves the focus into the toolbar and out of it again, and the
// arrow keys move it between the controls.
// The toolbar draws the storage controls that core.plugins.storage() gives
// (./storage.ts), and none where the app runs no storage plugin, each kind in
// its own look:
// - A button is one of the toolbar's buttons, greyed out while it is disabled.
// - A menu is a button that opens a list of what the menu holds, blockr.ui's
//   menu. A menu in that list is an item that opens the list of what it
//   holds beside its row, through blockr.ui's SubmenuTrigger.
// - A status is text in the muted colour, and in a menu an item that the user
//   cannot pick.
// A control's tooltip shows in a tooltip of blockr.ui in the toolbar, and in a
// menu as the item's description, a muted line below it, since React Aria's
// menu items take no tooltip.
// The trigger of a tooltip in the toolbar takes the focus, so that the user
// reaches the tooltip with the keyboard too: a button is its own trigger,
// and a status, as the warning in place of the controls, is text of the role
// "button", which React Aria asks of the trigger that its Focusable makes,
// so that a screen reader names the tooltip with it.
// React Aria shows no tooltip for a disabled button, which takes neither the
// pointer nor the focus, so the tooltip of a disabled button shows as text in
// the muted colour after it, which describes the button.
// A button's shortcut is named to assistive technology in the toolbar, and in
// a menu shows at the item's right, which React Aria names as well.
// The toolbar draws the controls again whenever the storage's subscribe()
// calls back, and as the board changes. Where a button's run() rejects, the
// frontend logs it, since the storage plugin or core tells the user why.

import { useEffect, useId, useReducer, useState } from "react";
import type { ReactElement } from "react";
import {
  Button as SdkButton,
  Focusable,
  Icon,
  Keyboard,
  Menu,
  MenuItem,
  MenuTrigger,
  Separator,
  SubmenuTrigger,
  Text,
  Toolbar as SdkToolbar,
  Tooltip,
  TooltipTrigger,
} from "@blockr/ui";
import { ChevronDown } from "../../icons/index.ts";
import type { StorageControl } from "../../store/index.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { NameField, runCommand, useHistory } from "../../ui/index.ts";
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
    <header className="bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:border-b bc:border-border bc:pb-3">
      {renaming ? (
        <NameField
          label="Board name"
          name={board.name}
          onSubmit={(name) => {
            void runCommand(core, core.id("blockr.core", "change_board"), { name });
          }}
          onDone={() => {
            setRenaming(false);
          }}
        />
      ) : (
        <h1 className="bc:m-0 bc:font-sans bc:text-lg bc:font-semibold">{board.name}</h1>
      )}
      {locked || renaming ? null : (
        <SdkButton
          variant="quiet"
          aria-label="Rename board"
          onPress={() => {
            setRenaming(true);
          }}
        >
          Rename
        </SdkButton>
      )}
      <SdkToolbar aria-label="Board" className="bc:ml-auto">
        <SdkButton
          variant="secondary"
          onPress={() => {
            showOptions();
          }}
        >
          Options
        </SdkButton>
        <Separator orientation="vertical" />
        <SdkButton
          variant="secondary"
          isDisabled={!canUndo}
          aria-keyshortcuts="Control+Z Meta+Z"
          onPress={() => {
            step(true);
          }}
        >
          Undo
        </SdkButton>
        <SdkButton
          variant="secondary"
          isDisabled={!canRedo}
          aria-keyshortcuts="Control+Shift+Z Meta+Shift+Z Control+Y"
          onPress={() => {
            step(false);
          }}
        >
          Redo
        </SdkButton>
        <StorageControls />
      </SdkToolbar>
    </header>
  );
}

// The storage controls after a separator, or a warning in their place where
// controls() failed, which tells why in its tooltip.
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

  if (drawn === undefined || (!drawn.failed && drawn.controls.length === 0)) return null;
  return (
    <>
      <Separator orientation="vertical" />
      {drawn.failed ? (
        <span role="alert" className="bc:font-sans bc:text-sm bc:text-destructive-text">
          <Tipped tooltip={problem}>Storage failed</Tipped>
        </span>
      ) : (
        drawn.controls.map((control) => <Control key={control.id} control={control} />)
      )}
    </>
  );
}

function Control({ control }: { control: StorageControl }): ReactElement {
  switch (control.kind) {
    case "button":
      return <StorageButton button={control} />;
    case "menu":
      return <StorageMenu label={control.label} items={control.items} />;
    case "status":
      return (
        <span role="status" className="bc:font-sans bc:text-sm bc:text-muted-foreground">
          <Tipped tooltip={control.tooltip}>{control.text}</Tipped>
        </span>
      );
  }
}

// A button, which runs as the user presses it, with its tooltip, or the text
// of its tooltip after it while it is disabled.
function StorageButton({ button }: { button: Button }): ReactElement {
  const { core } = useFrontEnd();
  const reason = useId();
  const off = button.disabled === true;
  const drawn = (
    <SdkButton
      variant="secondary"
      isDisabled={off}
      aria-keyshortcuts={ariaShortcuts(button.shortcut)}
      aria-describedby={off && button.tooltip !== undefined ? reason : undefined}
      onPress={() => {
        void runButton(core, button);
      }}
    >
      {button.label}
    </SdkButton>
  );
  if (button.tooltip === undefined) return drawn;
  if (off) {
    return (
      <>
        {drawn}
        <span id={reason} className="bc:font-sans bc:text-sm bc:text-muted-foreground">
          {button.tooltip}
        </span>
      </>
    );
  }
  return (
    <TooltipTrigger>
      {drawn}
      <Tooltip>{button.tooltip}</Tooltip>
    </TooltipTrigger>
  );
}

// A text of the toolbar, as a status, which shows its tooltip where it has
// one, as the pointer rests on it or the focus moves to it.
function Tipped({ tooltip, children }: { tooltip: string | undefined; children: string }): ReactElement {
  if (tooltip === undefined) return <>{children}</>;
  return (
    <TooltipTrigger>
      <Focusable>
        <span role="button" tabIndex={0} className="bc:cursor-default bc:rounded-sm bc:outline-none bc:focus-visible:outline-2 bc:focus-visible:outline-ring">
          {children}
        </span>
      </Focusable>
      <Tooltip>{tooltip}</Tooltip>
    </TooltipTrigger>
  );
}

// A menu, a button that opens a list of what it holds, which closes as one of
// its buttons runs, on Escape and on a press elsewhere. The button shows
// Lucide's chevron after its label, 12 pixels wide, as blockr.ui's select
// shows it.
function StorageMenu({ label, items }: { label: string; items: readonly StorageControl[] }): ReactElement {
  const { core } = useFrontEnd();
  const mac = onMac(window);
  return (
    <MenuTrigger placement="bottom end">
      <SdkButton variant="secondary">
        {label}
        <Icon icon={ChevronDown} size={12} />
      </SdkButton>
      <Menu aria-label={label}>{entries(items, core, mac)}</Menu>
    </MenuTrigger>
  );
}

// The buttons, statuses and menus of a menu, as the items of blockr.ui's menu,
// each button and status with its tooltip as its description and a button
// with its shortcut. A status is an item that the user cannot pick. A menu is
// an item that opens what it holds beside its row.
function entries(items: readonly StorageControl[], core: CoreApi, mac: boolean): ReactElement[] {
  return items.map((item) => {
    if (item.kind === "menu") {
      return (
        <SubmenuTrigger key={item.id}>
          <MenuItem id={item.id} textValue={item.label}>
            {item.label}
          </MenuItem>
          <Menu aria-label={item.label}>{entries(item.items, core, mac)}</Menu>
        </SubmenuTrigger>
      );
    }
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

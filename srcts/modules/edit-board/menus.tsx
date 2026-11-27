// The menus that the Edit board plugin opens, as What it shows has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#what-it-shows):
// that of each row's block, link or stack, and that of the selection, which
// the bar above the sections opens. Each opens through
// core.boardActions.menu(), so that a row offers every board action that
// applies to its object, a package's among them, as every other menu of the
// object does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-menus-of-board-objects).
// The plugin draws the button that opens a menu, a "…", only where
// core.boardActions.on() gives the target a board action.
// Beside the button, the plugin hands menu() two things:
// - A place for the advanced actions that the menu opens, such as the form of
//   Edit link. A row's menu shows its form in the row, which turns into the
//   form, in a cell across the row. The selection's menu shows its form below
//   the bar. The form shows until done() ends its advanced action, or until
//   the user closes it by its Close button or by Escape. The keyboard's focus
//   then goes back to the button that opened the menu.
// - A notifier, which takes the notifications of a board action whose batch
//   the store or the command registry refused, with the board action, so that
//   core raises none. The plugin shows why in a banner below the table whose
//   row's menu ran the board action, or below the bar. The banner starts with
//   a sentence that names the edit by the board action: in the plugin's own
//   words for core's board actions, as `Could not remove the stack "Input".`,
//   and by its label for another package's, as `Add to document failed for
//   the block "Filter".`
// The banner holds until the user opens the menu of a row of the same table,
// or of the bar, again, even where the user then picks nothing. Core tells
// the plugin of a board action that it refused, but not of one that ran.

import { useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { ReactElement } from "react";
import { Button } from "@blockr/ui";
import { TableStateContext } from "react-aria-components";
import type { BoardAction, Place, PlaceArgs, Target } from "../../actions/index.ts";
import { Mounted } from "../../card/index.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { ActionsButton, blockName, notifiedReasons } from "../../ui/index.ts";
import type { Refused } from "../../ui/index.ts";
import { idText, own } from "../../util/index.ts";

// The menus that one table or the bar opens, the places of the advanced
// actions that they open, and why the store or the command registry refused
// the last board action that one of them ran.
export interface Menus {
  // Why the store or the command registry refused the last board action that
  // one of the menus ran, or null where none was refused. It holds until the
  // user opens one of the menus again.
  readonly refusal: Refused | null;
  // The advanced action that each place shows, by the place's key, such as
  // the id of a row's object.
  readonly shown: ReadonlyMap<string, PlaceArgs>;
  // Opens the menu of the target at the button given, to which the keyboard's
  // focus returns as the menu closes. The advanced actions that the menu
  // opens show in the place of the key.
  open(target: Target, key: string, button: HTMLElement): void;
  // Closes the advanced action that the place of the key shows, as the user
  // does.
  close(key: string, args: PlaceArgs): void;
  // The key of the place whose advanced action has just closed, whose button
  // takes the keyboard's focus as it draws, or null.
  readonly back: string | null;
}

export function useMenus(core: CoreApi): Menus {
  const [refusal, setRefusal] = useState<Refused | null>(null);
  const [shown, setShown] = useState<ReadonlyMap<string, PlaceArgs>>(() => new Map());
  const [back, setBack] = useState<string | null>(null);
  // One place for each key, the same function each time, since core tells
  // the places apart by their functions, and a place shows one advanced
  // action at a time.
  const places = useRef(new Map<string, Place>());
  // What the places show as the plugin last drew, which they close as the
  // plugin goes.
  const latest = useRef(shown);

  useEffect(() => {
    latest.current = shown;
  }, [shown]);

  // The places tell core that the user closed them as the plugin goes, so
  // that core ends their advanced actions. They tell it once React has
  // finished the work in which the plugin went, as Mounted's cleanup runs,
  // since core then unmounts each advanced action at once, whose React root
  // React would otherwise unmount after core had emptied its element.
  useEffect(
    () => () => {
      const open = [...latest.current.values()];
      queueMicrotask(() => {
        for (const args of open) args.close();
      });
    },
    [],
  );

  // The button of the place takes the focus as it draws, which is once React
  // has drawn the table's rows again, within the same task. Where none takes
  // it by then, as where the board no longer holds the row's object, none
  // takes it later.
  useEffect(() => {
    if (back === null) return undefined;
    const timer = setTimeout(() => {
      setBack(null);
    });
    return () => {
      clearTimeout(timer);
    };
  }, [back]);

  const ended = useCallback((key: string, args: PlaceArgs) => {
    setShown((before) => (before.get(key) === args ? without(before, key) : before));
    setBack(key);
  }, []);

  const placeOf = useCallback(
    (key: string): Place => {
      const known = places.current.get(key);
      if (known !== undefined) return known;
      const place: Place = (args) => {
        setShown((before) => new Map(before).set(key, args));
        return () => {
          ended(key, args);
        };
      };
      places.current.set(key, place);
      return place;
    },
    [ended],
  );

  const open = useCallback(
    (target: Target, key: string, button: HTMLElement) => {
      setRefusal(null);
      core.boardActions.menu(target, {
        el: button,
        anchor: button.getBoundingClientRect(),
        notifier: (notifications, action) => {
          setRefusal({ title: editTitle(core.store.board(), action, target), reasons: notifiedReasons(notifications) });
        },
        advanced: placeOf(key),
      });
    },
    [core, placeOf],
  );

  const close = useCallback(
    (key: string, args: PlaceArgs) => {
      args.close();
      ended(key, args);
    },
    [ended],
  );

  return useMemo(() => ({ refusal, shown, open, close, back }), [refusal, shown, open, close, back]);
}

// The "…" button that opens the menu of the target, as the card of core's
// basic frontend has one. It takes the keyboard's focus as it draws where the
// advanced action that its menu opened has just closed.
export function MenuButton({ menus, target, place, label }: { menus: Menus; target: Target; place: string; label: string }): ReactElement {
  const ref = useRef<HTMLButtonElement>(null);
  const back = menus.back === place;
  useLayoutEffect(() => {
    if (back) ref.current?.focus();
  }, [back]);
  return (
    <ActionsButton
      ref={ref}
      label={label}
      onPress={() => {
        const el = ref.current;
        if (el !== null) menus.open(target, place, el);
      }}
    />
  );
}

// An advanced action that a place of the plugin shows, such as the form of
// Edit link in its row. Core renders it in an element of its own, above a
// Close button, which closes it, as Escape does. The form says itself why the
// store refused what it ran.
// In a table, React Aria's Table hears what the user does in the row's cells,
// as it hears a press of the row, which ticks the row. So a click in one of
// the form's fields would tick the row, and so would Space or Enter in it. The
// box stops what starts a press at itself (KEPT, below), and the table's
// keyboard navigation is off while the form shows (TakesKeys, below).
export function PlaceForm({ args, onClose }: { args: PlaceArgs; onClose: () => void }): ReactElement {
  const box = useRef<HTMLDivElement>(null);
  const mount = useCallback((el: HTMLElement) => args.mount(el), [args]);
  // The form is a module of its own, in a React root of its own, so the box
  // hears its keys as the page does, once the form has let them through.
  // Escape in a field whose list is open closes the list alone, which the box
  // tells as the key comes down to the field, since the field closes its list
  // before the key comes back up.
  useEffect(() => {
    const el = box.current;
    if (el === null) return undefined;
    let listOpen = false;
    const down = (event: KeyboardEvent): void => {
      listOpen = event.target instanceof Element && event.target.getAttribute("aria-expanded") === "true";
    };
    const heard = (event: KeyboardEvent): void => {
      if (event.key === "Escape" && !event.defaultPrevented && !listOpen) onClose();
    };
    el.addEventListener("keydown", down, { capture: true });
    el.addEventListener("keydown", heard);
    return () => {
      el.removeEventListener("keydown", down, { capture: true });
      el.removeEventListener("keydown", heard);
    };
  }, [onClose]);
  return (
    <div ref={box} role="group" aria-label={args.title} className="bc:grid bc:gap-2 bc:py-1 bc:whitespace-normal" {...KEPT}>
      <TakesKeys />
      <Mounted mount={mount} className="bc:min-w-0" />
      <div className="bc:flex bc:gap-2">
        <Button variant="secondary" onPress={onClose}>
          Close
        </Button>
      </div>
    </div>
  );
}

// What the box of a form stops at itself, which starts a press of the row
// that holds the form: the pointer, the mouse or a touch going down, a click,
// as a screen reader makes one, and a key. It lets go up what ends a press,
// such as the pointer coming up, since React Aria hears that on the document
// for the buttons of the form.
const KEPT = Object.fromEntries(
  ["onPointerDown", "onMouseDown", "onTouchStart", "onClick", "onDoubleClick", "onKeyDown", "onKeyUp"].map((handler) => [
    handler,
    (event: { stopPropagation(): void }) => {
      event.stopPropagation();
    },
  ]),
);

// How many forms in each table take the table's keys, by the function of the
// table's state that turns its keyboard navigation off, so that the last form
// to go turns it on again.
const taking = new WeakMap<(disabled: boolean) => void, number>();

// Turns the keyboard navigation of the table that it is in off while it is
// drawn, so that the fields of a form in a row hear their keys. React Aria's
// Table would take them otherwise: it moves the focus between the rows and
// the cells on the arrows, and to a row whose text starts with the letters
// that the user types, and clears the selection on Escape. React Aria's
// TableState turns it off for "a component within a cell" that handles the
// arrows, as here. Outside a table it does nothing.
function TakesKeys(): null {
  const disable = useContext(TableStateContext)?.setKeyboardNavigationDisabled;
  useEffect(() => {
    if (disable === undefined) return undefined;
    taking.set(disable, (taking.get(disable) ?? 0) + 1);
    disable(true);
    return () => {
      const left = (taking.get(disable) ?? 1) - 1;
      taking.set(disable, left);
      if (left === 0) disable(false);
    };
  }, [disable]);
  return null;
}

// The sentence that names the edit of a board action that one of the
// plugin's menus ran on the target, which heads why the store or the command
// registry refused it: in the plugin's own words for core's board actions, as
// `Could not remove the stack "Input".`, and by its label for another
// package's, as `Add to document failed for the block "Filter".` A board
// action of core's for which the plugin has no words takes its label too.
export function editTitle(board: Board, action: Pick<BoardAction, "package" | "id" | "label">, target: Target): string {
  const object = named(board, target);
  const edit = action.package === "blockr.core" ? own(CORE_EDITS, action.id) : undefined;
  return edit === undefined ? `${action.label} failed for ${object}.` : `Could not ${edit(object)}.`;
}

// What core's board actions do to their targets, in the plugin's words, by the
// board action's id within blockr.core, for those whose batch a menu of a row
// or of the selection runs.
const CORE_EDITS: Readonly<Record<string, (object: string) => string>> = {
  append_block_action: (object) => `append a block to ${object}`,
  prepend_block_action: (object) => `prepend a block to ${object}`,
  insert_block_action: (object) => `insert a block into ${object}`,
  add_link_action: (object) => `add a link for ${object}`,
  add_stack_action: (object) => `put ${object} in a new stack`,
  rename_action: (object) => `rename ${object}`,
  edit_link_action: (object) => `change ${object}`,
  edit_stack_action: (object) => `change ${object}`,
  add_to_stack_action: (object) => `add ${object} to a stack`,
  remove_block_action: (object) => `remove ${object}`,
  remove_link_action: (object) => `remove ${object}`,
  remove_stack_action: (object) => `remove ${object}`,
  remove_selected_action: (object) => `remove ${object}`,
};

// The target as a reason names it: a block and a stack by their names, and a
// link by the names of its blocks, as the tables show them, or by their ids
// where the board no longer holds them.
function named(board: Board, target: Target): string {
  if ("selection" in target) return "the selection";
  if ("board" in target) return "the board";
  if ("block" in target) return `the block "${blockName(board, target.block)}"`;
  if ("link" in target) {
    const link = own(board.links, target.link);
    return link === undefined ? `the link "${target.link}"` : `the link from "${blockName(board, link.from)}" to "${blockName(board, link.to)}"`;
  }
  if ("stack" in target) return `the stack "${own(board.stacks, target.stack)?.name ?? target.stack}"`;
  if ("option" in target) return `the board option "${target.option}"`;
  return `the plugin "${idText(target.plugin)}"`;
}

function without<V>(map: ReadonlyMap<string, V>, key: string): ReadonlyMap<string, V> {
  const next = new Map(map);
  next.delete(key);
  return next;
}

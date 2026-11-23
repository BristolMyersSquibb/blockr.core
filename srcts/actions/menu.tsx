// Core's menu, which draws every menu of board actions that menu() and
// trigger() open, whichever package opens it, and whichever frontend draws the
// page, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#what-a-frontend-gives).
// It draws the level that the menu shows (./quick.ts) with the menu of core's
// React SDK, React Aria's Menu in core's theme, in a popover at the anchor, so
// that it looks like the other menus on the page. It draws on Tailwind CSS,
// whose theme variables a frontend sets to restyle it, as the dock does
// through blockr.ui's design tokens.
// It draws the rows as the design system draws the block's menu
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1160-L1165):
// - The rows of each category together, which the menu of a target shows in
//   the order add, edit, clipboard, the packages' categories, and remove,
//   with a divider between two categories.
// - Each row with its icon before its label, and its detail as meta text at
//   its end, which assistive technology reads with the label.
// - The rows that remove in the theme's danger colour, on its danger
//   background, under the pointer and as the keyboard's row, and plain at
//   rest, as the design system draws a row that destroys something.
// - An item that gives `checked` with a check at its end while `checked` is
//   true, after its detail, as the design system marks the current pick and
//   a toggle that is on
//   (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1096-L1112).
// Assistive technology hears an item that gives `checked` as a checkbox of
// the menu, checked or not, as in blockr.ui's own menu
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/inst/assets/js/blockr-ui.js#L1112-L1117).
// React Aria's menu makes an item a checkbox only in a section that picks
// several.
// So the items that give `checked` sit in sections of their own, and each
// such section picks its items whose `checked` is true.
// Where this splits the items under one heading, the heading shows over the
// first of their sections.
// Picking such an item runs its pick, as for any item.
// What its section picks changes only with `checked`.
// A level shows its title at its top, its filter below the title, its text
// rows below the filter, and then its items and multis, those under a
// heading in a group under it, and each multi as a group of its choices, which
// tick. A level that loads says so, after a moment, so that one that loads at
// once shows nothing, and one that failed shows why, in the danger colour.
// A level whose board action has an advanced action shows the open icon at
// its top right, at the end of the title's line: a button named "Show all
// options". An item that gives `advanced` shows the icon at its end, and a
// click on the icon opens the advanced action, where a click elsewhere on the
// item picks it. The icon is in the shape of blockr.ui's `open`, a box with an
// arrow out of its top right corner
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/inst/assets/icons/open.svg).
// Each icon's tooltip gives its name with its key, Mod+Enter, in the notation
// of blockr.ui's design system, as "Show all options (⌘Enter)" on a Mac, since
// the design system names the tool by its tooltip
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1083-L1087).
// Assistive technology hears the key from the level's button and its filter,
// and the tooltip's text as the description of an item that shows the icon,
// since a menu item holds no control of its own.
// A level with a text row has the focus in its first one, with the value
// selected, one with a filter in the filter, and any other in its first row.
// The keys:
// - Escape goes back a level, or closes the menu from its first level.
// - Tab closes the menu, as a click outside it does, which applies the ticks of
//   a multi.
// - Enter picks the row with the focus, submits a text row, and in the filter
//   picks the first row left, or the row that the arrow keys moved to.
// - Mod+Enter, Ctrl or Cmd with Enter, opens the advanced action, as a click
//   on an open icon does: the icon of the row with the focus, or of the row
//   that the arrow keys moved to from the filter, where it shows one, and
//   otherwise the level's.
// - The arrow keys move between the rows, from the filter too.
// As the menu closes, the keyboard's focus returns to the element that the
// user triggered from. React Aria's popover keeps a click outside the menu
// from reaching what lies below it, so that the click only closes the menu.

import { useEffect, useRef, useSyncExternalStore } from "react";
import type { ReactElement, RefObject } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Autocomplete, Input, Separator, Text, TextField as AriaTextField } from "../aria/index.ts";
import { TextField } from "../inputs/index.ts";
import { Menu, MenuItem, MenuSection, Popover } from "../overlays/index.ts";
import { ariaShortcuts, matchesShortcut, onMac, shortcutLabel } from "../util/index.ts";
import type { MenuPlace, ShowMenu } from "./actions.ts";
import type { OpenMenu, ShownLevel, ShownRow } from "./quick.ts";

// Draws the open menu at its place, in an element of its own at the end of the
// page's body, until the function that it returns runs, which unmounts it and
// gives the keyboard's focus back to the element that the user triggered from.
export const showMenu: ShowMenu = (menu, at) => {
  const doc = at.el.ownerDocument;
  const host = doc.createElement("div");
  doc.body.append(host);
  const root = createRoot(host);
  const insideRef: RefObject<HTMLElement | null> = { current: null };
  // Tab closes the menu, as a click outside it does. React Aria's popover
  // moves the focus out of itself on Tab in a listener on the document, which
  // hears the key before the menu would, so the menu hears it on the window.
  const tab = (event: KeyboardEvent): void => {
    if (event.key !== "Tab" || event.isComposing || !(event.target instanceof Node) || insideRef.current?.contains(event.target) !== true) return;
    event.preventDefault();
    event.stopPropagation();
    void menu.close().catch(() => undefined);
  };
  doc.defaultView?.addEventListener("keydown", tab, true);
  flushSync(() => {
    root.render(<CoreMenu menu={menu} at={at} insideRef={insideRef} />);
  });
  return () => {
    doc.defaultView?.removeEventListener("keydown", tab, true);
    // The focus goes back where it is in the menu or nowhere, as after the user
    // picked a row, pressed Escape or clicked outside the menu, and stays
    // where code of the page moved it.
    const active = doc.activeElement;
    const lost = active === null || active === doc.body || (insideRef.current?.contains(active) ?? false);
    root.unmount();
    host.remove();
    if (lost && at.el.isConnected) at.el.focus({ preventScroll: true });
  };
};

// The popover, which holds the dialog of the level shown, named by the level's
// title or its label. React Aria's popover takes the focus as it opens, unless
// it holds a dialog, and so would React Aria's dialog. So the dialog is an
// element of core's, and the level alone moves the focus (Level, below).
function CoreMenu({ menu, at, insideRef }: { menu: OpenMenu; at: MenuPlace; insideRef: RefObject<HTMLElement | null> }): ReactElement {
  const level = useSyncExternalStore(menu.subscribe, menu.get);
  const anchor = useRef<HTMLDivElement>(null);
  const { x, y, width, height } = at.anchor;
  const tip = openTip(at.el.ownerDocument.defaultView ?? window);
  return (
    <>
      {/* The box that the popover hangs from, where the menu opens. */}
      <div ref={anchor} aria-hidden="true" style={{ position: "fixed", left: x, top: y, width, height, pointerEvents: "none" }} />
      <Popover
        triggerRef={anchor}
        isOpen
        placement="bottom start"
        isKeyboardDismissDisabled
        onOpenChange={(open) => {
          if (!open) void menu.close().catch(() => undefined);
        }}
        className="bc:max-w-80 bc:min-w-45"
      >
        {/* It hears the keys of the menu before the rows and the fields do. A
            level that holds nothing that takes the focus, as one that loads,
            gives it the focus, so that it hears Escape there too. */}
        <div
          ref={(el) => {
            insideRef.current = el;
          }}
          role="dialog"
          aria-label={level.title ?? level.label}
          tabIndex={-1}
          className="bc:grid bc:gap-1 bc:outline-none"
          onKeyDownCapture={(event) => {
            if (event.nativeEvent.isComposing) return;
            if (event.key === "Escape") {
              event.preventDefault();
              event.stopPropagation();
              menu.escape();
            } else if (level.advanced && matchesShortcut(OPEN_KEY, event.nativeEvent)) {
              // Mod+Enter, which opens the advanced action, as a click on the
              // open icon of the row with the focus does, or on the level's.
              event.preventDefault();
              event.stopPropagation();
              void menu.openAdvanced(openRow(event.target)).catch(() => undefined);
            } else if (event.key === "Enter" && isFilter(event.target) && !event.target.hasAttribute("aria-activedescendant")) {
              // Enter in the filter, where the arrow keys moved to no row.
              event.preventDefault();
              event.stopPropagation();
              void menu.pick().catch(() => undefined);
            }
          }}
        >
          <Level key={level.id} menu={menu} level={level} insideRef={insideRef} tip={tip} />
        </div>
      </Popover>
    </>
  );
}

// The level that the menu shows, drawn anew as another shows in its place,
// with the tooltip of its open icons.
function Level({
  menu,
  level,
  insideRef,
  tip,
}: {
  menu: OpenMenu;
  level: ShownLevel;
  insideRef: RefObject<HTMLElement | null>;
  tip: string;
}): ReactElement {
  const texts = level.rows.flatMap((row, i) => (row.kind === "text" ? [{ row, i }] : []));
  const listed = level.rows.flatMap((row, i) => (row.kind === "text" ? [] : [{ row, i }]));
  // As the level shows, the focus goes to its first text row, with the value
  // selected, or else to its filter, or to its first row, or where it holds
  // none of these, as while it loads, to the element that hears the menu's
  // keys. React Aria draws the rows of a menu a moment after the menu, so
  // the level waits for them a few frames.
  const rows = level.rows.length > 0;
  useEffect(() => {
    let frames = 0;
    let frame: number | undefined;
    const place = (): void => {
      const el = insideRef.current;
      if (el === null) return;
      const field = el.querySelector<HTMLInputElement>("input:not([data-filter])") ?? el.querySelector<HTMLInputElement>("input[data-filter]");
      const first = field ?? el.querySelector<HTMLElement>("[role=menuitem][data-key], [role=menuitemcheckbox][data-key]");
      if (first === null && rows && frames++ < 10) {
        frame = requestAnimationFrame(place);
        return;
      }
      (first ?? el).focus({ preventScroll: true });
      if (field !== null && !field.hasAttribute("data-filter")) field.select();
    };
    place();
    return () => {
      if (frame !== undefined) cancelAnimationFrame(frame);
    };
  }, [insideRef, rows]);
  const pick = (i?: number): void => {
    void menu.pick(i).catch(() => undefined);
  };
  const openAdvanced = (i?: number): void => {
    void menu.openAdvanced(i).catch(() => undefined);
  };
  const grouped = groups(listed);

  const list = (
    <Menu aria-label={level.label} renderEmptyState={() => <p className={NOTE}>No matches</p>}>
      {grouped.map((group, g) => {
        const divider = g > 0 && group.category !== grouped[g - 1]?.category;
        const section =
          group.kind === "multi" ? (
            <MenuSection
              key={`multi-${String(group.i)}`}
              id={`multi-${String(group.i)}`}
              title={group.row.label}
              selectionMode="multiple"
              selectedKeys={group.row.choices.filter((choice) => choice.picked).map(({ id }) => choiceKey(group.i, id))}
              onSelectionChange={(keys) => {
                    for (const choice of group.row.choices) {
                  const ticked = keys === "all" || keys.has(choiceKey(group.i, choice.id));
                  if (ticked !== choice.picked) menu.tick(group.i, choice.id);
                }
              }}
            >
              {group.row.choices.map((choice) => (
                <MenuItem key={choice.id} id={choiceKey(group.i, choice.id)} textValue={choice.label}>
                  {choice.label}
                </MenuItem>
              ))}
            </MenuSection>
          ) : (
            <MenuSection
              key={`items-${String(group.items[0]?.i ?? g)}`}
              id={`items-${String(group.items[0]?.i ?? g)}`}
              title={group.continues ? undefined : group.heading}
              {...(group.checks
                ? { selectionMode: "multiple", selectedKeys: group.items.filter(({ row }) => row.checked === true).map(({ i }) => itemKey(i)) }
                : {})}
            >
              {group.items.map(({ row, i }) => (
                <Item
                  key={i}
                  row={row}
                  index={i}
                  tip={tip}
                  onPick={() => {
                    pick(i);
                  }}
                  onOpen={() => {
                    openAdvanced(i);
                  }}
                />
              ))}
            </MenuSection>
          );
        return divider ? [<Separator key={`divider-${String(g)}`} className={DIVIDER} />, section] : [section];
      })}
    </Menu>
  );

  return (
    <>
      {level.advanced ? (
        <div className="bc:flex bc:items-center bc:gap-2">
          {level.title === undefined ? null : <p className={`${TITLE} bc:min-w-0 bc:flex-1`}>{level.title}</p>}
          {/* A plain button, since React Aria's Button drops `title`. */}
          <button
            type="button"
            aria-label={OPEN_NAME}
            aria-keyshortcuts={ariaShortcuts(OPEN_KEY)}
            title={tip}
            className={OPEN}
            onClick={() => {
              openAdvanced();
            }}
          >
            <OpenIcon />
          </button>
        </div>
      ) : level.title === undefined ? null : (
        <p className={TITLE}>{level.title}</p>
      )}
      {level.loading ? (
        <p role="status" className={`${NOTE} bc:animate-[bc-appear_300ms_ease-out_300ms_both]`}>
          Loading…
        </p>
      ) : level.error === undefined ? (
        <>
          {texts.map(({ row, i }) => (
            <TextRow
              key={i}
              row={row}
              onType={(value) => {
                menu.type(value, i);
              }}
              onSubmit={() => {
                pick(i);
              }}
            />
          ))}
          {level.filter === undefined ? (
            listed.length === 0 ? null : list
          ) : (
            <Autocomplete
              inputValue={level.filter}
              onInputChange={(value) => {
                menu.type(value);
              }}
            >
              <AriaTextField aria-label="Filter" className="bc:px-1 bc:pt-1">
                <Input
                  data-filter=""
                  placeholder="Filter"
                  spellCheck="false"
                  aria-keyshortcuts={level.advanced ? ariaShortcuts(OPEN_KEY) : undefined}
                  className={FILTER}
                />
              </AriaTextField>
              {list}
            </Autocomplete>
          )}
        </>
      ) : (
        <p role="alert" className="bc:m-0 bc:px-2 bc:py-1.5 bc:text-sm bc:break-words bc:text-text-danger">
          {level.error}
        </p>
      )}
    </>
  );
}

type ItemRow = Extract<ShownRow, { kind: "item" }>;
type MultiRow = Extract<ShownRow, { kind: "multi" }>;

// The rows that the menu lists, in groups: the items under one heading and of
// one category that follow each other, and each multi on its own. The items
// that give `checked` form groups of their own, which `checks` marks. Where
// the items under one heading form several groups, `continues` marks each
// group after the first, which shows the heading no more.
type Group =
  | {
      readonly kind: "items";
      readonly heading?: string;
      readonly category?: string;
      readonly checks: boolean;
      readonly continues: boolean;
      readonly items: { row: ItemRow; i: number }[];
    }
  | { readonly kind: "multi"; readonly category?: undefined; readonly row: MultiRow; readonly i: number };

function groups(listed: readonly { row: ShownRow; i: number }[]): Group[] {
  const all: Group[] = [];
  for (const { row, i } of listed) {
    if (row.kind === "text") continue;
    if (row.kind === "multi") {
      all.push({ kind: "multi", row, i });
      continue;
    }
    const last = all.at(-1);
    const checks = row.checked !== undefined;
    const under = last?.kind === "items" && last.heading === row.heading && last.category === row.category;
    if (under && last.checks === checks) {
      last.items.push({ row, i });
    } else {
      all.push({
        kind: "items",
        ...(row.heading === undefined ? {} : { heading: row.heading }),
        ...(row.category === undefined ? {} : { category: row.category }),
        checks,
        continues: under,
        items: [{ row, i }],
      });
    }
  }
  return all;
}

// An item, with its icon before its label and its detail at its end, both of
// which assistive technology reads with the label, in its name, and after
// them the open icon, where the item shows one. A click on the open icon
// opens the advanced action, and does not reach the item, so that the item is
// not picked. The open icon carries the item's index, by which Mod+Enter on
// the item finds it, and the tooltip, which assistive technology reads as the
// item's description. The item's name is given, so that it holds no more
// than its label and its detail. The item's `tick` is "end", so the SDK's
// item draws the check at the item's end, after the open icon, while the
// item's section picks the item.
function Item({
  row,
  index,
  tip,
  onPick,
  onOpen,
}: {
  row: ItemRow;
  index: number;
  tip: string;
  onPick: () => void;
  onOpen: () => void;
}): ReactElement {
  const { label, icon, detail } = row;
  const removes = row.category === "remove";
  return (
    <MenuItem
      id={itemKey(index)}
      tick="end"
      textValue={label}
      aria-label={detail === undefined ? label : `${label}, ${detail}`}
      onAction={onPick}
      className={removes ? REMOVES : undefined}
    >
      <span
        className={`bc:flex bc:min-w-0 bc:items-center bc:gap-2 ${
          removes ? "bc:group-data-focused:text-text-danger bc:group-data-hovered:text-text-danger" : ""
        }`}
      >
        {icon === undefined ? null : (
          <span
            aria-hidden="true"
            className="bc:flex bc:size-4 bc:shrink-0 bc:[&>svg]:size-full"
            // The icon is SVG markup that the board action's package gives, as
            // a block class's icon in the block registry, from code that runs
            // on the page already.
            dangerouslySetInnerHTML={{ __html: icon }}
          />
        )}
        <span className="bc:min-w-0 bc:truncate">{label}</span>
      </span>
      {detail === undefined ? null : <span className="bc:text-xs bc:text-text-muted">{detail}</span>}
      {row.advanced === true ? (
        <>
          <span
            aria-hidden="true"
            data-open-row={index}
            title={tip}
            className={`${OPEN} bc:-my-1`}
            onPointerDown={(event) => {
              event.stopPropagation();
            }}
            onClick={(event) => {
              event.stopPropagation();
              onOpen();
            }}
          >
            <OpenIcon />
          </span>
          <Text slot="description" className="bc:sr-only">
            {tip}
          </Text>
        </>
      ) : null}
    </MenuItem>
  );
}

// The open icon, in the shape of blockr.ui's `open`, at the stroke of the
// menu's icons.
function OpenIcon(): ReactElement {
  return (
    <svg
      aria-hidden="true"
      width="14"
      height="14"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth={1.25}
      strokeLinecap="round"
      strokeLinejoin="round"
    >
      <path d="M7 2.5H4A1.5 1.5 0 0 0 2.5 4v8A1.5 1.5 0 0 0 4 13.5h8a1.5 1.5 0 0 0 1.5-1.5V9" />
      <path d="M10 2.5h3.5V6M13.5 2.5 8 8" />
    </svg>
  );
}

// The row whose open icon Mod+Enter opens: the row with the focus, or the one
// that the arrow keys moved to from the filter, where its open icon shows, or
// undefined for the level's icon.
function openRow(target: EventTarget | null): number | undefined {
  if (!(target instanceof Element)) return undefined;
  const active = isFilter(target) ? target.getAttribute("aria-activedescendant") : null;
  const item = active === null ? target.closest('[role="menuitem"], [role="menuitemcheckbox"]') : target.ownerDocument.getElementById(active);
  const marked = item?.querySelector("[data-open-row]")?.getAttribute("data-open-row");
  return marked === undefined || marked === null ? undefined : Number(marked);
}

// A text row: a field that holds the row's value, and says below itself why
// its check refuses the value.
function TextRow({
  row,
  onType,
  onSubmit,
}: {
  row: Extract<ShownRow, { kind: "text" }>;
  onType: (value: string) => void;
  onSubmit: () => void;
}): ReactElement {
  return (
    <TextField
      label={row.label}
      value={row.value}
      spellCheck="false"
      isInvalid={row.refusal !== undefined}
      {...(row.refusal === undefined ? {} : { errorMessage: row.refusal })}
      className="bc:px-1 bc:pt-1"
      onChange={onType}
      onKeyDown={(event) => {
        // The key's default stays undone, since the focus returns to the
        // element that the user triggered from as the menu closes, which a
        // button would take as a click that opens the menu again.
        if (event.key === "Enter" && !event.nativeEvent.isComposing) {
          event.preventDefault();
          onSubmit();
        } else event.continuePropagation();
      }}
    />
  );
}

// The keys of an item and of a choice of a multi in React Aria's menu, by
// the index of their row in the level's rows.
function itemKey(row: number): string {
  return String(row);
}

function choiceKey(row: number, choice: string): string {
  return `${String(row)}:${choice}`;
}

function isFilter(target: EventTarget): target is HTMLInputElement {
  return target instanceof HTMLInputElement && target.hasAttribute("data-filter");
}

// A level's title.
const TITLE = "bc:m-0 bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted";
// The open icon, a button 24 px square at the end of the level's title, and
// the same at the end of an item.
const OPEN =
  "bc:ml-auto bc:inline-flex bc:size-6 bc:shrink-0 bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:cursor-pointer bc:outline-none bc:hover:bg-bg-subtle bc:hover:text-text-default bc:focus-visible:ring-2 bc:focus-visible:ring-border-accent";
// The open icon's name, and the key that opens the advanced action, as a click
// on an open icon does.
const OPEN_NAME = "Show all options";
const OPEN_KEY = "Mod+Enter";

// The tooltip of the open icons: their name, with their key as the design
// system writes it on the page's platform.
function openTip(win: Window): string {
  return `${OPEN_NAME} (${shortcutLabel(OPEN_KEY, onMac(win)) ?? OPEN_KEY})`;
}
// A note in place of rows, as that a level loads or that the filter leaves no
// row.
const NOTE = "bc:m-0 bc:px-2 bc:py-1.5 bc:text-center bc:text-sm bc:text-text-muted bc:italic";
// The filter, part of the menu rather than a field of a form, at the size of
// the menu's meta text.
const FILTER =
  "bc:box-border bc:w-full bc:rounded bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-xs bc:text-text-default bc:outline-none";
const DIVIDER = "bc:mx-0 bc:my-1 bc:h-px bc:border-0 bc:bg-border-default";
// A row that removes, on the theme's danger background under the pointer and
// as the keyboard's row. The background takes the place of the grey one that
// the SDK's item gives itself for both, so it is marked important, which
// leaves it no rule of the same weight to lose to.
const REMOVES = "bc:group bc:data-hovered:bg-bg-danger! bc:data-focused:bg-bg-danger!";

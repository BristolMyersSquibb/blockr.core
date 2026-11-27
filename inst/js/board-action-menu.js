// srcts/modules/board-action-menu/index.tsx
import { useEffect, useRef, useSyncExternalStore } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Autocomplete, Icon, Menu, MenuItem, MenuSection, Popover, Text, TextField } from "@blockr/ui";
import { Input, Separator, TextField as AriaTextField } from "react-aria-components";
import { SquareArrowUpRightExit } from "./vendor-64fbb1b0.js";

// srcts/util/shortcuts.ts
function matchesShortcut(text, event) {
  const shortcut = readShortcut(text);
  const key = event.key;
  if (shortcut === void 0 || typeof key !== "string" || key.toLowerCase() !== shortcut.key.toLowerCase()) {
    return false;
  }
  if (event.altKey !== shortcut.alt || event.shiftKey !== shortcut.shift) return false;
  if (shortcut.mod) return event.ctrlKey || event.metaKey;
  return event.ctrlKey === shortcut.ctrl && event.metaKey === shortcut.meta;
}
function ariaShortcuts(text) {
  const shortcut = text === void 0 ? void 0 : readShortcut(text);
  if (shortcut === void 0) return void 0;
  const rest = [...shortcut.alt ? ["Alt"] : [], ...shortcut.shift ? ["Shift"] : [], shortcut.key];
  const firsts = shortcut.mod ? [["Control"], ["Meta"]] : [[...shortcut.ctrl ? ["Control"] : [], ...shortcut.meta ? ["Meta"] : []]];
  return firsts.map((first) => [...first, ...rest].join("+")).join(" ");
}
function shortcutLabel(text, mac) {
  const shortcut = text === void 0 ? void 0 : readShortcut(text);
  if (shortcut === void 0) return void 0;
  const key = shortcut.key.length === 1 ? shortcut.key.toUpperCase() : shortcut.key;
  if (mac) {
    const glyphs = [
      ...shortcut.ctrl ? ["\u2303"] : [],
      ...shortcut.alt ? ["\u2325"] : [],
      ...shortcut.mod || shortcut.meta ? ["\u2318"] : [],
      ...shortcut.shift ? ["\u21E7"] : []
    ];
    return [...glyphs, key].join("");
  }
  const words = [
    ...shortcut.mod || shortcut.ctrl ? ["Ctrl"] : [],
    ...shortcut.alt ? ["Alt"] : [],
    ...shortcut.shift ? ["Shift"] : [],
    ...shortcut.meta ? ["Meta"] : []
  ];
  return [...words, key].join("+");
}
function onMac(win) {
  const nav = win.navigator;
  return /mac|iphone|ipad/i.test(nav.userAgentData?.platform ?? nav.platform);
}
var MODIFIERS = ["mod", "ctrl", "alt", "shift", "meta"];
function readShortcut(text) {
  const at = text.length < 2 ? -1 : text.lastIndexOf("+", text.length - 2);
  const key = text.slice(at + 1);
  const names = at < 0 ? [] : text.slice(0, at).toLowerCase().split("+");
  if (key === "" || new Set(names).size < names.length || !names.every((name) => MODIFIERS.includes(name))) {
    return void 0;
  }
  const has = (name) => names.includes(name);
  if (has("mod") && (has("ctrl") || has("meta"))) return void 0;
  return { key, mod: has("mod"), ctrl: has("ctrl"), alt: has("alt"), shift: has("shift"), meta: has("meta") };
}

// srcts/modules/board-action-menu/index.tsx
import { Fragment, jsx, jsxs } from "react/jsx-runtime";
var api = 1;
var board_action_menu_default = { render };
function render({ menu, el, anchor }) {
  const doc = el.ownerDocument;
  const host = doc.createElement("div");
  doc.body.append(host);
  const root = createRoot(host);
  const insideRef = { current: null };
  const tab = (event) => {
    if (event.key !== "Tab" || event.isComposing || !(event.target instanceof Node) || insideRef.current?.contains(event.target) !== true) return;
    event.preventDefault();
    event.stopPropagation();
    menu.close();
  };
  doc.defaultView?.addEventListener("keydown", tab, true);
  flushSync(() => {
    root.render(/* @__PURE__ */ jsx(CoreMenu, { menu, anchor, win: doc.defaultView ?? window, insideRef }));
  });
  return () => {
    doc.defaultView?.removeEventListener("keydown", tab, true);
    root.unmount();
    host.remove();
  };
}
function CoreMenu({
  menu,
  anchor,
  win,
  insideRef
}) {
  const level = useSyncExternalStore(menu.subscribe, menu.get);
  const box = useRef(null);
  const { x, y, width, height } = anchor;
  const tip = openTip(win);
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx("div", { ref: box, "aria-hidden": "true", style: { position: "fixed", left: x, top: y, width, height, pointerEvents: "none" } }),
    /* @__PURE__ */ jsx(
      Popover,
      {
        triggerRef: box,
        isOpen: true,
        placement: "bottom start",
        isKeyboardDismissDisabled: true,
        onOpenChange: (open) => {
          if (!open) menu.close();
        },
        className: "bc:max-w-80 bc:min-w-45",
        children: /* @__PURE__ */ jsx(
          "div",
          {
            ref: (el) => {
              insideRef.current = el;
            },
            role: "dialog",
            "aria-label": level.title ?? level.label,
            tabIndex: -1,
            className: "bc:grid bc:gap-1 bc:outline-none",
            onKeyDownCapture: (event) => {
              if (event.nativeEvent.isComposing) return;
              if (event.key === "Escape") {
                event.preventDefault();
                event.stopPropagation();
                menu.escape();
              } else if (level.advanced && matchesShortcut(OPEN_KEY, event.nativeEvent)) {
                event.preventDefault();
                event.stopPropagation();
                menu.openAdvanced(openRow(event.target));
              } else if (event.key === "Enter" && isFilter(event.target) && !event.target.hasAttribute("aria-activedescendant")) {
                event.preventDefault();
                event.stopPropagation();
                menu.pick();
              }
            },
            children: /* @__PURE__ */ jsx(Level, { menu, level, insideRef, tip }, level.id)
          }
        )
      }
    )
  ] });
}
function Level({
  menu,
  level,
  insideRef,
  tip
}) {
  const texts = level.rows.flatMap((row, i) => row.kind === "text" ? [{ row, i }] : []);
  const listed = level.rows.flatMap((row, i) => row.kind === "text" ? [] : [{ row, i }]);
  const rows = level.rows.length > 0;
  useEffect(() => {
    let frames = 0;
    let frame;
    const place = () => {
      const el = insideRef.current;
      if (el === null) return;
      const field = el.querySelector("input:not([data-filter])") ?? el.querySelector("input[data-filter]");
      const first = field ?? el.querySelector("[role=menuitem][data-key], [role=menuitemcheckbox][data-key]");
      if (first === null && rows && frames++ < 10) {
        frame = requestAnimationFrame(place);
        return;
      }
      (first ?? el).focus({ preventScroll: true });
      if (field !== null && !field.hasAttribute("data-filter")) field.select();
    };
    place();
    return () => {
      if (frame !== void 0) cancelAnimationFrame(frame);
    };
  }, [insideRef, rows]);
  const pick = (i) => {
    menu.pick(i);
  };
  const openAdvanced = (i) => {
    menu.openAdvanced(i);
  };
  const grouped = groups(listed);
  const list = /* @__PURE__ */ jsx(Menu, { "aria-label": level.label, renderEmptyState: () => /* @__PURE__ */ jsx("p", { className: NOTE, children: "No matches" }), children: grouped.map((group, g) => {
    const divider = g > 0 && group.category !== grouped[g - 1]?.category;
    const section = group.kind === "multi" ? /* @__PURE__ */ jsx(
      MenuSection,
      {
        id: `multi-${String(group.i)}`,
        title: group.row.label,
        selectionMode: "multiple",
        selectedKeys: group.row.choices.filter((choice) => choice.picked).map(({ id }) => choiceKey(group.i, id)),
        onSelectionChange: (keys) => {
          for (const choice of group.row.choices) {
            const ticked = keys === "all" || keys.has(choiceKey(group.i, choice.id));
            if (ticked !== choice.picked) menu.tick(group.i, choice.id);
          }
        },
        children: group.row.choices.map((choice) => /* @__PURE__ */ jsx(MenuItem, { id: choiceKey(group.i, choice.id), textValue: choice.label, children: choice.label }, choice.id))
      },
      `multi-${String(group.i)}`
    ) : /* @__PURE__ */ jsx(
      MenuSection,
      {
        id: `items-${String(group.items[0]?.i ?? g)}`,
        title: group.continues ? void 0 : group.heading,
        ...group.checks ? { selectionMode: "multiple", selectedKeys: group.items.filter(({ row }) => row.checked === true).map(({ i }) => itemKey(i)) } : {},
        children: group.items.map(({ row, i }) => /* @__PURE__ */ jsx(
          Item,
          {
            row,
            index: i,
            tip,
            onPick: () => {
              pick(i);
            },
            onOpen: () => {
              openAdvanced(i);
            }
          },
          i
        ))
      },
      `items-${String(group.items[0]?.i ?? g)}`
    );
    return divider ? [/* @__PURE__ */ jsx(Separator, { className: DIVIDER }, `divider-${String(g)}`), section] : [section];
  }) });
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    level.advanced ? /* @__PURE__ */ jsxs("div", { className: "bc:flex bc:items-center bc:gap-2", children: [
      level.title === void 0 ? null : /* @__PURE__ */ jsx("p", { className: `${TITLE} bc:min-w-0 bc:flex-1`, children: level.title }),
      /* @__PURE__ */ jsx(
        "button",
        {
          type: "button",
          "aria-label": OPEN_NAME,
          "aria-keyshortcuts": ariaShortcuts(OPEN_KEY),
          title: tip,
          className: OPEN,
          onClick: () => {
            openAdvanced();
          },
          children: /* @__PURE__ */ jsx(Icon, { icon: SquareArrowUpRightExit })
        }
      )
    ] }) : level.title === void 0 ? null : /* @__PURE__ */ jsx("p", { className: TITLE, children: level.title }),
    level.loading ? /* @__PURE__ */ jsx("p", { role: "status", className: `${NOTE} bc:animate-[bc-appear_300ms_ease-out_300ms_both]`, children: "Loading\u2026" }) : level.error === void 0 ? /* @__PURE__ */ jsxs(Fragment, { children: [
      texts.map(({ row, i }) => /* @__PURE__ */ jsx(
        TextRow,
        {
          row,
          onType: (value) => {
            menu.type(value, i);
          },
          onSubmit: () => {
            pick(i);
          }
        },
        i
      )),
      level.filter === void 0 ? listed.length === 0 ? null : list : /* @__PURE__ */ jsxs(
        Autocomplete,
        {
          inputValue: level.filter,
          onInputChange: (value) => {
            menu.type(value);
          },
          children: [
            /* @__PURE__ */ jsx(AriaTextField, { "aria-label": "Filter", className: "bc:px-1 bc:pt-1", children: /* @__PURE__ */ jsx(
              Input,
              {
                "data-filter": "",
                placeholder: "Filter",
                spellCheck: "false",
                "aria-keyshortcuts": level.advanced ? ariaShortcuts(OPEN_KEY) : void 0,
                className: FILTER
              }
            ) }),
            list
          ]
        }
      )
    ] }) : /* @__PURE__ */ jsx("p", { role: "alert", className: "bc:m-0 bc:px-2 bc:py-1.5 bc:text-sm bc:break-words bc:text-destructive-text", children: level.error })
  ] });
}
function groups(listed) {
  const all = [];
  for (const { row, i } of listed) {
    if (row.kind === "text") continue;
    if (row.kind === "multi") {
      all.push({ kind: "multi", row, i });
      continue;
    }
    const last = all.at(-1);
    const checks = row.checked !== void 0;
    const under = last?.kind === "items" && last.heading === row.heading && last.category === row.category;
    if (under && last.checks === checks) {
      last.items.push({ row, i });
    } else {
      all.push({
        kind: "items",
        ...row.heading === void 0 ? {} : { heading: row.heading },
        ...row.category === void 0 ? {} : { category: row.category },
        checks,
        continues: under,
        items: [{ row, i }]
      });
    }
  }
  return all;
}
function Item({
  row,
  index,
  tip,
  onPick,
  onOpen
}) {
  const { label, icon, detail } = row;
  const removes = row.category === "remove";
  return /* @__PURE__ */ jsxs(
    MenuItem,
    {
      id: itemKey(index),
      textValue: label,
      "aria-label": detail === void 0 ? label : `${label}, ${detail}`,
      onAction: onPick,
      className: removes ? REMOVES : void 0,
      children: [
        /* @__PURE__ */ jsxs(
          "span",
          {
            className: `bc:flex bc:min-w-0 bc:items-center bc:gap-2 ${removes ? "bc:group-data-focused:text-destructive-text bc:group-data-hovered:text-destructive-text" : ""}`,
            children: [
              icon === void 0 ? null : /* @__PURE__ */ jsx(
                "span",
                {
                  "aria-hidden": "true",
                  className: "bc:flex bc:size-4 bc:shrink-0 bc:[&>svg]:size-full",
                  dangerouslySetInnerHTML: { __html: icon }
                }
              ),
              /* @__PURE__ */ jsx("span", { className: "bc:min-w-0 bc:truncate", children: label })
            ]
          }
        ),
        detail === void 0 && row.advanced !== true ? null : /* @__PURE__ */ jsxs("span", { className: "bc:-my-1 bc:flex bc:items-center bc:gap-2", children: [
          detail === void 0 ? null : /* @__PURE__ */ jsx("span", { className: "bc:min-w-0 bc:truncate bc:text-xs bc:text-muted-foreground", children: detail }),
          row.advanced === true ? /* @__PURE__ */ jsx(
            "span",
            {
              "aria-hidden": "true",
              "data-open-row": index,
              title: tip,
              className: OPEN,
              onPointerDown: (event) => {
                event.stopPropagation();
              },
              onPointerUp: (event) => {
                event.stopPropagation();
              },
              onClick: (event) => {
                event.stopPropagation();
                onOpen();
              },
              children: /* @__PURE__ */ jsx(Icon, { icon: SquareArrowUpRightExit })
            }
          ) : null
        ] }),
        row.advanced === true ? /* @__PURE__ */ jsx(Text, { slot: "description", className: "bc:sr-only", children: tip }) : null
      ]
    }
  );
}
function openRow(target) {
  if (!(target instanceof Element)) return void 0;
  const active = isFilter(target) ? target.getAttribute("aria-activedescendant") : null;
  const item = active === null ? target.closest('[role="menuitem"], [role="menuitemcheckbox"]') : target.ownerDocument.getElementById(active);
  const marked = item?.querySelector("[data-open-row]")?.getAttribute("data-open-row");
  return marked === void 0 || marked === null ? void 0 : Number(marked);
}
function TextRow({
  row,
  onType,
  onSubmit
}) {
  return /* @__PURE__ */ jsx(
    TextField,
    {
      label: row.label,
      value: row.value,
      spellCheck: "false",
      isInvalid: row.refusal !== void 0,
      ...row.refusal === void 0 ? {} : { errorMessage: row.refusal },
      className: "bc:px-1 bc:pt-1",
      onChange: onType,
      onKeyDown: (event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) {
          event.preventDefault();
          onSubmit();
        } else event.continuePropagation();
      }
    }
  );
}
function itemKey(row) {
  return String(row);
}
function choiceKey(row, choice) {
  return `${String(row)}:${choice}`;
}
function isFilter(target) {
  return target instanceof HTMLInputElement && target.hasAttribute("data-filter");
}
var TITLE = "bc:m-0 bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-muted-foreground";
var OPEN = "bc:ml-auto bc:inline-flex bc:size-6 bc:shrink-0 bc:items-center bc:justify-center bc:rounded-sm bc:border-0 bc:bg-transparent bc:p-0 bc:text-muted-foreground bc:cursor-pointer bc:outline-none bc:hover:bg-accent bc:hover:text-accent-foreground bc:focus-visible:ring-2 bc:focus-visible:ring-ring";
var OPEN_NAME = "Show all options";
var OPEN_KEY = "Mod+Enter";
function openTip(win) {
  return `${OPEN_NAME} (${shortcutLabel(OPEN_KEY, onMac(win)) ?? OPEN_KEY})`;
}
var NOTE = "bc:m-0 bc:px-2 bc:py-1.5 bc:text-center bc:text-sm bc:text-muted-foreground bc:italic";
var FILTER = "bc:box-border bc:w-full bc:rounded-sm bc:border bc:border-border bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-xs bc:text-inherit bc:outline-none";
var DIVIDER = "bc:mx-0 bc:my-1 bc:h-px bc:border-0 bc:bg-border";
var REMOVES = "bc:group bc:data-hovered:bg-destructive/10! bc:data-focused:bg-destructive/10!";
export {
  api,
  board_action_menu_default as default
};

// srcts/modules/edit-board/index.tsx
import { flushSync as flushSync2 } from "react-dom";
import { createRoot as createRoot2 } from "react-dom/client";

// srcts/modules/edit-board/board.tsx
import { useCallback as useCallback5, useMemo as useMemo3 } from "react";

// srcts/card/footer.tsx
import { useCallback, useId, useState } from "react";
import { Fragment, jsx, jsxs } from "react/jsx-runtime";
var MARKS = {
  error: {
    label: "Error",
    noun: "error",
    text: "bc:text-text-danger",
    tint: "bc:bg-red-500/10",
    border: "bc:border-border-danger"
  },
  warning: {
    label: "Warning",
    noun: "warning",
    text: "bc:text-text-warning",
    tint: "bc:bg-amber-500/10",
    border: "bc:border-amber-500"
  },
  message: {
    label: "Message",
    noun: "message",
    text: "bc:text-blue-700 bc:dark:text-blue-400",
    tint: "bc:bg-blue-500/10",
    border: "bc:border-border-accent"
  }
};
function SeverityIcon({ severity }) {
  return /* @__PURE__ */ jsx(
    "svg",
    {
      "aria-hidden": "true",
      viewBox: "0 0 16 16",
      className: "bc:size-3 bc:shrink-0",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: 1.6,
      strokeLinecap: "round",
      strokeLinejoin: "round",
      children: severity === "warning" ? /* @__PURE__ */ jsxs(Fragment, { children: [
        /* @__PURE__ */ jsx("path", { d: "M8 1.8 14.6 13.6H1.4Z" }),
        /* @__PURE__ */ jsx("path", { d: "M8 6.2v3.2M8 11.6v.1" })
      ] }) : /* @__PURE__ */ jsxs(Fragment, { children: [
        /* @__PURE__ */ jsx("circle", { cx: "8", cy: "8", r: "6.4" }),
        /* @__PURE__ */ jsx("path", { d: severity === "error" ? "M5.7 5.7l4.6 4.6M10.3 5.7l-4.6 4.6" : "M8 7.4v3.6M8 4.9v.1" })
      ] })
    }
  );
}

// srcts/card/mounted.tsx
import { useLayoutEffect, useRef } from "react";
import { jsx as jsx2 } from "react/jsx-runtime";
function Mounted({
  mount,
  label,
  className,
  data
}) {
  const ref = useRef(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const cleanup = mount(el);
    return () => {
      queueMicrotask(cleanup);
    };
  }, [mount]);
  return /* @__PURE__ */ jsx2("div", { ref, role: label === void 0 ? void 0 : "region", "aria-label": label, className, ...data });
}

// srcts/card/status.ts
import { useState as useState2 } from "react";

// srcts/modules/edit-board/board.tsx
import {
  Button as Button3,
  Cell,
  Column,
  Disclosure,
  DisclosureHeader,
  DisclosurePanel,
  Row,
  Table,
  TableBody,
  TableHeader,
  useBoard
} from "@blockr/core/react";

// srcts/ui/blocks.tsx
import { ComboBoxItem, Text } from "@blockr/core/react";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}

// srcts/ui/blocks.tsx
import { jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function blockName(board, block) {
  return own(board.blocks, block)?.name ?? block;
}
function classInputs(board, registry, block) {
  const held = own(board.blocks, block);
  return (held === void 0 ? void 0 : own(registry, held.registry_id)?.inputs) ?? [];
}
function isVariadicBlock(board, registry, block) {
  return classInputs(board, registry, block).includes("...");
}

// srcts/ui/edits.ts
import { useMemo, useState as useState3 } from "react";
function notifiedReasons(notifications) {
  return notifications.flatMap(({ message }) => {
    const [, ...lines] = message.split("\n");
    const failures = lines.filter((line) => line.startsWith("- "));
    return failures.length > 0 && failures.length === lines.length ? failures.map((line) => line.slice(2)) : [message];
  });
}

// srcts/ui/hooks.ts
import { useCallback as useCallback2, useSyncExternalStore } from "react";

// srcts/ui/search.ts
import { useState as useState4 } from "react";

// srcts/ui/table.tsx
import { useState as useState6 } from "react";
import { SearchField } from "@blockr/core/react";

// srcts/ui/ui.tsx
import { useRef as useRef2, useState as useState5 } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Button, TextField } from "@blockr/core/react";

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:font-sans bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent bc:aria-invalid:accent-border-danger",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};
var buttons = {
  primary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-accent bc:bg-bg-accent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-on-accent bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50",
  secondary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50",
  destructive: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-red-700 bc:bg-red-700 bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-white bc:cursor-pointer bc:hover:bg-red-800 bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle"
};

// srcts/ui/ui.tsx
import { jsx as jsx4, jsxs as jsxs3 } from "react/jsx-runtime";
var classes = {
  button: buttons.secondary,
  primary: buttons.primary,
  quiet: buttons.quiet,
  // The board's page, which draws no background of its own, so that the
  // page's, which the browser draws in the page's theme, shows to its edges.
  page: "bc:text-gray-900 bc:dark:text-gray-100",
  // What sits over the board, as a dialog or a menu does,
  // on the background that core's theme gives it, bg-raised.
  surface: "bc:bg-bg-raised bc:text-gray-900 bc:dark:text-gray-100",
  card: "bc:bg-bg-subtle",
  heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold",
  list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0"
};
function ActionsButton({
  ref,
  label,
  onPress
}) {
  return /* @__PURE__ */ jsx4(Button, { ref, variant: "quiet", "aria-label": label, "aria-haspopup": "menu", onPress, children: /* @__PURE__ */ jsx4("span", { "aria-hidden": "true", className: "bc:text-base bc:leading-none", children: "\u22EF" }) });
}
function Refusal({ refusal }) {
  if (refusal === null) return null;
  const mark = MARKS.error;
  const { title, reasons } = refusal;
  return /* @__PURE__ */ jsxs3(
    "div",
    {
      role: "alert",
      className: `bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`,
      children: [
        /* @__PURE__ */ jsx4("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx4(SeverityIcon, { severity: "error" }) }),
        /* @__PURE__ */ jsxs3("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-1 bc:text-sm bc:break-words", children: [
          /* @__PURE__ */ jsx4("p", { className: `bc:m-0 bc:font-semibold ${mark.text}`, children: title }),
          reasons.length === 1 ? /* @__PURE__ */ jsx4("p", { className: "bc:m-0 bc:whitespace-pre-wrap", children: reasons[0] }) : /* @__PURE__ */ jsx4("ul", { className: "bc:m-0 bc:list-disc bc:pl-5", children: reasons.map((reason, i) => /* @__PURE__ */ jsx4("li", { className: "bc:whitespace-pre-wrap", children: reason }, i)) })
        ] })
      ]
    }
  );
}

// srcts/ui/table.tsx
import { jsx as jsx5, jsxs as jsxs4 } from "react/jsx-runtime";
var PAGE_SIZE = 10;
function useTableView(rows, text, size = PAGE_SIZE) {
  const [query, setQueryState] = useState6("");
  const [page, setPage] = useState6(0);
  const needle = query.trim().toLowerCase();
  const matching = needle === "" ? rows : rows.filter((row) => text(row).toLowerCase().includes(needle));
  const pages = Math.max(1, Math.ceil(matching.length / size));
  const shown = Math.min(page, pages - 1);
  const first = shown * size;
  return {
    rows: matching.slice(first, first + size),
    query,
    setQuery: (next) => {
      setQueryState(next);
      setPage(0);
    },
    page: shown,
    pages,
    setPage,
    found: matching.length,
    first
  };
}
function TableSearch({ label, view }) {
  return /* @__PURE__ */ jsx5("search", { children: /* @__PURE__ */ jsx5(SearchField, { label, spellCheck: "false", autoComplete: "off", value: view.query, onChange: view.setQuery }) });
}
function Pager({ label, view }) {
  const { page, pages, found, first, rows } = view;
  if (pages < 2) return null;
  return /* @__PURE__ */ jsxs4("nav", { "aria-label": label, className: "bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-text-muted", children: [
    /* @__PURE__ */ jsxs4("span", { "aria-live": "polite", children: [
      "Rows ",
      first + 1,
      "\u2013",
      first + rows.length,
      " of ",
      found
    ] }),
    /* @__PURE__ */ jsxs4("span", { className: "bc:flex bc:gap-1", children: [
      /* @__PURE__ */ jsx5(
        "button",
        {
          type: "button",
          className: classes.button,
          disabled: page === 0,
          onClick: () => {
            view.setPage(page - 1);
          },
          children: "Previous"
        }
      ),
      /* @__PURE__ */ jsx5(
        "button",
        {
          type: "button",
          className: classes.button,
          disabled: page >= pages - 1,
          onClick: () => {
            view.setPage(page + 1);
          },
          children: "Next"
        }
      )
    ] })
  ] });
}

// srcts/modules/edit-board/menus.tsx
import { useCallback as useCallback3, useContext, useEffect, useLayoutEffect as useLayoutEffect2, useMemo as useMemo2, useRef as useRef3, useState as useState7 } from "react";
import { TableStateContext } from "./vendor-c5b8952d.js";
import { Button as Button2 } from "@blockr/core/react";
import { jsx as jsx6, jsxs as jsxs5 } from "react/jsx-runtime";
function useMenus(core) {
  const [refusal, setRefusal] = useState7(null);
  const [shown, setShown] = useState7(() => /* @__PURE__ */ new Map());
  const [back, setBack] = useState7(null);
  const places = useRef3(/* @__PURE__ */ new Map());
  const latest = useRef3(shown);
  useEffect(() => {
    latest.current = shown;
  }, [shown]);
  useEffect(
    () => () => {
      const open2 = [...latest.current.values()];
      queueMicrotask(() => {
        for (const args of open2) args.close();
      });
    },
    []
  );
  useEffect(() => {
    if (back === null) return void 0;
    const timer = setTimeout(() => {
      setBack(null);
    });
    return () => {
      clearTimeout(timer);
    };
  }, [back]);
  const ended = useCallback3((key, args) => {
    setShown((before) => before.get(key) === args ? without2(before, key) : before);
    setBack(key);
  }, []);
  const placeOf = useCallback3(
    (key) => {
      const known = places.current.get(key);
      if (known !== void 0) return known;
      const place = (args) => {
        setShown((before) => new Map(before).set(key, args));
        return () => {
          ended(key, args);
        };
      };
      places.current.set(key, place);
      return place;
    },
    [ended]
  );
  const open = useCallback3(
    (target, key, button) => {
      setRefusal(null);
      core.boardActions.menu(target, {
        el: button,
        anchor: button.getBoundingClientRect(),
        notifier: (notifications, action) => {
          setRefusal({ title: editTitle(core.store.board(), action, target), reasons: notifiedReasons(notifications) });
        },
        advanced: placeOf(key)
      });
    },
    [core, placeOf]
  );
  const close = useCallback3(
    (key, args) => {
      args.close();
      ended(key, args);
    },
    [ended]
  );
  return useMemo2(() => ({ refusal, shown, open, close, back }), [refusal, shown, open, close, back]);
}
function MenuButton({ menus, target, place, label }) {
  const ref = useRef3(null);
  const back = menus.back === place;
  useLayoutEffect2(() => {
    if (back) ref.current?.focus();
  }, [back]);
  return /* @__PURE__ */ jsx6(
    ActionsButton,
    {
      ref,
      label,
      onPress: () => {
        const el = ref.current;
        if (el !== null) menus.open(target, place, el);
      }
    }
  );
}
function PlaceForm({ args, onClose }) {
  const box = useRef3(null);
  const mount = useCallback3((el) => args.mount(el), [args]);
  useEffect(() => {
    const el = box.current;
    if (el === null) return void 0;
    let listOpen = false;
    const down = (event) => {
      listOpen = event.target instanceof Element && event.target.getAttribute("aria-expanded") === "true";
    };
    const heard = (event) => {
      if (event.key === "Escape" && !event.defaultPrevented && !listOpen) onClose();
    };
    el.addEventListener("keydown", down, { capture: true });
    el.addEventListener("keydown", heard);
    return () => {
      el.removeEventListener("keydown", down, { capture: true });
      el.removeEventListener("keydown", heard);
    };
  }, [onClose]);
  return /* @__PURE__ */ jsxs5("div", { ref: box, role: "group", "aria-label": args.title, className: "bc:grid bc:gap-2 bc:py-1 bc:whitespace-normal", ...KEPT, children: [
    /* @__PURE__ */ jsx6(TakesKeys, {}),
    /* @__PURE__ */ jsx6(Mounted, { mount, className: "bc:min-w-0" }),
    /* @__PURE__ */ jsx6("div", { className: "bc:flex bc:gap-2", children: /* @__PURE__ */ jsx6(Button2, { variant: "secondary", onPress: onClose, children: "Close" }) })
  ] });
}
var KEPT = Object.fromEntries(
  ["onPointerDown", "onMouseDown", "onTouchStart", "onClick", "onDoubleClick", "onKeyDown", "onKeyUp"].map((handler) => [
    handler,
    (event) => {
      event.stopPropagation();
    }
  ])
);
var taking = /* @__PURE__ */ new WeakMap();
function TakesKeys() {
  const disable = useContext(TableStateContext)?.setKeyboardNavigationDisabled;
  useEffect(() => {
    if (disable === void 0) return void 0;
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
function editTitle(board, action, target) {
  const object = named(board, target);
  const edit = own(CORE_EDITS, action.id);
  return edit === void 0 ? `${action.label} failed for ${object}.` : `Could not ${edit(object)}.`;
}
var CORE_EDITS = {
  "blockr.core-append_block_action": (object) => `append a block to ${object}`,
  "blockr.core-prepend_block_action": (object) => `prepend a block to ${object}`,
  "blockr.core-insert_block_action": (object) => `insert a block into ${object}`,
  "blockr.core-add_link_action": (object) => `add a link for ${object}`,
  "blockr.core-add_stack_action": (object) => `put ${object} in a stack`,
  "blockr.core-rename_action": (object) => `rename ${object}`,
  "blockr.core-edit_link_action": (object) => `change ${object}`,
  "blockr.core-edit_stack_action": (object) => `change ${object}`,
  "blockr.core-remove_block_action": (object) => `remove ${object}`,
  "blockr.core-remove_link_action": (object) => `remove ${object}`,
  "blockr.core-remove_stack_action": (object) => `remove ${object}`,
  "blockr.core-remove_selected_action": (object) => `remove ${object}`
};
function named(board, target) {
  if ("selection" in target) return "the selection";
  if ("board" in target) return "the board";
  if ("block" in target) return `the block "${blockName(board, target.block)}"`;
  if ("link" in target) {
    const link = own(board.links, target.link);
    return link === void 0 ? `the link "${target.link}"` : `the link from "${blockName(board, link.from)}" to "${blockName(board, link.to)}"`;
  }
  if ("stack" in target) return `the stack "${own(board.stacks, target.stack)?.name ?? target.stack}"`;
  if ("option" in target) return `the board option "${target.option}"`;
  return `the plugin "${target.plugin}"`;
}
function without2(map, key) {
  const next = new Map(map);
  next.delete(key);
  return next;
}

// srcts/modules/edit-board/blocks.tsx
import { Fragment as Fragment2, jsx as jsx7, jsxs as jsxs6 } from "react/jsx-runtime";
var ROLE = "add_block_role";
var COLUMNS = [
  { id: "name", name: "Name", rowHeader: true },
  { id: "id", name: "ID" },
  { id: "class", name: "Class" }
];
function BlocksSection({ core, board }) {
  const registry = core.blockRegistry();
  const className = (block) => own(registry, block.registry_id)?.name ?? block.registry_id;
  const view = useTableView(Object.entries(board.blocks), ([id, block]) => `${block.name} ${id} ${className(block)}`);
  const count = Object.keys(board.blocks).length;
  const menus = useMenus(core);
  return /* @__PURE__ */ jsxs6(Section, { title: "Blocks", count, children: [
    offers(core, ROLE, BOARD) ? /* @__PURE__ */ jsx7(Part, { title: "Add a block", children: /* @__PURE__ */ jsx7(Advanced, { core, role: ROLE, target: BOARD }) }) : null,
    /* @__PURE__ */ jsx7(Part, { title: "Blocks on the board", children: count === 0 ? /* @__PURE__ */ jsx7("p", { className: styles.hint, children: "The board holds no blocks yet." }) : /* @__PURE__ */ jsxs6(Fragment2, { children: [
      /* @__PURE__ */ jsx7(TableSearch, { label: "Search blocks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx7("p", { className: styles.hint, children: "No block matches the search." }) : /* @__PURE__ */ jsx7(
        ObjectsTable,
        {
          core,
          menus,
          kind: "block",
          label: "The board's blocks",
          columns: COLUMNS,
          rows: view.rows.map(([id, block]) => ({
            id,
            text: block.name,
            menuLabel: `Actions of the block ${block.name}`,
            cells: [
              { content: /* @__PURE__ */ jsx7("span", { className: "bc:font-medium", children: block.name }), className: WRAP },
              { content: id, className: "bc:font-mono bc:text-xs bc:whitespace-normal! bc:break-all" },
              { content: className(block), className: `${WRAP} bc:text-text-muted` }
            ]
          }))
        }
      ),
      /* @__PURE__ */ jsx7(Refusal, { refusal: menus.refusal }),
      /* @__PURE__ */ jsx7(Pager, { label: "Pages of blocks", view })
    ] }) })
  ] });
}

// srcts/graph/graph.ts
function createGraph(board) {
  const blocks = Object.keys(board.blocks);
  const into = /* @__PURE__ */ new Map();
  const sources = /* @__PURE__ */ new Map();
  const targets = /* @__PURE__ */ new Map();
  for (const id of blocks) {
    into.set(id, []);
    sources.set(id, []);
    targets.set(id, []);
  }
  for (const [id, link] of Object.entries(board.links)) {
    const linksIn = into.get(link.to);
    const from = sources.get(link.to);
    const to = targets.get(link.from);
    if (linksIn === void 0 || from === void 0 || to === void 0) continue;
    linksIn.push(id);
    from.push(link.from);
    to.push(link.to);
  }
  const upstream = once(sources);
  const downstream = once(targets);
  const order = topological(blocks, sources, targets);
  let cycle;
  return {
    order,
    linksInto: (id) => into.get(id) ?? [],
    upstream: (id) => upstream.get(id) ?? [],
    downstream: (id) => downstream.get(id) ?? [],
    withUpstream: (ids) => closure(ids, upstream),
    withDownstream: (ids) => closure(ids, downstream),
    cycle() {
      cycle ??= { found: findCycle(board, targets) };
      return cycle.found;
    },
    path: (from, to) => findPath(from, to, downstream)
  };
}
function once(ends) {
  return new Map([...ends].map(([id, blocks]) => [id, [...new Set(blocks)]]));
}
function closure(ids, steps) {
  const reached = /* @__PURE__ */ new Set();
  const todo = [...ids].filter((id) => steps.has(id));
  for (const id of todo) reached.add(id);
  for (const id of todo) {
    for (const next of steps.get(id) ?? []) {
      if (reached.has(next)) continue;
      reached.add(next);
      todo.push(next);
    }
  }
  return reached;
}
function topological(blocks, sources, targets) {
  const left = new Map(blocks.map((id) => [id, sources.get(id)?.length ?? 0]));
  const order = blocks.filter((id) => left.get(id) === 0);
  for (const id of order) {
    for (const next of targets.get(id) ?? []) {
      const count = (left.get(next) ?? 0) - 1;
      left.set(next, count);
      if (count === 0) order.push(next);
    }
  }
  const placed = new Set(order);
  return [...order, ...blocks.filter((id) => !placed.has(id))];
}
function findCycle(board, targets) {
  const done = /* @__PURE__ */ new Set();
  const path = [];
  const onPath = /* @__PURE__ */ new Set();
  function visit(block) {
    if (onPath.has(block)) return path.slice(path.indexOf(block));
    if (done.has(block)) return void 0;
    path.push(block);
    onPath.add(block);
    for (const next of targets.get(block) ?? []) {
      const cycle = visit(next);
      if (cycle !== void 0) return cycle;
    }
    path.pop();
    onPath.delete(block);
    done.add(block);
    return void 0;
  }
  const starts = new Set(Object.values(board.links).map((link) => link.from));
  for (const block of starts) {
    if (!targets.has(block)) continue;
    const cycle = visit(block);
    if (cycle !== void 0) return cycle;
  }
  return void 0;
}
function findPath(from, to, downstream) {
  if (!downstream.has(from)) return void 0;
  const before = /* @__PURE__ */ new Map([[from, void 0]]);
  const todo = [from];
  for (const block of todo) {
    if (block === to) {
      const path = [block];
      for (let at = before.get(block); at !== void 0; at = before.get(at)) path.unshift(at);
      return path;
    }
    for (const next of downstream.get(block) ?? []) {
      if (before.has(next)) continue;
      before.set(next, block);
      todo.push(next);
    }
  }
  return void 0;
}

// srcts/modules/edit-board/links.tsx
import { Fragment as Fragment3, jsx as jsx8, jsxs as jsxs7 } from "react/jsx-runtime";
var ROLE2 = "add_link_role";
var COLUMNS2 = [
  { id: "from", name: "From", rowHeader: true },
  { id: "to", name: "To" },
  { id: "input", name: "Input" }
];
function LinksSection({ core, board, locked }) {
  const links = Object.entries(board.links);
  const view = useTableView(
    links,
    ([, link]) => [blockName(board, link.from), link.from, blockName(board, link.to), link.to, link.input].join(" ")
  );
  const variadic = variadicBlocks(board, core.blockRegistry());
  const menus = useMenus(core);
  return /* @__PURE__ */ jsxs7(Section, { title: "Links", count: links.length, children: [
    locked ? null : /* @__PURE__ */ jsx8(Part, { title: "Add a link", children: offers(core, ROLE2, BOARD) ? /* @__PURE__ */ jsx8(Advanced, { core, role: ROLE2, target: BOARD }) : /* @__PURE__ */ jsx8("p", { className: styles.hint, children: Object.keys(board.blocks).length < 2 ? "A link joins two blocks, so the board needs two blocks first." : "No new link can join two blocks of the board." }) }),
    /* @__PURE__ */ jsx8(Part, { title: "Links on the board", children: links.length === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "No link joins two blocks yet." }) : /* @__PURE__ */ jsxs7(Fragment3, { children: [
      /* @__PURE__ */ jsx8(TableSearch, { label: "Search links", view }),
      view.found === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "No link matches the search." }) : /* @__PURE__ */ jsx8(
        ObjectsTable,
        {
          core,
          menus,
          kind: "link",
          label: "The board's links",
          columns: COLUMNS2,
          rows: view.rows.map(([id, link]) => {
            const from = blockName(board, link.from);
            const to = blockName(board, link.to);
            return {
              id,
              text: `${from} \u2192 ${to}`,
              menuLabel: `Actions of the link from ${from} to ${to}`,
              cells: [
                { content: /* @__PURE__ */ jsx8(BlockLabel, { board, id: link.from }), className: WRAP },
                { content: /* @__PURE__ */ jsx8(BlockLabel, { board, id: link.to }), className: WRAP },
                {
                  content: variadic.has(link.to) ? /* @__PURE__ */ jsx8(VariadicInput, { board, id, link }) : /* @__PURE__ */ jsx8("span", { className: "bc:font-mono bc:text-xs", children: link.input }),
                  className: WRAP
                }
              ]
            };
          })
        }
      ),
      /* @__PURE__ */ jsx8(Refusal, { refusal: menus.refusal }),
      /* @__PURE__ */ jsx8(Pager, { label: "Pages of links", view })
    ] }) })
  ] });
}
function BlockLabel({ board, id }) {
  return /* @__PURE__ */ jsxs7("span", { className: "bc:grid", children: [
    /* @__PURE__ */ jsx8("span", { children: blockName(board, id) }),
    /* @__PURE__ */ jsx8("span", { className: "bc:font-mono bc:text-xs bc:text-text-muted bc:break-all", children: id })
  ] });
}
function VariadicInput({ board, id, link }) {
  const place = createGraph(board).linksInto(link.to).indexOf(id) + 1;
  return /* @__PURE__ */ jsxs7("span", { className: "bc:grid", children: [
    link.input === "" ? /* @__PURE__ */ jsx8("span", { className: "bc:text-text-muted", children: "No name" }) : /* @__PURE__ */ jsx8("span", { className: "bc:font-mono bc:text-xs", children: link.input }),
    /* @__PURE__ */ jsxs7("span", { className: "bc:text-xs bc:text-text-muted", children: [
      "Position ",
      place
    ] })
  ] });
}
function variadicBlocks(board, registry) {
  return new Set(Object.keys(board.blocks).filter((id) => isVariadicBlock(board, registry, id)));
}

// srcts/modules/edit-board/selection.ts
import { useCallback as useCallback4, useSyncExternalStore as useSyncExternalStore2 } from "react";
function useSelected(core) {
  const subscribe = useCallback4(
    (notify) => core.selection.subscribe(() => {
      notify();
    }),
    [core]
  );
  return useSyncExternalStore2(subscribe, () => core.selection.get());
}
function selectedIds(selected, kind) {
  return new Set(selected.flatMap((object) => idOf(object, kind) ?? []));
}
function tickRows(core, kind, shown, keys) {
  const ticked = (id) => keys === "all" || keys.has(id);
  const rows = new Set(shown);
  const current = core.selection.get();
  const kept = current.filter((object) => {
    const id = idOf(object, kind);
    return id === void 0 || !rows.has(id) || ticked(id);
  });
  const held = selectedIds(current, kind);
  const added = shown.filter((id) => ticked(id) && !held.has(id)).map((id) => objectOf(kind, id));
  core.selection.set([...kept, ...added]);
}
function objectOf(kind, id) {
  if (kind === "block") return { block: id };
  if (kind === "link") return { link: id };
  return { stack: id };
}
function idOf(object, kind) {
  if (kind === "block") return "block" in object ? object.block : void 0;
  if (kind === "link") return "link" in object ? object.link : void 0;
  return "stack" in object ? object.stack : void 0;
}

// srcts/modules/edit-board/stacks.tsx
import { ColorSwatch } from "@blockr/core/react";
import { Fragment as Fragment4, jsx as jsx9, jsxs as jsxs8 } from "react/jsx-runtime";
var ROLE3 = "add_stack_role";
var COLUMNS3 = [
  { id: "name", name: "Name", rowHeader: true },
  { id: "blocks", name: "Blocks" }
];
function StacksSection({ core, board, locked }) {
  const stacks = Object.entries(board.stacks);
  const view = useTableView(stacks, ([id, stack]) => [stack.name, id, ...stack.blocks.map((block) => blockName(board, block))].join(" "));
  const menus = useMenus(core);
  return /* @__PURE__ */ jsxs8(Section, { title: "Stacks", count: stacks.length, children: [
    locked || !offers(core, ROLE3, BOARD) ? null : /* @__PURE__ */ jsx9(Part, { title: "Add a stack", children: /* @__PURE__ */ jsx9(Advanced, { core, role: ROLE3, target: BOARD }) }),
    /* @__PURE__ */ jsx9(Part, { title: "Stacks on the board", children: stacks.length === 0 ? /* @__PURE__ */ jsx9("p", { className: styles.hint, children: "The board has no stacks yet." }) : /* @__PURE__ */ jsxs8(Fragment4, { children: [
      /* @__PURE__ */ jsx9(TableSearch, { label: "Search stacks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx9("p", { className: styles.hint, children: "No stack matches the search." }) : /* @__PURE__ */ jsx9(
        ObjectsTable,
        {
          core,
          menus,
          kind: "stack",
          label: "The board's stacks",
          columns: COLUMNS3,
          rows: view.rows.map(([id, stack]) => ({
            id,
            text: stack.name,
            menuLabel: `Actions of the stack ${stack.name}`,
            cells: [
              {
                content: /* @__PURE__ */ jsxs8("span", { className: "bc:flex bc:items-center bc:gap-1.5", children: [
                  /* @__PURE__ */ jsx9(Swatch, { color: stack.color }),
                  /* @__PURE__ */ jsx9("span", { className: "bc:font-medium", children: stack.name })
                ] }),
                className: WRAP
              },
              { content: /* @__PURE__ */ jsx9(StackBlocks, { board, blocks: stack.blocks }), className: WRAP }
            ]
          }))
        }
      ),
      /* @__PURE__ */ jsx9(Refusal, { refusal: menus.refusal }),
      /* @__PURE__ */ jsx9(Pager, { label: "Pages of stacks", view })
    ] }) })
  ] });
}
var SHOWN_BLOCKS = 6;
function StackBlocks({ board, blocks }) {
  if (blocks.length === 0) return /* @__PURE__ */ jsx9("span", { className: "bc:text-text-muted", children: "No blocks" });
  const rest = blocks.length - SHOWN_BLOCKS;
  return /* @__PURE__ */ jsxs8("ul", { "aria-label": "Blocks", className: "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0", children: [
    blocks.slice(0, SHOWN_BLOCKS).map((block) => /* @__PURE__ */ jsxs8("li", { className: "bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs", children: [
      blockName(board, block),
      " ",
      /* @__PURE__ */ jsx9("span", { className: "bc:font-mono bc:text-text-muted", children: block })
    ] }, block)),
    rest > 0 ? /* @__PURE__ */ jsxs8("li", { className: "bc:px-1 bc:text-xs bc:text-text-muted", children: [
      "and ",
      rest,
      " more"
    ] }) : null
  ] });
}
function Swatch({ color }) {
  return color === null ? /* @__PURE__ */ jsx9("span", { "aria-hidden": "true", className: RING }) : /* @__PURE__ */ jsx9(ColorSwatch, { color, className: DOT });
}
var RING = "bc:inline-block bc:size-3 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default";
var DOT = "bc:size-3! bc:rounded-full!";

// srcts/modules/edit-board/board.tsx
import { jsx as jsx10, jsxs as jsxs9 } from "react/jsx-runtime";
function EditBoard({ core }) {
  const board = useBoard(core);
  const locked = core.lock.board;
  return /* @__PURE__ */ jsxs9("div", { className: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsx10(SelectionBar, { core }),
    locked ? null : /* @__PURE__ */ jsx10(BlocksSection, { core, board }),
    /* @__PURE__ */ jsx10(LinksSection, { core, board, locked }),
    /* @__PURE__ */ jsx10(StacksSection, { core, board, locked })
  ] });
}
var SELECTION = "selection";
function SelectionBar({ core }) {
  const selected = useSelected(core);
  const menus = useMenus(core);
  const form = menus.shown.get(SELECTION);
  if (selected.length === 0 && form === void 0) return null;
  const target = { selection: selected };
  return /* @__PURE__ */ jsxs9("div", { role: "region", "aria-label": "Selection", className: "bc:grid bc:gap-2", children: [
    selected.length === 0 ? null : /* @__PURE__ */ jsxs9("div", { className: "bc:flex bc:items-center bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:px-3 bc:py-1.5 bc:text-sm", children: [
      /* @__PURE__ */ jsxs9("span", { "aria-live": "polite", children: [
        selected.length,
        " selected"
      ] }),
      /* @__PURE__ */ jsxs9("span", { className: "bc:ml-auto bc:flex bc:items-center bc:gap-1", children: [
        core.boardActions.on(target).length > 0 ? /* @__PURE__ */ jsx10(MenuButton, { menus, target, place: SELECTION, label: "Actions of the selection" }) : null,
        /* @__PURE__ */ jsx10(
          Button3,
          {
            variant: "quiet",
            onPress: () => {
              core.selection.set([]);
            },
            children: "Clear selection"
          }
        )
      ] })
    ] }),
    form === void 0 ? null : /* @__PURE__ */ jsx10(
      PlaceForm,
      {
        args: form,
        onClose: () => {
          menus.close(SELECTION, form);
        }
      }
    ),
    /* @__PURE__ */ jsx10(Refusal, { refusal: menus.refusal })
  ] });
}
function Section({ title, count, children }) {
  return /* @__PURE__ */ jsx10("section", { "aria-label": title, className: "bc:rounded-md bc:border bc:border-border-default", children: /* @__PURE__ */ jsxs9(Disclosure, { defaultExpanded: true, children: [
    /* @__PURE__ */ jsxs9(DisclosureHeader, { className: "bc:text-sm bc:font-semibold bc:[&>button]:gap-2 bc:[&>button]:px-3 bc:[&>button]:py-2", children: [
      title,
      /* @__PURE__ */ jsx10("span", { className: "bc:rounded-full bc:border bc:border-border-default bc:px-1.5 bc:text-xs bc:font-normal bc:text-text-muted", children: count })
    ] }),
    /* @__PURE__ */ jsx10(DisclosurePanel, { children: /* @__PURE__ */ jsx10("div", { className: "bc:grid bc:gap-4 bc:border-t bc:border-border-default bc:p-3", children }) })
  ] }) });
}
function Part({ title, children }) {
  return /* @__PURE__ */ jsxs9("div", { role: "group", "aria-label": title, className: "bc:grid bc:gap-2", children: [
    /* @__PURE__ */ jsx10("h4", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: title }),
    children
  ] });
}
var WRAP = "bc:whitespace-normal! bc:break-words";
function ObjectsTable({
  core,
  menus,
  kind,
  label,
  columns,
  rows
}) {
  const selected = useSelected(core);
  const keys = useMemo3(() => selectedIds(selected, kind), [selected, kind]);
  const offered = new Set(rows.flatMap(({ id }) => core.boardActions.on(objectOf(kind, id)).length > 0 ? [id] : []));
  const actions = offered.size > 0;
  const span = columns.length + (actions ? 1 : 0);
  return /* @__PURE__ */ jsxs9(
    Table,
    {
      "aria-label": label,
      selectionMode: "multiple",
      selectedKeys: keys,
      onSelectionChange: (picked) => {
        tickRows(
          core,
          kind,
          rows.map(({ id }) => id),
          picked
        );
      },
      children: [
        /* @__PURE__ */ jsxs9(TableHeader, { children: [
          columns.map((column) => /* @__PURE__ */ jsx10(Column, { id: column.id, isRowHeader: column.rowHeader === true, children: column.name }, column.id)),
          actions ? /* @__PURE__ */ jsx10(Column, { id: "actions", textValue: "Actions", className: "bc:w-px", children: /* @__PURE__ */ jsx10("span", { className: "bc:sr-only", children: "Actions" }) }) : null
        ] }),
        /* @__PURE__ */ jsx10(TableBody, { children: rows.map((row) => {
          const form = menus.shown.get(row.id);
          return /* @__PURE__ */ jsx10(Row, { id: row.id, textValue: row.text, children: form === void 0 ? [
            ...row.cells.map(({ content, className }, i) => /* @__PURE__ */ jsx10(Cell, { className, children: content }, i)),
            ...actions ? [
              /* @__PURE__ */ jsx10(Cell, { className: "bc:text-right", children: offered.has(row.id) ? /* @__PURE__ */ jsx10(MenuButton, { menus, target: objectOf(kind, row.id), place: row.id, label: row.menuLabel }) : null }, "actions")
            ] : []
          ] : /* @__PURE__ */ jsx10(Cell, { colSpan: span, className: WRAP, children: /* @__PURE__ */ jsx10(
            PlaceForm,
            {
              args: form,
              onClose: () => {
                menus.close(row.id, form);
              }
            }
          ) }) }, row.id);
        }) })
      ]
    }
  );
}
var BOARD = { board: true };
function offers(core, role, target) {
  return core.boardActions.on(target).some((action) => action.role === role);
}
function Advanced({ core, role, target }) {
  const key = JSON.stringify(target);
  const mount = useCallback5((el) => core.boardActions.mountAdvanced(el, role, JSON.parse(key)), [core, role, key]);
  return /* @__PURE__ */ jsx10(Mounted, { mount, className: "bc:min-w-0" });
}

// srcts/modules/edit-board/index.tsx
import { jsx as jsx11 } from "react/jsx-runtime";
var api = 1;
var edit_board_default = {
  ui: {
    render({ el, core }) {
      const root = createRoot2(el);
      flushSync2(() => {
        root.render(/* @__PURE__ */ jsx11(EditBoard, { core }));
      });
      return () => {
        root.unmount();
      };
    }
  }
};
export {
  api,
  edit_board_default as default
};

// srcts/modules/edit-board/index.tsx
import { flushSync as flushSync3 } from "react-dom";
import { createRoot } from "react-dom/client";

// srcts/modules/edit-board/board.tsx
import { useCallback as useCallback3, useLayoutEffect as useLayoutEffect3, useRef as useRef3, useState as useState8 } from "react";

// srcts/ui/combobox.tsx
import { useId, useLayoutEffect as useLayoutEffect2, useMemo, useRef as useRef2, useState as useState2 } from "react";

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

// srcts/ui/ui.tsx
import { useLayoutEffect, useRef, useState } from "react";
import { jsx } from "react/jsx-runtime";
var classes = {
  button: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50",
  primary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-accent bc:bg-bg-accent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-on-accent bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle",
  // The board's page, which draws no background of its own, so that the
  // page's, which the browser draws in the page's theme, shows to its edges.
  page: "bc:text-gray-900 bc:dark:text-gray-100",
  // What sits over the board, as a dialog or a menu does.
  surface: "bc:bg-white bc:text-gray-900 bc:dark:bg-gray-900 bc:dark:text-gray-100",
  card: "bc:bg-bg-subtle",
  heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold",
  list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0"
};
function NameField({
  label,
  name: name2,
  onSubmit,
  onDone
}) {
  const [text, setText] = useState(name2);
  const ref = useRef(null);
  const done = useRef(false);
  useLayoutEffect(() => {
    ref.current?.focus();
    ref.current?.select();
  }, []);
  function finish(submit) {
    if (done.current) return;
    done.current = true;
    const trimmed = text.trim();
    if (submit && trimmed !== "" && trimmed !== name2) onSubmit(trimmed);
    onDone();
  }
  return /* @__PURE__ */ jsx(
    "input",
    {
      ref,
      type: "text",
      "aria-label": label,
      value: text,
      spellCheck: false,
      className: `${styles.box} bc:min-w-40`,
      onChange: (event) => {
        setText(event.target.value);
      },
      onKeyDown: (event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) finish(true);
        else if (event.key === "Escape") finish(false);
      },
      onBlur: () => {
        finish(true);
      }
    }
  );
}

// srcts/ui/combobox.tsx
import { jsx as jsx2, jsxs } from "react/jsx-runtime";
function Combobox(props) {
  const { value: value2, onChange } = props;
  return /* @__PURE__ */ jsx2(Field, { ...props, picked: value2 === void 0 ? [] : [value2], pick: onChange, multiple: false });
}
function MultiCombobox(props) {
  const { values, onChange } = props;
  return /* @__PURE__ */ jsx2(
    Field,
    {
      ...props,
      picked: values,
      pick: (value2) => {
        onChange([...values, value2]);
      },
      unpick: (value2) => {
        onChange(values.filter((v) => v !== value2));
      },
      multiple: true
    }
  );
}
function Field({
  label,
  options,
  hint,
  error,
  placeholder,
  empty = "No option matches.",
  autofocus = false,
  picked,
  pick,
  unpick,
  multiple
}) {
  const id2 = useId();
  const listId = `${id2}-list`;
  const noteId = `${id2}-note`;
  const input2 = useRef2(null);
  const list = useRef2(null);
  const byValue = useMemo(() => new Map(options.map((option) => [option.value, option])), [options]);
  const single = multiple ? void 0 : byValue.get(picked[0] ?? "");
  const [open, setOpen] = useState2(false);
  const [query, setQuery] = useState2("");
  const [active, setActive] = useState2(void 0);
  const shown = useMemo(() => {
    const needle = query.trim().toLowerCase();
    const taken = new Set(multiple ? picked : []);
    return options.filter(
      (option) => !taken.has(option.value) && (needle === "" || [option.label, option.detail ?? "", option.group ?? "", option.badge ?? "", ...option.keywords ?? []].some(
        (text2) => text2.toLowerCase().includes(needle)
      ))
    );
  }, [options, query, picked, multiple]);
  const pickable = shown.filter((option) => option.disabled !== true);
  const marked = active !== void 0 && pickable.some((option) => option.value === active) ? active : pickable[0]?.value;
  useLayoutEffect2(() => {
    if (!open || marked === void 0) return;
    list.current?.ownerDocument.getElementById(optionId(listId, marked))?.scrollIntoView({ block: "nearest" });
  }, [open, marked, listId]);
  function show() {
    if (!open) {
      setOpen(true);
      setActive(multiple ? void 0 : single?.value);
    }
  }
  function close() {
    setOpen(false);
    setQuery("");
    setActive(void 0);
  }
  function choose(option) {
    if (option.disabled === true) return;
    pick(option.value);
    if (multiple) {
      setQuery("");
      setActive(void 0);
      input2.current?.focus();
    } else {
      close();
    }
  }
  function move(by) {
    if (!open) {
      show();
      return;
    }
    if (pickable.length === 0) return;
    const at = pickable.findIndex((option) => option.value === marked);
    const next = pickable[(at + by + pickable.length) % pickable.length];
    setActive(next?.value);
  }
  function onKeyDown(event) {
    if (event.nativeEvent.isComposing) return;
    if (event.key === "ArrowDown") {
      event.preventDefault();
      move(1);
    } else if (event.key === "ArrowUp") {
      event.preventDefault();
      move(-1);
    } else if (event.key === "Enter" && open) {
      event.preventDefault();
      const option = byValue.get(marked ?? "");
      if (option !== void 0) choose(option);
    } else if (event.key === "Escape" && open) {
      event.preventDefault();
      close();
    } else if (event.key === "Backspace" && multiple && query === "" && picked.length > 0) {
      const last = picked[picked.length - 1];
      if (last !== void 0) unpick?.(last);
    }
  }
  const groups = groupsOf(shown);
  const note = error ?? hint;
  const text = open ? query : multiple ? "" : single?.label ?? "";
  return /* @__PURE__ */ jsxs("div", { className: styles.field, children: [
    /* @__PURE__ */ jsx2("label", { htmlFor: id2, className: styles.label, children: label }),
    multiple && picked.length > 0 ? /* @__PURE__ */ jsx2("ul", { "aria-label": `${label}, picked`, className: "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0", children: picked.map((value2) => {
      const option = byValue.get(value2);
      const name2 = option?.label ?? value2;
      return /* @__PURE__ */ jsxs(
        "li",
        {
          className: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-full bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:text-xs",
          children: [
            /* @__PURE__ */ jsx2("span", { children: name2 }),
            /* @__PURE__ */ jsx2(
              "button",
              {
                type: "button",
                className: `${classes.quiet} bc:rounded-full bc:px-1`,
                "aria-label": `Remove ${name2}`,
                onClick: () => {
                  unpick?.(value2);
                  input2.current?.focus();
                },
                children: /* @__PURE__ */ jsx2("span", { "aria-hidden": "true", children: "\xD7" })
              }
            )
          ]
        },
        value2
      );
    }) }) : null,
    /* @__PURE__ */ jsxs("div", { className: "bc:relative bc:grid", children: [
      /* @__PURE__ */ jsx2(
        "input",
        {
          ref: input2,
          id: id2,
          type: "text",
          role: "combobox",
          autoComplete: "off",
          spellCheck: false,
          "aria-autocomplete": "list",
          "aria-expanded": open,
          "aria-controls": listId,
          "aria-activedescendant": open && marked !== void 0 ? optionId(listId, marked) : void 0,
          "aria-invalid": error === void 0 ? void 0 : true,
          "aria-describedby": note === void 0 ? void 0 : noteId,
          "data-autofocus": autofocus || void 0,
          className: `${styles.box} bc:pr-8`,
          value: text,
          placeholder,
          onChange: (event) => {
            setQuery(event.target.value);
            setActive(void 0);
            setOpen(true);
          },
          onClick: show,
          onKeyDown,
          onBlur: close
        }
      ),
      /* @__PURE__ */ jsx2(
        "button",
        {
          type: "button",
          tabIndex: -1,
          "aria-label": open ? `Close the options of ${label}` : `Show the options of ${label}`,
          className: "bc:absolute bc:inset-y-0 bc:right-0 bc:flex bc:w-8 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:text-text-muted",
          onMouseDown: (event) => {
            event.preventDefault();
          },
          onClick: () => {
            if (open) close();
            else {
              show();
              input2.current?.focus();
            }
          },
          children: /* @__PURE__ */ jsx2("span", { "aria-hidden": "true", children: "\u25BE" })
        }
      ),
      open ? /* @__PURE__ */ jsxs(
        "ul",
        {
          ref: list,
          id: listId,
          role: "listbox",
          "aria-label": label,
          "aria-multiselectable": multiple || void 0,
          className: `${classes.surface} bc:absolute bc:inset-x-0 bc:top-full bc:z-30 bc:m-0 bc:mt-1 bc:max-h-72 bc:list-none bc:overflow-y-auto bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:shadow-lg`,
          children: [
            shown.length === 0 ? /* @__PURE__ */ jsx2("li", { className: `${styles.hint} bc:px-2 bc:py-1`, children: empty }) : null,
            groups.map(
              ([group, members]) => group === void 0 ? members.map((option) => /* @__PURE__ */ jsx2(Option, { option, id: optionId(listId, option.value), marked: option.value === marked, picked: picked.includes(option.value), choose }, option.value)) : /* @__PURE__ */ jsxs("li", { role: "presentation", children: [
                /* @__PURE__ */ jsx2(
                  "div",
                  {
                    id: groupId(listId, group),
                    className: "bc:px-2 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted",
                    children: group
                  }
                ),
                /* @__PURE__ */ jsx2("ul", { role: "group", "aria-labelledby": groupId(listId, group), className: "bc:m-0 bc:list-none bc:p-0", children: members.map((option) => /* @__PURE__ */ jsx2(Option, { option, id: optionId(listId, option.value), marked: option.value === marked, picked: picked.includes(option.value), choose }, option.value)) })
              ] }, group)
            )
          ]
        }
      ) : null
    ] }),
    note === void 0 ? null : /* @__PURE__ */ jsx2("p", { id: noteId, className: error === void 0 ? styles.hint : styles.alert, children: note })
  ] });
}
function Option({
  option,
  id: id2,
  marked,
  picked,
  choose
}) {
  return /* @__PURE__ */ jsxs(
    "li",
    {
      id: id2,
      role: "option",
      "data-value": option.value,
      "aria-selected": picked,
      "aria-disabled": option.disabled === true || void 0,
      className: `bc:grid bc:cursor-pointer bc:gap-0.5 bc:rounded bc:px-2 bc:py-1 bc:text-sm ${marked ? "bc:bg-bg-subtle bc:outline bc:outline-1 bc:outline-border-accent" : ""} ${option.disabled === true ? "bc:cursor-default bc:opacity-50" : ""}`,
      onMouseDown: (event) => {
        event.preventDefault();
      },
      onClick: () => {
        choose(option);
      },
      children: [
        /* @__PURE__ */ jsxs("span", { className: "bc:flex bc:items-baseline bc:gap-2", children: [
          /* @__PURE__ */ jsxs("span", { className: `bc:min-w-0 bc:flex-1 ${picked ? "bc:font-semibold" : "bc:font-medium"}`, children: [
            picked ? /* @__PURE__ */ jsx2("span", { "aria-hidden": "true", children: "\u2713 " }) : null,
            option.label
          ] }),
          option.badge === void 0 ? null : /* @__PURE__ */ jsx2("span", { className: "bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:font-normal bc:text-text-muted", children: option.badge })
        ] }),
        option.detail === void 0 || option.detail === "" ? null : /* @__PURE__ */ jsx2("span", { className: "bc:text-xs bc:text-text-muted", children: option.detail })
      ]
    }
  );
}
function groupsOf(options) {
  const groups = /* @__PURE__ */ new Map();
  for (const option of options) groups.set(option.group, [...groups.get(option.group) ?? [], option]);
  const loose = groups.get(void 0);
  groups.delete(void 0);
  return [...loose === void 0 ? [] : [[void 0, loose]], ...groups];
}
function optionId(list, value2) {
  return `${list}-option-${safe(value2)}`;
}
function groupId(list, group) {
  return `${list}-group-${safe(group)}`;
}
function safe(text) {
  return text.replace(/[^A-Za-z0-9_-]/g, (c) => `_${c.charCodeAt(0).toString(16)}`);
}

// srcts/ui/fields.tsx
import { useId as useId2 } from "react";
import { jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function TextField({
  label,
  value: value2,
  hint,
  error,
  placeholder,
  identifier = false,
  onChange,
  onLeave
}) {
  const id2 = useId2();
  const noteId = `${id2}-note`;
  const note = error ?? hint;
  return /* @__PURE__ */ jsxs2("div", { className: styles.field, children: [
    /* @__PURE__ */ jsx3("label", { htmlFor: id2, className: styles.label, children: label }),
    /* @__PURE__ */ jsx3(
      "input",
      {
        id: id2,
        type: "text",
        className: styles.box,
        value: value2,
        placeholder,
        spellCheck: identifier ? false : void 0,
        autoCapitalize: identifier ? "none" : void 0,
        autoCorrect: identifier ? "off" : void 0,
        autoComplete: "off",
        "aria-invalid": error === void 0 ? void 0 : true,
        "aria-describedby": note === void 0 ? void 0 : noteId,
        onChange: (event) => {
          onChange(event.target.value);
        },
        onBlur: onLeave
      }
    ),
    note === void 0 ? null : /* @__PURE__ */ jsx3("p", { id: noteId, className: error === void 0 ? styles.hint : styles.alert, children: note })
  ] });
}

// srcts/ui/hooks.ts
import { useCallback, useSyncExternalStore } from "react";
function useBoard(core) {
  const subscribe = useStore(core);
  return useSyncExternalStore(subscribe, () => core.store.board());
}
function useStore(core) {
  return useCallback(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
}

// srcts/ui/table.tsx
import { useState as useState3 } from "react";
import { jsx as jsx4, jsxs as jsxs3 } from "react/jsx-runtime";
var PAGE_SIZE = 10;
function useTableView(rows, text, size = PAGE_SIZE) {
  const [query, setQueryState] = useState3("");
  const [page, setPage] = useState3(0);
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
  return /* @__PURE__ */ jsx4("search", { children: /* @__PURE__ */ jsxs3("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx4("span", { className: styles.label, children: label }),
    /* @__PURE__ */ jsx4(
      "input",
      {
        type: "search",
        spellCheck: false,
        autoComplete: "off",
        className: styles.box,
        value: view.query,
        onChange: (event) => {
          view.setQuery(event.target.value);
        }
      }
    )
  ] }) });
}
function Pager({ label, view }) {
  const { page, pages, found, first, rows } = view;
  if (pages < 2) return null;
  return /* @__PURE__ */ jsxs3("nav", { "aria-label": label, className: "bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-text-muted", children: [
    /* @__PURE__ */ jsxs3("span", { "aria-live": "polite", children: [
      "Rows ",
      first + 1,
      "\u2013",
      first + rows.length,
      " of ",
      found
    ] }),
    /* @__PURE__ */ jsxs3("span", { className: "bc:flex bc:gap-1", children: [
      /* @__PURE__ */ jsx4(
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
      /* @__PURE__ */ jsx4(
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

// srcts/modules/edit-board/blocks.tsx
import { useCallback as useCallback2, useState as useState5 } from "react";

// srcts/picker/picker.tsx
import { useMemo as useMemo2, useState as useState4 } from "react";
import { flushSync } from "react-dom";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}
function packageFromId(id2) {
  const at = id2.indexOf("-");
  if (at < 0) return null;
  const pkg = id2.slice(0, at);
  return isPackageName(pkg) ? pkg : null;
}
function isPackageName(name2) {
  return typeof name2 === "string" && /^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$/.test(name2);
}

// srcts/graph/graph.ts
function createGraph(board) {
  const blocks = Object.keys(board.blocks);
  const into = /* @__PURE__ */ new Map();
  const sources = /* @__PURE__ */ new Map();
  const targets = /* @__PURE__ */ new Map();
  for (const id2 of blocks) {
    into.set(id2, []);
    sources.set(id2, []);
    targets.set(id2, []);
  }
  for (const [id2, link] of Object.entries(board.links)) {
    const linksIn = into.get(link.to);
    const from = sources.get(link.to);
    const to = targets.get(link.from);
    if (linksIn === void 0 || from === void 0 || to === void 0) continue;
    linksIn.push(id2);
    from.push(link.from);
    to.push(link.to);
  }
  const upstream = once(sources);
  const downstream = once(targets);
  const order = topological(blocks, sources, targets);
  let cycle;
  return {
    order,
    linksInto: (id2) => into.get(id2) ?? [],
    upstream: (id2) => upstream.get(id2) ?? [],
    downstream: (id2) => downstream.get(id2) ?? [],
    withUpstream: (ids) => closure(ids, upstream),
    withDownstream: (ids) => closure(ids, downstream),
    cycle() {
      cycle ??= { found: findCycle(board, targets) };
      return cycle.found;
    }
  };
}
function once(ends) {
  return new Map([...ends].map(([id2, blocks]) => [id2, [...new Set(blocks)]]));
}
function closure(ids, steps) {
  const reached = /* @__PURE__ */ new Set();
  const todo = [...ids].filter((id2) => steps.has(id2));
  for (const id2 of todo) reached.add(id2);
  for (const id2 of todo) {
    for (const next of steps.get(id2) ?? []) {
      if (reached.has(next)) continue;
      reached.add(next);
      todo.push(next);
    }
  }
  return reached;
}
function topological(blocks, sources, targets) {
  const left = new Map(blocks.map((id2) => [id2, sources.get(id2)?.length ?? 0]));
  const order = blocks.filter((id2) => left.get(id2) === 0);
  for (const id2 of order) {
    for (const next of targets.get(id2) ?? []) {
      const count = (left.get(next) ?? 0) - 1;
      left.set(next, count);
      if (count === 0) order.push(next);
    }
  }
  const placed = new Set(order);
  return [...order, ...blocks.filter((id2) => !placed.has(id2))];
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

// srcts/store/ids.ts
function sentenceCase(id2) {
  const words = id2.replace(/([A-Z])/g, " $1").replace(/[_\-.]/g, " ").trim().split(" ");
  const [first = "", ...rest] = words;
  return [
    first.charAt(0).toUpperCase() + first.slice(1).toLowerCase(),
    ...rest.map((word) => word.toLowerCase())
  ].join(" ");
}

// srcts/store/check.ts
var HEX_COLOR = "^#[0-9a-fA-F]{6}$";
var hexColor = new RegExp(HEX_COLOR);

// srcts/store/open.ts
var COLLECTIONS = ["blocks", "links", "stacks", "options", "plugins"];
var PROPERTIES = ["id", "name", ...COLLECTIONS];

// srcts/commands/schemas.ts
var id = { type: "string", pattern: "\\D" };
var blockId = {
  ...id,
  description: "The id of a block, unique among the board's blocks."
};
var linkId = {
  ...id,
  description: "The id of a link, unique among the board's links."
};
var stackId = {
  ...id,
  description: "The id of a stack, unique among the board's stacks."
};
var registryId = {
  type: "string",
  minLength: 1,
  description: `A block class's id in the block registry, such as "blockr.core-head_block".`
};
var name = {
  type: "string",
  description: "A name that the user sees and may change, which nothing refers to."
};
var state = {
  type: "object",
  description: "Values of a block's state args, which the schema of its class types."
};

// srcts/commands/core.ts
var newBlock = {
  registry_id: { ...registryId, description: "The class of the new block." },
  id: {
    ...blockId,
    description: "The new block's id, which is minted where it is left out."
  },
  name: {
    ...name,
    description: "The new block's name, which its class gives where it is left out."
  },
  state: {
    ...state,
    description: "Values of the new block's state args, which the schema of its class types, in place of those its class starts a block with."
  }
};
function freeInputs(board, registry, to, except) {
  const block = own(board.blocks, to);
  if (block === void 0) return void 0;
  const inputs = own(registry, block.registry_id)?.inputs;
  if (inputs === void 0) return void 0;
  if (inputs.includes("...")) return [""];
  const fed = new Set(
    Object.entries(board.links).filter(([id2, link]) => id2 !== except && link.to === to).map(([, link]) => link.input)
  );
  return inputs.filter((input2) => !fed.has(input2));
}
function defaultBlockName(registryId2) {
  const pkg = packageFromId(registryId2);
  const name2 = pkg === null ? registryId2 : registryId2.slice(pkg.length + 1);
  return sentenceCase(name2.replace(/_block$/, ""));
}

// srcts/picker/picker.tsx
import { jsx as jsx5, jsxs as jsxs4 } from "react/jsx-runtime";
function BlockPicker({ core, mode, run, onAdded, autofocus = false }) {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const [picked, setPicked] = useState4(void 0);
  const [input2, setInput] = useState4(void 0);
  const [inputName, setInputName] = useState4("");
  const [id2, setId] = useState4("");
  const [name2, setName] = useState4("");
  const [errors, setErrors] = useState4({});
  const [busy, setBusy] = useState4(false);
  const entries = useMemo2(
    () => Object.values(registry).filter((entry2) => mode.kind !== "after" || entry2.inputs.length > 0),
    [registry, mode.kind]
  );
  const options = useMemo2(() => entries.map(classOption).sort(byGroupAndLabel), [entries]);
  const entry = picked === void 0 ? void 0 : own(registry, picked);
  const link = linkChoice(mode, board, registry, entry);
  const chosen = link.kind === "pick" ? input2 !== void 0 && link.inputs.includes(input2) ? input2 : link.inputs[0] : void 0;
  function check2() {
    return {
      ...entry === void 0 ? { registry: "Pick the class of the new block." } : {},
      ...optional("id", blockIdError(id2, board)),
      ...link.kind === "name" ? optional("input", inputNameError(inputName, link.taken)) : {}
    };
  }
  async function submit(event) {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check2();
    flushSync(() => {
      setErrors(found);
    });
    if (Object.keys(found).length > 0 || entry === void 0 || busy) {
      focusFirstInvalid(form);
      return;
    }
    const args = {
      registry_id: entry.id,
      ...filled("id", id2),
      ...filled("name", name2),
      ...chosen === void 0 ? {} : { input: chosen },
      ...link.kind === "name" ? filled("input", inputName) : {}
    };
    setBusy(true);
    const ran = mode.kind === "after" ? await run("blockr.core-append_block", { ...args, from: mode.from }) : mode.kind === "before" ? await run("blockr.core-prepend_block", { ...args, to: mode.to }) : await run("blockr.core-add_block", args);
    setBusy(false);
    if (!ran) return;
    setPicked(void 0);
    setInput(void 0);
    setInputName("");
    setId("");
    setName("");
    setErrors({});
    onAdded?.();
  }
  return /* @__PURE__ */ jsxs4(
    "form",
    {
      "aria-label": "Add block",
      noValidate: true,
      className: "bc:grid bc:gap-3",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        /* @__PURE__ */ jsx5(
          Combobox,
          {
            label: "Block class",
            autofocus,
            options,
            value: picked,
            placeholder: options.length === 0 ? "No block class to pick" : "Search by name, description or keyword",
            empty: "No block class matches.",
            error: errors.registry,
            hint: entry?.description === "" ? void 0 : entry?.description,
            onChange: (value2) => {
              setPicked(value2);
              if (mode.kind === "after") setInput(void 0);
              setErrors((now) => without2(now, "registry"));
            }
          }
        ),
        link.kind === "pick" ? /* @__PURE__ */ jsxs4("label", { className: styles.field, children: [
          /* @__PURE__ */ jsx5("span", { className: styles.label, children: link.label }),
          /* @__PURE__ */ jsx5(
            "select",
            {
              className: styles.box,
              value: chosen,
              onChange: (event) => {
                setInput(event.target.value);
              },
              children: link.inputs.map((option) => /* @__PURE__ */ jsx5("option", { value: option, children: option }, option))
            }
          )
        ] }) : null,
        link.kind === "name" ? /* @__PURE__ */ jsx5(
          TextField,
          {
            label: link.label,
            value: inputName,
            hint: "Optional. Leave it empty for an input without a name, which the block takes in the order of its links.",
            error: errors.input,
            onChange: (value2) => {
              setInputName(value2);
              setErrors((now) => without2(now, "input"));
            },
            onLeave: () => {
              setErrors((now) => ({ ...now, ...optional("input", inputNameError(inputName, link.taken)) }));
            }
          }
        ) : null,
        link.kind === "none" && mode.kind === "before" ? /* @__PURE__ */ jsx5("p", { className: styles.alert, children: link.why }) : null,
        /* @__PURE__ */ jsxs4("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx5(
            TextField,
            {
              label: "ID",
              value: id2,
              hint: "Optional. Made up where left empty.",
              error: errors.id,
              identifier: true,
              onChange: (value2) => {
                setId(value2);
                setErrors((now) => without2(now, "id"));
              },
              onLeave: () => {
                setErrors((now) => ({ ...now, ...optional("id", blockIdError(id2, board)) }));
              }
            }
          ),
          /* @__PURE__ */ jsx5(
            TextField,
            {
              label: "Name",
              value: name2,
              placeholder: entry === void 0 ? void 0 : defaultBlockName(entry.id),
              hint: "Optional. The class's name where left empty.",
              onChange: setName
            }
          )
        ] }),
        /* @__PURE__ */ jsx5("div", { children: /* @__PURE__ */ jsx5("button", { type: "submit", className: classes.primary, disabled: busy || link.kind === "none" && mode.kind === "before", children: "Add block" }) })
      ]
    }
  );
}
function linkChoice(mode, board, registry, entry) {
  if (mode.kind === "before") {
    const target2 = own(board.blocks, mode.to);
    const free = freeInputs(board, registry, mode.to) ?? [];
    const of = `of "${target2?.name ?? mode.to}"`;
    if (isVariadic(free)) return { kind: "name", label: `Input name ${of}`, taken: namesInto(board, mode.to) };
    if (free.length === 0) return { kind: "none", why: `Every input ${of} takes a link already.` };
    return free.length > 1 ? { kind: "pick", label: `Input ${of}`, inputs: free } : { kind: "none" };
  }
  if (mode.kind === "after" && entry !== void 0) {
    if (entry.inputs.includes("...")) return { kind: "name", label: `Input name of the new ${entry.name}`, taken: [] };
    if (entry.inputs.length > 1) return { kind: "pick", label: `Input of the new ${entry.name}`, inputs: entry.inputs };
  }
  return { kind: "none" };
}
function namesInto(board, to, except) {
  return Object.entries(board.links).filter(([id2, link]) => id2 !== except && link.to === to && link.input !== "").map(([, link]) => link.input);
}
function inputNameError(value2, taken) {
  const trimmed = value2.trim();
  if (trimmed === "") return void 0;
  if (taken.includes(trimmed)) return `Another link gives the input the name "${trimmed}" already.`;
  return void 0;
}
function isVariadic(free) {
  return free.length === 1 && free[0] === "";
}
function classOption(entry) {
  const pkg = packageFromId(entry.id);
  return {
    value: entry.id,
    label: entry.name,
    detail: entry.description,
    ...pkg === null ? {} : { badge: pkg },
    group: entry.category === "" ? UNCATEGORIZED : entry.category,
    keywords: [...entry.keywords, entry.id]
  };
}
var UNCATEGORIZED = "uncategorized";
function byGroupAndLabel(a, b) {
  const last = (option) => option.group?.toLowerCase() === UNCATEGORIZED ? 1 : 0;
  return last(a) - last(b) || (a.group ?? "").localeCompare(b.group ?? "") || a.label.localeCompare(b.label);
}
function filled(arg, value2) {
  const trimmed = value2.trim();
  return trimmed === "" ? {} : { [arg]: trimmed };
}
function optional(key, error) {
  return error === void 0 ? {} : { [key]: error };
}
function focusFirstInvalid(form) {
  form.querySelector('[aria-invalid="true"]')?.focus();
}
function blockIdError(value2, board) {
  return idError(value2, "block", (id2) => own(board.blocks, id2) !== void 0);
}
function idError(value2, what, taken) {
  const id2 = value2.trim();
  if (id2 === "") return void 0;
  if (/^\d+$/.test(id2)) return `An ID needs a character other than a digit, such as "${what}_${id2}".`;
  if (taken(id2)) return `The board holds a ${what} "${id2}" already.`;
  return void 0;
}
function without2(errors, key) {
  return Object.fromEntries(Object.entries(errors).filter(([field]) => field !== key));
}

// srcts/modules/edit-board/run.ts
function runCommand(core, id2, args) {
  return core.commands.run(id2, args).then(
    () => true,
    (error) => {
      core.notifications.notify({ message: errorMessage(error), severity: "error" });
      return false;
    }
  );
}

// srcts/modules/edit-board/blocks.tsx
import { Fragment, jsx as jsx6, jsxs as jsxs5 } from "react/jsx-runtime";
var ADD = { kind: "add" };
function BlocksSection({ core, board }) {
  const run = useCallback2((command, args) => runCommand(core, command, args), [core]);
  const registry = core.blockRegistry();
  const className = (block) => own(registry, block.registry_id)?.name ?? block.registry_id;
  const view = useTableView(Object.entries(board.blocks), ([id2, block]) => `${block.name} ${id2} ${className(block)}`);
  const count = Object.keys(board.blocks).length;
  return /* @__PURE__ */ jsxs5(Section, { title: "Blocks", count, children: [
    /* @__PURE__ */ jsx6(Part, { title: "Add a block", children: /* @__PURE__ */ jsx6(BlockPicker, { core, mode: ADD, run }) }),
    /* @__PURE__ */ jsx6(Part, { title: "Blocks on the board", children: count === 0 ? /* @__PURE__ */ jsx6("p", { className: styles.hint, children: "The board holds no blocks yet." }) : /* @__PURE__ */ jsxs5(Fragment, { children: [
      /* @__PURE__ */ jsx6(TableSearch, { label: "Search blocks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx6("p", { className: styles.hint, children: "No block matches the search." }) : /* @__PURE__ */ jsxs5("table", { className: table.table, children: [
        /* @__PURE__ */ jsx6("caption", { className: "bc:sr-only", children: "The board's blocks" }),
        /* @__PURE__ */ jsx6("thead", { children: /* @__PURE__ */ jsxs5("tr", { children: [
          /* @__PURE__ */ jsx6("th", { scope: "col", className: table.head, children: "Name" }),
          /* @__PURE__ */ jsx6("th", { scope: "col", className: table.head, children: "ID" }),
          /* @__PURE__ */ jsx6("th", { scope: "col", className: table.head, children: "Class" }),
          /* @__PURE__ */ jsx6("th", { scope: "col", className: `${table.head} bc:w-28`, children: /* @__PURE__ */ jsx6("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx6("tbody", { children: view.rows.map(([id2, block]) => /* @__PURE__ */ jsx6(BlockRow, { core, id: id2, block, className: className(block) }, id2)) })
      ] }),
      /* @__PURE__ */ jsx6(Pager, { label: "Pages of blocks", view })
    ] }) })
  ] });
}
function BlockRow({ core, id: id2, block, className }) {
  const [renaming, setRenaming] = useState5(false);
  return /* @__PURE__ */ jsxs5("tr", { "aria-label": block.name, className: table.row, children: [
    /* @__PURE__ */ jsx6("td", { className: table.cell, children: renaming ? /* @__PURE__ */ jsx6(
      NameField,
      {
        label: "Block name",
        name: block.name,
        onSubmit: (name2) => {
          void runCommand(core, "blockr.core-rename_block", { id: id2, name: name2 });
        },
        onDone: () => {
          setRenaming(false);
        }
      }
    ) : /* @__PURE__ */ jsx6("span", { className: "bc:font-medium bc:break-words", children: block.name }) }),
    /* @__PURE__ */ jsx6("td", { className: `${table.cell} bc:font-mono bc:text-xs bc:break-all`, children: id2 }),
    /* @__PURE__ */ jsx6("td", { className: `${table.cell} bc:text-text-muted`, children: className }),
    /* @__PURE__ */ jsxs5("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      renaming ? null : /* @__PURE__ */ jsx6(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Rename the block ${block.name}`,
          onClick: () => {
            setRenaming(true);
          },
          children: "Rename"
        }
      ),
      /* @__PURE__ */ jsx6(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Remove the block ${block.name}`,
          onClick: () => {
            void runCommand(core, "blockr.core-remove_block", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] })
  ] });
}

// srcts/modules/edit-board/links.tsx
import { useState as useState6 } from "react";
import { flushSync as flushSync2 } from "react-dom";
import { Fragment as Fragment2, jsx as jsx7, jsxs as jsxs6 } from "react/jsx-runtime";
function LinksSection({ core, board, locked: locked2 }) {
  const links = Object.entries(board.links);
  const view = useTableView(
    links,
    ([, link]) => [blockName(board, link.from), link.from, blockName(board, link.to), link.to, link.input].join(" ")
  );
  const variadic = variadicBlocks(board, core.blockRegistry());
  return /* @__PURE__ */ jsxs6(Section, { title: "Links", count: links.length, children: [
    locked2 ? null : /* @__PURE__ */ jsx7(Part, { title: "Add a link", children: /* @__PURE__ */ jsx7(LinkForm, { core, board, submitLabel: "Add link" }) }),
    /* @__PURE__ */ jsx7(Part, { title: "Links on the board", children: links.length === 0 ? /* @__PURE__ */ jsx7("p", { className: styles.hint, children: "No link joins two blocks yet." }) : /* @__PURE__ */ jsxs6(Fragment2, { children: [
      /* @__PURE__ */ jsx7(TableSearch, { label: "Search links", view }),
      view.found === 0 ? /* @__PURE__ */ jsx7("p", { className: styles.hint, children: "No link matches the search." }) : /* @__PURE__ */ jsxs6("table", { className: table.table, children: [
        /* @__PURE__ */ jsx7("caption", { className: "bc:sr-only", children: "The board's links" }),
        /* @__PURE__ */ jsx7("thead", { children: /* @__PURE__ */ jsxs6("tr", { children: [
          /* @__PURE__ */ jsx7("th", { scope: "col", className: table.head, children: "From" }),
          /* @__PURE__ */ jsx7("th", { scope: "col", className: table.head, children: "To" }),
          /* @__PURE__ */ jsx7("th", { scope: "col", className: table.head, children: "Input" }),
          locked2 ? null : /* @__PURE__ */ jsx7("th", { scope: "col", className: `${table.head} bc:w-24`, children: /* @__PURE__ */ jsx7("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx7("tbody", { children: view.rows.map(([id2, link]) => /* @__PURE__ */ jsx7(LinkRow, { core, board, id: id2, link, variadic: variadic.has(link.to), locked: locked2 }, id2)) })
      ] }),
      /* @__PURE__ */ jsx7(Pager, { label: "Pages of links", view })
    ] }) })
  ] });
}
function LinkRow({
  core,
  board,
  id: id2,
  link,
  variadic,
  locked: locked2
}) {
  const { editing, open, close, button } = useRowEditing();
  const label = `${blockName(board, link.from)} \u2192 ${blockName(board, link.to)}`;
  if (editing) {
    return /* @__PURE__ */ jsx7("tr", { "aria-label": label, className: table.row, children: /* @__PURE__ */ jsx7("td", { colSpan: 4, className: `${table.cell} bc:py-2`, children: /* @__PURE__ */ jsx7(
      LinkForm,
      {
        core,
        board,
        editing: { id: id2, link },
        submitLabel: "Save",
        onDone: close
      }
    ) }) });
  }
  return /* @__PURE__ */ jsxs6("tr", { "aria-label": label, className: table.row, children: [
    /* @__PURE__ */ jsx7("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx7(BlockLabel, { board, id: link.from }) }),
    /* @__PURE__ */ jsx7("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx7(BlockLabel, { board, id: link.to }) }),
    /* @__PURE__ */ jsx7("td", { className: `${table.cell} bc:break-words`, children: variadic ? /* @__PURE__ */ jsx7(VariadicInput, { board, id: id2, link }) : /* @__PURE__ */ jsx7("span", { className: "bc:font-mono bc:text-xs", children: link.input }) }),
    locked2 ? null : /* @__PURE__ */ jsxs6("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      /* @__PURE__ */ jsx7(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Edit the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`,
          ref: button,
          onClick: open,
          children: "Edit"
        }
      ),
      /* @__PURE__ */ jsx7(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Remove the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`,
          onClick: () => {
            void runCommand(core, "blockr.core-remove_link", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] })
  ] });
}
function BlockLabel({ board, id: id2 }) {
  return /* @__PURE__ */ jsxs6("span", { className: "bc:grid", children: [
    /* @__PURE__ */ jsx7("span", { children: blockName(board, id2) }),
    /* @__PURE__ */ jsx7("span", { className: "bc:font-mono bc:text-xs bc:text-text-muted bc:break-all", children: id2 })
  ] });
}
function VariadicInput({ board, id: id2, link }) {
  const place = createGraph(board).linksInto(link.to).indexOf(id2) + 1;
  return /* @__PURE__ */ jsxs6("span", { className: "bc:grid", children: [
    link.input === "" ? /* @__PURE__ */ jsx7("span", { className: "bc:text-text-muted", children: "No name" }) : /* @__PURE__ */ jsx7("span", { className: "bc:font-mono bc:text-xs", children: link.input }),
    /* @__PURE__ */ jsxs6("span", { className: "bc:text-xs bc:text-text-muted", children: [
      "Position ",
      place
    ] })
  ] });
}
function LinkForm({
  core,
  board,
  editing,
  submitLabel,
  onDone
}) {
  const registry = core.blockRegistry();
  const except = editing?.id;
  const [from, setFrom] = useState6(editing?.link.from);
  const [to, setTo] = useState6(editing?.link.to);
  const [input2, setInput] = useState6(editing?.link.input);
  const [name2, setName] = useState6(editing !== void 0 && isVariadicBlock(board, registry, editing.link.to) ? editing.link.input : "");
  const [place, setPlace] = useState6(void 0);
  const [errors, setErrors] = useState6({});
  const [busy, setBusy] = useState6(false);
  const others = withoutLink(board, except);
  const graph = createGraph(others);
  const held = (block) => block !== void 0 && own(board.blocks, block) !== void 0;
  const source = held(from) ? from : void 0;
  const target2 = held(to) ? to : void 0;
  const upstream = source === void 0 ? /* @__PURE__ */ new Set() : graph.withUpstream([source]);
  const downstream = target2 === void 0 ? /* @__PURE__ */ new Set() : graph.withDownstream([target2]);
  const fromOptions = Object.entries(board.blocks).filter(([id2]) => !downstream.has(id2)).map(([id2]) => blockOption(board, registry, id2));
  const toOptions = Object.entries(board.blocks).filter(([id2]) => !upstream.has(id2) && takesLink(others, registry, id2)).map(([id2]) => blockOption(board, registry, id2));
  const free = target2 === void 0 ? [] : freeInputs(others, registry, target2) ?? [];
  const variadic = isVariadic(free);
  const chosen = variadic ? void 0 : input2 !== void 0 && free.includes(input2) ? input2 : free[0];
  const taken = target2 === void 0 ? [] : namesInto(board, target2, except);
  const among = editing === void 0 || target2 === void 0 || !variadic ? void 0 : placeAmong(board, editing.id, target2);
  const position = among === void 0 ? void 0 : place !== void 0 && place <= among.others.length + 1 ? place : among.place;
  function check2() {
    return {
      ...source === void 0 ? { from: "Pick the block that the link comes from." } : {},
      ...target2 === void 0 ? { to: "Pick the block that the link goes to." } : {},
      ...variadic ? optional2("input", inputNameError(name2, taken)) : {}
    };
  }
  async function submit(event) {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check2();
    flushSync2(() => {
      setErrors(found);
    });
    if (Object.keys(found).length > 0 || source === void 0 || target2 === void 0 || busy) {
      focusFirstInvalid(form);
      return;
    }
    const linkInput = variadic ? name2.trim() : chosen;
    const args = { from: source, to: target2, ...linkInput === void 0 ? {} : { input: linkInput } };
    setBusy(true);
    let ran = editing === void 0 ? await runCommand(core, "blockr.core-add_link", args) : await runCommand(core, "blockr.core-change_link", { id: editing.id, ...args });
    if (ran && editing !== void 0 && among !== void 0 && position !== void 0 && position !== among.place) {
      const before = placeAmong(core.store.board(), editing.id, target2).others[position - 1];
      ran = await runCommand(core, "blockr.core-move_link", { id: editing.id, ...before === void 0 ? {} : { before } });
    }
    setBusy(false);
    if (!ran) return;
    if (editing === void 0) {
      setTo(void 0);
      setInput(void 0);
      setName("");
      setErrors({});
    }
    onDone?.();
  }
  return /* @__PURE__ */ jsxs6(
    "form",
    {
      "aria-label": editing === void 0 ? "Add link" : "Change link",
      noValidate: true,
      className: "bc:grid bc:gap-3",
      onSubmit: (event) => {
        void submit(event);
      },
      onKeyDown: (event) => {
        if (event.key === "Escape" && editing !== void 0 && !event.defaultPrevented) onDone?.();
      },
      children: [
        /* @__PURE__ */ jsxs6("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx7(
            Combobox,
            {
              label: "From",
              options: fromOptions,
              value: source,
              placeholder: "Search blocks",
              empty: "No block matches.",
              error: errors.from,
              onChange: (value2) => {
                setFrom(value2);
                setErrors((now) => without2(now, "from"));
              }
            }
          ),
          /* @__PURE__ */ jsx7(
            Combobox,
            {
              label: "To",
              options: toOptions,
              value: target2,
              placeholder: "Search blocks",
              empty: "No block that takes a link matches.",
              error: errors.to,
              hint: toOptions.length === 0 ? "No block takes a link from this block." : void 0,
              onChange: (value2) => {
                setTo(value2);
                setInput(void 0);
                setPlace(void 0);
                setErrors((now) => without2(without2(now, "to"), "input"));
              }
            }
          )
        ] }),
        target2 === void 0 ? null : variadic ? /* @__PURE__ */ jsxs6("div", { className: among === void 0 ? "bc:grid" : "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx7(
            TextField,
            {
              label: "Input name",
              value: name2,
              hint: `Optional. The block "${blockName(board, target2)}" takes any number of links, each by its name or, where it has none, by its position.`,
              error: errors.input,
              identifier: true,
              onChange: (value2) => {
                setName(value2);
                setErrors((now) => without2(now, "input"));
              },
              onLeave: () => {
                setErrors((now) => ({ ...now, ...optional2("input", inputNameError(name2, taken)) }));
              }
            }
          ),
          among === void 0 ? null : /* @__PURE__ */ jsxs6("label", { className: styles.field, children: [
            /* @__PURE__ */ jsx7("span", { className: styles.label, children: "Position" }),
            /* @__PURE__ */ jsx7(
              "select",
              {
                className: styles.box,
                value: position,
                disabled: among.others.length === 0,
                onChange: (event) => {
                  setPlace(Number(event.target.value));
                },
                children: Array.from({ length: among.others.length + 1 }, (_, i) => i + 1).map((n) => /* @__PURE__ */ jsx7("option", { value: n, children: n }, n))
              }
            )
          ] })
        ] }) : /* @__PURE__ */ jsxs6("label", { className: styles.field, children: [
          /* @__PURE__ */ jsx7("span", { className: styles.label, children: "Input" }),
          /* @__PURE__ */ jsx7(
            "select",
            {
              className: styles.box,
              value: chosen,
              disabled: free.length < 2,
              onChange: (event) => {
                setInput(event.target.value);
              },
              children: free.map((option) => /* @__PURE__ */ jsx7("option", { value: option, children: option }, option))
            }
          )
        ] }),
        editing === void 0 && Object.keys(board.blocks).length < 2 ? /* @__PURE__ */ jsx7("p", { className: styles.hint, children: "A link joins two blocks, so the board needs two blocks first." }) : null,
        /* @__PURE__ */ jsxs6("div", { className: "bc:flex bc:gap-2", children: [
          /* @__PURE__ */ jsx7("button", { type: "submit", className: classes.primary, disabled: busy, children: submitLabel }),
          editing === void 0 ? null : /* @__PURE__ */ jsx7(
            "button",
            {
              type: "button",
              className: classes.button,
              onClick: () => {
                onDone?.();
              },
              children: "Cancel"
            }
          )
        ] })
      ]
    }
  );
}
function blockOption(board, registry, id2) {
  const block = own(board.blocks, id2);
  const cls = block === void 0 ? void 0 : own(registry, block.registry_id)?.name ?? block.registry_id;
  return {
    value: id2,
    label: block?.name ?? id2,
    detail: cls === void 0 ? id2 : `${id2} \xB7 ${cls}`,
    keywords: [id2]
  };
}
function takesLink(board, registry, id2) {
  return (freeInputs(board, registry, id2)?.length ?? 0) > 0;
}
function isVariadicBlock(board, registry, id2) {
  const block = own(board.blocks, id2);
  return block !== void 0 && (own(registry, block.registry_id)?.inputs.includes("...") ?? false);
}
function variadicBlocks(board, registry) {
  return new Set(Object.keys(board.blocks).filter((id2) => isVariadicBlock(board, registry, id2)));
}
function placeAmong(board, id2, to) {
  const ids = Object.keys(board.links).filter((other) => other === id2 || own(board.links, other)?.to === to);
  return { others: ids.filter((other) => other !== id2), place: ids.indexOf(id2) + 1 };
}
function withoutLink(board, id2) {
  if (id2 === void 0 || !Object.hasOwn(board.links, id2)) return board;
  const links = Object.fromEntries(Object.entries(board.links).filter(([other]) => other !== id2));
  return { ...board, links };
}
function optional2(key, error) {
  return error === void 0 ? {} : { [key]: error };
}
function blockName(board, block) {
  return own(board.blocks, block)?.name ?? block;
}

// srcts/modules/edit-board/stacks.tsx
import { useId as useId3, useState as useState7 } from "react";
import { Fragment as Fragment3, jsx as jsx8, jsxs as jsxs7 } from "react/jsx-runtime";
var PALETTE = ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"];
function suggestedColor(stacks) {
  const used = new Set(stacks.map((stack) => stack.color?.toLowerCase()));
  return PALETTE.find((color2) => !used.has(color2)) ?? PALETTE[stacks.length % PALETTE.length] ?? PALETTE[0];
}
function suggestedName(stacks) {
  const names = new Set(stacks.map((stack) => stack.name));
  let n = 1;
  while (names.has(`Stack ${String(n)}`)) n++;
  return `Stack ${String(n)}`;
}
function StacksSection({ core, board, locked: locked2 }) {
  const stacks = Object.entries(board.stacks);
  const view = useTableView(stacks, ([id2, stack]) => [stack.name, id2, ...stack.blocks.map((block) => blockName(board, block))].join(" "));
  return /* @__PURE__ */ jsxs7(Section, { title: "Stacks", count: stacks.length, children: [
    locked2 ? null : /* @__PURE__ */ jsx8(Part, { title: "Add a stack", children: /* @__PURE__ */ jsx8(StackForm, { core, board, submitLabel: "Add stack" }) }),
    /* @__PURE__ */ jsx8(Part, { title: "Stacks on the board", children: stacks.length === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "The board has no stacks yet." }) : /* @__PURE__ */ jsxs7(Fragment3, { children: [
      /* @__PURE__ */ jsx8(TableSearch, { label: "Search stacks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "No stack matches the search." }) : /* @__PURE__ */ jsxs7("table", { className: table.table, children: [
        /* @__PURE__ */ jsx8("caption", { className: "bc:sr-only", children: "The board's stacks" }),
        /* @__PURE__ */ jsx8("thead", { children: /* @__PURE__ */ jsxs7("tr", { children: [
          /* @__PURE__ */ jsx8("th", { scope: "col", className: table.head, children: "Name" }),
          /* @__PURE__ */ jsx8("th", { scope: "col", className: table.head, children: "Blocks" }),
          locked2 ? null : /* @__PURE__ */ jsx8("th", { scope: "col", className: `${table.head} bc:w-24`, children: /* @__PURE__ */ jsx8("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx8("tbody", { children: view.rows.map(([id2, stack]) => /* @__PURE__ */ jsx8(StackRow, { core, board, id: id2, stack, locked: locked2 }, id2)) })
      ] }),
      /* @__PURE__ */ jsx8(Pager, { label: "Pages of stacks", view })
    ] }) })
  ] });
}
function StackRow({
  core,
  board,
  id: id2,
  stack,
  locked: locked2
}) {
  const { editing, open, close, button } = useRowEditing();
  if (editing) {
    return /* @__PURE__ */ jsx8("tr", { "aria-label": stack.name, className: table.row, children: /* @__PURE__ */ jsx8("td", { colSpan: 3, className: `${table.cell} bc:py-2`, children: /* @__PURE__ */ jsx8(
      StackForm,
      {
        core,
        board,
        editing: { id: id2, stack },
        submitLabel: "Save",
        onDone: close
      }
    ) }) });
  }
  return /* @__PURE__ */ jsxs7("tr", { "aria-label": stack.name, className: table.row, children: [
    /* @__PURE__ */ jsx8("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsxs7("span", { className: "bc:flex bc:items-center bc:gap-1.5", children: [
      /* @__PURE__ */ jsx8(Swatch, { color: stack.color }),
      /* @__PURE__ */ jsx8("span", { className: "bc:font-medium", children: stack.name })
    ] }) }),
    /* @__PURE__ */ jsx8("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx8(StackBlocks, { board, blocks: stack.blocks }) }),
    locked2 ? null : /* @__PURE__ */ jsxs7("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      /* @__PURE__ */ jsx8(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Edit the stack ${stack.name}`,
          ref: button,
          onClick: open,
          children: "Edit"
        }
      ),
      /* @__PURE__ */ jsx8(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Remove the stack ${stack.name}`,
          onClick: () => {
            void runCommand(core, "blockr.core-remove_stack", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] })
  ] });
}
var SHOWN_BLOCKS = 6;
function StackBlocks({ board, blocks }) {
  if (blocks.length === 0) return /* @__PURE__ */ jsx8("span", { className: "bc:text-text-muted", children: "No blocks" });
  const rest = blocks.length - SHOWN_BLOCKS;
  return /* @__PURE__ */ jsxs7("ul", { "aria-label": "Blocks", className: "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0", children: [
    blocks.slice(0, SHOWN_BLOCKS).map((block) => /* @__PURE__ */ jsxs7("li", { className: "bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs", children: [
      blockName(board, block),
      " ",
      /* @__PURE__ */ jsx8("span", { className: "bc:font-mono bc:text-text-muted", children: block })
    ] }, block)),
    rest > 0 ? /* @__PURE__ */ jsxs7("li", { className: "bc:px-1 bc:text-xs bc:text-text-muted", children: [
      "and ",
      rest,
      " more"
    ] }) : null
  ] });
}
function Swatch({ color: color2 }) {
  return /* @__PURE__ */ jsx8(
    "span",
    {
      "aria-hidden": "true",
      className: "bc:inline-block bc:size-3 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default",
      style: color2 === null ? void 0 : { backgroundColor: color2 }
    }
  );
}
function StackForm({
  core,
  board,
  editing,
  submitLabel,
  onDone
}) {
  const registry = core.blockRegistry();
  const stacks = Object.values(board.stacks);
  const [name2, setName] = useState7(editing?.stack.name ?? "");
  const [color2, setColor] = useState7(editing === void 0 ? suggestedColor(stacks) : editing.stack.color);
  const [blocks, setBlocks] = useState7(editing?.stack.blocks ?? []);
  const [busy, setBusy] = useState7(false);
  const fallbackName = editing === void 0 ? suggestedName(stacks) : editing.stack.name;
  const holder = /* @__PURE__ */ new Map();
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (id2 === editing?.id) continue;
    for (const block of stack.blocks) holder.set(block, stack.name);
  }
  const options = Object.keys(board.blocks).map((id2) => {
    const option = blockOption(board, registry, id2);
    const other = holder.get(id2);
    return other === void 0 ? { ...option, group: "In no stack" } : { ...option, group: "In another stack", detail: `${option.detail ?? id2} \xB7 leaves the stack ${other}` };
  }).sort((a, b) => a.group === b.group ? 0 : a.group === "In no stack" ? -1 : 1);
  const kept = blocks.filter((block) => Object.hasOwn(board.blocks, block));
  async function submit(event) {
    event.preventDefault();
    if (busy) return;
    const trimmed = name2.trim();
    setBusy(true);
    let ran;
    if (editing === void 0) {
      const args = { blocks: kept, color: color2, name: trimmed === "" ? fallbackName : trimmed };
      ran = await runCommand(core, "blockr.core-add_stack", args);
    } else {
      const { id: id2, stack } = editing;
      const renamed = trimmed !== "" && trimmed !== stack.name;
      const changed = {
        ...sameBlocks(kept, stack.blocks) ? {} : { blocks: kept },
        ...color2 === stack.color ? {} : { color: color2 }
      };
      ran = !renamed || await runCommand(core, "blockr.core-rename_stack", { id: id2, name: trimmed });
      if (ran && Object.keys(changed).length > 0) ran = await runCommand(core, "blockr.core-change_stack", { id: id2, ...changed });
    }
    setBusy(false);
    if (!ran) return;
    if (editing === void 0) {
      setName("");
      setColor(suggestedColor(Object.values(core.store.board().stacks)));
      setBlocks([]);
    }
    onDone?.();
  }
  return /* @__PURE__ */ jsxs7(
    "form",
    {
      "aria-label": editing === void 0 ? "Add stack" : "Change stack",
      noValidate: true,
      className: "bc:grid bc:gap-3",
      onSubmit: (event) => {
        void submit(event);
      },
      onKeyDown: (event) => {
        if (event.key === "Escape" && editing !== void 0 && !event.defaultPrevented) onDone?.();
      },
      children: [
        /* @__PURE__ */ jsxs7("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx8(
            TextField,
            {
              label: "Name",
              value: name2,
              placeholder: fallbackName,
              hint: editing === void 0 ? `Optional. "${fallbackName}" where left empty.` : void 0,
              onChange: setName
            }
          ),
          /* @__PURE__ */ jsx8(ColorField, { color: color2, onChange: setColor })
        ] }),
        /* @__PURE__ */ jsx8(
          MultiCombobox,
          {
            label: "Blocks",
            options,
            values: kept,
            placeholder: "Search blocks to add",
            empty: "No other block matches.",
            hint: "A block sits in one stack at most.",
            onChange: setBlocks
          }
        ),
        /* @__PURE__ */ jsxs7("div", { className: "bc:flex bc:gap-2", children: [
          /* @__PURE__ */ jsx8("button", { type: "submit", className: classes.primary, disabled: busy, children: submitLabel }),
          editing === void 0 ? null : /* @__PURE__ */ jsx8(
            "button",
            {
              type: "button",
              className: classes.button,
              onClick: () => {
                onDone?.();
              },
              children: "Cancel"
            }
          )
        ] })
      ]
    }
  );
}
function ColorField({ color: color2, onChange }) {
  const id2 = useId3();
  const [last, setLast] = useState7(color2 ?? PALETTE[0]);
  return /* @__PURE__ */ jsxs7("fieldset", { className: styles.fieldset, children: [
    /* @__PURE__ */ jsx8("legend", { className: styles.legend, children: "Colour" }),
    /* @__PURE__ */ jsxs7("div", { className: "bc:flex bc:items-center bc:gap-2", children: [
      /* @__PURE__ */ jsxs7("label", { className: styles.check, children: [
        /* @__PURE__ */ jsx8(
          "input",
          {
            type: "checkbox",
            className: styles.checkbox,
            checked: color2 !== null,
            onChange: (event) => {
              onChange(event.target.checked ? last : null);
            }
          }
        ),
        "A colour of its own"
      ] }),
      /* @__PURE__ */ jsx8(
        "input",
        {
          id: id2,
          type: "color",
          "aria-label": "The stack's colour",
          disabled: color2 === null,
          value: color2 ?? last,
          className: "bc:h-7 bc:w-10 bc:cursor-pointer bc:rounded bc:border bc:border-border-default bc:bg-transparent bc:p-0.5 bc:disabled:cursor-default bc:disabled:opacity-50",
          onChange: (event) => {
            setLast(event.target.value);
            onChange(event.target.value);
          }
        }
      )
    ] })
  ] });
}
function sameBlocks(a, b) {
  return a.length === b.length && a.every((block, i) => block === b[i]);
}

// srcts/modules/edit-board/board.tsx
import { jsx as jsx9, jsxs as jsxs8 } from "react/jsx-runtime";
function EditBoard({ core }) {
  const board = useBoard(core);
  const locked2 = core.lock.board;
  return /* @__PURE__ */ jsxs8("div", { className: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default", children: [
    locked2 ? null : /* @__PURE__ */ jsx9(BlocksSection, { core, board }),
    /* @__PURE__ */ jsx9(LinksSection, { core, board, locked: locked2 }),
    /* @__PURE__ */ jsx9(StacksSection, { core, board, locked: locked2 })
  ] });
}
function Section({ title, count, children }) {
  return /* @__PURE__ */ jsx9("section", { "aria-label": title, className: "bc:rounded-md bc:border bc:border-border-default", children: /* @__PURE__ */ jsxs8("details", { open: true, className: "bc:group", children: [
    /* @__PURE__ */ jsxs8("summary", { className: "bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:px-3 bc:py-2 bc:font-sans bc:text-sm bc:font-semibold", children: [
      /* @__PURE__ */ jsx9("h3", { className: "bc:m-0 bc:text-sm bc:font-semibold", children: title }),
      /* @__PURE__ */ jsx9("span", { className: "bc:rounded-full bc:border bc:border-border-default bc:px-1.5 bc:text-xs bc:font-normal bc:text-text-muted", children: count })
    ] }),
    /* @__PURE__ */ jsx9("div", { className: "bc:grid bc:gap-4 bc:border-t bc:border-border-default bc:p-3", children })
  ] }) });
}
function Part({ title, children }) {
  return /* @__PURE__ */ jsxs8("div", { role: "group", "aria-label": title, className: "bc:grid bc:gap-2", children: [
    /* @__PURE__ */ jsx9("h4", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: title }),
    children
  ] });
}
var table = {
  table: "bc:w-full bc:table-fixed bc:border-collapse bc:text-sm",
  head: "bc:border-b bc:border-border-default bc:px-1 bc:py-1 bc:text-left bc:text-xs bc:font-semibold bc:text-text-muted",
  row: "bc:border-b bc:border-border-default bc:align-top",
  cell: "bc:px-1 bc:py-1.5"
};
function useRowEditing() {
  const [editing, setEditing] = useState8(false);
  const closed = useRef3(false);
  const button = useRef3(null);
  useLayoutEffect3(() => {
    if (editing || !closed.current) return;
    closed.current = false;
    button.current?.focus();
  }, [editing]);
  const open = useCallback3(() => {
    setEditing(true);
  }, []);
  const close = useCallback3(() => {
    closed.current = true;
    setEditing(false);
  }, []);
  return { editing, open, close, button };
}

// srcts/modules/edit-board/index.tsx
import { jsx as jsx10 } from "react/jsx-runtime";
var api = 1;
var edit_board_default = {
  render({ el, core }) {
    const root = createRoot(el);
    flushSync3(() => {
      root.render(/* @__PURE__ */ jsx10(EditBoard, { core }));
    });
    return () => {
      root.unmount();
    };
  }
};
export {
  api,
  edit_board_default as default
};

// srcts/modules/edit-board/index.tsx
import { flushSync as flushSync3 } from "react-dom";
import { createRoot } from "react-dom/client";

// srcts/modules/edit-board/board.tsx
import { useCallback as useCallback2, useLayoutEffect, useRef as useRef2, useState as useState8 } from "react";
import { useBoard as useBoard2 } from "@blockr/core/react";

// srcts/modules/edit-board/blocks.tsx
import { useState as useState5 } from "react";

// srcts/picker/picker.tsx
import { useMemo, useState as useState4 } from "react";
import { flushSync } from "react-dom";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
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
    },
    path: (from, to) => findPath(from, to, downstream)
  };
}
function once(ends) {
  return new Map([...ends].map(([id2, blocks]) => [id2, [...new Set(blocks)]]));
}
function closure(ids, steps2) {
  const reached = /* @__PURE__ */ new Set();
  const todo = [...ids].filter((id2) => steps2.has(id2));
  for (const id2 of todo) reached.add(id2);
  for (const id2 of todo) {
    for (const next of steps2.get(id2) ?? []) {
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
function linkFeeding(board, registry, to, input2, except) {
  const block = own(board.blocks, to);
  const inputs = block === void 0 ? void 0 : own(registry, block.registry_id)?.inputs;
  if (inputs === void 0 || inputs.includes("...")) return void 0;
  return Object.entries(board.links).find(([id2, link]) => id2 !== except && link.to === to && link.input === input2)?.[0];
}
function defaultBlockName(registryId2) {
  const pkg = packageFromId(registryId2);
  const name2 = pkg === null ? registryId2 : registryId2.slice(pkg.length + 1);
  return sentenceCase(name2.replace(/_block$/, ""));
}

// srcts/picker/picker.tsx
import { ComboBox, ComboBoxItem, ComboBoxSection, Text, TextField as TextField3, useBoard } from "@blockr/core/react";

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
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle"
};

// srcts/ui/edits.ts
function runCommand(core, target2, args) {
  const running = typeof target2 === "string" ? core.commands.run(target2, args) : core.commands.run(target2);
  return running.then(
    () => true,
    () => false
  );
}

// srcts/ui/hooks.ts
import { useCallback, useSyncExternalStore } from "react";

// srcts/ui/search.ts
import { useState } from "react";
import { useFilter } from "./vendor-d953828e.js";
function useSearch(all, texts, none = "No options") {
  const { contains } = useFilter({ sensitivity: "base" });
  const [query, setQuery] = useState("");
  return {
    items: query === "" ? all : all.filter((item) => texts(item).some((text) => contains(text, query))),
    onInputChange: setQuery,
    onOpenChange: (isOpen, trigger) => {
      if (isOpen && trigger === "manual") setQuery("");
    },
    renderEmptyState: () => all.length === 0 ? none : "No matches"
  };
}

// srcts/ui/table.tsx
import { useState as useState3 } from "react";
import { TextField as TextField2 } from "@blockr/core/react";

// srcts/ui/ui.tsx
import { useRef, useState as useState2 } from "react";
import { TextField } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
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
function NameField({
  label,
  name: name2,
  onSubmit,
  onDone
}) {
  const [text, setText] = useState2(name2);
  const done = useRef(false);
  function finish(submit) {
    if (done.current) return;
    done.current = true;
    const trimmed = text.trim();
    if (submit && trimmed !== "" && trimmed !== name2) onSubmit(trimmed);
    onDone();
  }
  return /* @__PURE__ */ jsx(
    TextField,
    {
      "aria-label": label,
      value: text,
      autoFocus: true,
      spellCheck: "false",
      className: "bc:min-w-40",
      onChange: setText,
      onFocus: (event) => {
        event.target.select();
      },
      onKeyDown: (event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) finish(true);
        else if (event.key === "Escape") finish(false);
        else event.continuePropagation();
      },
      onBlur: () => {
        finish(true);
      }
    }
  );
}
var identifier = { spellCheck: "false", autoCapitalize: "none", autoCorrect: "off" };

// srcts/ui/table.tsx
import { jsx as jsx2, jsxs } from "react/jsx-runtime";
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
  return /* @__PURE__ */ jsx2("search", { children: /* @__PURE__ */ jsx2(TextField2, { label, type: "search", spellCheck: "false", autoComplete: "off", value: view.query, onChange: view.setQuery }) });
}
function Pager({ label, view }) {
  const { page, pages, found, first, rows } = view;
  if (pages < 2) return null;
  return /* @__PURE__ */ jsxs("nav", { "aria-label": label, className: "bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-text-muted", children: [
    /* @__PURE__ */ jsxs("span", { "aria-live": "polite", children: [
      "Rows ",
      first + 1,
      "\u2013",
      first + rows.length,
      " of ",
      found
    ] }),
    /* @__PURE__ */ jsxs("span", { className: "bc:flex bc:gap-1", children: [
      /* @__PURE__ */ jsx2(
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
      /* @__PURE__ */ jsx2(
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

// srcts/picker/picker.tsx
import { jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function BlockPicker({ core, mode, onAdded, autofocus = false }) {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const [picked, setPicked] = useState4(void 0);
  const [input2, setInput] = useState4(void 0);
  const [inputName, setInputName] = useState4("");
  const [id2, setId] = useState4("");
  const [name2, setName] = useState4("");
  const [errors, setErrors] = useState4({});
  const [busy, setBusy] = useState4(false);
  const entries = useMemo(
    () => Object.values(registry).filter((entry2) => mode.kind !== "after" || entry2.inputs.length > 0),
    [registry, mode.kind]
  );
  const options = useMemo(() => entries.map(classItem).sort(byGroupAndName), [entries]);
  const search = useSearch(
    options,
    (item) => [item.name, item.description, item.id, item.category, item.pkg ?? "", ...item.keywords],
    "No block class to pick."
  );
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
      ...filled("name", name2)
    };
    const into = {
      ...chosen === void 0 ? {} : { input: chosen },
      ...link.kind === "name" ? filled("input", inputName) : {}
    };
    setBusy(true);
    const ran = mode.kind === "add" ? await runCommand(core, "blockr.core-add_block", args) : await runCommand(core, addAndLink(core, mode, args, into));
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
  return /* @__PURE__ */ jsxs2(
    "form",
    {
      "aria-label": "Add block",
      noValidate: true,
      className: "bc:grid bc:gap-3",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        /* @__PURE__ */ jsx3(
          ComboBox,
          {
            label: "Block class",
            autoFocus: autofocus,
            ...search,
            items: byCategory(search.items),
            value: picked ?? null,
            placeholder: options.length === 0 ? "No block class to pick" : "Search by name, description or keyword",
            isInvalid: errors.registry !== void 0,
            errorMessage: errors.registry,
            description: entry?.description === "" ? void 0 : entry?.description,
            onChange: (key) => {
              setPicked(key === null ? void 0 : String(key));
              if (mode.kind === "after") setInput(void 0);
              setErrors((now) => without2(now, "registry"));
            },
            children: (category) => /* @__PURE__ */ jsx3(ComboBoxSection, { id: category.id, title: category.name, items: category.items, children: (item) => /* @__PURE__ */ jsxs2(ComboBoxItem, { id: item.id, textValue: item.name, children: [
              /* @__PURE__ */ jsx3(Text, { slot: "label", className: "bc:shrink-0 bc:truncate", children: item.name }),
              item.description === "" ? null : /* @__PURE__ */ jsx3(Text, { slot: "description", children: item.description }),
              item.pkg === null ? null : /* @__PURE__ */ jsx3("span", { className: BADGE, children: item.pkg })
            ] }) })
          }
        ),
        link.kind === "pick" ? /* @__PURE__ */ jsx3(
          ComboBox,
          {
            label: link.label,
            isRequired: true,
            defaultItems: link.inputs.map((input3) => ({ id: input3 })),
            value: chosen ?? null,
            onChange: (key) => {
              if (key !== null) setInput(String(key));
            },
            children: (item) => /* @__PURE__ */ jsx3(ComboBoxItem, { id: item.id, children: item.id })
          }
        ) : null,
        link.kind === "name" ? /* @__PURE__ */ jsx3(
          TextField3,
          {
            label: link.label,
            value: inputName,
            description: "Optional. Leave it empty for an input without a name, which the block takes in the order of its links.",
            errorMessage: errors.input,
            isInvalid: errors.input !== void 0,
            autoComplete: "off",
            onChange: (value2) => {
              setInputName(value2);
              setErrors((now) => without2(now, "input"));
            },
            onBlur: () => {
              setErrors((now) => ({ ...now, ...optional("input", inputNameError(inputName, link.taken)) }));
            }
          }
        ) : null,
        link.kind === "no_free_inputs" ? /* @__PURE__ */ jsx3("p", { className: styles.alert, children: link.why }) : null,
        /* @__PURE__ */ jsxs2("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx3(
            TextField3,
            {
              label: "ID",
              value: id2,
              description: "Optional. Made up where left empty.",
              errorMessage: errors.id,
              isInvalid: errors.id !== void 0,
              autoComplete: "off",
              ...identifier,
              onChange: (value2) => {
                setId(value2);
                setErrors((now) => without2(now, "id"));
              },
              onBlur: () => {
                setErrors((now) => ({ ...now, ...optional("id", blockIdError(id2, board)) }));
              }
            }
          ),
          /* @__PURE__ */ jsx3(
            TextField3,
            {
              label: "Name",
              value: name2,
              placeholder: entry === void 0 ? void 0 : defaultBlockName(entry.id),
              description: "Optional. The class's name where left empty.",
              autoComplete: "off",
              onChange: setName
            }
          )
        ] }),
        /* @__PURE__ */ jsx3("div", { children: /* @__PURE__ */ jsx3("button", { type: "submit", className: classes.primary, disabled: busy || link.kind === "no_free_inputs", children: "Add block" }) })
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
    if (free.length === 0) return { kind: "no_free_inputs", why: `Every input ${of} takes a link already.` };
    return free.length > 1 ? { kind: "pick", label: `Input ${of}`, inputs: free } : { kind: "none" };
  }
  if (mode.kind === "after" && entry !== void 0) {
    if (entry.inputs.includes("...")) return { kind: "name", label: `Input name of the new ${entry.name}`, taken: [] };
    if (entry.inputs.length > 1) return { kind: "pick", label: `Input of the new ${entry.name}`, inputs: entry.inputs };
  }
  return { kind: "none" };
}
function addAndLink(core, mode, args, into) {
  const block = typeof args.id === "string" ? args.id : core.mintId((id2) => Object.hasOwn(core.store.board().blocks, id2));
  const ends = mode.kind === "after" ? { from: mode.from, to: block } : { from: block, to: mode.to };
  return [
    { id: "blockr.core-add_block", args: { ...args, id: block } },
    { id: "blockr.core-add_link", args: { ...ends, ...into } }
  ];
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
function classItem(entry) {
  return {
    id: entry.id,
    name: entry.name,
    description: entry.description,
    pkg: packageFromId(entry.id),
    category: entry.category === "" ? UNCATEGORIZED : entry.category,
    keywords: entry.keywords
  };
}
var UNCATEGORIZED = "uncategorized";
function byGroupAndName(a, b) {
  const last = (item) => item.category.toLowerCase() === UNCATEGORIZED ? 1 : 0;
  return last(a) - last(b) || a.category.localeCompare(b.category) || a.name.localeCompare(b.name);
}
function byCategory(items) {
  const categories = /* @__PURE__ */ new Map();
  for (const item of items) {
    const members = categories.get(item.category) ?? [];
    members.push(item);
    categories.set(item.category, members);
  }
  return [...categories].map(([name2, members]) => ({ id: `category:${name2}`, name: name2, items: members }));
}
var BADGE = "bc:ml-auto bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:text-text-muted";
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

// srcts/modules/edit-board/blocks.tsx
import { Fragment, jsx as jsx4, jsxs as jsxs3 } from "react/jsx-runtime";
var ADD = { kind: "add" };
function BlocksSection({ core, board }) {
  const registry = core.blockRegistry();
  const className = (block) => own(registry, block.registry_id)?.name ?? block.registry_id;
  const view = useTableView(Object.entries(board.blocks), ([id2, block]) => `${block.name} ${id2} ${className(block)}`);
  const count = Object.keys(board.blocks).length;
  return /* @__PURE__ */ jsxs3(Section, { title: "Blocks", count, children: [
    /* @__PURE__ */ jsx4(Part, { title: "Add a block", children: /* @__PURE__ */ jsx4(BlockPicker, { core, mode: ADD }) }),
    /* @__PURE__ */ jsx4(Part, { title: "Blocks on the board", children: count === 0 ? /* @__PURE__ */ jsx4("p", { className: styles.hint, children: "The board holds no blocks yet." }) : /* @__PURE__ */ jsxs3(Fragment, { children: [
      /* @__PURE__ */ jsx4(TableSearch, { label: "Search blocks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx4("p", { className: styles.hint, children: "No block matches the search." }) : /* @__PURE__ */ jsxs3("table", { className: table.table, children: [
        /* @__PURE__ */ jsx4("caption", { className: "bc:sr-only", children: "The board's blocks" }),
        /* @__PURE__ */ jsx4("thead", { children: /* @__PURE__ */ jsxs3("tr", { children: [
          /* @__PURE__ */ jsx4("th", { scope: "col", className: table.head, children: "Name" }),
          /* @__PURE__ */ jsx4("th", { scope: "col", className: table.head, children: "ID" }),
          /* @__PURE__ */ jsx4("th", { scope: "col", className: table.head, children: "Class" }),
          /* @__PURE__ */ jsx4("th", { scope: "col", className: `${table.head} bc:w-28`, children: /* @__PURE__ */ jsx4("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx4("tbody", { children: view.rows.map(([id2, block]) => /* @__PURE__ */ jsx4(BlockRow, { core, id: id2, block, className: className(block) }, id2)) })
      ] }),
      /* @__PURE__ */ jsx4(Pager, { label: "Pages of blocks", view })
    ] }) })
  ] });
}
function BlockRow({ core, id: id2, block, className }) {
  const [renaming, setRenaming] = useState5(false);
  return /* @__PURE__ */ jsxs3("tr", { "aria-label": block.name, className: table.row, children: [
    /* @__PURE__ */ jsx4("td", { className: table.cell, children: renaming ? /* @__PURE__ */ jsx4(
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
    ) : /* @__PURE__ */ jsx4("span", { className: "bc:font-medium bc:break-words", children: block.name }) }),
    /* @__PURE__ */ jsx4("td", { className: `${table.cell} bc:font-mono bc:text-xs bc:break-all`, children: id2 }),
    /* @__PURE__ */ jsx4("td", { className: `${table.cell} bc:text-text-muted`, children: className }),
    /* @__PURE__ */ jsxs3("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      renaming ? null : /* @__PURE__ */ jsx4(
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
      /* @__PURE__ */ jsx4(
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
import { ComboBox as ComboBox2, ComboBoxItem as ComboBoxItem2, Text as Text2, TextField as TextField4 } from "@blockr/core/react";
import { Fragment as Fragment2, jsx as jsx5, jsxs as jsxs4 } from "react/jsx-runtime";
function LinksSection({ core, board, locked: locked2 }) {
  const links = Object.entries(board.links);
  const view = useTableView(
    links,
    ([, link]) => [blockName(board, link.from), link.from, blockName(board, link.to), link.to, link.input].join(" ")
  );
  const variadic = variadicBlocks(board, core.blockRegistry());
  return /* @__PURE__ */ jsxs4(Section, { title: "Links", count: links.length, children: [
    locked2 ? null : /* @__PURE__ */ jsx5(Part, { title: "Add a link", children: /* @__PURE__ */ jsx5(LinkForm, { core, board, submitLabel: "Add link" }) }),
    /* @__PURE__ */ jsx5(Part, { title: "Links on the board", children: links.length === 0 ? /* @__PURE__ */ jsx5("p", { className: styles.hint, children: "No link joins two blocks yet." }) : /* @__PURE__ */ jsxs4(Fragment2, { children: [
      /* @__PURE__ */ jsx5(TableSearch, { label: "Search links", view }),
      view.found === 0 ? /* @__PURE__ */ jsx5("p", { className: styles.hint, children: "No link matches the search." }) : /* @__PURE__ */ jsxs4("table", { className: table.table, children: [
        /* @__PURE__ */ jsx5("caption", { className: "bc:sr-only", children: "The board's links" }),
        /* @__PURE__ */ jsx5("thead", { children: /* @__PURE__ */ jsxs4("tr", { children: [
          /* @__PURE__ */ jsx5("th", { scope: "col", className: table.head, children: "From" }),
          /* @__PURE__ */ jsx5("th", { scope: "col", className: table.head, children: "To" }),
          /* @__PURE__ */ jsx5("th", { scope: "col", className: table.head, children: "Input" }),
          locked2 ? null : /* @__PURE__ */ jsx5("th", { scope: "col", className: `${table.head} bc:w-24`, children: /* @__PURE__ */ jsx5("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx5("tbody", { children: view.rows.map(([id2, link]) => /* @__PURE__ */ jsx5(LinkRow, { core, board, id: id2, link, variadic: variadic.has(link.to), locked: locked2 }, id2)) })
      ] }),
      /* @__PURE__ */ jsx5(Pager, { label: "Pages of links", view })
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
    return /* @__PURE__ */ jsx5("tr", { "aria-label": label, className: table.row, children: /* @__PURE__ */ jsx5("td", { colSpan: 4, className: `${table.cell} bc:py-2`, children: /* @__PURE__ */ jsx5(
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
  return /* @__PURE__ */ jsxs4("tr", { "aria-label": label, className: table.row, children: [
    /* @__PURE__ */ jsx5("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx5(BlockLabel, { board, id: link.from }) }),
    /* @__PURE__ */ jsx5("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx5(BlockLabel, { board, id: link.to }) }),
    /* @__PURE__ */ jsx5("td", { className: `${table.cell} bc:break-words`, children: variadic ? /* @__PURE__ */ jsx5(VariadicInput, { board, id: id2, link }) : /* @__PURE__ */ jsx5("span", { className: "bc:font-mono bc:text-xs", children: link.input }) }),
    locked2 ? null : /* @__PURE__ */ jsxs4("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      /* @__PURE__ */ jsx5(
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
      /* @__PURE__ */ jsx5(
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
  return /* @__PURE__ */ jsxs4("span", { className: "bc:grid", children: [
    /* @__PURE__ */ jsx5("span", { children: blockName(board, id2) }),
    /* @__PURE__ */ jsx5("span", { className: "bc:font-mono bc:text-xs bc:text-text-muted bc:break-all", children: id2 })
  ] });
}
function VariadicInput({ board, id: id2, link }) {
  const place = createGraph(board).linksInto(link.to).indexOf(id2) + 1;
  return /* @__PURE__ */ jsxs4("span", { className: "bc:grid", children: [
    link.input === "" ? /* @__PURE__ */ jsx5("span", { className: "bc:text-text-muted", children: "No name" }) : /* @__PURE__ */ jsx5("span", { className: "bc:font-mono bc:text-xs", children: link.input }),
    /* @__PURE__ */ jsxs4("span", { className: "bc:text-xs bc:text-text-muted", children: [
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
  const fromOptions = Object.entries(board.blocks).filter(([id2]) => !downstream.has(id2)).map(([id2]) => blockItem(board, registry, id2));
  const toOptions = Object.entries(board.blocks).filter(([id2]) => !upstream.has(id2) && takesLink(others, registry, id2)).map(([id2]) => blockItem(board, registry, id2));
  const fromSearch = useSearch(fromOptions, blockTexts);
  const toSearch = useSearch(toOptions, blockTexts);
  const free = target2 === void 0 ? [] : freeInputs(others, registry, target2) ?? [];
  const variadic = isVariadic(free);
  const offered = !variadic && editing !== void 0 && target2 === editing.link.to ? classInputs(board, registry, target2) : free;
  const chosen = variadic ? void 0 : input2 !== void 0 && offered.includes(input2) ? input2 : free[0];
  const hint = editing !== void 0 && target2 === editing.link.to && chosen !== void 0 ? swapHint(board, registry, editing, chosen) : void 0;
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
  return /* @__PURE__ */ jsxs4(
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
        /* @__PURE__ */ jsxs4("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx5(
            ComboBox2,
            {
              label: "From",
              ...fromSearch,
              value: source ?? null,
              placeholder: "Search blocks",
              isInvalid: errors.from !== void 0,
              errorMessage: errors.from,
              onChange: (key) => {
                setFrom(key === null ? void 0 : String(key));
                setErrors((now) => without2(now, "from"));
              },
              children: blockRow
            }
          ),
          /* @__PURE__ */ jsx5(
            ComboBox2,
            {
              label: "To",
              ...toSearch,
              value: target2 ?? null,
              placeholder: "Search blocks",
              isInvalid: errors.to !== void 0,
              errorMessage: errors.to,
              description: toOptions.length === 0 ? "No block takes a link from this block." : void 0,
              onChange: (key) => {
                setTo(key === null ? void 0 : String(key));
                setInput(void 0);
                setPlace(void 0);
                setErrors((now) => without2(without2(now, "to"), "input"));
              },
              children: blockRow
            }
          )
        ] }),
        target2 === void 0 ? null : variadic ? /* @__PURE__ */ jsxs4("div", { className: among === void 0 ? "bc:grid" : "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx5(
            TextField4,
            {
              label: "Input name",
              value: name2,
              description: `Optional. The block "${blockName(board, target2)}" takes any number of links, each by its name or, where it has none, by its position.`,
              errorMessage: errors.input,
              isInvalid: errors.input !== void 0,
              autoComplete: "off",
              ...identifier,
              onChange: (value2) => {
                setName(value2);
                setErrors((now) => without2(now, "input"));
              },
              onBlur: () => {
                setErrors((now) => ({ ...now, ...optional2("input", inputNameError(name2, taken)) }));
              }
            }
          ),
          among === void 0 ? null : /* @__PURE__ */ jsx5(
            ComboBox2,
            {
              label: "Position",
              isRequired: true,
              defaultItems: Array.from({ length: among.others.length + 1 }, (_, i) => ({ id: String(i + 1) })),
              value: position === void 0 ? null : String(position),
              isDisabled: among.others.length === 0,
              onChange: (key) => {
                if (key !== null) setPlace(Number(key));
              },
              children: (item) => /* @__PURE__ */ jsx5(ComboBoxItem2, { id: item.id, children: item.id })
            }
          )
        ] }) : /* @__PURE__ */ jsx5(
          ComboBox2,
          {
            label: "Input",
            isRequired: true,
            defaultItems: offered.map((id2) => ({ id: id2 })),
            value: chosen ?? null,
            isDisabled: offered.length < 2,
            description: hint,
            onChange: (key) => {
              if (key !== null) setInput(String(key));
            },
            children: (item) => /* @__PURE__ */ jsx5(ComboBoxItem2, { id: item.id, children: item.id })
          }
        ),
        editing === void 0 && Object.keys(board.blocks).length < 2 ? /* @__PURE__ */ jsx5("p", { className: styles.hint, children: "A link joins two blocks, so the board needs two blocks first." }) : null,
        /* @__PURE__ */ jsxs4("div", { className: "bc:flex bc:gap-2", children: [
          /* @__PURE__ */ jsx5("button", { type: "submit", className: classes.primary, disabled: busy, children: submitLabel }),
          editing === void 0 ? null : /* @__PURE__ */ jsx5(
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
function blockItem(board, registry, id2) {
  const block = own(board.blocks, id2);
  const cls = block === void 0 ? void 0 : own(registry, block.registry_id)?.name ?? block.registry_id;
  return { id: id2, name: block?.name ?? id2, detail: cls === void 0 ? id2 : `${id2} \xB7 ${cls}` };
}
function blockTexts(item) {
  return [item.name, item.detail, item.id];
}
function blockRow(item) {
  return /* @__PURE__ */ jsxs4(ComboBoxItem2, { id: item.id, textValue: item.name, children: [
    /* @__PURE__ */ jsx5(Text2, { slot: "label", className: "bc:shrink-0 bc:truncate", children: item.name }),
    /* @__PURE__ */ jsx5(Text2, { slot: "description", children: item.detail })
  ] });
}
function takesLink(board, registry, id2) {
  return (freeInputs(board, registry, id2)?.length ?? 0) > 0;
}
function classInputs(board, registry, id2) {
  const block = own(board.blocks, id2);
  return (block === void 0 ? void 0 : own(registry, block.registry_id)?.inputs) ?? [];
}
function swapHint(board, registry, editing, input2) {
  const other = linkFeeding(board, registry, editing.link.to, input2, editing.id);
  const from = other === void 0 ? void 0 : own(board.links, other)?.from;
  if (from === void 0) return void 0;
  return `The link from "${blockName(board, from)}" feeds "${input2}". Saving gives that link "${editing.link.input}", so the two links swap their inputs.`;
}
function isVariadicBlock(board, registry, id2) {
  return classInputs(board, registry, id2).includes("...");
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
import { useId, useState as useState7 } from "react";
import { ComboBox as ComboBox3, ComboBoxSection as ComboBoxSection2, TextField as TextField5 } from "@blockr/core/react";
import { Fragment as Fragment3, jsx as jsx6, jsxs as jsxs5 } from "react/jsx-runtime";
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
  return /* @__PURE__ */ jsxs5(Section, { title: "Stacks", count: stacks.length, children: [
    locked2 ? null : /* @__PURE__ */ jsx6(Part, { title: "Add a stack", children: /* @__PURE__ */ jsx6(StackForm, { core, board, submitLabel: "Add stack" }) }),
    /* @__PURE__ */ jsx6(Part, { title: "Stacks on the board", children: stacks.length === 0 ? /* @__PURE__ */ jsx6("p", { className: styles.hint, children: "The board has no stacks yet." }) : /* @__PURE__ */ jsxs5(Fragment3, { children: [
      /* @__PURE__ */ jsx6(TableSearch, { label: "Search stacks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx6("p", { className: styles.hint, children: "No stack matches the search." }) : /* @__PURE__ */ jsxs5("table", { className: table.table, children: [
        /* @__PURE__ */ jsx6("caption", { className: "bc:sr-only", children: "The board's stacks" }),
        /* @__PURE__ */ jsx6("thead", { children: /* @__PURE__ */ jsxs5("tr", { children: [
          /* @__PURE__ */ jsx6("th", { scope: "col", className: table.head, children: "Name" }),
          /* @__PURE__ */ jsx6("th", { scope: "col", className: table.head, children: "Blocks" }),
          locked2 ? null : /* @__PURE__ */ jsx6("th", { scope: "col", className: `${table.head} bc:w-24`, children: /* @__PURE__ */ jsx6("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx6("tbody", { children: view.rows.map(([id2, stack]) => /* @__PURE__ */ jsx6(StackRow, { core, board, id: id2, stack, locked: locked2 }, id2)) })
      ] }),
      /* @__PURE__ */ jsx6(Pager, { label: "Pages of stacks", view })
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
    return /* @__PURE__ */ jsx6("tr", { "aria-label": stack.name, className: table.row, children: /* @__PURE__ */ jsx6("td", { colSpan: 3, className: `${table.cell} bc:py-2`, children: /* @__PURE__ */ jsx6(
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
  return /* @__PURE__ */ jsxs5("tr", { "aria-label": stack.name, className: table.row, children: [
    /* @__PURE__ */ jsx6("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsxs5("span", { className: "bc:flex bc:items-center bc:gap-1.5", children: [
      /* @__PURE__ */ jsx6(Swatch, { color: stack.color }),
      /* @__PURE__ */ jsx6("span", { className: "bc:font-medium", children: stack.name })
    ] }) }),
    /* @__PURE__ */ jsx6("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx6(StackBlocks, { board, blocks: stack.blocks }) }),
    locked2 ? null : /* @__PURE__ */ jsxs5("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      /* @__PURE__ */ jsx6(
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
      /* @__PURE__ */ jsx6(
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
  if (blocks.length === 0) return /* @__PURE__ */ jsx6("span", { className: "bc:text-text-muted", children: "No blocks" });
  const rest = blocks.length - SHOWN_BLOCKS;
  return /* @__PURE__ */ jsxs5("ul", { "aria-label": "Blocks", className: "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0", children: [
    blocks.slice(0, SHOWN_BLOCKS).map((block) => /* @__PURE__ */ jsxs5("li", { className: "bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs", children: [
      blockName(board, block),
      " ",
      /* @__PURE__ */ jsx6("span", { className: "bc:font-mono bc:text-text-muted", children: block })
    ] }, block)),
    rest > 0 ? /* @__PURE__ */ jsxs5("li", { className: "bc:px-1 bc:text-xs bc:text-text-muted", children: [
      "and ",
      rest,
      " more"
    ] }) : null
  ] });
}
function Swatch({ color: color2 }) {
  return /* @__PURE__ */ jsx6(
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
    const item = blockItem(board, registry, id2);
    const other = holder.get(id2);
    return other === void 0 ? { ...item, free: true } : { ...item, free: false, detail: `${item.detail} \xB7 leaves the stack ${other}` };
  });
  const search = useSearch(options, blockTexts, "The board holds no block.");
  const sections = [
    { id: "free", title: "In no stack", items: search.items.filter((item) => item.free) },
    { id: "held", title: "In another stack", items: search.items.filter((item) => !item.free) }
  ].filter((section) => section.items.length > 0);
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
  return /* @__PURE__ */ jsxs5(
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
        /* @__PURE__ */ jsxs5("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx6(
            TextField5,
            {
              label: "Name",
              value: name2,
              placeholder: fallbackName,
              description: editing === void 0 ? `Optional. "${fallbackName}" where left empty.` : void 0,
              autoComplete: "off",
              onChange: setName
            }
          ),
          /* @__PURE__ */ jsx6(ColorField, { color: color2, onChange: setColor })
        ] }),
        /* @__PURE__ */ jsx6(
          ComboBox3,
          {
            selectionMode: "multiple",
            label: "Blocks",
            ...search,
            items: sections,
            value: kept,
            placeholder: "No blocks",
            description: "A block sits in one stack at most.",
            onChange: (keys) => {
              setBlocks(keys.map(String));
            },
            children: (section) => /* @__PURE__ */ jsx6(ComboBoxSection2, { id: section.id, title: section.title, items: section.items, children: blockRow })
          }
        ),
        /* @__PURE__ */ jsxs5("div", { className: "bc:flex bc:gap-2", children: [
          /* @__PURE__ */ jsx6("button", { type: "submit", className: classes.primary, disabled: busy, children: submitLabel }),
          editing === void 0 ? null : /* @__PURE__ */ jsx6(
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
  const id2 = useId();
  const [last, setLast] = useState7(color2 ?? PALETTE[0]);
  return /* @__PURE__ */ jsxs5("fieldset", { className: styles.fieldset, children: [
    /* @__PURE__ */ jsx6("legend", { className: styles.legend, children: "Colour" }),
    /* @__PURE__ */ jsxs5("div", { className: "bc:flex bc:items-center bc:gap-2", children: [
      /* @__PURE__ */ jsxs5("label", { className: styles.check, children: [
        /* @__PURE__ */ jsx6(
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
      /* @__PURE__ */ jsx6(
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
import { jsx as jsx7, jsxs as jsxs6 } from "react/jsx-runtime";
function EditBoard({ core }) {
  const board = useBoard2(core);
  const locked2 = core.lock.board;
  return /* @__PURE__ */ jsxs6("div", { className: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default", children: [
    locked2 ? null : /* @__PURE__ */ jsx7(BlocksSection, { core, board }),
    /* @__PURE__ */ jsx7(LinksSection, { core, board, locked: locked2 }),
    /* @__PURE__ */ jsx7(StacksSection, { core, board, locked: locked2 })
  ] });
}
function Section({ title, count, children }) {
  return /* @__PURE__ */ jsx7("section", { "aria-label": title, className: "bc:rounded-md bc:border bc:border-border-default", children: /* @__PURE__ */ jsxs6("details", { open: true, className: "bc:group", children: [
    /* @__PURE__ */ jsxs6("summary", { className: "bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:px-3 bc:py-2 bc:font-sans bc:text-sm bc:font-semibold", children: [
      /* @__PURE__ */ jsx7("h3", { className: "bc:m-0 bc:text-sm bc:font-semibold", children: title }),
      /* @__PURE__ */ jsx7("span", { className: "bc:rounded-full bc:border bc:border-border-default bc:px-1.5 bc:text-xs bc:font-normal bc:text-text-muted", children: count })
    ] }),
    /* @__PURE__ */ jsx7("div", { className: "bc:grid bc:gap-4 bc:border-t bc:border-border-default bc:p-3", children })
  ] }) });
}
function Part({ title, children }) {
  return /* @__PURE__ */ jsxs6("div", { role: "group", "aria-label": title, className: "bc:grid bc:gap-2", children: [
    /* @__PURE__ */ jsx7("h4", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: title }),
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
  const closed = useRef2(false);
  const button = useRef2(null);
  useLayoutEffect(() => {
    if (editing || !closed.current) return;
    closed.current = false;
    button.current?.focus();
  }, [editing]);
  const open = useCallback2(() => {
    setEditing(true);
  }, []);
  const close = useCallback2(() => {
    closed.current = true;
    setEditing(false);
  }, []);
  return { editing, open, close, button };
}

// srcts/modules/edit-board/index.tsx
import { jsx as jsx8 } from "react/jsx-runtime";
var api = 1;
var edit_board_default = {
  ui: {
    render({ el, core }) {
      const root = createRoot(el);
      flushSync3(() => {
        root.render(/* @__PURE__ */ jsx8(EditBoard, { core }));
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

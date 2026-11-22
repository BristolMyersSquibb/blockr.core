// srcts/modules/edit-board/index.tsx
import { flushSync as flushSync3 } from "react-dom";
import { createRoot as createRoot2 } from "react-dom/client";

// srcts/modules/edit-board/board.tsx
import { useCallback as useCallback3, useEffect, useLayoutEffect as useLayoutEffect2, useRef as useRef3, useState as useState9 } from "react";

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
import { useBoard as useBoard2 } from "@blockr/core/react";

// srcts/ui/blocks.tsx
import { ComboBoxItem, Text } from "@blockr/core/react";

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
function useEdits(core, shows = true) {
  const [refusal2, setRefusal] = useState3(null);
  return useMemo(() => {
    const opts = { notifier: () => void 0 };
    function attempt(title, start) {
      if (!shows) {
        return start({}).then(
          () => true,
          () => false
        );
      }
      setRefusal(null);
      return start(opts).then(
        () => true,
        (error) => {
          setRefusal({ title, reasons: reasons(error) });
          return false;
        }
      );
    }
    function run(title, target2, args) {
      return attempt(
        title,
        (given) => typeof target2 === "string" ? core.commands.run(target2, args, "user", given) : core.commands.run(target2, "user", given)
      );
    }
    return {
      refusal: refusal2,
      run,
      attempt,
      clear: () => {
        setRefusal(null);
      }
    };
  }, [core, shows, refusal2]);
}
function reasons(error) {
  const failures = error instanceof Error && "failures" in error ? error.failures : void 0;
  return isSentences(failures) ? failures : [errorMessage(error)];
}
function isSentences(x) {
  return Array.isArray(x) && x.length > 0 && x.every((item) => typeof item === "string");
}

// srcts/ui/hooks.ts
import { useCallback as useCallback2, useSyncExternalStore } from "react";

// srcts/ui/search.ts
import { useState as useState4 } from "react";
import { useFilter } from "./vendor-48cbe59f.js";
function useSearch(all, texts, none = "No options") {
  const { contains } = useFilter({ sensitivity: "base" });
  const [query, setQuery] = useState4("");
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
import { useState as useState6 } from "react";
import { SearchField } from "@blockr/core/react";

// srcts/ui/ui.tsx
import { useRef as useRef2, useState as useState5 } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { TextField } from "@blockr/core/react";

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
function NameField({
  label,
  name: name2,
  onSubmit,
  onDone,
  fill = false
}) {
  const [text, setText] = useState5(name2);
  const done = useRef2(false);
  function finish(submit) {
    if (done.current) return;
    done.current = true;
    const trimmed = text.trim();
    if (submit && trimmed !== "" && trimmed !== name2) onSubmit(trimmed);
    onDone();
  }
  return /* @__PURE__ */ jsx4(
    TextField,
    {
      "aria-label": label,
      value: text,
      autoFocus: true,
      spellCheck: "false",
      className: fill ? "bc:w-full" : "bc:min-w-40",
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
function Refusal({ refusal: refusal2 }) {
  if (refusal2 === null) return null;
  const mark = MARKS.error;
  const { title, reasons: reasons2 } = refusal2;
  return /* @__PURE__ */ jsxs3(
    "div",
    {
      role: "alert",
      className: `bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`,
      children: [
        /* @__PURE__ */ jsx4("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx4(SeverityIcon, { severity: "error" }) }),
        /* @__PURE__ */ jsxs3("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-1 bc:text-sm bc:break-words", children: [
          /* @__PURE__ */ jsx4("p", { className: `bc:m-0 bc:font-semibold ${mark.text}`, children: title }),
          reasons2.length === 1 ? /* @__PURE__ */ jsx4("p", { className: "bc:m-0 bc:whitespace-pre-wrap", children: reasons2[0] }) : /* @__PURE__ */ jsx4("ul", { className: "bc:m-0 bc:list-disc bc:pl-5", children: reasons2.map((reason, i) => /* @__PURE__ */ jsx4("li", { className: "bc:whitespace-pre-wrap", children: reason }, i)) })
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

// srcts/modules/edit-board/blocks.tsx
import { useState as useState8 } from "react";

// srcts/picker/picker.tsx
import { useMemo as useMemo2, useState as useState7 } from "react";
import { flushSync as flushSync2 } from "react-dom";

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
function defaultBlockName(registryId2) {
  const pkg = packageFromId(registryId2);
  const name2 = pkg === null ? registryId2 : registryId2.slice(pkg.length + 1);
  return sentenceCase(name2.replace(/_block$/, ""));
}

// srcts/picker/picker.tsx
import { ComboBox, ComboBoxItem as ComboBoxItem2, ComboBoxSection, Form, Select, SelectItem, Text as Text2, TextField as TextField2, useBoard } from "@blockr/core/react";
import { jsx as jsx6, jsxs as jsxs5 } from "react/jsx-runtime";
function BlockPicker({ core, mode, onAdded, autofocus = false, showsRefusal = false }) {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const edits = useEdits(core, showsRefusal);
  const [picked, setPicked] = useState7(void 0);
  const [input2, setInput] = useState7(void 0);
  const [inputName, setInputName] = useState7("");
  const [id2, setId] = useState7("");
  const [name2, setName] = useState7("");
  const [errors, setErrors] = useState7({});
  const [busy, setBusy] = useState7(false);
  const entries = useMemo2(
    () => Object.values(registry).filter((entry2) => mode.kind !== "after" || entry2.inputs.length > 0),
    [registry, mode.kind]
  );
  const options = useMemo2(() => entries.map(classItem).sort(byGroupAndName), [entries]);
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
    flushSync2(() => {
      setErrors(found);
      edits.clear();
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
    const title = "Could not add the block.";
    const ran = mode.kind === "add" ? await edits.run(title, "blockr.core-add_block", args) : await edits.run(title, addAndLink(core, mode, args, into));
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
  return /* @__PURE__ */ jsxs5(
    Form,
    {
      "aria-label": "Add block",
      validationBehavior: "aria",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        /* @__PURE__ */ jsx6(
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
            children: (category) => /* @__PURE__ */ jsx6(ComboBoxSection, { id: category.id, title: category.name, items: category.items, children: (item) => /* @__PURE__ */ jsxs5(ComboBoxItem2, { id: item.id, textValue: item.name, children: [
              /* @__PURE__ */ jsx6(Text2, { slot: "label", className: "bc:shrink-0 bc:truncate", children: item.name }),
              item.description === "" ? null : /* @__PURE__ */ jsx6(Text2, { slot: "description", children: item.description }),
              item.pkg === null ? null : /* @__PURE__ */ jsx6("span", { className: BADGE, children: item.pkg })
            ] }) })
          }
        ),
        link.kind === "pick" ? /* @__PURE__ */ jsx6(
          Select,
          {
            label: link.label,
            isRequired: true,
            items: link.inputs.map((input3) => ({ id: input3 })),
            value: chosen ?? null,
            onChange: (key) => {
              if (key !== null) setInput(String(key));
            },
            children: (item) => /* @__PURE__ */ jsx6(SelectItem, { id: item.id, children: item.id })
          }
        ) : null,
        link.kind === "name" ? /* @__PURE__ */ jsx6(
          TextField2,
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
        link.kind === "no_free_inputs" ? /* @__PURE__ */ jsx6("p", { className: styles.alert, children: link.why }) : null,
        /* @__PURE__ */ jsxs5("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx6(
            TextField2,
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
          /* @__PURE__ */ jsx6(
            TextField2,
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
        /* @__PURE__ */ jsx6("div", { children: /* @__PURE__ */ jsx6("button", { type: "submit", className: classes.primary, disabled: busy || link.kind === "no_free_inputs", children: "Add block" }) }),
        /* @__PURE__ */ jsx6(Refusal, { refusal: edits.refusal })
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
import { Fragment as Fragment2, jsx as jsx7, jsxs as jsxs6 } from "react/jsx-runtime";
var ADD = { kind: "add" };
function BlocksSection({ core, board }) {
  const registry = core.blockRegistry();
  const className = (block) => own(registry, block.registry_id)?.name ?? block.registry_id;
  const view = useTableView(Object.entries(board.blocks), ([id2, block]) => `${block.name} ${id2} ${className(block)}`);
  const count = Object.keys(board.blocks).length;
  const edits = useEdits(core);
  return /* @__PURE__ */ jsxs6(Section, { title: "Blocks", count, children: [
    /* @__PURE__ */ jsx7(Part, { title: "Add a block", children: /* @__PURE__ */ jsx7(BlockPicker, { core, mode: ADD, showsRefusal: true }) }),
    /* @__PURE__ */ jsx7(Part, { title: "Blocks on the board", children: count === 0 ? /* @__PURE__ */ jsx7("p", { className: styles.hint, children: "The board holds no blocks yet." }) : /* @__PURE__ */ jsxs6(Fragment2, { children: [
      /* @__PURE__ */ jsx7(TableSearch, { label: "Search blocks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx7("p", { className: styles.hint, children: "No block matches the search." }) : /* @__PURE__ */ jsxs6("table", { className: table.table, children: [
        /* @__PURE__ */ jsx7("caption", { className: "bc:sr-only", children: "The board's blocks" }),
        /* @__PURE__ */ jsx7("thead", { children: /* @__PURE__ */ jsxs6("tr", { children: [
          /* @__PURE__ */ jsx7("th", { scope: "col", className: table.head, children: "Name" }),
          /* @__PURE__ */ jsx7("th", { scope: "col", className: table.head, children: "ID" }),
          /* @__PURE__ */ jsx7("th", { scope: "col", className: table.head, children: "Class" }),
          /* @__PURE__ */ jsx7("th", { scope: "col", className: `${table.head} bc:w-28`, children: /* @__PURE__ */ jsx7("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx7("tbody", { children: view.rows.map(([id2, block]) => /* @__PURE__ */ jsx7(BlockRow, { edits, id: id2, block, className: className(block) }, id2)) })
      ] }),
      /* @__PURE__ */ jsx7(Refusal, { refusal: edits.refusal }),
      /* @__PURE__ */ jsx7(Pager, { label: "Pages of blocks", view })
    ] }) })
  ] });
}
function BlockRow({ edits, id: id2, block, className }) {
  const [renaming, setRenaming] = useState8(false);
  return /* @__PURE__ */ jsxs6("tr", { "aria-label": block.name, className: table.row, children: [
    /* @__PURE__ */ jsx7("td", { className: table.cell, children: renaming ? /* @__PURE__ */ jsx7(
      NameField,
      {
        label: "Block name",
        name: block.name,
        fill: true,
        onSubmit: (name2) => {
          void edits.run(`Could not rename the block "${block.name}".`, "blockr.core-rename_block", { id: id2, name: name2 });
        },
        onDone: () => {
          setRenaming(false);
        }
      }
    ) : /* @__PURE__ */ jsx7("span", { className: "bc:font-medium bc:break-words", children: block.name }) }),
    /* @__PURE__ */ jsx7("td", { className: `${table.cell} bc:font-mono bc:text-xs bc:break-all`, children: id2 }),
    /* @__PURE__ */ jsx7("td", { className: `${table.cell} bc:text-text-muted`, children: className }),
    /* @__PURE__ */ jsxs6("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      renaming ? null : /* @__PURE__ */ jsx7(
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
      /* @__PURE__ */ jsx7(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Remove the block ${block.name}`,
          onClick: () => {
            void edits.run(`Could not remove the block "${block.name}".`, "blockr.core-remove_block", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] })
  ] });
}

// srcts/modules/edit-board/links.tsx
import { Fragment as Fragment3, jsx as jsx8, jsxs as jsxs7 } from "react/jsx-runtime";
var CREATE_LINK = "blockr.core-add_link_action";
var EDIT_LINK = "blockr.core-edit_link_action";
var BOARD = { board: true };
function LinksSection({ core, board, locked: locked2 }) {
  const links = Object.entries(board.links);
  const view = useTableView(
    links,
    ([, link]) => [blockName(board, link.from), link.from, blockName(board, link.to), link.to, link.input].join(" ")
  );
  const variadic = variadicBlocks(board, core.blockRegistry());
  const edits = useEdits(core);
  return /* @__PURE__ */ jsxs7(Section, { title: "Links", count: links.length, children: [
    locked2 ? null : /* @__PURE__ */ jsx8(Part, { title: "Add a link", children: offers(core, CREATE_LINK, BOARD) ? /* @__PURE__ */ jsx8(Advanced, { core, id: CREATE_LINK, target: BOARD }) : /* @__PURE__ */ jsx8("p", { className: styles.hint, children: Object.keys(board.blocks).length < 2 ? "A link joins two blocks, so the board needs two blocks first." : "No new link can join two blocks of the board." }) }),
    /* @__PURE__ */ jsx8(Part, { title: "Links on the board", children: links.length === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "No link joins two blocks yet." }) : /* @__PURE__ */ jsxs7(Fragment3, { children: [
      /* @__PURE__ */ jsx8(TableSearch, { label: "Search links", view }),
      view.found === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "No link matches the search." }) : /* @__PURE__ */ jsxs7("table", { className: table.table, children: [
        /* @__PURE__ */ jsx8("caption", { className: "bc:sr-only", children: "The board's links" }),
        /* @__PURE__ */ jsx8("thead", { children: /* @__PURE__ */ jsxs7("tr", { children: [
          /* @__PURE__ */ jsx8("th", { scope: "col", className: table.head, children: "From" }),
          /* @__PURE__ */ jsx8("th", { scope: "col", className: table.head, children: "To" }),
          /* @__PURE__ */ jsx8("th", { scope: "col", className: table.head, children: "Input" }),
          locked2 ? null : /* @__PURE__ */ jsx8("th", { scope: "col", className: `${table.head} bc:w-24`, children: /* @__PURE__ */ jsx8("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx8("tbody", { children: view.rows.map(([id2, link]) => /* @__PURE__ */ jsx8(
          LinkRow,
          {
            core,
            edits,
            board,
            id: id2,
            link,
            variadic: variadic.has(link.to),
            locked: locked2
          },
          id2
        )) })
      ] }),
      /* @__PURE__ */ jsx8(Refusal, { refusal: edits.refusal }),
      /* @__PURE__ */ jsx8(Pager, { label: "Pages of links", view })
    ] }) })
  ] });
}
function LinkRow({
  core,
  edits,
  board,
  id: id2,
  link,
  variadic,
  locked: locked2
}) {
  const { editing, open, close, button } = useRowEditing();
  const label = `${blockName(board, link.from)} \u2192 ${blockName(board, link.to)}`;
  if (editing) {
    return /* @__PURE__ */ jsx8(RowAdvanced, { core, id: EDIT_LINK, target: { link: id2 }, label, columns: 4, onDone: close });
  }
  return /* @__PURE__ */ jsxs7("tr", { "aria-label": label, className: table.row, children: [
    /* @__PURE__ */ jsx8("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx8(BlockLabel, { board, id: link.from }) }),
    /* @__PURE__ */ jsx8("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx8(BlockLabel, { board, id: link.to }) }),
    /* @__PURE__ */ jsx8("td", { className: `${table.cell} bc:break-words`, children: variadic ? /* @__PURE__ */ jsx8(VariadicInput, { board, id: id2, link }) : /* @__PURE__ */ jsx8("span", { className: "bc:font-mono bc:text-xs", children: link.input }) }),
    locked2 ? null : /* @__PURE__ */ jsxs7("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      /* @__PURE__ */ jsx8(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Edit the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`,
          ref: button,
          onClick: () => {
            edits.clear();
            open();
          },
          children: "Edit"
        }
      ),
      /* @__PURE__ */ jsx8(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Remove the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`,
          onClick: () => {
            const title = `Could not remove the link from "${blockName(board, link.from)}" to "${blockName(board, link.to)}".`;
            void edits.run(title, "blockr.core-remove_link", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] })
  ] });
}
function BlockLabel({ board, id: id2 }) {
  return /* @__PURE__ */ jsxs7("span", { className: "bc:grid", children: [
    /* @__PURE__ */ jsx8("span", { children: blockName(board, id2) }),
    /* @__PURE__ */ jsx8("span", { className: "bc:font-mono bc:text-xs bc:text-text-muted bc:break-all", children: id2 })
  ] });
}
function VariadicInput({ board, id: id2, link }) {
  const place = createGraph(board).linksInto(link.to).indexOf(id2) + 1;
  return /* @__PURE__ */ jsxs7("span", { className: "bc:grid", children: [
    link.input === "" ? /* @__PURE__ */ jsx8("span", { className: "bc:text-text-muted", children: "No name" }) : /* @__PURE__ */ jsx8("span", { className: "bc:font-mono bc:text-xs", children: link.input }),
    /* @__PURE__ */ jsxs7("span", { className: "bc:text-xs bc:text-text-muted", children: [
      "Position ",
      place
    ] })
  ] });
}
function variadicBlocks(board, registry) {
  return new Set(Object.keys(board.blocks).filter((id2) => isVariadicBlock(board, registry, id2)));
}

// srcts/modules/edit-board/stacks.tsx
import { ColorSwatch } from "@blockr/core/react";
import { Fragment as Fragment4, jsx as jsx9, jsxs as jsxs8 } from "react/jsx-runtime";
var CREATE_STACK = "blockr.core-add_stack_action";
var EDIT_STACK = "blockr.core-edit_stack_action";
var BOARD2 = { board: true };
function StacksSection({ core, board, locked: locked2 }) {
  const stacks = Object.entries(board.stacks);
  const view = useTableView(stacks, ([id2, stack]) => [stack.name, id2, ...stack.blocks.map((block) => blockName(board, block))].join(" "));
  const edits = useEdits(core);
  return /* @__PURE__ */ jsxs8(Section, { title: "Stacks", count: stacks.length, children: [
    locked2 || !offers(core, CREATE_STACK, BOARD2) ? null : /* @__PURE__ */ jsx9(Part, { title: "Add a stack", children: /* @__PURE__ */ jsx9(Advanced, { core, id: CREATE_STACK, target: BOARD2 }) }),
    /* @__PURE__ */ jsx9(Part, { title: "Stacks on the board", children: stacks.length === 0 ? /* @__PURE__ */ jsx9("p", { className: styles.hint, children: "The board has no stacks yet." }) : /* @__PURE__ */ jsxs8(Fragment4, { children: [
      /* @__PURE__ */ jsx9(TableSearch, { label: "Search stacks", view }),
      view.found === 0 ? /* @__PURE__ */ jsx9("p", { className: styles.hint, children: "No stack matches the search." }) : /* @__PURE__ */ jsxs8("table", { className: table.table, children: [
        /* @__PURE__ */ jsx9("caption", { className: "bc:sr-only", children: "The board's stacks" }),
        /* @__PURE__ */ jsx9("thead", { children: /* @__PURE__ */ jsxs8("tr", { children: [
          /* @__PURE__ */ jsx9("th", { scope: "col", className: table.head, children: "Name" }),
          /* @__PURE__ */ jsx9("th", { scope: "col", className: table.head, children: "Blocks" }),
          locked2 ? null : /* @__PURE__ */ jsx9("th", { scope: "col", className: `${table.head} bc:w-24`, children: /* @__PURE__ */ jsx9("span", { className: "bc:sr-only", children: "Actions" }) })
        ] }) }),
        /* @__PURE__ */ jsx9("tbody", { children: view.rows.map(([id2, stack]) => /* @__PURE__ */ jsx9(StackRow, { core, edits, board, id: id2, stack, locked: locked2 }, id2)) })
      ] }),
      /* @__PURE__ */ jsx9(Refusal, { refusal: edits.refusal }),
      /* @__PURE__ */ jsx9(Pager, { label: "Pages of stacks", view })
    ] }) })
  ] });
}
function StackRow({
  core,
  edits,
  board,
  id: id2,
  stack,
  locked: locked2
}) {
  const { editing, open, close, button } = useRowEditing();
  if (editing) {
    return /* @__PURE__ */ jsx9(RowAdvanced, { core, id: EDIT_STACK, target: { stack: id2 }, label: stack.name, columns: 3, onDone: close });
  }
  return /* @__PURE__ */ jsxs8("tr", { "aria-label": stack.name, className: table.row, children: [
    /* @__PURE__ */ jsx9("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsxs8("span", { className: "bc:flex bc:items-center bc:gap-1.5", children: [
      /* @__PURE__ */ jsx9(Swatch, { color: stack.color }),
      /* @__PURE__ */ jsx9("span", { className: "bc:font-medium", children: stack.name })
    ] }) }),
    /* @__PURE__ */ jsx9("td", { className: `${table.cell} bc:break-words`, children: /* @__PURE__ */ jsx9(StackBlocks, { board, blocks: stack.blocks }) }),
    locked2 ? null : /* @__PURE__ */ jsxs8("td", { className: `${table.cell} bc:text-right bc:whitespace-nowrap`, children: [
      /* @__PURE__ */ jsx9(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Edit the stack ${stack.name}`,
          ref: button,
          onClick: () => {
            edits.clear();
            open();
          },
          children: "Edit"
        }
      ),
      /* @__PURE__ */ jsx9(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Remove the stack ${stack.name}`,
          onClick: () => {
            void edits.run(`Could not remove the stack "${stack.name}".`, "blockr.core-remove_stack", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] })
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
function Swatch({ color: color2 }) {
  return color2 === null ? /* @__PURE__ */ jsx9("span", { "aria-hidden": "true", className: RING }) : /* @__PURE__ */ jsx9(ColorSwatch, { color: color2, className: DOT });
}
var RING = "bc:inline-block bc:size-3 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default";
var DOT = "bc:size-3! bc:rounded-full!";

// srcts/modules/edit-board/board.tsx
import { jsx as jsx10, jsxs as jsxs9 } from "react/jsx-runtime";
function EditBoard({ core }) {
  const board = useBoard2(core);
  const locked2 = core.lock.board;
  return /* @__PURE__ */ jsxs9("div", { className: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default", children: [
    locked2 ? null : /* @__PURE__ */ jsx10(BlocksSection, { core, board }),
    /* @__PURE__ */ jsx10(LinksSection, { core, board, locked: locked2 }),
    /* @__PURE__ */ jsx10(StacksSection, { core, board, locked: locked2 })
  ] });
}
function Section({ title, count, children }) {
  return /* @__PURE__ */ jsx10("section", { "aria-label": title, className: "bc:rounded-md bc:border bc:border-border-default", children: /* @__PURE__ */ jsxs9("details", { open: true, className: "bc:group", children: [
    /* @__PURE__ */ jsxs9("summary", { className: "bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:px-3 bc:py-2 bc:font-sans bc:text-sm bc:font-semibold", children: [
      /* @__PURE__ */ jsx10("h3", { className: "bc:m-0 bc:text-sm bc:font-semibold", children: title }),
      /* @__PURE__ */ jsx10("span", { className: "bc:rounded-full bc:border bc:border-border-default bc:px-1.5 bc:text-xs bc:font-normal bc:text-text-muted", children: count })
    ] }),
    /* @__PURE__ */ jsx10("div", { className: "bc:grid bc:gap-4 bc:border-t bc:border-border-default bc:p-3", children })
  ] }) });
}
function Part({ title, children }) {
  return /* @__PURE__ */ jsxs9("div", { role: "group", "aria-label": title, className: "bc:grid bc:gap-2", children: [
    /* @__PURE__ */ jsx10("h4", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: title }),
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
  const [editing, setEditing] = useState9(false);
  const closed = useRef3(false);
  const button = useRef3(null);
  useLayoutEffect2(() => {
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
function offers(core, id2, target2) {
  return core.boardActions.on(target2).some((action) => action.id === id2);
}
function Advanced({ core, id: id2, target: target2 }) {
  const key = JSON.stringify(target2);
  const mount = useCallback3((el) => core.boardActions.mountAdvanced(el, id2, JSON.parse(key)), [core, id2, key]);
  return /* @__PURE__ */ jsx10(Mounted, { mount, className: "bc:min-w-0" });
}
function RowAdvanced({
  core,
  id: id2,
  target: target2,
  label,
  columns,
  onDone
}) {
  const box = useRef3(null);
  useEffect(() => {
    const el = box.current;
    if (el === null) return void 0;
    let listOpen = false;
    const down = (event) => {
      listOpen = event.target instanceof Element && event.target.getAttribute("aria-expanded") === "true";
    };
    const heard = (event) => {
      if (event.key === "Escape" && !event.defaultPrevented && !listOpen) onDone();
    };
    el.addEventListener("keydown", down, { capture: true });
    el.addEventListener("keydown", heard);
    return () => {
      el.removeEventListener("keydown", down, { capture: true });
      el.removeEventListener("keydown", heard);
    };
  }, [onDone]);
  return /* @__PURE__ */ jsx10("tr", { "aria-label": label, className: table.row, children: /* @__PURE__ */ jsx10("td", { colSpan: columns, className: `${table.cell} bc:py-2`, children: /* @__PURE__ */ jsxs9("div", { ref: box, className: "bc:grid bc:gap-2", children: [
    /* @__PURE__ */ jsx10(Advanced, { core, id: id2, target: target2 }),
    /* @__PURE__ */ jsx10("div", { className: "bc:flex bc:gap-2", children: /* @__PURE__ */ jsx10("button", { type: "button", className: classes.button, onClick: onDone, children: "Close" }) })
  ] }) }) });
}

// srcts/modules/edit-board/index.tsx
import { jsx as jsx11 } from "react/jsx-runtime";
var api = 1;
var edit_board_default = {
  ui: {
    render({ el, core }) {
      const root = createRoot2(el);
      flushSync3(() => {
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

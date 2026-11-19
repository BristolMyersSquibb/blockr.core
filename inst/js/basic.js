// srcts/modules/basic/index.tsx
import { flushSync as flushSync2 } from "react-dom";
import { createRoot } from "react-dom/client";

// srcts/modules/basic/board.tsx
import { useCallback as useCallback6, useEffect as useEffect2, useMemo as useMemo2, useState as useState9 } from "react";
import { createPortal } from "react-dom";

// srcts/card/footer.tsx
import { useCallback, useId, useState } from "react";
import { Fragment, jsx, jsxs } from "react/jsx-runtime";
var SEVERITIES = ["error", "warning", "message"];
function useFooter(status, notifications, severities = SEVERITIES) {
  const shown = useShown(status);
  const [open, setOpen] = useState(OPEN_AT_START);
  const panel = useId();
  const toggle = useCallback((severity) => {
    setOpen((was) => {
      const next = new Set(was);
      if (!next.delete(severity)) next.add(severity);
      return next;
    });
  }, []);
  return { status, shown, notifications, severities, open, toggle, panel };
}
function Footer({ status, shown }) {
  return /* @__PURE__ */ jsx(
    "footer",
    {
      "aria-busy": status?.running === true,
      "data-status": shown?.status,
      className: `bc:min-h-4 bc:shrink-0 bc:px-5 bc:pb-1 bc:text-center bc:font-mono bc:text-xs bc:text-text-muted bc:transition-opacity ${settled(status, shown) ? "" : "bc:opacity-50"}`,
      children: shown?.status === "ready" ? shown.summary : null
    }
  );
}
function Pills({ status, shown, notifications, severities, open, toggle, panel }) {
  const pills = SEVERITIES.flatMap((severity) => {
    const n = counted(shown, notifications, severities, severity);
    return n === 0 ? [] : [{ severity, n }];
  });
  if (pills.length === 0) return null;
  return /* @__PURE__ */ jsx(
    "div",
    {
      role: "group",
      "aria-label": "Status",
      "aria-busy": status?.running === true,
      className: `bc:flex bc:flex-wrap bc:items-center bc:justify-center bc:gap-2 bc:px-5 bc:font-mono bc:text-xs bc:transition-opacity ${settled(status, shown) ? "" : "bc:opacity-50"}`,
      children: pills.map(({ severity, n }) => {
        const mark = MARKS[severity];
        return /* @__PURE__ */ jsxs(
          "button",
          {
            type: "button",
            "aria-expanded": open.has(severity),
            "aria-controls": panel,
            onClick: () => {
              toggle(severity);
            },
            className: `bc:inline-flex bc:cursor-pointer bc:items-center bc:gap-1 bc:rounded-full bc:border-0 bc:px-1.5 bc:py-0.5 bc:font-mono bc:text-xs bc:hover:underline bc:aria-expanded:ring-1 bc:aria-expanded:ring-current ${mark.tint} ${mark.text}`,
            children: [
              /* @__PURE__ */ jsx(SeverityIcon, { severity }),
              severity === "error" ? mark.noun : `${String(n)} ${mark.noun}${n === 1 ? "" : "s"}`
            ]
          },
          severity
        );
      })
    }
  );
}
function Banners({ shown, notifications, severities, open, panel }) {
  const listed = notifications.filter(
    (notification) => severities.includes(notification.severity) && open.has(notification.severity)
  );
  const reason = shown?.reason ?? null;
  if (reason !== null && open.has("message")) listed.push({ severity: "message", message: reason });
  if (listed.length === 0) return null;
  return /* @__PURE__ */ jsx("ul", { id: panel, "aria-label": "Conditions", className: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0", children: listed.map(({ severity, message }, i) => /* @__PURE__ */ jsx(Banner, { severity, message }, i)) });
}
function settled(status, shown) {
  return status !== void 0 && status === shown && !status.running;
}
function counted(status, notifications, severities, severity) {
  const n = severities.includes(severity) ? notifications.filter((notification) => notification.severity === severity).length : 0;
  return severity === "message" && (status?.reason ?? null) !== null ? n + 1 : n;
}
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
function Banner({ severity, message }) {
  const mark = MARKS[severity];
  return /* @__PURE__ */ jsxs(
    "li",
    {
      "data-severity": severity,
      className: `bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`,
      children: [
        /* @__PURE__ */ jsx("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx(SeverityIcon, { severity }) }),
        /* @__PURE__ */ jsxs("span", { className: "bc:grid bc:min-w-0 bc:gap-0.5", children: [
          /* @__PURE__ */ jsx("span", { className: `bc:font-sans bc:text-xs bc:font-semibold ${mark.text}`, children: mark.label }),
          /* @__PURE__ */ jsx("span", { className: "bc:font-mono bc:text-xs bc:whitespace-pre-wrap", children: message })
        ] })
      ]
    }
  );
}
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
function useShown(status) {
  const [kept, setKept] = useState(void 0);
  const now = status !== void 0 && status.status !== "stale" && status.status !== "unevaluated" ? status : void 0;
  if (now !== void 0 && now !== kept) setKept(now);
  return now ?? kept;
}
var OPEN_AT_START = /* @__PURE__ */ new Set(["error"]);

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
function useSized(status) {
  const ready = status?.status === "ready";
  const [sized, setSized] = useState2(ready);
  if (ready && !sized) setSized(true);
  return sized || ready;
}

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}
function asError(error) {
  return error instanceof Error ? error : new Error(String(error));
}
function listing(items) {
  if (items.length <= 1) return items.join("");
  return `${items.slice(0, -1).join(", ")} and ${items.at(-1) ?? ""}`;
}
function quoted(items) {
  return listing(items.map((item) => `"${item}"`));
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
import { useMemo, useState as useState6 } from "react";
import { flushSync } from "react-dom";
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
import { useCallback as useCallback2, useSyncExternalStore } from "react";
function useHistory(core) {
  const subscribe = useCallback2(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
  const canUndo = useSyncExternalStore(subscribe, () => core.store.canUndo());
  const canRedo = useSyncExternalStore(subscribe, () => core.store.canRedo());
  return { canUndo, canRedo };
}

// srcts/ui/search.ts
import { useState as useState3 } from "react";
import { useFilter } from "./vendor-b25170e7.js";
function useSearch(all, texts, none = "No options") {
  const { contains } = useFilter({ sensitivity: "base" });
  const [query, setQuery] = useState3("");
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
import { useState as useState5 } from "react";
import { TextField as TextField2 } from "@blockr/core/react";

// srcts/ui/ui.tsx
import { useRef as useRef2, useState as useState4 } from "react";
import { TextField } from "@blockr/core/react";
import { jsx as jsx3 } from "react/jsx-runtime";
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
  const [text, setText] = useState4(name2);
  const done = useRef2(false);
  function finish(submit) {
    if (done.current) return;
    done.current = true;
    const trimmed = text.trim();
    if (submit && trimmed !== "" && trimmed !== name2) onSubmit(trimmed);
    onDone();
  }
  return /* @__PURE__ */ jsx3(
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
import { jsx as jsx4, jsxs as jsxs2 } from "react/jsx-runtime";

// srcts/picker/picker.tsx
import { jsx as jsx5, jsxs as jsxs3 } from "react/jsx-runtime";
function BlockPicker({ core, mode, onAdded, autofocus = false }) {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const [picked, setPicked] = useState6(void 0);
  const [input2, setInput] = useState6(void 0);
  const [inputName, setInputName] = useState6("");
  const [id2, setId] = useState6("");
  const [name2, setName] = useState6("");
  const [errors, setErrors] = useState6({});
  const [busy, setBusy] = useState6(false);
  const entries2 = useMemo(
    () => Object.values(registry).filter((entry2) => mode.kind !== "after" || entry2.inputs.length > 0),
    [registry, mode.kind]
  );
  const options = useMemo(() => entries2.map(classItem).sort(byGroupAndName), [entries2]);
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
  return /* @__PURE__ */ jsxs3(
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
            children: (category) => /* @__PURE__ */ jsx5(ComboBoxSection, { id: category.id, title: category.name, items: category.items, children: (item) => /* @__PURE__ */ jsxs3(ComboBoxItem, { id: item.id, textValue: item.name, children: [
              /* @__PURE__ */ jsx5(Text, { slot: "label", className: "bc:shrink-0 bc:truncate", children: item.name }),
              item.description === "" ? null : /* @__PURE__ */ jsx5(Text, { slot: "description", children: item.description }),
              item.pkg === null ? null : /* @__PURE__ */ jsx5("span", { className: BADGE, children: item.pkg })
            ] }) })
          }
        ),
        link.kind === "pick" ? /* @__PURE__ */ jsx5(
          ComboBox,
          {
            label: link.label,
            isRequired: true,
            defaultItems: link.inputs.map((input3) => ({ id: input3 })),
            value: chosen ?? null,
            onChange: (key) => {
              if (key !== null) setInput(String(key));
            },
            children: (item) => /* @__PURE__ */ jsx5(ComboBoxItem, { id: item.id, children: item.id })
          }
        ) : null,
        link.kind === "name" ? /* @__PURE__ */ jsx5(
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
        link.kind === "no_free_inputs" ? /* @__PURE__ */ jsx5("p", { className: styles.alert, children: link.why }) : null,
        /* @__PURE__ */ jsxs3("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx5(
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
          /* @__PURE__ */ jsx5(
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
        /* @__PURE__ */ jsx5("div", { children: /* @__PURE__ */ jsx5("button", { type: "submit", className: classes.primary, disabled: busy || link.kind === "no_free_inputs", children: "Add block" }) })
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

// srcts/modules/basic/board.tsx
import { useBoard as useBoard3 } from "@blockr/core/react";

// srcts/modules/basic/card.tsx
import { memo, useCallback as useCallback3, useLayoutEffect as useLayoutEffect3, useRef as useRef4, useState as useState7 } from "react";
import { useNotified, useStatus } from "@blockr/core/react";

// srcts/modules/basic/context.ts
import { createContext, useContext } from "react";
var FrontEndContext = createContext(null);
function useFrontEnd() {
  const given = useContext(FrontEndContext);
  if (given === null) throw new Error("A component of the basic frontend is drawn outside of it.");
  return given;
}

// srcts/modules/basic/ui.tsx
import { useLayoutEffect as useLayoutEffect2, useRef as useRef3 } from "react";
import { Button, Dialog as SdkDialog, Heading, Menu as SdkMenu, MenuItem as SdkMenuItem, MenuTrigger, Modal } from "@blockr/core/react";
import { jsx as jsx6, jsxs as jsxs4 } from "react/jsx-runtime";
function Dialog({
  title,
  onClose,
  children
}) {
  const { toasts } = useFrontEnd();
  const toastsRef = useRef3(null);
  useLayoutEffect2(() => {
    const inside = toastsRef.current;
    if (inside === null) return;
    return toasts.showIn(inside);
  }, [toasts]);
  return /* @__PURE__ */ jsx6(
    Modal,
    {
      isOpen: true,
      onOpenChange: (open) => {
        if (!open) onClose();
      },
      children: /* @__PURE__ */ jsxs4(SdkDialog, { children: [
        /* @__PURE__ */ jsxs4("div", { className: "bc:grid bc:gap-3", children: [
          /* @__PURE__ */ jsxs4("div", { className: "bc:flex bc:items-center bc:justify-between bc:gap-2", children: [
            /* @__PURE__ */ jsx6(Heading, { slot: "title", className: classes.heading, children: title }),
            /* @__PURE__ */ jsx6("button", { type: "button", className: classes.button, onClick: onClose, children: "Close" })
          ] }),
          children
        ] }),
        /* @__PURE__ */ jsx6("div", { ref: toastsRef })
      ] })
    }
  );
}
function Menu({ label, items }) {
  return /* @__PURE__ */ jsxs4(MenuTrigger, { placement: "bottom end", children: [
    /* @__PURE__ */ jsx6(Button, { variant: "quiet", "aria-label": label, children: /* @__PURE__ */ jsx6("span", { "aria-hidden": "true", className: "bc:text-base bc:leading-none", children: "\u22EF" }) }),
    /* @__PURE__ */ jsx6(SdkMenu, { "aria-label": label, children: items.map((item) => /* @__PURE__ */ jsx6(SdkMenuItem, { id: item.label, isDisabled: item.disabled, onAction: item.onSelect, children: item.label }, item.label)) })
  ] });
}

// srcts/modules/basic/card.tsx
import { jsx as jsx7, jsxs as jsxs5 } from "react/jsx-runtime";
var BlockCard = memo(function BlockCard2({
  id: id2,
  block,
  severities,
  prependable,
  add
}) {
  const front = useFrontEnd();
  const { core, inView: inView2 } = front;
  const status = useStatus(core, id2);
  const notified = useNotified(core, id2);
  const footer = useFooter(status, notified, severities);
  const sized = useSized(status);
  const [renaming, setRenaming] = useState7(false);
  const ref = useRef4(null);
  useLayoutEffect3(() => {
    const el = ref.current;
    return el === null ? void 0 : inView2.watch({ block: id2 }, el);
  }, [inView2, id2]);
  const mountUi = useCallback3((el) => core.mountBlock(el, id2), [core, id2]);
  const mountOutput = useCallback3((el) => core.mountOutput(el, id2), [core, id2]);
  const focus = useCallback3(() => {
    if (core.focus.get() !== id2) core.focus.set(id2);
  }, [core, id2]);
  const locked2 = core.lock.board;
  const inputs = !core.store.locked(id2);
  const entry = own(core.blockRegistry(), block.registry_id);
  const ready = status?.status === "ready";
  return /* @__PURE__ */ jsxs5(
    "article",
    {
      ref,
      "aria-label": block.name,
      className: `${classes.card} bc:grid bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:p-3`,
      onPointerDown: focus,
      onFocus: focus,
      children: [
        /* @__PURE__ */ jsxs5("header", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
          renaming ? /* @__PURE__ */ jsx7(
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
          ) : /* @__PURE__ */ jsx7("h3", { className: classes.heading, children: block.name }),
          /* @__PURE__ */ jsx7("span", { className: "bc:text-xs bc:text-text-muted", children: entry?.name ?? block.registry_id }),
          /* @__PURE__ */ jsx7(StatusBadge, { status }),
          locked2 ? null : /* @__PURE__ */ jsx7("div", { className: "bc:ml-auto bc:flex bc:items-center bc:gap-1", children: /* @__PURE__ */ jsx7(
            Menu,
            {
              label: `Actions of ${block.name}`,
              items: [
                {
                  label: "Rename",
                  onSelect: () => {
                    setRenaming(true);
                  }
                },
                {
                  label: "Add block before",
                  disabled: !prependable,
                  onSelect: () => {
                    add({ kind: "before", to: id2 });
                  }
                },
                {
                  label: "Add block after",
                  onSelect: () => {
                    add({ kind: "after", from: id2 });
                  }
                },
                {
                  label: "Remove",
                  onSelect: () => {
                    void runCommand(core, "blockr.core-remove_block", { id: id2 });
                  }
                }
              ]
            }
          ) })
        ] }),
        inputs ? /* @__PURE__ */ jsx7(Mounted, { mount: mountUi, label: "Inputs", className: "bc:min-w-0" }) : null,
        /* @__PURE__ */ jsxs5(
          "div",
          {
            className: `bc:min-w-0 ${sized ? "bc:flex bc:h-96 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`,
            children: [
              /* @__PURE__ */ jsx7(
                Mounted,
                {
                  mount: mountOutput,
                  label: "Output",
                  data: { "data-ready": String(ready) },
                  className: `bc:min-w-0 bc:transition-opacity ${ready ? "" : "bc:opacity-50 bc:grayscale"} ${sized ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"}`
                }
              ),
              sized ? /* @__PURE__ */ jsx7(Footer, { ...footer }) : null
            ]
          }
        ),
        /* @__PURE__ */ jsx7(Pills, { ...footer }),
        /* @__PURE__ */ jsx7(Banners, { ...footer })
      ]
    }
  );
});
function StackGroup({ stack, children }) {
  const color2 = stack.color ?? void 0;
  return /* @__PURE__ */ jsx7(
    "section",
    {
      "aria-label": stack.name,
      className: "bc:rounded-xl bc:border-2 bc:border-border-default bc:p-3",
      style: color2 === void 0 ? void 0 : { borderColor: color2 },
      children: /* @__PURE__ */ jsxs5("details", { open: true, children: [
        /* @__PURE__ */ jsx7("summary", { className: `${classes.heading} bc:cursor-pointer`, children: stack.name }),
        /* @__PURE__ */ jsx7("div", { className: "bc:mt-3 bc:grid bc:gap-3", children })
      ] })
    }
  );
}
var TONES = {
  ready: "bc:bg-status-ready",
  failed: "bc:bg-status-failed",
  invalid: "bc:bg-status-invalid",
  waiting: "bc:bg-status-waiting",
  stale: "bc:bg-status-stale",
  unevaluated: "bc:bg-status-unevaluated"
};
function StatusBadge({ status }) {
  if (status === void 0) return null;
  return /* @__PURE__ */ jsxs5(
    "span",
    {
      "data-status": status.status,
      "aria-busy": status.running,
      className: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-full bc:border bc:border-border-default bc:px-2 bc:py-0.5 bc:text-xs bc:text-text-muted",
      children: [
        /* @__PURE__ */ jsx7(
          "span",
          {
            "aria-hidden": "true",
            className: `bc:size-2 bc:rounded-full ${TONES[status.status]} ${status.running ? "bc:animate-pulse" : ""}`
          }
        ),
        status.running ? `${status.status}, running` : status.status
      ]
    }
  );
}

// srcts/modules/basic/layout.ts
function layoutItems(board) {
  const stackByBlock = /* @__PURE__ */ new Map();
  for (const [id2, stack] of Object.entries(board.stacks)) {
    for (const block of stack.blocks) stackByBlock.set(block, id2);
  }
  const items = [];
  const placed2 = /* @__PURE__ */ new Set();
  for (const block of Object.keys(board.blocks)) {
    const stack = stackByBlock.get(block);
    if (stack === void 0) {
      items.push({ block });
    } else if (!placed2.has(stack)) {
      placed2.add(stack);
      items.push({ stack, blocks: stackBlocks(board, stack) });
    }
  }
  for (const stack of Object.keys(board.stacks)) {
    if (!placed2.has(stack)) items.push({ stack, blocks: [] });
  }
  return items;
}
function stackBlocks(board, stack) {
  return (board.stacks[stack]?.blocks ?? []).filter((block) => Object.hasOwn(board.blocks, block));
}

// srcts/modules/basic/options.tsx
import { useCallback as useCallback4 } from "react";
import { useBoard as useBoard2 } from "@blockr/core/react";
import { jsx as jsx8, jsxs as jsxs6 } from "react/jsx-runtime";
function OptionsDialog({ onClose }) {
  const { core } = useFrontEnd();
  const board = useBoard2(core);
  const registered = core.options.registered();
  const categories = /* @__PURE__ */ new Map();
  for (const id2 of Object.keys(board.options)) {
    const def = Object.hasOwn(registered, id2) ? registered[id2] : void 0;
    if (def === void 0) continue;
    categories.set(def.category, [...categories.get(def.category) ?? [], id2]);
  }
  return /* @__PURE__ */ jsxs6(Dialog, { title: "Board options", onClose, children: [
    categories.size === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "The board holds no board options." }) : null,
    [...categories].map(([category, ids]) => /* @__PURE__ */ jsxs6("section", { "aria-label": category, className: "bc:grid bc:gap-2", children: [
      /* @__PURE__ */ jsx8("h3", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: category }),
      ids.map((id2) => /* @__PURE__ */ jsx8(OptionUi, { id: id2 }, id2))
    ] }, category))
  ] });
}
function OptionUi({ id: id2 }) {
  const { core } = useFrontEnd();
  const mount = useCallback4((el) => core.mountOption(el, id2), [core, id2]);
  return /* @__PURE__ */ jsx8(Mounted, { mount });
}

// srcts/modules/basic/plugins.tsx
import { useCallback as useCallback5, useLayoutEffect as useLayoutEffect4, useRef as useRef5 } from "react";
import { jsx as jsx9, jsxs as jsxs7 } from "react/jsx-runtime";
function placed(core) {
  return Object.entries(core.plugins.appPlugins()).filter(([id2]) => core.plugins.pluginClass(id2).ui !== void 0);
}
function Plugins() {
  const { core } = useFrontEnd();
  const shown = placed(core);
  if (shown.length === 0) return null;
  return /* @__PURE__ */ jsx9(
    "aside",
    {
      "aria-label": "Plugins",
      className: "bc:grid bc:min-w-0 bc:content-start bc:gap-4 bc:md:sticky bc:md:top-4 bc:md:max-h-[calc(100dvh-2rem)] bc:md:w-1/3 bc:md:min-w-md bc:md:shrink-0 bc:md:overflow-auto",
      children: shown.map(([id2, plugin]) => /* @__PURE__ */ jsx9(PluginSection, { id: id2, plugin }, id2))
    }
  );
}
function PluginSection({ id: id2, plugin }) {
  const { core, inView: inView2 } = useFrontEnd();
  const ref = useRef5(null);
  useLayoutEffect4(() => {
    const el = ref.current;
    return el === null ? void 0 : inView2.watch({ plugin: id2 }, el);
  }, [inView2, id2]);
  const mount = useCallback5((el) => core.mountPlugin(el, id2), [core, id2]);
  return /* @__PURE__ */ jsxs7("details", { open: true, className: `${classes.card} bc:min-w-0 bc:rounded-lg bc:border bc:border-border-default bc:p-3`, children: [
    /* @__PURE__ */ jsx9("summary", { className: `${classes.heading} bc:cursor-pointer`, children: plugin.name }),
    /* @__PURE__ */ jsx9("div", { ref, className: "bc:mt-3 bc:min-w-0", children: /* @__PURE__ */ jsx9(Mounted, { mount, className: "bc:min-w-0" }) })
  ] });
}

// srcts/modules/basic/toasts.ts
import { useSyncExternalStore as useSyncExternalStore2 } from "react";
var SHOWN_FOR = 5e3;
var FADES_FOR = 300;
function createToasts(notifications) {
  const shown = /* @__PURE__ */ new Map();
  const gone = /* @__PURE__ */ new Set();
  const inside = [];
  const listeners = /* @__PURE__ */ new Set();
  let heard = notifications.get();
  let list = [];
  let stopped = false;
  function publish() {
    const next = heard.flatMap((notification) => {
      const toast = shown.get(notification.id)?.toast;
      return toast === void 0 ? [] : [toast];
    });
    if (next.length !== list.length || next.some((toast, i) => toast !== list[i])) list = next;
    for (const fn of [...listeners]) fn();
  }
  function countDown(id2, toast) {
    if (stopped || toast.toast.notification.severity !== "message" || toast.kept.size > 0) return;
    clearTimeout(toast.timer);
    toast.timer = setTimeout(() => {
      toast.toast = { ...toast.toast, fading: true };
      toast.timer = setTimeout(() => {
        hide(id2);
      }, FADES_FOR);
      publish();
    }, SHOWN_FOR);
  }
  function hide(id2) {
    const toast = shown.get(id2);
    if (toast === void 0) return;
    clearTimeout(toast.timer);
    shown.delete(id2);
    gone.add(id2);
    publish();
  }
  function update() {
    const holding = new Set(heard.map((notification) => notification.id));
    for (const [id2, toast] of shown) {
      if (holding.has(id2)) continue;
      clearTimeout(toast.timer);
      shown.delete(id2);
    }
    for (const id2 of gone) {
      if (!holding.has(id2)) gone.delete(id2);
    }
    for (const notification of heard) {
      if (aboutBlock(notification) || gone.has(notification.id)) continue;
      const toast = shown.get(notification.id);
      if (toast === void 0) {
        const added = { toast: { notification, fading: false }, timer: void 0, kept: /* @__PURE__ */ new Set() };
        shown.set(notification.id, added);
        countDown(notification.id, added);
      } else if (toast.toast.notification !== notification) {
        toast.toast = { ...toast.toast, notification };
      }
    }
    publish();
  }
  const off = notifications.subscribe((all) => {
    heard = all;
    update();
  });
  update();
  return {
    list: () => list,
    shownIn: () => inside.at(-1) ?? null,
    subscribe(fn) {
      const wrapped = () => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    showIn(el) {
      inside.push(el);
      publish();
      let back = false;
      return () => {
        if (back) return;
        back = true;
        const at = inside.lastIndexOf(el);
        if (at >= 0) inside.splice(at, 1);
        publish();
      };
    },
    close: hide,
    keep(id2, by) {
      const toast = shown.get(id2);
      if (toast === void 0) return;
      toast.kept.add(by);
      clearTimeout(toast.timer);
      toast.timer = void 0;
      if (!toast.toast.fading) return;
      toast.toast = { ...toast.toast, fading: false };
      publish();
    },
    release(id2, by) {
      const toast = shown.get(id2);
      if (toast?.kept.delete(by) !== true) return;
      countDown(id2, toast);
    },
    stop() {
      stopped = true;
      off();
      for (const toast of shown.values()) clearTimeout(toast.timer);
    }
  };
}
function aboutBlock(notification) {
  const { about } = notification;
  return about !== void 0 && "block" in about;
}
function useToasts(toasts) {
  return useSyncExternalStore2(toasts.subscribe, toasts.list);
}
function useShownIn(toasts) {
  return useSyncExternalStore2(toasts.subscribe, toasts.shownIn);
}
function concerned(about, core, board) {
  if (about === void 0) return "Board";
  if ("option" in about) return `Board option "${own(core.options.registered(), about.option)?.label ?? about.option}"`;
  if ("plugin" in about) return `Plugin "${own(core.plugins.appPlugins(), about.plugin)?.name ?? about.plugin}"`;
  if ("stack" in about) return `Stack "${own(board.stacks, about.stack)?.name ?? about.stack}"`;
  const block = (id2) => `"${own(board.blocks, id2)?.name ?? id2}"`;
  if ("block" in about) return `Block ${block(about.block)}`;
  const link = own(board.links, about.link);
  return link === void 0 ? `Link "${about.link}"` : `Link from ${block(link.from)} to ${block(link.to)}`;
}
function sourcePackage(notification) {
  return notification.source.module?.package;
}
function isReload(notification) {
  return notification.source.kind === "core" && notification.source.code === "page_reload_needed";
}

// srcts/modules/basic/toolbar.tsx
import { useEffect, useReducer, useState as useState8 } from "react";
import { Button as SdkButton, Keyboard, Menu as Menu2, MenuItem, MenuSection, MenuTrigger as MenuTrigger2, Text as Text2 } from "@blockr/core/react";

// srcts/modules/basic/storage.ts
var KINDS = /* @__PURE__ */ new Set(["button", "menu", "status"]);
function drawnControls(core) {
  const storage = core.plugins.storage();
  const plugin = storage === void 0 ? void 0 : Object.keys(core.plugins.appPlugins()).find((id2) => core.plugins.pluginClass(id2).storage === storage);
  if (storage === void 0 || plugin === void 0) return void 0;
  try {
    const twice = /* @__PURE__ */ new Set();
    const controls = distinct(storage.controls({ core, plugin }), twice);
    return {
      controls,
      failed: false,
      ...twice.size === 0 ? {} : { problem: `Storage controls of one list share the ids ${quoted([...twice])}, so only the first of each draws.` }
    };
  } catch (error) {
    return { controls: [], failed: true, problem: `The storage controls failed: ${errorMessage(error)}` };
  }
}
function buttonsFromControls(controls) {
  return controls.flatMap((control) => {
    if (control.kind === "button") return [control];
    if (control.kind === "menu") return buttonsFromControls(control.items);
    return [];
  });
}
async function runButton(core, button) {
  try {
    await button.run();
  } catch (error) {
    core.log("blockr.core").error(asError(error));
  }
}
function distinct(list, twice) {
  const seen = /* @__PURE__ */ new Set();
  const kept = [];
  for (const control of list) {
    if (!KINDS.has(control.kind)) continue;
    if (seen.has(control.id)) {
      twice.add(control.id);
      continue;
    }
    seen.add(control.id);
    kept.push(control.kind === "menu" ? { ...control, items: distinct(control.items, twice) } : control);
  }
  return kept;
}

// srcts/modules/basic/shortcuts.ts
function bindShortcuts(doc, core) {
  function onKey(event) {
    if (event.defaultPrevented || editable(event.target)) return;
    const step = undoOrRedo(event);
    if (step !== void 0) {
      event.preventDefault();
      try {
        if (step === "undo") core.store.undo();
        else core.store.redo();
      } catch {
      }
      return;
    }
    const button = buttonsFromControls(drawnControls(core)?.controls ?? []).find(
      (b) => b.disabled !== true && b.shortcut !== void 0 && matchesShortcut(b.shortcut, event)
    );
    if (button === void 0) return;
    event.preventDefault();
    if (!event.repeat) void runButton(core, button);
  }
  doc.addEventListener("keydown", onKey);
  return () => {
    doc.removeEventListener("keydown", onKey);
  };
}
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
  if (key === "" || new Set(names).size < names.length || !names.every((name2) => MODIFIERS.includes(name2))) {
    return void 0;
  }
  const has = (name2) => names.includes(name2);
  if (has("mod") && (has("ctrl") || has("meta"))) return void 0;
  return { key, mod: has("mod"), ctrl: has("ctrl"), alt: has("alt"), shift: has("shift"), meta: has("meta") };
}
function undoOrRedo(event) {
  if (!(event.ctrlKey || event.metaKey) || event.altKey) return void 0;
  const key = event.key.toLowerCase();
  if (key === "z") return event.shiftKey ? "redo" : "undo";
  if (key === "y" && !event.shiftKey) return "redo";
  return void 0;
}
function editable(target2) {
  if (!(target2 instanceof HTMLElement)) return false;
  return target2.isContentEditable || ["INPUT", "TEXTAREA", "SELECT"].includes(target2.tagName);
}

// srcts/modules/basic/toolbar.tsx
import { Fragment as Fragment2, jsx as jsx10, jsxs as jsxs8 } from "react/jsx-runtime";
function Toolbar({ board, showOptions }) {
  const { core } = useFrontEnd();
  const { canUndo, canRedo } = useHistory(core);
  const [renaming, setRenaming] = useState8(false);
  const locked2 = core.lock.board;
  function step(back) {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch {
    }
  }
  return /* @__PURE__ */ jsxs8("header", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:border-b bc:border-border-default bc:pb-3", children: [
    renaming ? /* @__PURE__ */ jsx10(
      NameField,
      {
        label: "Board name",
        name: board.name,
        onSubmit: (name2) => {
          void runCommand(core, "blockr.core-rename_board", { name: name2 });
        },
        onDone: () => {
          setRenaming(false);
        }
      }
    ) : /* @__PURE__ */ jsx10("h1", { className: "bc:m-0 bc:font-sans bc:text-lg bc:font-semibold", children: board.name }),
    locked2 || renaming ? null : /* @__PURE__ */ jsx10(
      "button",
      {
        type: "button",
        className: classes.quiet,
        "aria-label": "Rename board",
        onClick: () => {
          setRenaming(true);
        },
        children: "Rename"
      }
    ),
    /* @__PURE__ */ jsxs8("div", { role: "toolbar", "aria-label": "Board", className: "bc:ml-auto bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
      /* @__PURE__ */ jsx10(
        "button",
        {
          type: "button",
          className: classes.button,
          onClick: () => {
            showOptions();
          },
          children: "Options"
        }
      ),
      /* @__PURE__ */ jsx10(
        "button",
        {
          type: "button",
          className: classes.button,
          disabled: !canUndo,
          "aria-keyshortcuts": "Control+Z Meta+Z",
          onClick: () => {
            step(true);
          },
          children: "Undo"
        }
      ),
      /* @__PURE__ */ jsx10(
        "button",
        {
          type: "button",
          className: classes.button,
          disabled: !canRedo,
          "aria-keyshortcuts": "Control+Shift+Z Meta+Shift+Z Control+Y",
          onClick: () => {
            step(false);
          },
          children: "Redo"
        }
      ),
      /* @__PURE__ */ jsx10(StorageControls, {})
    ] })
  ] });
}
function StorageControls() {
  const { core } = useFrontEnd();
  const storage = core.plugins.storage();
  const [, redraw] = useReducer((n) => n + 1, 0);
  useEffect(() => {
    if (storage?.subscribe === void 0) return void 0;
    try {
      return storage.subscribe(redraw);
    } catch (error) {
      core.log("blockr.core").error(asError(error));
      return void 0;
    }
  }, [core, storage]);
  const drawn2 = drawnControls(core);
  const problem = drawn2?.problem;
  useEffect(() => {
    if (problem !== void 0) core.log("blockr.core").error(problem);
  }, [core, problem]);
  if (drawn2 === void 0) return null;
  if (drawn2.failed) {
    return /* @__PURE__ */ jsx10("span", { role: "alert", title: problem, className: "bc:font-sans bc:text-sm bc:text-text-danger", children: "Storage failed" });
  }
  return /* @__PURE__ */ jsx10(Fragment2, { children: drawn2.controls.map((control) => /* @__PURE__ */ jsx10(Control, { control }, control.id)) });
}
function Control({ control }) {
  switch (control.kind) {
    case "button":
      return /* @__PURE__ */ jsx10(StorageButton, { button: control, className: classes.button });
    case "menu":
      return /* @__PURE__ */ jsx10(StorageMenu, { label: control.label, items: control.items });
    case "status":
      return /* @__PURE__ */ jsx10("span", { role: "status", title: control.tooltip, className: "bc:font-sans bc:text-sm bc:text-text-muted", children: control.text });
  }
}
function StorageButton({ button, className }) {
  const { core } = useFrontEnd();
  return /* @__PURE__ */ jsx10(
    "button",
    {
      type: "button",
      className,
      disabled: button.disabled,
      title: button.tooltip,
      "aria-keyshortcuts": ariaShortcuts(button.shortcut),
      onClick: () => {
        void runButton(core, button);
      },
      children: button.label
    }
  );
}
function StorageMenu({ label, items }) {
  const { core } = useFrontEnd();
  const mac = onMac(window);
  return /* @__PURE__ */ jsxs8(MenuTrigger2, { placement: "bottom end", children: [
    /* @__PURE__ */ jsxs8(SdkButton, { variant: "secondary", children: [
      label,
      /* @__PURE__ */ jsx10("span", { "aria-hidden": "true", children: "\u25BE" })
    ] }),
    /* @__PURE__ */ jsx10(Menu2, { "aria-label": label, children: groups(label, items).flatMap(
      (group) => group.title === void 0 ? entries(group.items, core, mac) : [
        /* @__PURE__ */ jsx10(MenuSection, { id: group.id, title: group.title, children: entries(group.items, core, mac) }, group.id)
      ]
    ) })
  ] });
}
function entries(items, core, mac) {
  return items.map((item) => {
    const label = item.kind === "button" ? item.label : item.text;
    const shortcut = item.kind === "button" ? shortcutLabel(item.shortcut, mac) : void 0;
    return /* @__PURE__ */ jsxs8(
      MenuItem,
      {
        id: item.id,
        textValue: label,
        isDisabled: item.kind === "status" || item.disabled,
        onAction: item.kind === "button" ? () => {
          void runButton(core, item);
        } : void 0,
        children: [
          /* @__PURE__ */ jsx10(Text2, { slot: "label", children: label }),
          item.tooltip === void 0 ? null : /* @__PURE__ */ jsx10(Text2, { slot: "description", children: item.tooltip }),
          shortcut === void 0 ? null : /* @__PURE__ */ jsx10(Keyboard, { children: shortcut })
        ]
      },
      item.id
    );
  });
}
function groups(id2, items, title) {
  const own2 = items.filter((item) => item.kind !== "menu");
  const inner = items.flatMap((item) => item.kind === "menu" ? groups(`${id2}/${item.id}`, item.items, item.label) : []);
  return [...own2.length === 0 && title === void 0 ? [] : [{ id: id2, title, items: own2 }], ...inner];
}

// srcts/modules/basic/board.tsx
import { jsx as jsx11, jsxs as jsxs9 } from "react/jsx-runtime";
function BasicBoard({ front }) {
  return /* @__PURE__ */ jsx11(FrontEndContext, { value: front, children: /* @__PURE__ */ jsx11(Page, {}) });
}
function Page() {
  const { core } = useFrontEnd();
  const board = useBoard3(core);
  const items = useMemo2(() => layoutItems(board), [board]);
  const shown = board.options.show_conditions;
  const severities = useMemo2(() => asSeverities(shown), [shown]);
  const [dialog, setDialog] = useState9(null);
  const add = useCallback6((mode) => {
    setDialog({ kind: "add", mode });
  }, []);
  const close = useCallback6(() => {
    setDialog(null);
  }, []);
  const card = (id2) => {
    const block = board.blocks[id2];
    if (block === void 0) return null;
    return /* @__PURE__ */ jsx11(
      BlockCard,
      {
        id: id2,
        block,
        severities,
        prependable: (freeInputs(board, core.blockRegistry(), id2)?.length ?? 0) > 0,
        add
      },
      `block-${id2}`
    );
  };
  return /* @__PURE__ */ jsxs9("div", { className: `${classes.page} bc:flex bc:min-h-full bc:flex-col bc:gap-4 bc:p-4 bc:font-sans`, children: [
    /* @__PURE__ */ jsx11(
      Toolbar,
      {
        board,
        showOptions: () => {
          setDialog({ kind: "options" });
        }
      }
    ),
    /* @__PURE__ */ jsxs9("div", { className: "bc:flex bc:flex-col bc:gap-4 bc:md:flex-row-reverse bc:md:items-start", children: [
      /* @__PURE__ */ jsx11(Plugins, {}),
      /* @__PURE__ */ jsxs9("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-4", children: [
        items.length === 0 ? /* @__PURE__ */ jsx11("p", { className: styles.hint, children: "The board holds no blocks yet." }) : null,
        items.map((item) => {
          if ("block" in item) return card(item.block);
          const stack = board.stacks[item.stack];
          if (stack === void 0) return null;
          return /* @__PURE__ */ jsx11(StackGroup, { stack, children: item.blocks.length === 0 ? /* @__PURE__ */ jsx11("p", { className: styles.hint, children: "The stack holds no blocks." }) : item.blocks.map(card) }, `stack-${item.stack}`);
        })
      ] })
    ] }),
    dialog?.kind === "add" ? /* @__PURE__ */ jsx11(AddBlockDialog, { mode: dialog.mode, onClose: close }) : null,
    dialog?.kind === "options" ? /* @__PURE__ */ jsx11(OptionsDialog, { onClose: close }) : null,
    /* @__PURE__ */ jsx11(Toasts, {})
  ] });
}
function AddBlockDialog({ mode, onClose }) {
  const front = useFrontEnd();
  const board = useBoard3(front.core);
  const target2 = mode.kind === "add" ? void 0 : own(board.blocks, mode.kind === "after" ? mode.from : mode.to);
  const title = target2 === void 0 ? "Add block" : `Add block ${mode.kind} "${target2.name}"`;
  return /* @__PURE__ */ jsx11(Dialog, { title, onClose, children: /* @__PURE__ */ jsx11(BlockPicker, { core: front.core, mode, onAdded: onClose, autofocus: true }) });
}
function Toasts() {
  const { core, toasts } = useFrontEnd();
  const list = useToasts(toasts);
  const inside = useShownIn(toasts);
  const board = useBoard3(core);
  if (list.length === 0) return null;
  const shown = /* @__PURE__ */ jsx11(
    "section",
    {
      "aria-label": "Notifications",
      className: "bc:pointer-events-none bc:fixed bc:right-2 bc:bottom-2 bc:z-50 bc:m-0 bc:box-border bc:grid bc:max-h-[calc(100dvh-1rem)] bc:w-[21rem] bc:max-w-[calc(100vw-1rem)] bc:gap-2 bc:overflow-y-auto bc:p-2 bc:font-sans",
      children: list.map((toast) => /* @__PURE__ */ jsx11(ToastItem, { toast, title: concerned(toast.notification.about, core, board) }, toast.notification.id))
    }
  );
  return inside === null ? shown : createPortal(shown, inside);
}
function ToastItem({ toast, title }) {
  const { toasts } = useFrontEnd();
  const { notification, fading } = toast;
  const { id: id2, severity } = notification;
  const mark = MARKS[severity];
  const pkg = sourcePackage(notification);
  useEffect2(
    () => () => {
      toasts.release(id2, "pointer");
      toasts.release(id2, "focus");
    },
    [toasts, id2]
  );
  return /* @__PURE__ */ jsxs9(
    "div",
    {
      role: severity === "message" ? "status" : "alert",
      "data-severity": severity,
      onPointerEnter: () => {
        toasts.keep(id2, "pointer");
      },
      onPointerLeave: () => {
        toasts.release(id2, "pointer");
      },
      onFocus: () => {
        toasts.keep(id2, "focus");
      },
      onBlur: (event) => {
        if (!(event.relatedTarget instanceof Node && event.currentTarget.contains(event.relatedTarget))) {
          toasts.release(id2, "focus");
        }
      },
      className: `${classes.surface} bc:pointer-events-auto bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 bc:shadow-lg bc:transition-opacity bc:duration-300 bc:motion-reduce:transition-none ${mark.border} ${fading ? "bc:opacity-0" : ""}`,
      children: [
        /* @__PURE__ */ jsx11("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx11(SeverityIcon, { severity }) }),
        /* @__PURE__ */ jsxs9("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-1", children: [
          /* @__PURE__ */ jsxs9("p", { className: "bc:m-0 bc:text-xs bc:font-semibold", children: [
            title,
            pkg === void 0 ? null : /* @__PURE__ */ jsxs9("span", { className: "bc:font-normal bc:text-text-muted", children: [
              " \xB7 ",
              pkg
            ] })
          ] }),
          /* @__PURE__ */ jsx11("p", { className: "bc:m-0 bc:text-sm bc:break-words bc:whitespace-pre-wrap", children: notification.message }),
          isReload(notification) ? /* @__PURE__ */ jsx11("div", { children: /* @__PURE__ */ jsx11(
            "button",
            {
              type: "button",
              className: classes.button,
              onClick: () => {
                location.reload();
              },
              children: "Reload"
            }
          ) }) : null
        ] }),
        /* @__PURE__ */ jsx11(
          "button",
          {
            type: "button",
            "aria-label": "Close",
            className: classes.quiet,
            onClick: () => {
              toasts.close(id2);
            },
            children: /* @__PURE__ */ jsx11("span", { "aria-hidden": "true", children: "\xD7" })
          }
        )
      ]
    }
  );
}
function asSeverities(value2) {
  if (Array.isArray(value2) && value2.every(isSeverity)) return value2;
  throw new TypeError(
    `The board option show_conditions is ${JSON.stringify(value2)}, where the board takes an array of "message", "warning" and "error".`
  );
}
function isSeverity(value2) {
  return value2 === "error" || value2 === "warning" || value2 === "message";
}

// srcts/modules/basic/demand.ts
var OWNER = "blockr.core-basic";
var SHOWN = 1;
var IN_VIEW = 2;
function demandShown(core, view = window) {
  const every = core.scheduler.demand(OWNER, (board) => Object.keys(board.blocks), { priority: SHOWN });
  const near = core.scheduler.demand(OWNER, [], { priority: IN_VIEW });
  const watched = /* @__PURE__ */ new Map();
  const seen = /* @__PURE__ */ new Set();
  let released = false;
  function update() {
    if (released) return;
    const blocks = /* @__PURE__ */ new Set();
    const plugins = /* @__PURE__ */ new Set();
    for (const el of seen) {
      const object = watched.get(el);
      if (object === void 0) continue;
      if ("block" in object) blocks.add(object.block);
      else plugins.add(object.plugin);
    }
    near.set([...blocks]);
    core.onScreen.set({ blocks: [...blocks], plugins: [...plugins] });
  }
  const Observer = view.IntersectionObserver;
  const observer = Observer === void 0 ? void 0 : new Observer((entries2) => {
    for (const entry of entries2) {
      if (!watched.has(entry.target)) continue;
      if (entry.isIntersecting) seen.add(entry.target);
      else seen.delete(entry.target);
    }
    update();
  });
  return {
    watch(object, el) {
      watched.set(el, object);
      observer?.observe(el);
      return () => {
        observer?.unobserve(el);
        watched.delete(el);
        if (seen.delete(el)) update();
      };
    },
    element(object) {
      for (const [el, at] of watched) {
        if ("block" in object ? "block" in at && at.block === object.block : "plugin" in at && at.plugin === object.plugin) return el;
      }
      return void 0;
    },
    measure() {
      seen.clear();
      for (const el of watched.keys()) if (inView(el, view)) seen.add(el);
      update();
    },
    release() {
      released = true;
      observer?.disconnect();
      every.release();
      near.release();
    }
  };
}
function inView(el, view) {
  const rect = el.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0 && rect.bottom > 0 && rect.right > 0 && rect.top < view.innerHeight && rect.left < view.innerWidth;
}

// srcts/modules/basic/index.tsx
import { jsx as jsx12 } from "react/jsx-runtime";
var api = 1;
var basic_default = {
  options: ["show_conditions", "dark_mode"],
  frontend: { render, show }
};
var drawn = /* @__PURE__ */ new WeakMap();
function render({ el, core }) {
  const doc = el.ownerDocument;
  const shown = demandShown(core, doc.defaultView ?? window);
  const toasts = createToasts(core.notifications);
  const front = { core, toasts, inView: shown };
  const root = createRoot(el);
  flushSync2(() => {
    root.render(/* @__PURE__ */ jsx12(BasicBoard, { front }));
  });
  shown.measure();
  const unbind = bindShortcuts(doc, core);
  drawn.set(el, shown);
  return () => {
    if (drawn.get(el) === shown) drawn.delete(el);
    unbind();
    toasts.stop();
    shown.release();
    root.unmount();
  };
}
function show({ object, el }) {
  const target2 = drawn.get(el)?.element(object);
  if (target2 === void 0) return;
  for (let node = target2.parentElement; node !== null && node !== el; node = node.parentElement) {
    if (node.localName === "details" && !node.hasAttribute("open")) node.setAttribute("open", "");
  }
  const into = "plugin" in object ? target2.closest("details") ?? target2 : target2;
  into.scrollIntoView({ block: "nearest", inline: "nearest" });
}
export {
  api,
  basic_default as default
};

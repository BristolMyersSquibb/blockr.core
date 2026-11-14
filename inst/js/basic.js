// srcts/modules/basic/index.tsx
import { flushSync as flushSync2 } from "react-dom";
import { createRoot } from "react-dom/client";

// srcts/modules/basic/board.tsx
import { useCallback as useCallback7, useEffect as useEffect3, useMemo as useMemo4, useState as useState10 } from "react";
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
    text: "bc:text-blue-600 bc:dark:text-blue-400",
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
import { useCallback as useCallback2, useMemo, useState as useState2, useSyncExternalStore } from "react";
function useStatus(core, block) {
  const subscribe = useCallback2(
    (notify) => core.scheduler.subscribe((changed) => {
      if (changed.includes(block)) notify();
    }),
    [core, block]
  );
  return useSyncExternalStore(subscribe, () => core.scheduler.status(block));
}
function useNotified(core, block) {
  const subscribe = useCallback2(
    (notify) => core.notifications.subscribe(() => {
      notify();
    }),
    [core]
  );
  const all = useSyncExternalStore(subscribe, () => core.notifications.get());
  return useMemo(
    () => block === null ? all : all.filter((notification) => notification.about !== void 0 && "block" in notification.about && notification.about.block === block),
    [all, block]
  );
}
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
import { useMemo as useMemo3, useState as useState6 } from "react";
import { flushSync } from "react-dom";

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

// srcts/ui/combobox.tsx
import { useId as useId2, useLayoutEffect as useLayoutEffect3, useMemo as useMemo2, useRef as useRef3, useState as useState4 } from "react";

// srcts/ui/ui.tsx
import { useLayoutEffect as useLayoutEffect2, useRef as useRef2, useState as useState3 } from "react";
import { jsx as jsx3 } from "react/jsx-runtime";
var classes = {
  button: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50",
  primary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-accent bc:bg-bg-accent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-on-accent bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle",
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
  const [text, setText] = useState3(name2);
  const ref = useRef2(null);
  const done = useRef2(false);
  useLayoutEffect2(() => {
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
  return /* @__PURE__ */ jsx3(
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
import { jsx as jsx4, jsxs as jsxs2 } from "react/jsx-runtime";
function Combobox(props) {
  const { value: value2, onChange } = props;
  return /* @__PURE__ */ jsx4(Field, { ...props, picked: value2 === void 0 ? [] : [value2], pick: onChange, multiple: false });
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
  const id2 = useId2();
  const listId = `${id2}-list`;
  const noteId = `${id2}-note`;
  const input2 = useRef3(null);
  const list = useRef3(null);
  const byValue = useMemo2(() => new Map(options.map((option) => [option.value, option])), [options]);
  const single = multiple ? void 0 : byValue.get(picked[0] ?? "");
  const [open, setOpen] = useState4(false);
  const [query, setQuery] = useState4("");
  const [active, setActive] = useState4(void 0);
  const shown = useMemo2(() => {
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
  useLayoutEffect3(() => {
    if (!open || marked === void 0) return;
    list.current?.ownerDocument.getElementById(optionId2(listId, marked))?.scrollIntoView({ block: "nearest" });
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
  return /* @__PURE__ */ jsxs2("div", { className: styles.field, children: [
    /* @__PURE__ */ jsx4("label", { htmlFor: id2, className: styles.label, children: label }),
    multiple && picked.length > 0 ? /* @__PURE__ */ jsx4("ul", { "aria-label": `${label}, picked`, className: "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0", children: picked.map((value2) => {
      const option = byValue.get(value2);
      const name2 = option?.label ?? value2;
      return /* @__PURE__ */ jsxs2(
        "li",
        {
          className: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-full bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:text-xs",
          children: [
            /* @__PURE__ */ jsx4("span", { children: name2 }),
            /* @__PURE__ */ jsx4(
              "button",
              {
                type: "button",
                className: `${classes.quiet} bc:rounded-full bc:px-1`,
                "aria-label": `Remove ${name2}`,
                onClick: () => {
                  unpick?.(value2);
                  input2.current?.focus();
                },
                children: /* @__PURE__ */ jsx4("span", { "aria-hidden": "true", children: "\xD7" })
              }
            )
          ]
        },
        value2
      );
    }) }) : null,
    /* @__PURE__ */ jsxs2("div", { className: "bc:relative bc:grid", children: [
      /* @__PURE__ */ jsx4(
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
          "aria-activedescendant": open && marked !== void 0 ? optionId2(listId, marked) : void 0,
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
      /* @__PURE__ */ jsx4(
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
          children: /* @__PURE__ */ jsx4("span", { "aria-hidden": "true", children: "\u25BE" })
        }
      ),
      open ? /* @__PURE__ */ jsxs2(
        "ul",
        {
          ref: list,
          id: listId,
          role: "listbox",
          "aria-label": label,
          "aria-multiselectable": multiple || void 0,
          className: `${classes.surface} bc:absolute bc:inset-x-0 bc:top-full bc:z-30 bc:m-0 bc:mt-1 bc:max-h-72 bc:list-none bc:overflow-y-auto bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:shadow-lg`,
          children: [
            shown.length === 0 ? /* @__PURE__ */ jsx4("li", { className: `${styles.hint} bc:px-2 bc:py-1`, children: empty }) : null,
            groups.map(
              ([group, members]) => group === void 0 ? members.map((option) => /* @__PURE__ */ jsx4(Option, { option, id: optionId2(listId, option.value), marked: option.value === marked, picked: picked.includes(option.value), choose }, option.value)) : /* @__PURE__ */ jsxs2("li", { role: "presentation", children: [
                /* @__PURE__ */ jsx4(
                  "div",
                  {
                    id: groupId(listId, group),
                    className: "bc:px-2 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted",
                    children: group
                  }
                ),
                /* @__PURE__ */ jsx4("ul", { role: "group", "aria-labelledby": groupId(listId, group), className: "bc:m-0 bc:list-none bc:p-0", children: members.map((option) => /* @__PURE__ */ jsx4(Option, { option, id: optionId2(listId, option.value), marked: option.value === marked, picked: picked.includes(option.value), choose }, option.value)) })
              ] }, group)
            )
          ]
        }
      ) : null
    ] }),
    note === void 0 ? null : /* @__PURE__ */ jsx4("p", { id: noteId, className: error === void 0 ? styles.hint : styles.alert, children: note })
  ] });
}
function Option({
  option,
  id: id2,
  marked,
  picked,
  choose
}) {
  return /* @__PURE__ */ jsxs2(
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
        /* @__PURE__ */ jsxs2("span", { className: "bc:flex bc:items-baseline bc:gap-2", children: [
          /* @__PURE__ */ jsxs2("span", { className: `bc:min-w-0 bc:flex-1 ${picked ? "bc:font-semibold" : "bc:font-medium"}`, children: [
            picked ? /* @__PURE__ */ jsx4("span", { "aria-hidden": "true", children: "\u2713 " }) : null,
            option.label
          ] }),
          option.badge === void 0 ? null : /* @__PURE__ */ jsx4("span", { className: "bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:font-normal bc:text-text-muted", children: option.badge })
        ] }),
        option.detail === void 0 || option.detail === "" ? null : /* @__PURE__ */ jsx4("span", { className: "bc:text-xs bc:text-text-muted", children: option.detail })
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
function optionId2(list, value2) {
  return `${list}-option-${safe(value2)}`;
}
function groupId(list, group) {
  return `${list}-group-${safe(group)}`;
}
function safe(text) {
  return text.replace(/[^A-Za-z0-9_-]/g, (c) => `_${c.charCodeAt(0).toString(16)}`);
}

// srcts/ui/edits.ts
function runCommand(core, id2, args) {
  return core.commands.run(id2, args).then(
    () => true,
    () => false
  );
}

// srcts/ui/fields.tsx
import { useId as useId3 } from "react";
import { jsx as jsx5, jsxs as jsxs3 } from "react/jsx-runtime";
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
  const id2 = useId3();
  const noteId = `${id2}-note`;
  const note = error ?? hint;
  return /* @__PURE__ */ jsxs3("div", { className: styles.field, children: [
    /* @__PURE__ */ jsx5("label", { htmlFor: id2, className: styles.label, children: label }),
    /* @__PURE__ */ jsx5(
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
    note === void 0 ? null : /* @__PURE__ */ jsx5("p", { id: noteId, className: error === void 0 ? styles.hint : styles.alert, children: note })
  ] });
}

// srcts/ui/hooks.ts
import { useCallback as useCallback3, useSyncExternalStore as useSyncExternalStore2 } from "react";
function useBoard(core) {
  const subscribe = useStore(core);
  return useSyncExternalStore2(subscribe, () => core.store.board());
}
function useHistory(core) {
  const subscribe = useStore(core);
  const canUndo = useSyncExternalStore2(subscribe, () => core.store.canUndo());
  const canRedo = useSyncExternalStore2(subscribe, () => core.store.canRedo());
  return { canUndo, canRedo };
}
function useStore(core) {
  return useCallback3(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
}

// srcts/ui/table.tsx
import { useState as useState5 } from "react";
import { jsx as jsx6, jsxs as jsxs4 } from "react/jsx-runtime";

// srcts/picker/picker.tsx
import { jsx as jsx7, jsxs as jsxs5 } from "react/jsx-runtime";
function BlockPicker({ core, mode, run, onAdded, autofocus = false }) {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const [picked, setPicked] = useState6(void 0);
  const [input2, setInput] = useState6(void 0);
  const [inputName, setInputName] = useState6("");
  const [id2, setId] = useState6("");
  const [name2, setName] = useState6("");
  const [errors, setErrors] = useState6({});
  const [busy, setBusy] = useState6(false);
  const entries = useMemo3(
    () => Object.values(registry).filter((entry2) => mode.kind !== "after" || entry2.inputs.length > 0),
    [registry, mode.kind]
  );
  const options = useMemo3(() => entries.map(classOption).sort(byGroupAndLabel), [entries]);
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
  return /* @__PURE__ */ jsxs5(
    "form",
    {
      "aria-label": "Add block",
      noValidate: true,
      className: "bc:grid bc:gap-3",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        /* @__PURE__ */ jsx7(
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
        link.kind === "pick" ? /* @__PURE__ */ jsxs5("label", { className: styles.field, children: [
          /* @__PURE__ */ jsx7("span", { className: styles.label, children: link.label }),
          /* @__PURE__ */ jsx7(
            "select",
            {
              className: styles.box,
              value: chosen,
              onChange: (event) => {
                setInput(event.target.value);
              },
              children: link.inputs.map((option) => /* @__PURE__ */ jsx7("option", { value: option, children: option }, option))
            }
          )
        ] }) : null,
        link.kind === "name" ? /* @__PURE__ */ jsx7(
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
        link.kind === "none" && mode.kind === "before" ? /* @__PURE__ */ jsx7("p", { className: styles.alert, children: link.why }) : null,
        /* @__PURE__ */ jsxs5("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx7(
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
          /* @__PURE__ */ jsx7(
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
        /* @__PURE__ */ jsx7("div", { children: /* @__PURE__ */ jsx7("button", { type: "submit", className: classes.primary, disabled: busy || link.kind === "none" && mode.kind === "before", children: "Add block" }) })
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

// srcts/modules/basic/card.tsx
import { memo, useCallback as useCallback4, useLayoutEffect as useLayoutEffect5, useRef as useRef5, useState as useState8 } from "react";

// srcts/modules/basic/context.ts
import { createContext, useContext } from "react";
var FrontEndContext = createContext(null);
function useFrontEnd() {
  const given = useContext(FrontEndContext);
  if (given === null) throw new Error("A component of the basic frontend is drawn outside of it.");
  return given;
}

// srcts/modules/basic/ui.tsx
import { useEffect, useLayoutEffect as useLayoutEffect4, useRef as useRef4, useState as useState7 } from "react";
import { jsx as jsx8, jsxs as jsxs6 } from "react/jsx-runtime";
function Dialog({
  title,
  onClose,
  children
}) {
  const { toasts } = useFrontEnd();
  const ref = useRef4(null);
  const toastsRef = useRef4(null);
  useLayoutEffect4(() => {
    const dialog = ref.current;
    const inside = toastsRef.current;
    if (dialog === null || inside === null) return;
    if (!dialog.open) dialog.showModal();
    dialog.querySelector("[data-autofocus]")?.focus();
    const back = toasts.showIn(inside);
    return () => {
      back();
      if (dialog.open) dialog.close();
    };
  }, [toasts]);
  return /* @__PURE__ */ jsxs6(
    "dialog",
    {
      ref,
      "aria-label": title,
      onClose,
      className: `${classes.surface} bc:w-full bc:max-w-xl bc:rounded-lg bc:border bc:border-border-default bc:p-4 bc:font-sans bc:shadow-xl bc:backdrop:bg-black/40`,
      children: [
        /* @__PURE__ */ jsxs6("div", { className: "bc:grid bc:gap-3", children: [
          /* @__PURE__ */ jsxs6("div", { className: "bc:flex bc:items-center bc:justify-between bc:gap-2", children: [
            /* @__PURE__ */ jsx8("h2", { className: classes.heading, children: title }),
            /* @__PURE__ */ jsx8("button", { type: "button", className: classes.button, onClick: onClose, children: "Close" })
          ] }),
          children
        ] }),
        /* @__PURE__ */ jsx8("div", { ref: toastsRef })
      ]
    }
  );
}
var menuClasses = {
  list: `${classes.surface} bc:absolute bc:right-0 bc:z-20 bc:m-0 bc:mt-1 bc:grid bc:min-w-44 bc:list-none bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:shadow-lg`,
  item: "bc:w-full bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50"
};
function useDismiss(ref, open, setOpen) {
  useEffect(() => {
    if (!open) return;
    const doc = ref.current?.ownerDocument ?? document;
    const away = (event) => {
      if (!(event.target instanceof Node && ref.current?.contains(event.target) === true)) setOpen(false);
    };
    const escape = (event) => {
      if (event.key === "Escape") setOpen(false);
    };
    doc.addEventListener("pointerdown", away);
    doc.addEventListener("keydown", escape);
    return () => {
      doc.removeEventListener("pointerdown", away);
      doc.removeEventListener("keydown", escape);
    };
  }, [ref, open, setOpen]);
}
function Menu({ label, items }) {
  const [open, setOpen] = useState7(false);
  const ref = useRef4(null);
  useDismiss(ref, open, setOpen);
  return /* @__PURE__ */ jsxs6("div", { ref, className: "bc:relative", children: [
    /* @__PURE__ */ jsx8(
      "button",
      {
        type: "button",
        "aria-label": label,
        "aria-haspopup": "menu",
        "aria-expanded": open,
        className: classes.quiet,
        onClick: () => {
          setOpen(!open);
        },
        children: /* @__PURE__ */ jsx8("span", { "aria-hidden": "true", className: "bc:text-base bc:leading-none", children: "\u22EF" })
      }
    ),
    open ? /* @__PURE__ */ jsx8("ul", { role: "menu", "aria-label": label, className: menuClasses.list, children: items.map((item) => /* @__PURE__ */ jsx8("li", { role: "none", children: /* @__PURE__ */ jsx8(
      "button",
      {
        type: "button",
        role: "menuitem",
        disabled: item.disabled,
        className: menuClasses.item,
        onClick: () => {
          setOpen(false);
          item.onSelect();
        },
        children: item.label
      }
    ) }, item.label)) }) : null
  ] });
}

// srcts/modules/basic/card.tsx
import { jsx as jsx9, jsxs as jsxs7 } from "react/jsx-runtime";
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
  const [renaming, setRenaming] = useState8(false);
  const ref = useRef5(null);
  useLayoutEffect5(() => {
    const el = ref.current;
    return el === null ? void 0 : inView2.watch(id2, el);
  }, [inView2, id2]);
  const mountUi = useCallback4((el) => core.mountBlock(el, id2), [core, id2]);
  const mountOutput = useCallback4((el) => core.mountOutput(el, id2), [core, id2]);
  const focus = useCallback4(() => {
    if (core.focus.get() !== id2) core.focus.set(id2);
  }, [core, id2]);
  const locked2 = core.lock.board;
  const inputs = !core.store.locked(id2);
  const entry = own(core.blockRegistry(), block.registry_id);
  const ready = status?.status === "ready";
  return /* @__PURE__ */ jsxs7(
    "article",
    {
      ref,
      "aria-label": block.name,
      className: `${classes.card} bc:grid bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:p-3`,
      onPointerDown: focus,
      onFocus: focus,
      children: [
        /* @__PURE__ */ jsxs7("header", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
          renaming ? /* @__PURE__ */ jsx9(
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
          ) : /* @__PURE__ */ jsx9("h3", { className: classes.heading, children: block.name }),
          /* @__PURE__ */ jsx9("span", { className: "bc:text-xs bc:text-text-muted", children: entry?.name ?? block.registry_id }),
          /* @__PURE__ */ jsx9(StatusBadge, { status }),
          locked2 ? null : /* @__PURE__ */ jsx9("div", { className: "bc:ml-auto bc:flex bc:items-center bc:gap-1", children: /* @__PURE__ */ jsx9(
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
        inputs ? /* @__PURE__ */ jsx9(Mounted, { mount: mountUi, label: "Inputs", className: "bc:min-w-0" }) : null,
        /* @__PURE__ */ jsxs7(
          "div",
          {
            className: `bc:min-w-0 ${sized ? "bc:flex bc:h-96 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`,
            children: [
              /* @__PURE__ */ jsx9(
                Mounted,
                {
                  mount: mountOutput,
                  label: "Output",
                  data: { "data-ready": String(ready) },
                  className: `bc:min-w-0 bc:transition-opacity ${ready ? "" : "bc:opacity-50 bc:grayscale"} ${sized ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"}`
                }
              ),
              sized ? /* @__PURE__ */ jsx9(Footer, { ...footer }) : null
            ]
          }
        ),
        /* @__PURE__ */ jsx9(Pills, { ...footer }),
        /* @__PURE__ */ jsx9(Banners, { ...footer })
      ]
    }
  );
});
function StackGroup({ stack, children }) {
  const color2 = stack.color ?? void 0;
  return /* @__PURE__ */ jsx9(
    "section",
    {
      "aria-label": stack.name,
      className: "bc:rounded-xl bc:border-2 bc:border-border-default bc:p-3",
      style: color2 === void 0 ? void 0 : { borderColor: color2 },
      children: /* @__PURE__ */ jsxs7("details", { open: true, children: [
        /* @__PURE__ */ jsx9("summary", { className: `${classes.heading} bc:cursor-pointer`, children: stack.name }),
        /* @__PURE__ */ jsx9("div", { className: "bc:mt-3 bc:grid bc:gap-3", children })
      ] })
    }
  );
}
var TONES = {
  ready: "bc:text-text-success",
  failed: "bc:text-text-danger",
  invalid: "bc:text-text-warning",
  waiting: "bc:text-text-warning",
  stale: "bc:text-text-muted",
  unevaluated: "bc:text-text-muted"
};
function StatusBadge({ status }) {
  if (status === void 0) return null;
  return /* @__PURE__ */ jsxs7(
    "span",
    {
      "data-status": status.status,
      "aria-busy": status.running,
      className: `bc:inline-flex bc:items-center bc:gap-1 bc:rounded-full bc:border bc:border-border-default bc:px-2 bc:py-0.5 bc:text-xs ${TONES[status.status]}`,
      children: [
        /* @__PURE__ */ jsx9("span", { "aria-hidden": "true", className: `bc:size-2 bc:rounded-full bc:bg-current ${status.running ? "bc:animate-pulse" : ""}` }),
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
import { useCallback as useCallback5 } from "react";
import { jsx as jsx10, jsxs as jsxs8 } from "react/jsx-runtime";
function OptionsDialog({ onClose }) {
  const { core } = useFrontEnd();
  const board = useBoard(core);
  const registered = core.options.registered();
  const categories = /* @__PURE__ */ new Map();
  for (const id2 of Object.keys(board.options)) {
    const def = Object.hasOwn(registered, id2) ? registered[id2] : void 0;
    if (def === void 0) continue;
    categories.set(def.category, [...categories.get(def.category) ?? [], id2]);
  }
  return /* @__PURE__ */ jsxs8(Dialog, { title: "Board options", onClose, children: [
    categories.size === 0 ? /* @__PURE__ */ jsx10("p", { className: styles.hint, children: "The board holds no board options." }) : null,
    [...categories].map(([category, ids]) => /* @__PURE__ */ jsxs8("section", { "aria-label": category, className: "bc:grid bc:gap-2", children: [
      /* @__PURE__ */ jsx10("h3", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: category }),
      ids.map((id2) => /* @__PURE__ */ jsx10(OptionUi, { id: id2 }, id2))
    ] }, category))
  ] });
}
function OptionUi({ id: id2 }) {
  const { core } = useFrontEnd();
  const mount = useCallback5((el) => core.mountOption(el, id2), [core, id2]);
  return /* @__PURE__ */ jsx10(Mounted, { mount });
}

// srcts/modules/basic/plugins.tsx
import { useCallback as useCallback6 } from "react";
import { jsx as jsx11, jsxs as jsxs9 } from "react/jsx-runtime";
function placed(core) {
  return Object.entries(core.plugins.appPlugins()).filter(([id2]) => core.plugins.pluginClass(id2).ui !== void 0);
}
function Plugins() {
  const { core } = useFrontEnd();
  const shown = placed(core);
  if (shown.length === 0) return null;
  return /* @__PURE__ */ jsx11(
    "aside",
    {
      "aria-label": "Plugins",
      className: "bc:grid bc:min-w-0 bc:content-start bc:gap-4 bc:md:sticky bc:md:top-4 bc:md:max-h-[calc(100dvh-2rem)] bc:md:w-1/3 bc:md:min-w-md bc:md:shrink-0 bc:md:overflow-auto",
      children: shown.map(([id2, plugin]) => /* @__PURE__ */ jsx11(PluginSection, { id: id2, plugin }, id2))
    }
  );
}
function PluginSection({ id: id2, plugin }) {
  const { core } = useFrontEnd();
  const mount = useCallback6((el) => core.mountPlugin(el, id2), [core, id2]);
  return /* @__PURE__ */ jsxs9("details", { open: true, className: `${classes.card} bc:min-w-0 bc:rounded-lg bc:border bc:border-border-default bc:p-3`, children: [
    /* @__PURE__ */ jsx11("summary", { className: `${classes.heading} bc:cursor-pointer`, children: plugin.name }),
    /* @__PURE__ */ jsx11(Mounted, { mount, className: "bc:mt-3 bc:min-w-0" })
  ] });
}

// srcts/modules/basic/toasts.ts
import { useSyncExternalStore as useSyncExternalStore3 } from "react";
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
  return useSyncExternalStore3(toasts.subscribe, toasts.list);
}
function useShownIn(toasts) {
  return useSyncExternalStore3(toasts.subscribe, toasts.shownIn);
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
import { useEffect as useEffect2, useReducer, useRef as useRef6, useState as useState9 } from "react";

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
      controls: [...controls.filter((c) => c.placement !== "end"), ...controls.filter((c) => c.placement === "end")],
      failed: false,
      ...twice.size === 0 ? {} : { problem: `Storage controls of one list share the ids ${quoted([...twice])}, so only the first of each draws.` }
    };
  } catch (error) {
    return { controls: [], failed: true, problem: `The storage controls failed: ${errorMessage(error)}` };
  }
}
function buttonsOf(controls) {
  return controls.flatMap((control) => {
    if (control.kind === "button") return [control];
    if (control.kind === "menu") return buttonsOf(control.items);
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
    const button = buttonsOf(drawnControls(core)?.controls ?? []).find(
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
import { Fragment as Fragment2, jsx as jsx12, jsxs as jsxs10 } from "react/jsx-runtime";
function Toolbar({ board, showOptions }) {
  const { core } = useFrontEnd();
  const { canUndo, canRedo } = useHistory(core);
  const [renaming, setRenaming] = useState9(false);
  const locked2 = core.lock.board;
  function step(back) {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch {
    }
  }
  return /* @__PURE__ */ jsxs10("header", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:border-b bc:border-border-default bc:pb-3", children: [
    renaming ? /* @__PURE__ */ jsx12(
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
    ) : /* @__PURE__ */ jsx12("h1", { className: "bc:m-0 bc:font-sans bc:text-lg bc:font-semibold", children: board.name }),
    locked2 || renaming ? null : /* @__PURE__ */ jsx12(
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
    /* @__PURE__ */ jsxs10("div", { role: "toolbar", "aria-label": "Board", className: "bc:ml-auto bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
      /* @__PURE__ */ jsx12(
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
      /* @__PURE__ */ jsx12(
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
      /* @__PURE__ */ jsx12(
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
      /* @__PURE__ */ jsx12(StorageControls, {})
    ] })
  ] });
}
function StorageControls() {
  const { core } = useFrontEnd();
  const storage = core.plugins.storage();
  const [, redraw] = useReducer((n) => n + 1, 0);
  useEffect2(() => {
    if (storage?.subscribe === void 0) return void 0;
    try {
      return storage.subscribe(redraw);
    } catch (error) {
      core.log("blockr.core").error(asError(error));
      return void 0;
    }
  }, [core, storage]);
  const drawn = drawnControls(core);
  const problem = drawn?.problem;
  useEffect2(() => {
    if (problem !== void 0) core.log("blockr.core").error(problem);
  }, [core, problem]);
  if (drawn === void 0) return null;
  if (drawn.failed) {
    return /* @__PURE__ */ jsx12("span", { role: "alert", title: problem, className: "bc:font-sans bc:text-sm bc:text-text-danger", children: "Storage failed" });
  }
  return /* @__PURE__ */ jsx12(Fragment2, { children: drawn.controls.map((control) => /* @__PURE__ */ jsx12(Control, { control }, control.id)) });
}
function Control({ control }) {
  switch (control.kind) {
    case "button":
      return /* @__PURE__ */ jsx12(StorageButton, { button: control, className: classes.button });
    case "menu":
      return /* @__PURE__ */ jsx12(StorageMenu, { label: control.label, items: control.items });
    case "status":
      return /* @__PURE__ */ jsx12("span", { role: "status", title: control.tooltip, className: "bc:font-sans bc:text-sm bc:text-text-muted", children: control.text });
  }
}
function StorageButton({
  button,
  className,
  role,
  before
}) {
  const { core } = useFrontEnd();
  return /* @__PURE__ */ jsx12(
    "button",
    {
      type: "button",
      role,
      className,
      disabled: button.disabled,
      title: button.tooltip,
      "aria-keyshortcuts": ariaShortcuts(button.shortcut),
      onClick: () => {
        before?.();
        void runButton(core, button);
      },
      children: button.label
    }
  );
}
function StorageMenu({ label, items }) {
  const [open, setOpen] = useState9(false);
  const ref = useRef6(null);
  useDismiss(ref, open, setOpen);
  return /* @__PURE__ */ jsxs10("div", { ref, className: "bc:relative", children: [
    /* @__PURE__ */ jsxs10(
      "button",
      {
        type: "button",
        "aria-haspopup": "menu",
        "aria-expanded": open,
        className: classes.button,
        onClick: () => {
          setOpen(!open);
        },
        children: [
          label,
          /* @__PURE__ */ jsx12("span", { "aria-hidden": "true", children: "\u25BE" })
        ]
      }
    ),
    open ? /* @__PURE__ */ jsx12("ul", { role: "menu", "aria-label": label, className: menuClasses.list, children: /* @__PURE__ */ jsx12(
      MenuItems,
      {
        items,
        close: () => {
          setOpen(false);
        }
      }
    ) }) : null
  ] });
}
function MenuItems({ items, close }) {
  return /* @__PURE__ */ jsx12(Fragment2, { children: items.map((item) => {
    switch (item.kind) {
      case "button":
        return /* @__PURE__ */ jsx12("li", { role: "none", children: /* @__PURE__ */ jsx12(StorageButton, { button: item, className: menuClasses.item, role: "menuitem", before: close }) }, item.id);
      case "menu":
        return /* @__PURE__ */ jsx12("li", { role: "none", children: /* @__PURE__ */ jsxs10("div", { role: "group", "aria-label": item.label, children: [
          /* @__PURE__ */ jsx12("span", { "aria-hidden": "true", className: "bc:block bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted", children: item.label }),
          /* @__PURE__ */ jsx12("ul", { role: "none", className: "bc:m-0 bc:grid bc:list-none bc:gap-0.5 bc:p-0 bc:pl-2", children: /* @__PURE__ */ jsx12(MenuItems, { items: item.items, close }) })
        ] }) }, item.id);
      case "status":
        return /* @__PURE__ */ jsx12("li", { role: "none", title: item.tooltip, className: "bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-muted", children: item.text }, item.id);
    }
  }) });
}

// srcts/modules/basic/board.tsx
import { jsx as jsx13, jsxs as jsxs11 } from "react/jsx-runtime";
function BasicBoard({ front }) {
  return /* @__PURE__ */ jsx13(FrontEndContext, { value: front, children: /* @__PURE__ */ jsx13(Page, {}) });
}
function Page() {
  const { core } = useFrontEnd();
  const board = useBoard(core);
  const items = useMemo4(() => layoutItems(board), [board]);
  const shown = board.options.show_conditions;
  const severities = useMemo4(() => asSeverities(shown), [shown]);
  const [dialog, setDialog] = useState10(null);
  const add = useCallback7((mode) => {
    setDialog({ kind: "add", mode });
  }, []);
  const close = useCallback7(() => {
    setDialog(null);
  }, []);
  const card = (id2) => {
    const block = board.blocks[id2];
    if (block === void 0) return null;
    return /* @__PURE__ */ jsx13(
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
  return /* @__PURE__ */ jsxs11("div", { className: `${classes.page} bc:flex bc:min-h-full bc:flex-col bc:gap-4 bc:p-4 bc:font-sans`, children: [
    /* @__PURE__ */ jsx13(
      Toolbar,
      {
        board,
        showOptions: () => {
          setDialog({ kind: "options" });
        }
      }
    ),
    /* @__PURE__ */ jsxs11("div", { className: "bc:flex bc:flex-col bc:gap-4 bc:md:flex-row-reverse bc:md:items-start", children: [
      /* @__PURE__ */ jsx13(Plugins, {}),
      /* @__PURE__ */ jsxs11("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-4", children: [
        items.length === 0 ? /* @__PURE__ */ jsx13("p", { className: styles.hint, children: "The board holds no blocks yet." }) : null,
        items.map((item) => {
          if ("block" in item) return card(item.block);
          const stack = board.stacks[item.stack];
          if (stack === void 0) return null;
          return /* @__PURE__ */ jsx13(StackGroup, { stack, children: item.blocks.length === 0 ? /* @__PURE__ */ jsx13("p", { className: styles.hint, children: "The stack holds no blocks." }) : item.blocks.map(card) }, `stack-${item.stack}`);
        })
      ] })
    ] }),
    dialog?.kind === "add" ? /* @__PURE__ */ jsx13(AddBlockDialog, { mode: dialog.mode, onClose: close }) : null,
    dialog?.kind === "options" ? /* @__PURE__ */ jsx13(OptionsDialog, { onClose: close }) : null,
    /* @__PURE__ */ jsx13(Toasts, {})
  ] });
}
function AddBlockDialog({ mode, onClose }) {
  const front = useFrontEnd();
  const board = useBoard(front.core);
  const target2 = mode.kind === "add" ? void 0 : own(board.blocks, mode.kind === "after" ? mode.from : mode.to);
  const title = target2 === void 0 ? "Add block" : `Add block ${mode.kind} "${target2.name}"`;
  const run = useCallback7((command, args) => runCommand(front.core, command, args), [front.core]);
  return /* @__PURE__ */ jsx13(Dialog, { title, onClose, children: /* @__PURE__ */ jsx13(BlockPicker, { core: front.core, mode, run, onAdded: onClose, autofocus: true }) });
}
function Toasts() {
  const { core, toasts } = useFrontEnd();
  const list = useToasts(toasts);
  const inside = useShownIn(toasts);
  const board = useBoard(core);
  if (list.length === 0) return null;
  const shown = /* @__PURE__ */ jsx13(
    "section",
    {
      "aria-label": "Notifications",
      className: "bc:pointer-events-none bc:fixed bc:right-2 bc:bottom-2 bc:z-50 bc:m-0 bc:box-border bc:grid bc:max-h-[calc(100dvh-1rem)] bc:w-[21rem] bc:max-w-[calc(100vw-1rem)] bc:gap-2 bc:overflow-y-auto bc:p-2 bc:font-sans",
      children: list.map((toast) => /* @__PURE__ */ jsx13(ToastItem, { toast, title: concerned(toast.notification.about, core, board) }, toast.notification.id))
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
  useEffect3(
    () => () => {
      toasts.release(id2, "pointer");
      toasts.release(id2, "focus");
    },
    [toasts, id2]
  );
  return /* @__PURE__ */ jsxs11(
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
        /* @__PURE__ */ jsx13("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx13(SeverityIcon, { severity }) }),
        /* @__PURE__ */ jsxs11("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-1", children: [
          /* @__PURE__ */ jsxs11("p", { className: "bc:m-0 bc:text-xs bc:font-semibold", children: [
            title,
            pkg === void 0 ? null : /* @__PURE__ */ jsxs11("span", { className: "bc:font-normal bc:text-text-muted", children: [
              " \xB7 ",
              pkg
            ] })
          ] }),
          /* @__PURE__ */ jsx13("p", { className: "bc:m-0 bc:text-sm bc:break-words bc:whitespace-pre-wrap", children: notification.message }),
          isReload(notification) ? /* @__PURE__ */ jsx13("div", { children: /* @__PURE__ */ jsx13(
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
        /* @__PURE__ */ jsx13(
          "button",
          {
            type: "button",
            "aria-label": "Close",
            className: classes.quiet,
            onClick: () => {
              toasts.close(id2);
            },
            children: /* @__PURE__ */ jsx13("span", { "aria-hidden": "true", children: "\xD7" })
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
    near.set([...new Set([...seen].flatMap((el) => watched.get(el) ?? []))]);
  }
  const Observer = view.IntersectionObserver;
  const observer = Observer === void 0 ? void 0 : new Observer((entries) => {
    for (const entry of entries) {
      if (!watched.has(entry.target)) continue;
      if (entry.isIntersecting) seen.add(entry.target);
      else seen.delete(entry.target);
    }
    update();
  });
  return {
    watch(block, el) {
      watched.set(el, block);
      observer?.observe(el);
      return () => {
        observer?.unobserve(el);
        watched.delete(el);
        if (seen.delete(el)) update();
      };
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
import { jsx as jsx14 } from "react/jsx-runtime";
var api = 1;
var basic_default = {
  options: ["show_conditions", "dark_mode"],
  frontend: { render }
};
function render({ el, core }) {
  const doc = el.ownerDocument;
  const shown = demandShown(core, doc.defaultView ?? window);
  const toasts = createToasts(core.notifications);
  const front = { core, toasts, inView: shown };
  const root = createRoot(el);
  flushSync2(() => {
    root.render(/* @__PURE__ */ jsx14(BasicBoard, { front }));
  });
  shown.measure();
  const unbind = bindShortcuts(doc, core);
  return () => {
    unbind();
    toasts.stop();
    shown.release();
    root.unmount();
  };
}
export {
  api,
  basic_default as default
};

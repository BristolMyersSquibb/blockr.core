// srcts/modules/basic/index.tsx
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";

// srcts/modules/basic/board.tsx
import { useCallback as useCallback4, useMemo as useMemo2, useState as useState7 } from "react";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function messageOf(error) {
  return error instanceof Error ? error.message : String(error);
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
var pluginId = {
  ...id,
  description: "The id of a plugin, unique among the board's plugins."
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

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:font-sans bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};

// srcts/modules/basic/card.tsx
import { memo, useCallback as useCallback2, useLayoutEffect as useLayoutEffect2, useRef as useRef2, useState as useState2 } from "react";

// srcts/modules/basic/context.ts
import { createContext, useContext } from "react";
var FrontEndContext = createContext(null);
function useFrontEnd() {
  const given = useContext(FrontEndContext);
  if (given === null) throw new Error("A component of the basic front-end is drawn outside of it.");
  return given;
}
function runCommand(front, id2, args) {
  return front.core.commands.run(id2, args).then(
    () => true,
    (error) => {
      front.notices.refused(error);
      return false;
    }
  );
}

// srcts/modules/basic/hooks.ts
import { useCallback, useSyncExternalStore } from "react";
function useBoard(core) {
  const subscribe = useStore(core);
  return useSyncExternalStore(subscribe, () => core.store.board());
}
function useHistory(core) {
  const subscribe = useStore(core);
  const canUndo = useSyncExternalStore(subscribe, () => core.store.canUndo());
  const canRedo = useSyncExternalStore(subscribe, () => core.store.canRedo());
  return { canUndo, canRedo };
}
function useStatus(core, block) {
  const subscribe = useCallback(
    (notify) => core.scheduler.subscribe((changed) => {
      if (changed.includes(block)) notify();
    }),
    [core, block]
  );
  return useSyncExternalStore(subscribe, () => core.scheduler.status(block));
}
function useNotices(notices) {
  return useSyncExternalStore(notices.subscribe, notices.list);
}
function useStore(core) {
  return useCallback(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
}

// srcts/modules/basic/ui.tsx
import { useEffect, useLayoutEffect, useRef, useState } from "react";
import { jsx, jsxs } from "react/jsx-runtime";
var classes = {
  button: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50",
  primary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-accent bc:bg-bg-accent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-on-accent bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle",
  // The board's page, which draws no background of its own, so that the
  // page's, which the browser draws in the page's theme, shows to its edges.
  page: "bc:text-gray-900 bc:dark:text-gray-100",
  // What sits over the board, as a dialog or a menu does.
  surface: "bc:bg-white bc:text-gray-900 bc:dark:bg-gray-900 bc:dark:text-gray-100",
  // Opaque, since core measures the background behind an output for the plots
  // that R draws, and reads a translucent one as opaque.
  card: "bc:bg-gray-50 bc:dark:bg-gray-800",
  heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold",
  list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0"
};
function Dialog({
  title,
  onClose,
  children
}) {
  const ref = useRef(null);
  useLayoutEffect(() => {
    const dialog = ref.current;
    if (dialog === null) return;
    if (!dialog.open) dialog.showModal();
    dialog.querySelector("[data-autofocus]")?.focus();
    return () => {
      if (dialog.open) dialog.close();
    };
  }, []);
  return /* @__PURE__ */ jsx(
    "dialog",
    {
      ref,
      "aria-label": title,
      onClose,
      className: `${classes.surface} bc:w-full bc:max-w-xl bc:rounded-lg bc:border bc:border-border-default bc:p-4 bc:font-sans bc:shadow-xl bc:backdrop:bg-black/40`,
      children: /* @__PURE__ */ jsxs("div", { className: "bc:grid bc:gap-3", children: [
        /* @__PURE__ */ jsxs("div", { className: "bc:flex bc:items-center bc:justify-between bc:gap-2", children: [
          /* @__PURE__ */ jsx("h2", { className: classes.heading, children: title }),
          /* @__PURE__ */ jsx("button", { type: "button", className: classes.button, onClick: onClose, children: "Close" })
        ] }),
        children
      ] })
    }
  );
}
function Menu({ label, items }) {
  const [open, setOpen] = useState(false);
  const ref = useRef(null);
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
  }, [open]);
  return /* @__PURE__ */ jsxs("div", { ref, className: "bc:relative", children: [
    /* @__PURE__ */ jsx(
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
        children: /* @__PURE__ */ jsx("span", { "aria-hidden": "true", className: "bc:text-base bc:leading-none", children: "\u22EF" })
      }
    ),
    open ? /* @__PURE__ */ jsx(
      "ul",
      {
        role: "menu",
        "aria-label": label,
        className: `${classes.surface} bc:absolute bc:right-0 bc:z-20 bc:m-0 bc:mt-1 bc:grid bc:min-w-44 bc:list-none bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:shadow-lg`,
        children: items.map((item) => /* @__PURE__ */ jsx("li", { role: "none", children: /* @__PURE__ */ jsx(
          "button",
          {
            type: "button",
            role: "menuitem",
            disabled: item.disabled,
            className: "bc:w-full bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50",
            onClick: () => {
              setOpen(false);
              item.onSelect();
            },
            children: item.label
          }
        ) }, item.label))
      }
    ) : null
  ] });
}
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
function Mounted({
  mount,
  label,
  className
}) {
  const ref = useRef(null);
  useEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const cleanup = mount(el);
    return () => {
      queueMicrotask(cleanup);
    };
  }, [mount]);
  return /* @__PURE__ */ jsx("div", { ref, "aria-label": label, className });
}

// srcts/modules/basic/card.tsx
import { jsx as jsx2, jsxs as jsxs2 } from "react/jsx-runtime";
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
  const [renaming, setRenaming] = useState2(false);
  const ref = useRef2(null);
  useLayoutEffect2(() => {
    const el = ref.current;
    return el === null ? void 0 : inView2.watch(id2, el);
  }, [inView2, id2]);
  const mountUi = useCallback2((el) => core.mountBlock(el, id2), [core, id2]);
  const mountOutput = useCallback2((el) => core.mountOutput(el, id2), [core, id2]);
  const { lock } = core;
  const inputs = lock === null || lock.open.includes(id2);
  const entry = own(core.blockRegistry(), block.registry_id);
  const ready = status?.status === "ready";
  const replied = status !== void 0 && status.evaluated !== null;
  return /* @__PURE__ */ jsxs2(
    "article",
    {
      ref,
      "aria-label": block.name,
      className: `${classes.card} bc:grid bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:p-3`,
      children: [
        /* @__PURE__ */ jsxs2("header", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
          renaming ? /* @__PURE__ */ jsx2(
            NameField,
            {
              label: "Block name",
              name: block.name,
              onSubmit: (name2) => {
                void runCommand(front, "blockr.core-rename_block", { id: id2, name: name2 });
              },
              onDone: () => {
                setRenaming(false);
              }
            }
          ) : /* @__PURE__ */ jsx2("h3", { className: classes.heading, children: block.name }),
          /* @__PURE__ */ jsx2("span", { className: "bc:text-xs bc:text-text-muted", children: entry?.name ?? block.registry_id }),
          /* @__PURE__ */ jsx2(StatusBadge, { status }),
          lock === null ? /* @__PURE__ */ jsx2("div", { className: "bc:ml-auto bc:flex bc:items-center bc:gap-1", children: /* @__PURE__ */ jsx2(
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
                    void runCommand(front, "blockr.core-remove_block", { id: id2 });
                  }
                }
              ]
            }
          ) }) : null
        ] }),
        inputs ? /* @__PURE__ */ jsx2(Mounted, { mount: mountUi, label: "Inputs", className: "bc:min-w-0" }) : null,
        /* @__PURE__ */ jsxs2(
          "div",
          {
            role: "region",
            "aria-label": "Output",
            "data-ready": ready,
            className: `bc:min-w-0 bc:transition-opacity ${ready ? "" : "bc:opacity-50 bc:grayscale"} ${replied ? "bc:flex bc:h-96 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`,
            children: [
              /* @__PURE__ */ jsx2(
                Mounted,
                {
                  mount: mountOutput,
                  className: replied ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"
                }
              ),
              replied ? /* @__PURE__ */ jsx2("p", { className: "bc:m-0 bc:min-h-4 bc:shrink-0 bc:px-5 bc:text-center bc:font-mono bc:text-xs bc:text-text-muted", children: status.summary }) : null
            ]
          }
        ),
        /* @__PURE__ */ jsx2(Conditions, { status, severities })
      ]
    }
  );
});
function StackGroup({ stack, children }) {
  const color2 = stack.color ?? void 0;
  return /* @__PURE__ */ jsxs2(
    "section",
    {
      "aria-label": stack.name,
      className: "bc:grid bc:gap-3 bc:rounded-xl bc:border-2 bc:border-border-default bc:p-3",
      style: color2 === void 0 ? void 0 : { borderColor: color2 },
      children: [
        /* @__PURE__ */ jsxs2("header", { className: "bc:flex bc:items-center bc:gap-2", children: [
          /* @__PURE__ */ jsx2(
            "span",
            {
              "aria-hidden": "true",
              className: "bc:size-3 bc:rounded-full bc:bg-current bc:text-text-muted",
              style: color2 === void 0 ? void 0 : { backgroundColor: color2 }
            }
          ),
          /* @__PURE__ */ jsx2("h2", { className: classes.heading, children: stack.name })
        ] }),
        children
      ]
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
  return /* @__PURE__ */ jsxs2(
    "span",
    {
      "data-status": status.status,
      "aria-busy": status.running,
      className: `bc:inline-flex bc:items-center bc:gap-1 bc:rounded-full bc:border bc:border-border-default bc:px-2 bc:py-0.5 bc:text-xs ${TONES[status.status]}`,
      children: [
        /* @__PURE__ */ jsx2("span", { "aria-hidden": "true", className: `bc:size-2 bc:rounded-full bc:bg-current ${status.running ? "bc:animate-pulse" : ""}` }),
        status.running ? `${status.status}, running` : status.status
      ]
    }
  );
}
var SEVERITY_TONES = {
  error: "bc:text-text-danger",
  warning: "bc:text-text-warning",
  message: "bc:text-text-muted"
};
function Conditions({ status, severities }) {
  if (status === void 0) return null;
  const shown = status.conditions.filter((condition) => severities.includes(condition.severity));
  const omitted = ["warning", "message"].filter((severity) => severities.includes(severity) && status.omitted[severity] > 0);
  if (shown.length === 0 && omitted.length === 0) return null;
  return /* @__PURE__ */ jsxs2("ul", { "aria-label": "Conditions", className: "bc:m-0 bc:grid bc:list-none bc:gap-1 bc:p-0 bc:text-sm", children: [
    shown.map((condition, i) => /* @__PURE__ */ jsxs2("li", { "data-severity": condition.severity, className: `bc:whitespace-pre-wrap ${SEVERITY_TONES[condition.severity]}`, children: [
      /* @__PURE__ */ jsxs2("span", { className: "bc:font-medium", children: [
        condition.severity,
        ":"
      ] }),
      " ",
      condition.message
    ] }, i)),
    omitted.map((severity) => /* @__PURE__ */ jsxs2("li", { className: "bc:text-text-muted", children: [
      String(status.omitted[severity]),
      " more ",
      severity === "warning" ? "warnings" : "messages",
      " not kept"
    ] }, `omitted-${severity}`))
  ] });
}

// srcts/modules/basic/layout.ts
function itemsOf(board) {
  const stackOf = /* @__PURE__ */ new Map();
  for (const [id2, stack] of Object.entries(board.stacks)) {
    for (const block of stack.blocks) stackOf.set(block, id2);
  }
  const items = [];
  const placed = /* @__PURE__ */ new Set();
  for (const block of Object.keys(board.blocks)) {
    const stack = stackOf.get(block);
    if (stack === void 0) {
      items.push({ block });
    } else if (!placed.has(stack)) {
      placed.add(stack);
      items.push({ stack, blocks: blocksOf(board, stack) });
    }
  }
  for (const stack of Object.keys(board.stacks)) {
    if (!placed.has(stack)) items.push({ stack, blocks: [] });
  }
  return items;
}
function blocksOf(board, stack) {
  return (board.stacks[stack]?.blocks ?? []).filter((block) => Object.hasOwn(board.blocks, block));
}

// srcts/modules/basic/links.tsx
import { useState as useState3 } from "react";
import { jsx as jsx3, jsxs as jsxs3 } from "react/jsx-runtime";
function LinksDialog({ onClose }) {
  const front = useFrontEnd();
  const { core } = front;
  const board = useBoard(core);
  const ids = Object.keys(board.blocks);
  const [from, setFrom] = useState3(ids[0] ?? "");
  const [to, setTo] = useState3(ids[1] ?? ids[0] ?? "");
  const [input2, setInput] = useState3(void 0);
  const free = freeInputs(board, core.blockRegistry(), to) ?? [];
  const variadic = free.length === 1 && free[0] === "";
  const chosen = variadic ? void 0 : input2 !== void 0 && free.includes(input2) ? input2 : free[0];
  const args = { from, to, ...chosen === void 0 ? {} : { input: chosen } };
  const links = Object.entries(board.links);
  return /* @__PURE__ */ jsxs3(Dialog, { title: "Links", onClose, children: [
    links.length === 0 ? /* @__PURE__ */ jsx3("p", { className: styles.hint, children: "No link joins two blocks yet." }) : /* @__PURE__ */ jsx3("ul", { "aria-label": "Links", className: classes.list, children: links.map(([id2, link]) => /* @__PURE__ */ jsxs3("li", { className: "bc:flex bc:items-center bc:gap-2 bc:text-sm", children: [
      /* @__PURE__ */ jsxs3("span", { className: "bc:flex-1", children: [
        nameOf(board, link.from),
        " \u2192 ",
        nameOf(board, link.to),
        link.input === "" ? null : /* @__PURE__ */ jsxs3("span", { className: "bc:text-text-muted", children: [
          " as ",
          link.input
        ] })
      ] }),
      /* @__PURE__ */ jsx3(
        "button",
        {
          type: "button",
          className: classes.button,
          "aria-label": `Remove the link from ${nameOf(board, link.from)} to ${nameOf(board, link.to)}`,
          onClick: () => {
            void runCommand(front, "blockr.core-remove_link", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] }, id2)) }),
    /* @__PURE__ */ jsxs3(
      "form",
      {
        "aria-label": "Add link",
        className: "bc:grid bc:gap-2 bc:border-t bc:border-border-default bc:pt-3",
        onSubmit: (event) => {
          event.preventDefault();
          setInput(void 0);
          void runCommand(front, "blockr.core-add_link", args);
        },
        children: [
          /* @__PURE__ */ jsxs3("div", { className: "bc:grid bc:grid-cols-2 bc:gap-2", children: [
            /* @__PURE__ */ jsx3(BlockSelect, { label: "From", board, value: from, onChange: setFrom }),
            /* @__PURE__ */ jsx3(
              BlockSelect,
              {
                label: "To",
                board,
                value: to,
                onChange: (next) => {
                  setTo(next);
                  setInput(void 0);
                }
              }
            )
          ] }),
          variadic ? null : /* @__PURE__ */ jsxs3("label", { className: styles.field, children: [
            /* @__PURE__ */ jsx3("span", { className: styles.label, children: "Input" }),
            /* @__PURE__ */ jsxs3(
              "select",
              {
                className: styles.box,
                value: chosen ?? "",
                disabled: free.length === 0,
                onChange: (event) => {
                  setInput(event.target.value);
                },
                children: [
                  free.length === 0 ? /* @__PURE__ */ jsx3("option", { value: "", children: "No free input" }) : null,
                  free.map((name2) => /* @__PURE__ */ jsx3("option", { value: name2, children: name2 }, name2))
                ]
              }
            )
          ] }),
          /* @__PURE__ */ jsx3("div", { children: /* @__PURE__ */ jsx3("button", { type: "submit", className: classes.primary, disabled: !core.commands.enabled("blockr.core-add_link", args), children: "Add link" }) })
        ]
      }
    )
  ] });
}
function BlockSelect({
  label,
  board,
  value: value2,
  onChange
}) {
  return /* @__PURE__ */ jsxs3("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx3("span", { className: styles.label, children: label }),
    /* @__PURE__ */ jsx3(
      "select",
      {
        className: styles.box,
        value: value2,
        onChange: (event) => {
          onChange(event.target.value);
        },
        children: Object.entries(board.blocks).map(([id2, block]) => /* @__PURE__ */ jsxs3("option", { value: id2, children: [
          block.name,
          " (",
          id2,
          ")"
        ] }, id2))
      }
    )
  ] });
}
function nameOf(board, block) {
  return own(board.blocks, block)?.name ?? block;
}

// srcts/modules/basic/options.tsx
import { useCallback as useCallback3 } from "react";
import { jsx as jsx4, jsxs as jsxs4 } from "react/jsx-runtime";
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
  return /* @__PURE__ */ jsxs4(Dialog, { title: "Board options", onClose, children: [
    categories.size === 0 ? /* @__PURE__ */ jsx4("p", { className: styles.hint, children: "The board holds no board options." }) : null,
    [...categories].map(([category, ids]) => /* @__PURE__ */ jsxs4("section", { "aria-label": category, className: "bc:grid bc:gap-2", children: [
      /* @__PURE__ */ jsx4("h3", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: category }),
      ids.map((id2) => /* @__PURE__ */ jsx4(OptionUi, { id: id2 }, id2))
    ] }, category))
  ] });
}
function OptionUi({ id: id2 }) {
  const { core } = useFrontEnd();
  const mount = useCallback3((el) => core.mountOption(el, id2), [core, id2]);
  return /* @__PURE__ */ jsx4(Mounted, { mount });
}

// srcts/modules/basic/picker.tsx
import { useMemo, useState as useState4 } from "react";
import { jsx as jsx5, jsxs as jsxs5 } from "react/jsx-runtime";
function AddBlockDialog({ mode, onClose }) {
  const front = useFrontEnd();
  const { core } = front;
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const [search, setSearch] = useState4("");
  const [picked, setPicked] = useState4(void 0);
  const [input2, setInput] = useState4(void 0);
  const entries = useMemo(
    () => Object.values(registry).filter((entry) => mode.kind !== "after" || entry.inputs.length > 0).filter((entry) => matches(entry, search)),
    [registry, mode.kind, search]
  );
  const categories = useMemo(() => byCategory(entries), [entries]);
  const target2 = mode.kind === "add" ? void 0 : own(board.blocks, mode.kind === "after" ? mode.from : mode.to);
  const title = target2 === void 0 ? "Add block" : `Add block ${mode.kind} "${target2.name}"`;
  const before = mode.kind === "before" ? several(freeInputs(board, registry, mode.to) ?? []) : [];
  const after = picked === void 0 ? [] : inputsOf(picked);
  function add(entry, into) {
    onClose();
    const link = into === void 0 ? {} : { input: into };
    if (mode.kind === "add") void runCommand(front, "blockr.core-add_block", { registry_id: entry.id });
    else if (mode.kind === "after") void runCommand(front, "blockr.core-append_block", { registry_id: entry.id, from: mode.from, ...link });
    else void runCommand(front, "blockr.core-prepend_block", { registry_id: entry.id, to: mode.to, ...link });
  }
  function pick(entry) {
    if (mode.kind === "after" && inputsOf(entry).length > 0) {
      setPicked(entry);
      setInput(void 0);
    } else {
      add(entry, before.length > 0 ? input2 ?? before[0] : void 0);
    }
  }
  return /* @__PURE__ */ jsxs5(Dialog, { title, onClose, children: [
    /* @__PURE__ */ jsxs5("label", { className: styles.field, children: [
      /* @__PURE__ */ jsx5("span", { className: styles.label, children: "Search" }),
      /* @__PURE__ */ jsx5(
        "input",
        {
          type: "search",
          "data-autofocus": true,
          className: styles.box,
          value: search,
          placeholder: "Name, description or keyword",
          onChange: (event) => {
            setSearch(event.target.value);
          }
        }
      )
    ] }),
    before.length > 0 ? /* @__PURE__ */ jsx5(InputSelect, { label: `Input of "${target2?.name ?? ""}"`, inputs: before, value: input2 ?? before[0], onChange: setInput }) : null,
    picked !== void 0 && after.length > 0 ? /* @__PURE__ */ jsxs5("div", { className: "bc:grid bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:p-2", children: [
      /* @__PURE__ */ jsx5(InputSelect, { label: `Input of the new ${picked.name}`, inputs: after, value: input2 ?? after[0], onChange: setInput }),
      /* @__PURE__ */ jsxs5("div", { className: "bc:flex bc:gap-2", children: [
        /* @__PURE__ */ jsxs5(
          "button",
          {
            type: "button",
            className: classes.primary,
            onClick: () => {
              add(picked, input2 ?? after[0]);
            },
            children: [
              "Add ",
              picked.name
            ]
          }
        ),
        /* @__PURE__ */ jsx5(
          "button",
          {
            type: "button",
            className: classes.button,
            onClick: () => {
              setPicked(void 0);
              setInput(void 0);
            },
            children: "Back"
          }
        )
      ] })
    ] }) : /* @__PURE__ */ jsxs5("div", { className: "bc:grid bc:max-h-96 bc:gap-3 bc:overflow-y-auto", children: [
      categories.length === 0 ? /* @__PURE__ */ jsx5("p", { className: styles.hint, children: "No block class matches." }) : null,
      categories.map(([category, members]) => /* @__PURE__ */ jsxs5("section", { "aria-label": category, className: "bc:grid bc:gap-1", children: [
        /* @__PURE__ */ jsx5("h3", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: category }),
        /* @__PURE__ */ jsx5("ul", { className: classes.list, children: members.map((entry) => /* @__PURE__ */ jsx5("li", { children: /* @__PURE__ */ jsxs5(
          "button",
          {
            type: "button",
            className: "bc:grid bc:w-full bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:p-2 bc:text-left bc:font-sans bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle",
            onClick: () => {
              pick(entry);
            },
            children: [
              /* @__PURE__ */ jsx5("span", { className: "bc:text-sm bc:font-medium", children: entry.name }),
              entry.description === "" ? null : /* @__PURE__ */ jsx5("span", { className: "bc:text-xs bc:text-text-muted", children: entry.description })
            ]
          }
        ) }, entry.id)) })
      ] }, category))
    ] })
  ] });
}
function InputSelect({
  label,
  inputs,
  value: value2,
  onChange
}) {
  return /* @__PURE__ */ jsxs5("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx5("span", { className: styles.label, children: label }),
    /* @__PURE__ */ jsx5(
      "select",
      {
        className: styles.box,
        value: value2,
        onChange: (event) => {
          onChange(event.target.value);
        },
        children: inputs.map((input2) => /* @__PURE__ */ jsx5("option", { value: input2, children: input2 }, input2))
      }
    )
  ] });
}
function inputsOf(entry) {
  return entry.inputs.includes("...") ? [] : several(entry.inputs);
}
function several(inputs) {
  return inputs.length > 1 ? inputs : [];
}
function matches(entry, text) {
  const needle = text.trim().toLowerCase();
  if (needle === "") return true;
  return [entry.name, entry.description, entry.id, ...entry.keywords].some((field) => field.toLowerCase().includes(needle));
}
function byCategory(entries) {
  const groups = /* @__PURE__ */ new Map();
  for (const entry of entries) {
    const category = entry.category === "" ? "Other" : entry.category;
    groups.set(category, [...groups.get(category) ?? [], entry]);
  }
  return [...groups].map(([category, members]) => [category, members.sort((a, b) => a.name.localeCompare(b.name))]).sort(([a], [b]) => a.localeCompare(b));
}

// srcts/modules/basic/stacks.tsx
import { useEffect as useEffect2, useRef as useRef3, useState as useState5 } from "react";
import { jsx as jsx6, jsxs as jsxs6 } from "react/jsx-runtime";
var NO_COLOR = "#808080";
function StacksDialog({ onClose }) {
  const front = useFrontEnd();
  const board = useBoard(front.core);
  const stacks = Object.entries(board.stacks);
  return /* @__PURE__ */ jsxs6(Dialog, { title: "Stacks", onClose, children: [
    stacks.length === 0 ? /* @__PURE__ */ jsx6("p", { className: styles.hint, children: "The board has no stacks yet." }) : null,
    /* @__PURE__ */ jsx6("ul", { "aria-label": "Stacks", className: classes.list, children: stacks.map(([id2, stack]) => /* @__PURE__ */ jsx6(StackItem, { id: id2, stack, board }, id2)) }),
    /* @__PURE__ */ jsx6("div", { children: /* @__PURE__ */ jsx6(
      "button",
      {
        type: "button",
        className: classes.primary,
        onClick: () => {
          void runCommand(front, "blockr.core-add_stack", {});
        },
        children: "Add stack"
      }
    ) })
  ] });
}
function StackItem({ id: id2, stack, board }) {
  const front = useFrontEnd();
  const [renaming, setRenaming] = useState5(false);
  return /* @__PURE__ */ jsxs6("li", { "aria-label": stack.name, className: "bc:grid bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:p-2", children: [
    /* @__PURE__ */ jsxs6("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
      renaming ? /* @__PURE__ */ jsx6(
        NameField,
        {
          label: "Stack name",
          name: stack.name,
          onSubmit: (name2) => {
            void runCommand(front, "blockr.core-rename_stack", { id: id2, name: name2 });
          },
          onDone: () => {
            setRenaming(false);
          }
        }
      ) : /* @__PURE__ */ jsx6("span", { className: "bc:text-sm bc:font-medium", children: stack.name }),
      renaming ? null : /* @__PURE__ */ jsx6(
        "button",
        {
          type: "button",
          className: classes.quiet,
          onClick: () => {
            setRenaming(true);
          },
          children: "Rename"
        }
      ),
      /* @__PURE__ */ jsx6(
        ColorField,
        {
          label: `Colour of ${stack.name}`,
          color: stack.color,
          onChange: (color2) => {
            void runCommand(front, "blockr.core-change_stack", { id: id2, color: color2 });
          }
        }
      ),
      stack.color === null ? null : /* @__PURE__ */ jsx6(
        "button",
        {
          type: "button",
          className: classes.quiet,
          onClick: () => {
            void runCommand(front, "blockr.core-change_stack", { id: id2, color: null });
          },
          children: "No colour"
        }
      ),
      /* @__PURE__ */ jsx6(
        "button",
        {
          type: "button",
          className: `${classes.button} bc:ml-auto`,
          "aria-label": `Remove the stack ${stack.name}`,
          onClick: () => {
            void runCommand(front, "blockr.core-remove_stack", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] }),
    /* @__PURE__ */ jsxs6("fieldset", { className: styles.fieldset, children: [
      /* @__PURE__ */ jsx6("legend", { className: styles.legend, children: "Blocks" }),
      Object.keys(board.blocks).length === 0 ? /* @__PURE__ */ jsx6("p", { className: styles.hint, children: "The board holds no blocks." }) : null,
      /* @__PURE__ */ jsx6("div", { className: styles.list, children: Object.entries(board.blocks).map(([block, { name: name2 }]) => /* @__PURE__ */ jsxs6("label", { className: styles.check, children: [
        /* @__PURE__ */ jsx6(
          "input",
          {
            type: "checkbox",
            className: styles.checkbox,
            checked: stack.blocks.includes(block),
            onChange: (event) => {
              const blocks = event.target.checked ? [...stack.blocks, block] : stack.blocks.filter((b) => b !== block);
              void runCommand(front, "blockr.core-change_stack", { id: id2, blocks });
            }
          }
        ),
        name2,
        " (",
        block,
        ")"
      ] }, block)) })
    ] })
  ] });
}
function ColorField({
  label,
  color: color2,
  onChange
}) {
  const ref = useRef3(null);
  const latest = useRef3(onChange);
  useEffect2(() => {
    latest.current = onChange;
  });
  useEffect2(() => {
    const el = ref.current;
    if (el === null) return;
    const picked = () => {
      latest.current(el.value);
    };
    el.addEventListener("change", picked);
    return () => {
      el.removeEventListener("change", picked);
    };
  }, []);
  return /* @__PURE__ */ jsx6(
    "input",
    {
      ref,
      type: "color",
      "aria-label": label,
      defaultValue: color2 ?? NO_COLOR,
      className: "bc:h-7 bc:w-10 bc:cursor-pointer bc:rounded bc:border bc:border-border-default bc:bg-transparent bc:p-0.5"
    },
    color2 ?? NO_COLOR
  );
}

// srcts/modules/basic/toolbar.tsx
import { useRef as useRef4, useState as useState6 } from "react";

// srcts/modules/basic/file.ts
function fileName(board, now = /* @__PURE__ */ new Date()) {
  const two = (n) => String(n).padStart(2, "0");
  const date = `${String(now.getFullYear())}-${two(now.getMonth() + 1)}-${two(now.getDate())}`;
  const time = `${two(now.getHours())}-${two(now.getMinutes())}-${two(now.getSeconds())}`;
  return `${board.id}_${date}_${time}.json`;
}
function download(text, name2, doc = document) {
  const url = URL.createObjectURL(new Blob([text], { type: "application/json" }));
  const link = doc.createElement("a");
  link.href = url;
  link.download = name2;
  link.hidden = true;
  doc.body.append(link);
  link.click();
  link.remove();
  setTimeout(() => {
    URL.revokeObjectURL(url);
  }, 0);
}

// srcts/modules/basic/toolbar.tsx
import { Fragment, jsx as jsx7, jsxs as jsxs7 } from "react/jsx-runtime";
function Toolbar({ board, show }) {
  const front = useFrontEnd();
  const { core, notices } = front;
  const { canUndo, canRedo } = useHistory(core);
  const [renaming, setRenaming] = useState6(false);
  const picker = useRef4(null);
  const locked = core.lock !== null;
  function step(back) {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch (error) {
      notices.refused(error);
    }
  }
  function save() {
    try {
      download(core.store.save(), fileName(board));
    } catch (error) {
      notices.refused(error);
    }
  }
  async function open(input2) {
    const file = input2.files?.[0];
    input2.value = "";
    if (file === void 0) return;
    try {
      await core.store.open(await file.text());
    } catch (error) {
      notices.refused(error);
    }
  }
  return /* @__PURE__ */ jsxs7("header", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:border-b bc:border-border-default bc:pb-3", children: [
    renaming ? /* @__PURE__ */ jsx7(
      NameField,
      {
        label: "Board name",
        name: board.name,
        onSubmit: (name2) => {
          void runCommand(front, "blockr.core-rename_board", { name: name2 });
        },
        onDone: () => {
          setRenaming(false);
        }
      }
    ) : /* @__PURE__ */ jsx7("h1", { className: "bc:m-0 bc:font-sans bc:text-lg bc:font-semibold", children: board.name }),
    locked || renaming ? null : /* @__PURE__ */ jsx7(
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
    /* @__PURE__ */ jsxs7("div", { role: "toolbar", "aria-label": "Board", className: "bc:ml-auto bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
      locked ? null : /* @__PURE__ */ jsxs7(Fragment, { children: [
        /* @__PURE__ */ jsx7(
          "button",
          {
            type: "button",
            className: classes.primary,
            onClick: () => {
              show("add");
            },
            children: "Add block"
          }
        ),
        /* @__PURE__ */ jsx7(
          "button",
          {
            type: "button",
            className: classes.button,
            onClick: () => {
              show("links");
            },
            children: "Links"
          }
        ),
        /* @__PURE__ */ jsx7(
          "button",
          {
            type: "button",
            className: classes.button,
            onClick: () => {
              show("stacks");
            },
            children: "Stacks"
          }
        ),
        /* @__PURE__ */ jsx7(
          "button",
          {
            type: "button",
            className: classes.button,
            onClick: () => {
              show("options");
            },
            children: "Options"
          }
        )
      ] }),
      /* @__PURE__ */ jsx7(
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
      /* @__PURE__ */ jsx7(
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
      /* @__PURE__ */ jsx7("button", { type: "button", className: classes.button, onClick: save, children: "Save" }),
      locked ? null : /* @__PURE__ */ jsxs7(Fragment, { children: [
        /* @__PURE__ */ jsx7(
          "button",
          {
            type: "button",
            className: classes.button,
            onClick: () => {
              picker.current?.click();
            },
            children: "Open"
          }
        ),
        /* @__PURE__ */ jsx7(
          "input",
          {
            ref: picker,
            type: "file",
            accept: ".json,application/json",
            "aria-label": "Board file to open",
            hidden: true,
            onChange: (event) => {
              void open(event.currentTarget);
            }
          }
        )
      ] })
    ] })
  ] });
}

// srcts/modules/basic/board.tsx
import { jsx as jsx8, jsxs as jsxs8 } from "react/jsx-runtime";
function BasicBoard({ front }) {
  return /* @__PURE__ */ jsx8(FrontEndContext, { value: front, children: /* @__PURE__ */ jsx8(Page, {}) });
}
function Page() {
  const { core } = useFrontEnd();
  const board = useBoard(core);
  const items = useMemo2(() => itemsOf(board), [board]);
  const shown = board.options.show_conditions;
  const severities = useMemo2(() => severitiesOf(shown), [shown]);
  const [dialog, setDialog] = useState7(null);
  const add = useCallback4((mode) => {
    setDialog({ kind: "add", mode });
  }, []);
  const close = useCallback4(() => {
    setDialog(null);
  }, []);
  const card = (id2) => {
    const block = board.blocks[id2];
    if (block === void 0) return null;
    return /* @__PURE__ */ jsx8(
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
  return /* @__PURE__ */ jsxs8("div", { className: `${classes.page} bc:flex bc:min-h-full bc:flex-col bc:gap-4 bc:p-4 bc:font-sans`, children: [
    /* @__PURE__ */ jsx8(
      Toolbar,
      {
        board,
        show: (kind) => {
          setDialog(kind === "add" ? { kind, mode: { kind: "add" } } : { kind });
        }
      }
    ),
    /* @__PURE__ */ jsx8(Notices, {}),
    items.length === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "The board holds no blocks yet." }) : null,
    /* @__PURE__ */ jsx8("div", { className: "bc:grid bc:gap-4", children: items.map((item) => {
      if ("block" in item) return card(item.block);
      const stack = board.stacks[item.stack];
      if (stack === void 0) return null;
      return /* @__PURE__ */ jsx8(StackGroup, { stack, children: item.blocks.length === 0 ? /* @__PURE__ */ jsx8("p", { className: styles.hint, children: "The stack holds no blocks." }) : item.blocks.map(card) }, `stack-${item.stack}`);
    }) }),
    dialog?.kind === "add" ? /* @__PURE__ */ jsx8(AddBlockDialog, { mode: dialog.mode, onClose: close }) : null,
    dialog?.kind === "links" ? /* @__PURE__ */ jsx8(LinksDialog, { onClose: close }) : null,
    dialog?.kind === "stacks" ? /* @__PURE__ */ jsx8(StacksDialog, { onClose: close }) : null,
    dialog?.kind === "options" ? /* @__PURE__ */ jsx8(OptionsDialog, { onClose: close }) : null
  ] });
}
function Notices() {
  const { notices } = useFrontEnd();
  const list = useNotices(notices);
  if (list.length === 0) return null;
  return /* @__PURE__ */ jsx8("ul", { "aria-label": "Notices", className: classes.list, children: list.map((notice) => /* @__PURE__ */ jsxs8(
    "li",
    {
      role: "alert",
      className: "bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border bc:border-border-danger bc:p-2 bc:text-sm bc:text-text-danger",
      children: [
        /* @__PURE__ */ jsx8("span", { className: "bc:flex-1 bc:whitespace-pre-wrap", children: notice.text }),
        /* @__PURE__ */ jsx8(
          "button",
          {
            type: "button",
            className: classes.quiet,
            onClick: () => {
              notices.dismiss(notice.id);
            },
            children: "Dismiss"
          }
        )
      ]
    },
    notice.id
  )) });
}
function severitiesOf(value2) {
  if (!Array.isArray(value2)) return [];
  return value2.filter((severity) => severity === "error" || severity === "warning" || severity === "message");
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

// srcts/modules/basic/notices.ts
function createNotices() {
  let notices = [];
  let last = 0;
  const listeners = /* @__PURE__ */ new Set();
  function set(next) {
    notices = next;
    for (const fn of [...listeners]) fn();
  }
  return {
    list: () => notices,
    refused(error) {
      set([...notices, { id: ++last, text: messageOf(error) }]);
    },
    dismiss(id2) {
      if (notices.some((notice) => notice.id === id2)) set(notices.filter((notice) => notice.id !== id2));
    },
    subscribe(fn) {
      const wrapped = () => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    }
  };
}

// srcts/modules/basic/shortcuts.ts
function bindShortcuts(doc, core, refused) {
  function onKey(event) {
    const step = stepOf(event);
    if (step === void 0 || event.defaultPrevented || editable(event.target)) return;
    event.preventDefault();
    try {
      if (step === "undo") core.store.undo();
      else core.store.redo();
    } catch (error) {
      refused(error);
    }
  }
  doc.addEventListener("keydown", onKey);
  return () => {
    doc.removeEventListener("keydown", onKey);
  };
}
function stepOf(event) {
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

// srcts/modules/basic/index.tsx
import { jsx as jsx9 } from "react/jsx-runtime";
var api = 1;
var basic_default = {
  render({ el, core }) {
    const doc = el.ownerDocument;
    const shown = demandShown(core, doc.defaultView ?? window);
    const notices = createNotices();
    const front = { core, notices, inView: shown };
    const root = createRoot(el);
    flushSync(() => {
      root.render(/* @__PURE__ */ jsx9(BasicBoard, { front }));
    });
    shown.measure();
    const unbind = bindShortcuts(doc, core, (error) => {
      notices.refused(error);
    });
    return () => {
      unbind();
      shown.release();
      root.unmount();
    };
  }
};
export {
  api,
  basic_default as default
};

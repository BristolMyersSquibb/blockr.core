// srcts/modules/edit-board/index.tsx
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";

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

// srcts/ui/ui.tsx
import { useLayoutEffect, useRef, useState } from "react";

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

// srcts/modules/edit-board/blocks.tsx
import { useCallback as useCallback2, useState as useState3 } from "react";

// srcts/picker/picker.tsx
import { useMemo, useState as useState2 } from "react";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
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

// srcts/picker/picker.tsx
import { jsx as jsx2, jsxs } from "react/jsx-runtime";
function BlockPicker({ core, mode, run, onPicked }) {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const [search, setSearch] = useState2("");
  const [id2, setId] = useState2("");
  const [name2, setName] = useState2("");
  const [picked, setPicked] = useState2(void 0);
  const [input2, setInput] = useState2(void 0);
  const entries = useMemo(
    () => Object.values(registry).filter((entry) => mode.kind !== "after" || entry.inputs.length > 0).filter((entry) => matches(entry, search)),
    [registry, mode.kind, search]
  );
  const categories = useMemo(() => byCategory(entries), [entries]);
  const target2 = mode.kind === "add" ? void 0 : own(board.blocks, mode.kind === "after" ? mode.from : mode.to);
  const before = mode.kind === "before" ? several(freeInputs(board, registry, mode.to) ?? []) : [];
  const after = picked === void 0 ? [] : dataInputChoices(picked);
  async function add(entry, into) {
    onPicked?.();
    const link = into === void 0 ? {} : { input: into };
    if (mode.kind === "after") {
      await run("blockr.core-append_block", { registry_id: entry.id, from: mode.from, ...link });
    } else if (mode.kind === "before") {
      await run("blockr.core-prepend_block", { registry_id: entry.id, to: mode.to, ...link });
    } else {
      const given = { ...filled("id", id2), ...filled("name", name2) };
      if (await run("blockr.core-add_block", { registry_id: entry.id, ...given })) {
        setId("");
        setName("");
      }
    }
  }
  function pick(entry) {
    if (mode.kind === "after" && dataInputChoices(entry).length > 0) {
      setPicked(entry);
      setInput(void 0);
    } else {
      void add(entry, before.length > 0 ? input2 ?? before[0] : void 0);
    }
  }
  return /* @__PURE__ */ jsxs("div", { className: "bc:grid bc:gap-3", children: [
    /* @__PURE__ */ jsxs("label", { className: styles.field, children: [
      /* @__PURE__ */ jsx2("span", { className: styles.label, children: "Search" }),
      /* @__PURE__ */ jsx2(
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
    mode.kind === "add" ? /* @__PURE__ */ jsxs("div", { className: "bc:grid bc:grid-cols-2 bc:gap-2", children: [
      /* @__PURE__ */ jsx2(TextField, { label: "ID", value: id2, placeholder: "Made up if left empty", onChange: setId }),
      /* @__PURE__ */ jsx2(TextField, { label: "Name", value: name2, placeholder: "The class's name if left empty", onChange: setName })
    ] }) : null,
    before.length > 0 ? /* @__PURE__ */ jsx2(InputSelect, { label: `Input of "${target2?.name ?? ""}"`, inputs: before, value: input2 ?? before[0], onChange: setInput }) : null,
    picked !== void 0 && after.length > 0 ? /* @__PURE__ */ jsxs("div", { className: "bc:grid bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:p-2", children: [
      /* @__PURE__ */ jsx2(InputSelect, { label: `Input of the new ${picked.name}`, inputs: after, value: input2 ?? after[0], onChange: setInput }),
      /* @__PURE__ */ jsxs("div", { className: "bc:flex bc:gap-2", children: [
        /* @__PURE__ */ jsxs(
          "button",
          {
            type: "button",
            className: classes.primary,
            onClick: () => {
              void add(picked, input2 ?? after[0]);
            },
            children: [
              "Add ",
              picked.name
            ]
          }
        ),
        /* @__PURE__ */ jsx2(
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
    ] }) : /* @__PURE__ */ jsxs("div", { className: "bc:grid bc:max-h-96 bc:gap-3 bc:overflow-y-auto", children: [
      categories.length === 0 ? /* @__PURE__ */ jsx2("p", { className: styles.hint, children: "No block class matches." }) : null,
      categories.map(([category, members]) => /* @__PURE__ */ jsxs("section", { "aria-label": category, className: "bc:grid bc:gap-1", children: [
        /* @__PURE__ */ jsx2("h3", { className: "bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted", children: category }),
        /* @__PURE__ */ jsx2("ul", { className: classes.list, children: members.map((entry) => /* @__PURE__ */ jsx2("li", { children: /* @__PURE__ */ jsxs(
          "button",
          {
            type: "button",
            className: "bc:grid bc:w-full bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:p-2 bc:text-left bc:font-sans bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle",
            onClick: () => {
              pick(entry);
            },
            children: [
              /* @__PURE__ */ jsx2("span", { className: "bc:text-sm bc:font-medium", children: entry.name }),
              entry.description === "" ? null : /* @__PURE__ */ jsx2("span", { className: "bc:text-xs bc:text-text-muted", children: entry.description })
            ]
          }
        ) }, entry.id)) })
      ] }, category))
    ] })
  ] });
}
function TextField({
  label,
  value: value2,
  placeholder,
  onChange
}) {
  return /* @__PURE__ */ jsxs("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx2("span", { className: styles.label, children: label }),
    /* @__PURE__ */ jsx2(
      "input",
      {
        type: "text",
        spellCheck: false,
        className: styles.box,
        value: value2,
        placeholder,
        onChange: (event) => {
          onChange(event.target.value);
        }
      }
    )
  ] });
}
function InputSelect({
  label,
  inputs,
  value: value2,
  onChange
}) {
  return /* @__PURE__ */ jsxs("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx2("span", { className: styles.label, children: label }),
    /* @__PURE__ */ jsx2(
      "select",
      {
        className: styles.box,
        value: value2,
        onChange: (event) => {
          onChange(event.target.value);
        },
        children: inputs.map((input2) => /* @__PURE__ */ jsx2("option", { value: input2, children: input2 }, input2))
      }
    )
  ] });
}
function filled(arg, value2) {
  const trimmed = value2.trim();
  return trimmed === "" ? {} : { [arg]: trimmed };
}
function dataInputChoices(entry) {
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
import { jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
var ADD = { kind: "add" };
function BlocksSection({ core, board }) {
  const run = useCallback2((command, args) => runCommand(core, command, args), [core]);
  const blocks = Object.entries(board.blocks);
  return /* @__PURE__ */ jsxs2(Section, { title: "Blocks", children: [
    /* @__PURE__ */ jsx3(BlockPicker, { core, mode: ADD, run }),
    blocks.length === 0 ? /* @__PURE__ */ jsx3("p", { className: styles.hint, children: "The board holds no blocks yet." }) : /* @__PURE__ */ jsx3("ul", { "aria-label": "Blocks", className: `${classes.list} bc:border-t bc:border-border-default bc:pt-2`, children: blocks.map(([id2, block]) => /* @__PURE__ */ jsx3(BlockItem, { core, id: id2, block }, id2)) })
  ] });
}
function BlockItem({ core, id: id2, block }) {
  const [renaming, setRenaming] = useState3(false);
  return /* @__PURE__ */ jsxs2("li", { "aria-label": block.name, className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:text-sm", children: [
    renaming ? /* @__PURE__ */ jsx3(
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
    ) : /* @__PURE__ */ jsxs2("span", { className: "bc:min-w-0 bc:flex-1", children: [
      block.name,
      " ",
      /* @__PURE__ */ jsxs2("span", { className: "bc:text-text-muted", children: [
        "(",
        id2,
        ")"
      ] })
    ] }),
    renaming ? null : /* @__PURE__ */ jsx3(
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
    /* @__PURE__ */ jsx3(
      "button",
      {
        type: "button",
        className: `${classes.button} bc:ml-auto`,
        "aria-label": `Remove the block ${block.name}`,
        onClick: () => {
          void runCommand(core, "blockr.core-remove_block", { id: id2 });
        },
        children: "Remove"
      }
    )
  ] });
}

// srcts/modules/edit-board/links.tsx
import { useState as useState4 } from "react";
import { Fragment, jsx as jsx4, jsxs as jsxs3 } from "react/jsx-runtime";
function LinksSection({ core, board, locked: locked2 }) {
  const links = Object.entries(board.links);
  return /* @__PURE__ */ jsxs3(Section, { title: "Links", children: [
    links.length === 0 ? /* @__PURE__ */ jsx4("p", { className: styles.hint, children: "No link joins two blocks yet." }) : /* @__PURE__ */ jsx4("ul", { "aria-label": "Links", className: classes.list, children: links.map(
      ([id2, link]) => locked2 ? /* @__PURE__ */ jsx4("li", { className: "bc:text-sm", children: /* @__PURE__ */ jsx4(LinkText, { board, link }) }, id2) : /* @__PURE__ */ jsx4(LinkItem, { core, board, id: id2, link }, id2)
    ) }),
    locked2 ? null : /* @__PURE__ */ jsx4(AddLink, { core, board })
  ] });
}
function LinkText({ board, link }) {
  return /* @__PURE__ */ jsxs3(Fragment, { children: [
    blockName(board, link.from),
    " \u2192 ",
    blockName(board, link.to),
    link.input === "" ? null : /* @__PURE__ */ jsxs3("span", { className: "bc:text-text-muted", children: [
      " as ",
      link.input
    ] })
  ] });
}
function LinkItem({ core, board, id: id2, link }) {
  const registry = core.blockRegistry();
  const free = freeInputs(board, registry, link.to, id2) ?? [];
  function change(args) {
    void runCommand(core, "blockr.core-change_link", { id: id2, ...args });
  }
  function retarget(to) {
    const [first] = freeInputs(board, registry, to, id2) ?? [];
    change(first === void 0 || first === "" ? { to } : { to, input: first });
  }
  return /* @__PURE__ */ jsxs3(
    "li",
    {
      "aria-label": `${blockName(board, link.from)} \u2192 ${blockName(board, link.to)}`,
      className: "bc:flex bc:flex-wrap bc:items-end bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:p-2",
      children: [
        /* @__PURE__ */ jsx4(
          BlockSelect,
          {
            label: "From",
            board,
            value: link.from,
            disabled: (block) => block === link.to,
            onChange: (from) => {
              change({ from });
            }
          }
        ),
        /* @__PURE__ */ jsx4(
          BlockSelect,
          {
            label: "To",
            board,
            value: link.to,
            disabled: (block) => block === link.from || block !== link.to && (freeInputs(board, registry, block, id2)?.length ?? 0) === 0,
            onChange: retarget
          }
        ),
        isVariadic(free) ? null : /* @__PURE__ */ jsx4(
          InputSelect2,
          {
            inputs: free,
            value: link.input,
            onChange: (input2) => {
              change({ input: input2 });
            }
          }
        ),
        /* @__PURE__ */ jsx4(
          "button",
          {
            type: "button",
            className: `${classes.button} bc:ml-auto`,
            "aria-label": `Remove the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`,
            onClick: () => {
              void runCommand(core, "blockr.core-remove_link", { id: id2 });
            },
            children: "Remove"
          }
        )
      ]
    }
  );
}
function AddLink({ core, board }) {
  const ids = Object.keys(board.blocks);
  const [pickedFrom, setFrom] = useState4(void 0);
  const [pickedTo, setTo] = useState4(void 0);
  const [input2, setInput] = useState4(void 0);
  const held = (block) => block !== void 0 && Object.hasOwn(board.blocks, block);
  const from = held(pickedFrom) ? pickedFrom : ids[0] ?? "";
  const to = held(pickedTo) ? pickedTo : ids[1] ?? ids[0] ?? "";
  const free = freeInputs(board, core.blockRegistry(), to) ?? [];
  const chosen = isVariadic(free) ? void 0 : input2 !== void 0 && free.includes(input2) ? input2 : free[0];
  const args = { from, to, ...chosen === void 0 ? {} : { input: chosen } };
  return /* @__PURE__ */ jsxs3(
    "form",
    {
      "aria-label": "Add link",
      className: "bc:grid bc:gap-2 bc:border-t bc:border-border-default bc:pt-2",
      onSubmit: (event) => {
        event.preventDefault();
        setInput(void 0);
        void runCommand(core, "blockr.core-add_link", args);
      },
      children: [
        /* @__PURE__ */ jsxs3("div", { className: "bc:grid bc:grid-cols-2 bc:gap-2", children: [
          /* @__PURE__ */ jsx4(BlockSelect, { label: "From", board, value: from, onChange: setFrom }),
          /* @__PURE__ */ jsx4(
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
        isVariadic(free) ? null : /* @__PURE__ */ jsx4(InputSelect2, { inputs: free, value: chosen ?? "", onChange: setInput }),
        /* @__PURE__ */ jsx4("div", { children: /* @__PURE__ */ jsx4("button", { type: "submit", className: classes.primary, disabled: !core.commands.enabled("blockr.core-add_link", args), children: "Add link" }) })
      ]
    }
  );
}
function BlockSelect({
  label,
  board,
  value: value2,
  disabled,
  onChange
}) {
  return /* @__PURE__ */ jsxs3("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx4("span", { className: styles.label, children: label }),
    /* @__PURE__ */ jsx4(
      "select",
      {
        className: styles.box,
        value: value2,
        onChange: (event) => {
          onChange(event.target.value);
        },
        children: Object.entries(board.blocks).map(([id2, block]) => /* @__PURE__ */ jsxs3("option", { value: id2, disabled: id2 !== value2 && disabled?.(id2) === true, children: [
          block.name,
          " (",
          id2,
          ")"
        ] }, id2))
      }
    )
  ] });
}
function InputSelect2({
  inputs,
  value: value2,
  onChange
}) {
  return /* @__PURE__ */ jsxs3("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx4("span", { className: styles.label, children: "Input" }),
    /* @__PURE__ */ jsxs3(
      "select",
      {
        className: styles.box,
        value: value2,
        disabled: inputs.length === 0,
        onChange: (event) => {
          onChange(event.target.value);
        },
        children: [
          inputs.length === 0 ? /* @__PURE__ */ jsx4("option", { value: "", children: "No free input" }) : null,
          inputs.map((name2) => /* @__PURE__ */ jsx4("option", { value: name2, children: name2 }, name2))
        ]
      }
    )
  ] });
}
function isVariadic(free) {
  return free.length === 1 && free[0] === "";
}
function blockName(board, block) {
  return own(board.blocks, block)?.name ?? block;
}

// srcts/modules/edit-board/stacks.tsx
import { useEffect, useRef as useRef2, useState as useState5 } from "react";
import { jsx as jsx5, jsxs as jsxs4 } from "react/jsx-runtime";
var NO_COLOR = "#808080";
function StacksSection({ core, board, locked: locked2 }) {
  const stacks = Object.entries(board.stacks);
  return /* @__PURE__ */ jsxs4(Section, { title: "Stacks", children: [
    stacks.length === 0 ? /* @__PURE__ */ jsx5("p", { className: styles.hint, children: "The board has no stacks yet." }) : null,
    /* @__PURE__ */ jsx5("ul", { "aria-label": "Stacks", className: classes.list, children: stacks.map(
      ([id2, stack]) => locked2 ? /* @__PURE__ */ jsx5(StackText, { stack, board }, id2) : /* @__PURE__ */ jsx5(StackItem, { core, id: id2, stack, board }, id2)
    ) }),
    locked2 ? null : /* @__PURE__ */ jsx5("div", { children: /* @__PURE__ */ jsx5(
      "button",
      {
        type: "button",
        className: classes.primary,
        onClick: () => {
          void runCommand(core, "blockr.core-add_stack", {});
        },
        children: "Add stack"
      }
    ) })
  ] });
}
function StackText({ stack, board }) {
  return /* @__PURE__ */ jsxs4("li", { "aria-label": stack.name, className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:text-sm", children: [
    /* @__PURE__ */ jsx5(
      "span",
      {
        "aria-hidden": "true",
        className: "bc:size-3 bc:rounded-full bc:border bc:border-border-default",
        style: stack.color === null ? void 0 : { backgroundColor: stack.color }
      }
    ),
    /* @__PURE__ */ jsx5("span", { className: "bc:font-medium", children: stack.name }),
    /* @__PURE__ */ jsx5("span", { className: "bc:text-text-muted", children: stack.blocks.length === 0 ? "No blocks" : stack.blocks.map((block) => blockName(board, block)).join(", ") })
  ] });
}
function StackItem({ core, id: id2, stack, board }) {
  const [renaming, setRenaming] = useState5(false);
  return /* @__PURE__ */ jsxs4("li", { "aria-label": stack.name, className: "bc:grid bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:p-2", children: [
    /* @__PURE__ */ jsxs4("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-2", children: [
      renaming ? /* @__PURE__ */ jsx5(
        NameField,
        {
          label: "Stack name",
          name: stack.name,
          onSubmit: (name2) => {
            void runCommand(core, "blockr.core-rename_stack", { id: id2, name: name2 });
          },
          onDone: () => {
            setRenaming(false);
          }
        }
      ) : /* @__PURE__ */ jsx5("span", { className: "bc:text-sm bc:font-medium", children: stack.name }),
      renaming ? null : /* @__PURE__ */ jsx5(
        "button",
        {
          type: "button",
          className: classes.quiet,
          "aria-label": `Rename the stack ${stack.name}`,
          onClick: () => {
            setRenaming(true);
          },
          children: "Rename"
        }
      ),
      /* @__PURE__ */ jsx5(
        ColorField,
        {
          label: `Colour of ${stack.name}`,
          color: stack.color,
          onChange: (color2) => {
            void runCommand(core, "blockr.core-change_stack", { id: id2, color: color2 });
          }
        }
      ),
      stack.color === null ? null : /* @__PURE__ */ jsx5(
        "button",
        {
          type: "button",
          className: classes.quiet,
          onClick: () => {
            void runCommand(core, "blockr.core-change_stack", { id: id2, color: null });
          },
          children: "No colour"
        }
      ),
      /* @__PURE__ */ jsx5(
        "button",
        {
          type: "button",
          className: `${classes.button} bc:ml-auto`,
          "aria-label": `Remove the stack ${stack.name}`,
          onClick: () => {
            void runCommand(core, "blockr.core-remove_stack", { id: id2 });
          },
          children: "Remove"
        }
      )
    ] }),
    /* @__PURE__ */ jsxs4("fieldset", { className: styles.fieldset, children: [
      /* @__PURE__ */ jsx5("legend", { className: styles.legend, children: "Blocks" }),
      Object.keys(board.blocks).length === 0 ? /* @__PURE__ */ jsx5("p", { className: styles.hint, children: "The board holds no blocks." }) : null,
      /* @__PURE__ */ jsx5("div", { className: styles.list, children: Object.entries(board.blocks).map(([block, { name: name2 }]) => /* @__PURE__ */ jsxs4("label", { className: styles.check, children: [
        /* @__PURE__ */ jsx5(
          "input",
          {
            type: "checkbox",
            className: styles.checkbox,
            checked: stack.blocks.includes(block),
            onChange: (event) => {
              const blocks = event.target.checked ? [...stack.blocks, block] : stack.blocks.filter((b) => b !== block);
              void runCommand(core, "blockr.core-change_stack", { id: id2, blocks });
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
  const ref = useRef2(null);
  const latest = useRef2(onChange);
  useEffect(() => {
    latest.current = onChange;
  });
  useEffect(() => {
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
  return /* @__PURE__ */ jsx5(
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

// srcts/modules/edit-board/board.tsx
import { jsx as jsx6, jsxs as jsxs5 } from "react/jsx-runtime";
function EditBoard({ core }) {
  const board = useBoard(core);
  const locked2 = core.lock.board;
  return /* @__PURE__ */ jsxs5("div", { className: "bc:grid bc:gap-4 bc:font-sans bc:text-text-default", children: [
    locked2 ? null : /* @__PURE__ */ jsx6(BlocksSection, { core, board }),
    /* @__PURE__ */ jsx6(LinksSection, { core, board, locked: locked2 }),
    /* @__PURE__ */ jsx6(StacksSection, { core, board, locked: locked2 })
  ] });
}
function Section({ title, children }) {
  return /* @__PURE__ */ jsxs5("section", { "aria-label": title, className: "bc:grid bc:gap-2", children: [
    /* @__PURE__ */ jsx6("h3", { className: "bc:m-0 bc:font-sans bc:text-sm bc:font-semibold", children: title }),
    children
  ] });
}

// srcts/modules/edit-board/index.tsx
import { jsx as jsx7 } from "react/jsx-runtime";
var api = 1;
var edit_board_default = {
  render({ el, core }) {
    const root = createRoot(el);
    flushSync(() => {
      root.render(/* @__PURE__ */ jsx7(EditBoard, { core }));
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

// srcts/picker/picker.tsx
import { useMemo as useMemo2, useState as useState7 } from "react";
import { flushSync as flushSync2 } from "react-dom";
import { Button as Button3, ComboBox, ComboBoxItem as ComboBoxItem2, ComboBoxSection, Form, Select, SelectItem, Text as Text2, TextField as TextField2 } from "@blockr/ui";

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

// srcts/util/blocks.ts
function namesInto(board, to, except) {
  return Object.entries(board.links).filter(([id2, link]) => id2 !== except && link.to === to && link.input !== "").map(([, link]) => link.input);
}
function inputNameError(value2, taken) {
  const trimmed = value2.trim();
  if (trimmed === "") return void 0;
  if (taken.includes(trimmed)) return `Another link gives the input the name "${trimmed}" already.`;
  return void 0;
}
function blockClasses(entries) {
  return entries.map(classItem).sort(byGroupAndName);
}
function classItem(entry) {
  return {
    id: entry.id,
    name: entry.name,
    description: entry.description,
    icon: entry.icon,
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
import { useBoard } from "@blockr/core/react";

// srcts/ui/blocks.tsx
import { ComboBoxItem, Text } from "@blockr/ui";
import { jsx, jsxs } from "react/jsx-runtime";

// srcts/ui/edits.ts
import { useMemo, useState } from "react";
function useEdits(core) {
  const [refusal2, setRefusal] = useState(null);
  return useMemo(() => {
    const opts = { notifier: () => void 0 };
    function run(title, target2, given) {
      setRefusal(null);
      const running = typeof target2 === "string" ? core.commands.run(target2, given, "user", opts) : typeof given === "function" ? given(target2, opts) : core.commands.run(target2, "user", opts);
      return running.then(
        () => true,
        (error) => {
          setRefusal({ title, reasons: reasons(error) });
          return false;
        }
      );
    }
    return {
      refusal: refusal2,
      run,
      clear: () => {
        setRefusal(null);
      }
    };
  }, [core, refusal2]);
}
function reasons(error) {
  const failures = error instanceof Error && "failures" in error ? error.failures : void 0;
  return isSentences(failures) ? failures : [errorMessage(error)];
}
function isSentences(x) {
  return Array.isArray(x) && x.length > 0 && x.every((item) => typeof item === "string");
}

// srcts/ui/hooks.ts
import { useCallback, useSyncExternalStore } from "react";

// srcts/ui/search.ts
import { useState as useState2 } from "react";
import { useFilter } from "@blockr/ui";
function useSearch(all, texts, none = "No options", start = "") {
  const { contains } = useFilter({ sensitivity: "base" });
  const [query, setQuery] = useState2(start);
  const [started, setStarted] = useState2(start !== "");
  return {
    items: query === "" ? all : all.filter((item) => texts(item).some((text) => contains(text, query))),
    onInputChange: (text) => {
      setStarted(false);
      setQuery(text);
    },
    onOpenChange: (isOpen, trigger) => {
      if (isOpen && trigger === "manual" && !started) setQuery("");
    },
    renderEmptyState: () => all.length === 0 ? none : "No matches"
  };
}

// srcts/ui/table.tsx
import { useState as useState3 } from "react";
import { Button, SearchField } from "@blockr/ui";
import { jsx as jsx2, jsxs as jsxs2 } from "react/jsx-runtime";

// srcts/ui/ui.tsx
import { useRef as useRef2, useState as useState6 } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Button as Button2, TextField } from "@blockr/ui";

// srcts/card/footer.tsx
import { useCallback as useCallback2, useId, useState as useState4 } from "react";
import { Fragment, jsx as jsx3, jsxs as jsxs3 } from "react/jsx-runtime";
var MARKS = {
  error: {
    label: "Error",
    noun: "error",
    text: "bc:text-destructive-text",
    tint: "bc:bg-destructive/10",
    border: "bc:border-destructive"
  },
  warning: {
    label: "Warning",
    noun: "warning",
    text: "bc:text-warning-text",
    tint: "bc:bg-warning/10",
    border: "bc:border-warning"
  },
  message: {
    label: "Message",
    noun: "message",
    text: "bc:text-primary-text",
    tint: "bc:bg-primary/10",
    border: "bc:border-primary"
  }
};
function SeverityIcon({ severity }) {
  return /* @__PURE__ */ jsx3(
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
      children: severity === "warning" ? /* @__PURE__ */ jsxs3(Fragment, { children: [
        /* @__PURE__ */ jsx3("path", { d: "M8 1.8 14.6 13.6H1.4Z" }),
        /* @__PURE__ */ jsx3("path", { d: "M8 6.2v3.2M8 11.6v.1" })
      ] }) : /* @__PURE__ */ jsxs3(Fragment, { children: [
        /* @__PURE__ */ jsx3("circle", { cx: "8", cy: "8", r: "6.4" }),
        /* @__PURE__ */ jsx3("path", { d: severity === "error" ? "M5.7 5.7l4.6 4.6M10.3 5.7l-4.6 4.6" : "M8 7.4v3.6M8 4.9v.1" })
      ] })
    }
  );
}

// srcts/card/mounted.tsx
import { useLayoutEffect, useRef } from "react";
import { jsx as jsx4 } from "react/jsx-runtime";

// srcts/card/status.ts
import { useState as useState5 } from "react";

// srcts/ui/ui.tsx
import { jsx as jsx5, jsxs as jsxs4 } from "react/jsx-runtime";
var classes = {
  // The board's page, in the page's colour and the colour of its text,
  // `background` and `foreground`
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#the-defaults).
  page: "bc:bg-background bc:text-foreground",
  // What sits over the board, as a dialog or a menu does, in the colours of
  // every overlay, `popover`.
  surface: "bc:bg-popover bc:text-popover-foreground",
  // A card on the page, in the colours of a card, `card`.
  card: "bc:bg-card bc:text-card-foreground",
  heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold",
  list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0",
  // A hint beside the fields of a form or in place of a list, in the colour
  // of secondary text, as blockr.ui draws a field's description.
  hint: "bc:m-0 bc:text-sm bc:text-muted-foreground",
  // A line that says why something cannot happen, in the colour of an error's
  // text, as blockr.ui draws a field's error.
  alert: "bc:m-0 bc:text-sm bc:text-destructive-text bc:whitespace-pre-wrap",
  // A group of controls under a legend, which takes the look of a field's
  // label, as blockr.ui draws it.
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans",
  legend: "bc:mb-1 bc:p-0 bc:text-sm bc:font-medium"
};
function drawForm(el, form) {
  const root = createRoot(el);
  flushSync(() => {
    root.render(form);
  });
  return () => {
    root.unmount();
  };
}
var identifier = { spellCheck: "false", autoCapitalize: "none", autoCorrect: "off" };
function Refusal({ refusal: refusal2 }) {
  if (refusal2 === null) return null;
  const mark = MARKS.error;
  const { title, reasons: reasons2 } = refusal2;
  return /* @__PURE__ */ jsxs4(
    "div",
    {
      role: "alert",
      className: `bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`,
      children: [
        /* @__PURE__ */ jsx5("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx5(SeverityIcon, { severity: "error" }) }),
        /* @__PURE__ */ jsxs4("div", { className: "bc:grid bc:min-w-0 bc:flex-1 bc:gap-1 bc:text-sm bc:break-words", children: [
          /* @__PURE__ */ jsx5("p", { className: `bc:m-0 bc:font-semibold ${mark.text}`, children: title }),
          reasons2.length === 1 ? /* @__PURE__ */ jsx5("p", { className: "bc:m-0 bc:whitespace-pre-wrap", children: reasons2[0] }) : /* @__PURE__ */ jsx5("ul", { className: "bc:m-0 bc:list-disc bc:pl-5", children: reasons2.map((reason, i) => /* @__PURE__ */ jsx5("li", { className: "bc:whitespace-pre-wrap", children: reason }, i)) })
        ] })
      ]
    }
  );
}

// srcts/picker/picker.tsx
import { jsx as jsx6, jsxs as jsxs5 } from "react/jsx-runtime";
function drawPicker(el, props) {
  return drawForm(el, /* @__PURE__ */ jsx6(BlockPicker, { ...props }));
}
function BlockPicker({ core, mode, done, filter = "", autofocus = false }) {
  const registry = core.blockRegistry();
  const edits = useEdits(core);
  const [picked, setPicked] = useState7(void 0);
  const [input2, setInput] = useState7(void 0);
  const [inputName, setInputName] = useState7("");
  const [place, setPlace] = useState7(void 0);
  const [id2, setId] = useState7("");
  const [name2, setName] = useState7("");
  const [errors, setErrors] = useState7({});
  const [busy, setBusy] = useState7(false);
  const takes = mode.kind === "after" || mode.kind === "on";
  const options = useMemo2(() => blockClasses(Object.values(registry).filter((entry2) => !takes || entry2.inputs.length > 0)), [registry, takes]);
  const search = useSearch(
    options,
    (item) => [item.name, item.description, item.id, item.category, item.pkg ?? "", ...item.keywords],
    "No block class to pick.",
    filter
  );
  const entry = picked === void 0 ? void 0 : own(registry, picked);
  const link = useBoard(core, (board) => linkChoice(mode, board, core.blockRegistry(), entry));
  const chosen = link.kind === "pick" ? input2 !== void 0 && link.inputs.includes(input2) ? input2 : link.inputs[0] : void 0;
  const places = link.kind === "name" ? link.places : void 0;
  const position = places === void 0 ? void 0 : place !== void 0 && place <= places.length ? place : places.length + 1;
  const on = useBoard(core, (board) => mode.kind === "on" ? own(board.links, mode.link) : void 0);
  if (mode.kind === "on" && on === void 0) return null;
  function check2() {
    return {
      ...entry === void 0 ? { registry: "Pick the class of the new block." } : {},
      ...optional("id", blockIdError(id2, core.store.board())),
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
      ...link.kind === "name" ? filled("input", inputName) : {},
      ...link.kind === "fixed" ? { input: link.input } : {}
    };
    const before = places === void 0 || position === void 0 ? void 0 : places[position - 1];
    setBusy(true);
    await edits.run("Could not add the block.", pickedBatch(core, mode, on, args, into, before), done);
    setBusy(false);
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
            defaultInputValue: filter,
            ...search,
            items: byCategory(search.items),
            value: picked ?? null,
            placeholder: options.length === 0 ? "No block class to pick" : "Search by name, description or keyword",
            isInvalid: errors.registry !== void 0,
            errorMessage: errors.registry,
            description: entry?.description === "" ? void 0 : entry?.description,
            onChange: (key) => {
              setPicked(key === null ? void 0 : String(key));
              if (takes) setInput(void 0);
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
        link.kind === "name" ? /* @__PURE__ */ jsxs5("div", { className: places === void 0 ? "bc:grid" : "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx6(
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
          ),
          places === void 0 ? null : /* @__PURE__ */ jsx6(
            Select,
            {
              label: "Position",
              isRequired: true,
              items: Array.from({ length: places.length + 1 }, (_, i) => ({ id: String(i + 1) })),
              value: position === void 0 ? null : String(position),
              isDisabled: places.length === 0,
              onChange: (key) => {
                if (key !== null) setPlace(Number(key));
              },
              children: (item) => /* @__PURE__ */ jsx6(SelectItem, { id: item.id, children: item.id })
            }
          )
        ] }) : null,
        link.kind === "fixed" ? /* @__PURE__ */ jsx6("p", { className: classes.hint, children: link.line }) : null,
        link.kind === "no_free_inputs" ? /* @__PURE__ */ jsx6("p", { className: classes.alert, children: link.why }) : null,
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
                setErrors((now) => ({ ...now, ...optional("id", blockIdError(id2, core.store.board())) }));
              }
            }
          ),
          /* @__PURE__ */ jsx6(
            TextField2,
            {
              label: "Name",
              value: name2,
              placeholder: entry?.block_name,
              description: "Optional. The class's name where left empty.",
              autoComplete: "off",
              onChange: setName
            }
          )
        ] }),
        /* @__PURE__ */ jsx6("div", { children: /* @__PURE__ */ jsx6(Button3, { type: "submit", variant: "primary", isDisabled: busy || link.kind === "no_free_inputs", children: "Add block" }) }),
        /* @__PURE__ */ jsx6(Refusal, { refusal: edits.refusal })
      ]
    }
  );
}
function linkChoice(mode, board, registry, entry) {
  if (mode.kind === "before") {
    const target2 = own(board.blocks, mode.to);
    const free = freeInputs(board, registry, mode.to) ?? [];
    const name2 = target2?.name ?? mode.to;
    const of = `of "${name2}"`;
    if (mode.input !== void 0 && mode.input !== "") return fixedInput(free, mode.input, name2);
    if (isVariadic(free)) return { kind: "name", label: `Input name ${of}`, taken: namesInto(board, mode.to), places: linksInto(board, mode.to) };
    if (free.length === 0) return { kind: "no_free_inputs", why: `Every input ${of} takes a link already.` };
    return free.length > 1 ? { kind: "pick", label: `Input ${of}`, inputs: free } : { kind: "none" };
  }
  if ((mode.kind === "after" || mode.kind === "on") && entry !== void 0) {
    if (entry.inputs.includes("...")) return { kind: "name", label: `Input name of the new ${entry.name}`, taken: [] };
    if (entry.inputs.length > 1) return { kind: "pick", label: `Input of the new ${entry.name}`, inputs: entry.inputs };
  }
  return { kind: "none" };
}
function fixedInput(free, input2, name2) {
  const named = `the input "${input2}" of "${name2}"`;
  if (free.includes(input2)) return { kind: "fixed", input: input2, line: `The new block feeds ${named}.` };
  return { kind: "no_free_inputs", why: `A link feeds ${named} already.` };
}
function pickedBatch(core, mode, on, args, into, before) {
  const block = typeof args.id === "string" ? args.id : core.mintId((id2) => Object.hasOwn(core.store.board().blocks, id2));
  const add = { id: "blockr.core-add_block", args: { ...args, id: block } };
  const linked = (from, to, id2) => ({
    id: "blockr.core-add_link",
    args: { ...id2 === void 0 ? {} : { id: id2 }, from, to, ...into }
  });
  switch (mode.kind) {
    case "add":
      return [add];
    case "after":
      return [add, linked(mode.from, block)];
    case "before": {
      if (before === void 0) return [add, linked(block, mode.to)];
      const link = core.mintId((id2) => Object.hasOwn(core.store.board().links, id2));
      return [add, linked(block, mode.to, link), { id: "blockr.core-change_link", args: { id: link, before } }];
    }
    case "on":
      return on === void 0 ? [] : [add, linked(on.from, block), { id: "blockr.core-change_link", args: { id: mode.link, from: block } }];
  }
}
function linksInto(board, to) {
  return Object.keys(board.links).filter((id2) => own(board.links, id2)?.to === to);
}
function isVariadic(free) {
  return free.length === 1 && free[0] === "";
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
var BADGE = "bc:ml-auto bc:shrink-0 bc:rounded-sm bc:border bc:border-border bc:px-1 bc:text-xs bc:text-muted-foreground";
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
export {
  BlockPicker,
  blockIdError,
  drawPicker,
  focusFirstInvalid,
  isVariadic,
  without2 as without
};

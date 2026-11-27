// srcts/actions/inputs.tsx
import { useState as useState8 } from "react";
import { flushSync as flushSync3 } from "react-dom";
import { Button as Button4, ComboBox as ComboBox2, Form as Form2, GridList, GridListItem, TextField as TextField3, useDragAndDrop } from "@blockr/ui";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}

// srcts/util/blocks.ts
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
function inputNameError(value2, taken) {
  const trimmed = value2.trim();
  if (trimmed === "") return void 0;
  if (taken.includes(trimmed)) return `Another link gives the input the name "${trimmed}" already.`;
  return void 0;
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
function linkFeeding(board, registry, to, input2, except) {
  const block = own(board.blocks, to);
  const inputs = block === void 0 ? void 0 : own(registry, block.registry_id)?.inputs;
  if (inputs === void 0 || inputs.includes("...")) return void 0;
  return Object.entries(board.links).find(([id2, link]) => id2 !== except && link.to === to && link.input === input2)?.[0];
}

// srcts/picker/picker.tsx
import { useMemo as useMemo2, useState as useState7 } from "react";
import { flushSync as flushSync2 } from "react-dom";
import { Button as Button3, ComboBox, ComboBoxItem as ComboBoxItem2, ComboBoxSection, Form, Select, SelectItem, Text as Text2, TextField as TextField2 } from "@blockr/ui";
import { useBoard } from "@blockr/core/react";

// srcts/ui/blocks.tsx
import { ComboBoxItem, Text } from "@blockr/ui";
import { jsx, jsxs } from "react/jsx-runtime";
function blockItem(board, registry, id2) {
  const block = own(board.blocks, id2);
  const cls = block === void 0 ? void 0 : own(registry, block.registry_id)?.name ?? block.registry_id;
  return { id: id2, name: block?.name ?? id2, detail: cls === void 0 ? id2 : `${id2} \xB7 ${cls}` };
}
function blockTexts(item) {
  return [item.name, item.detail, item.id];
}
function blockRow(item) {
  return /* @__PURE__ */ jsxs(ComboBoxItem, { id: item.id, textValue: item.name, children: [
    /* @__PURE__ */ jsx(Text, { slot: "label", className: "bc:shrink-0 bc:truncate", children: item.name }),
    /* @__PURE__ */ jsx(Text, { slot: "description", children: item.detail })
  ] });
}

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
function focusFirstInvalid(form) {
  form.querySelector('[aria-invalid="true"]')?.focus();
}

// srcts/actions/inputs.tsx
import { useBoard as useBoard2 } from "@blockr/core/react";
import { jsx as jsx7, jsxs as jsxs6 } from "react/jsx-runtime";
var ADD_LINK = "blockr.core-add_link";
var CHANGE_LINK = "blockr.core-change_link";
var REMOVE_LINK = "blockr.core-remove_link";
var editInputs = {
  render({ el, core, target: target2, done }) {
    if (!("block" in target2)) return void 0;
    return drawForm(el, /* @__PURE__ */ jsx7(EditInputs, { core, block: target2.block, done }));
  }
};
function EditInputs({ core, block, done }) {
  const board = useBoard2(core);
  if (own(board.blocks, block) === void 0) return null;
  const registry = core.blockRegistry();
  const parts = { core, done, board, registry, block, sources: sourcesOf(board, registry, block) };
  return isVariadicBlock(board, registry, block) ? /* @__PURE__ */ jsx7(VariadicInputs, { ...parts }) : /* @__PURE__ */ jsx7(FixedInputs, { ...parts });
}
function sourcesOf(board, registry, block) {
  const downstream = createGraph(board).withDownstream([block]);
  return Object.keys(board.blocks).filter((id2) => !downstream.has(id2)).map((id2) => blockItem(board, registry, id2));
}
function InputsForm({
  core,
  done,
  board,
  block,
  variadic,
  prepare,
  children
}) {
  const edits = useEdits(core);
  const [busy, setBusy] = useState8(false);
  const name2 = blockName(board, block);
  async function submit(event) {
    event.preventDefault();
    if (busy) return;
    const form = event.currentTarget;
    const calls = prepare();
    if (calls === void 0) {
      edits.clear();
      focusFirstInvalid(form);
      return;
    }
    setBusy(true);
    await edits.run(`Could not change the inputs of the block "${name2}".`, calls, done);
    setBusy(false);
  }
  return /* @__PURE__ */ jsxs6(
    Form2,
    {
      "aria-label": "Change inputs",
      validationBehavior: "aria",
      className: "bc:font-sans bc:text-sm",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        /* @__PURE__ */ jsx7("p", { className: classes.hint, children: variadic ? `The inputs of the block "${name2}", which it takes in this order.` : `The inputs of the block "${name2}".` }),
        children,
        /* @__PURE__ */ jsx7("div", { className: "bc:flex bc:gap-2", children: /* @__PURE__ */ jsx7(Button4, { type: "submit", variant: "primary", isDisabled: busy, children: "Apply" }) }),
        /* @__PURE__ */ jsx7(Refusal, { refusal: edits.refusal })
      ]
    }
  );
}
function FixedInputs(parts) {
  const { core, board, registry, block, sources } = parts;
  const [given, setGiven] = useState8({});
  const inputs = classInputs(board, registry, block);
  return /* @__PURE__ */ jsx7(InputsForm, { ...parts, variadic: false, prepare: () => fixedBatch(core, board, registry, block, given), children: /* @__PURE__ */ jsx7("div", { className: "bc:grid bc:gap-2", children: inputs.map((input2) => /* @__PURE__ */ jsx7(
    FixedInput,
    {
      sources,
      input: input2,
      source: shownSource(board, registry, block, input2, given),
      onChange: (source) => {
        setGiven((now) => ({ ...now, [input2]: source }));
      }
    },
    input2
  )) }) });
}
function shownSource(board, registry, block, input2, given) {
  const source = own(given, input2);
  if (source === null || source !== void 0 && Object.hasOwn(board.blocks, source)) return source;
  const id2 = linkFeeding(board, registry, block, input2);
  return (id2 === void 0 ? void 0 : own(board.links, id2)?.from) ?? null;
}
function fixedBatch(core, board, registry, block, given) {
  const removes = [];
  const changes = [];
  const adds = [];
  const minted = /* @__PURE__ */ new Set();
  for (const input2 of classInputs(board, registry, block)) {
    const source = shownSource(board, registry, block, input2, given);
    const id2 = linkFeeding(board, registry, block, input2);
    const from = id2 === void 0 ? void 0 : own(board.links, id2)?.from;
    if (id2 === void 0) {
      if (source !== null) adds.push({ id: ADD_LINK, args: { id: mint(core, minted), from: source, to: block, input: input2 } });
    } else if (source === null) {
      removes.push({ id: REMOVE_LINK, args: { id: id2 } });
    } else if (source !== from) {
      changes.push({ id: CHANGE_LINK, args: { id: id2, from: source } });
    }
  }
  return [...removes, ...changes, ...adds];
}
function mint(core, minted) {
  const id2 = core.mintId((taken) => minted.has(taken) || Object.hasOwn(core.store.board().links, taken));
  minted.add(id2);
  return id2;
}
function FixedInput({
  sources,
  input: input2,
  source,
  onChange
}) {
  const search = useSearch(sources, blockTexts, "No block can feed the input.");
  return /* @__PURE__ */ jsxs6("div", { className: "bc:flex bc:items-end bc:gap-2", children: [
    /* @__PURE__ */ jsx7(
      ComboBox2,
      {
        label: input2,
        ...search,
        value: source,
        placeholder: "Search blocks",
        isRequired: source !== null,
        className: "bc:min-w-0 bc:flex-1",
        onChange: (key) => {
          if (key !== null) onChange(String(key));
        },
        children: blockRow
      }
    ),
    /* @__PURE__ */ jsx7("div", { className: "bc:flex bc:w-7 bc:shrink-0 bc:justify-center bc:pb-1", children: source === null ? null : /* @__PURE__ */ jsx7(
      RemoveButton,
      {
        label: `Remove the source of ${input2}`,
        onPress: () => {
          onChange(null);
        }
      }
    ) })
  ] });
}
var UNCHANGED = { links: {}, removed: [], added: [], next: 1 };
function VariadicInputs(parts) {
  const { core, board, block, sources } = parts;
  const [changes, setChanges] = useState8(UNCHANGED);
  const [checked, setChecked] = useState8(/* @__PURE__ */ new Set());
  const shown = shownInputs(board, block, changes);
  const { dragAndDropHooks } = useDragAndDrop({
    getItems: (keys) => [...keys].map((key) => ({ "text/plain": String(key) })),
    onReorder(event) {
      const [moved] = [...event.keys];
      if (moved === void 0) return;
      const keys = shown.map(({ key }) => key);
      const order = reordered(keys, String(moved), String(event.target.key), event.target.dropPosition);
      if (order !== null) {
        setChanges((now) => ({ ...now, order }));
      }
    }
  });
  const rows = shown.map((input2, i) => ({
    id: input2.key,
    input: input2,
    place: i + 1,
    error: checked.has(input2.key) ? nameError(board, shown, input2.key) : void 0
  }));
  function prepare() {
    flushSync3(() => {
      setChecked(new Set(shown.map(({ key }) => key)));
    });
    return shown.some(({ key }) => nameError(board, shown, key) !== void 0) ? void 0 : variadicBatch(core, board, block, shown);
  }
  return /* @__PURE__ */ jsxs6(InputsForm, { ...parts, variadic: true, prepare, children: [
    /* @__PURE__ */ jsx7(
      GridList,
      {
        "aria-label": `Inputs of ${blockName(board, block)}`,
        items: rows,
        dragAndDropHooks,
        keyboardNavigationBehavior: "tab",
        renderEmptyState: () => "No link feeds the block yet.",
        className: "bc:gap-1.5",
        children: (row) => /* @__PURE__ */ jsx7(GridListItem, { id: row.id, textValue: inputText(board, row.input, row.place), className: ROW, children: /* @__PURE__ */ jsx7(
          VariadicInput,
          {
            sources,
            input: row.input,
            place: row.place,
            error: row.error,
            onChange: (change) => {
              setChanges((now) => withChange(now, row.id, change));
            },
            onCheck: () => {
              setChecked((now) => /* @__PURE__ */ new Set([...now, row.id]));
            },
            onRemove: () => {
              setChanges((now) => withRemoved(now, row.id));
            }
          }
        ) })
      }
    ),
    /* @__PURE__ */ jsx7(
      AddInput,
      {
        sources,
        onAdd: (from) => {
          setChanges((now) => ({ ...now, added: [...now.added, { key: String(now.next), from, input: "" }], next: now.next + 1 }));
        }
      }
    )
  ] });
}
function shownInputs(board, block, changes) {
  const removed = new Set(changes.removed);
  const held = (from) => from !== void 0 && Object.hasOwn(board.blocks, from);
  const links = createGraph(board).linksInto(block).flatMap((id2) => {
    const link = own(board.links, id2);
    if (link === void 0 || removed.has(id2)) return [];
    const change = own(changes.links, id2);
    const from = change?.from;
    return [{ key: id2, from: held(from) ? from : link.from, input: change?.input ?? link.input }];
  });
  const inputs = [...links, ...changes.added.filter(({ from }) => held(from))];
  const { order } = changes;
  if (order === void 0) return inputs;
  const place = ({ key }) => {
    const at = order.indexOf(key);
    return at === -1 ? order.length : at;
  };
  return inputs.sort((a, b) => place(a) - place(b));
}
function withChange(changes, key, change) {
  if (changes.added.some((input2) => input2.key === key)) {
    return { ...changes, added: changes.added.map((input2) => input2.key === key ? { ...input2, ...change } : input2) };
  }
  return { ...changes, links: { ...changes.links, [key]: { ...own(changes.links, key), ...change } } };
}
function withRemoved(changes, key) {
  if (changes.added.some((input2) => input2.key === key)) {
    return { ...changes, added: changes.added.filter((input2) => input2.key !== key) };
  }
  return { ...changes, removed: [...changes.removed, key] };
}
function reordered(keys, moved, target2, position) {
  const others = keys.filter((key) => key !== moved);
  const at = others.indexOf(target2);
  if (moved === target2 || position === "on" || at === -1) return null;
  const index = position === "before" ? at : at + 1;
  const next = [...others.slice(0, index), moved, ...others.slice(index)];
  return next.every((key, i) => key === keys[i]) ? null : next;
}
function nameError(board, shown, key) {
  const input2 = shown.find((other) => other.key === key);
  if (input2 === void 0 || input2.input.trim() === own(board.links, key)?.input) return void 0;
  const taken = shown.flatMap((other) => other.key === key ? [] : [other.input.trim()]);
  return inputNameError(input2.input, taken);
}
function variadicBatch(core, board, block, shown) {
  const into = createGraph(board).linksInto(block);
  const minted = /* @__PURE__ */ new Set();
  const order = [];
  const changes = [];
  const adds = [];
  for (const { key, from, input: input2 } of shown) {
    const name2 = input2.trim();
    const link = own(board.links, key);
    if (link === void 0) {
      const id2 = mint(core, minted);
      order.push(id2);
      adds.push({ id: ADD_LINK, args: { id: id2, from, to: block, ...name2 === "" ? {} : { input: name2 } } });
      continue;
    }
    order.push(key);
    const args = { ...from === link.from ? {} : { from }, ...name2 === link.input ? {} : { input: name2 } };
    if (Object.keys(args).length > 0) changes.push({ id: CHANGE_LINK, args: { id: key, ...args } });
  }
  const kept = into.filter((id2) => order.includes(id2));
  return [
    ...into.filter((id2) => !order.includes(id2)).map((id2) => ({ id: REMOVE_LINK, args: { id: id2 } })),
    ...changes,
    ...adds,
    ...moves([...kept, ...minted], order)
  ];
}
function moves(from, to) {
  if (from.length === to.length && from.every((id2, i) => id2 === to[i])) return [];
  return to.map((id2) => ({ id: CHANGE_LINK, args: { id: id2, before: null } }));
}
function inputText(board, { from, input: input2 }, place) {
  const source = blockName(board, from);
  const name2 = input2.trim();
  return name2 === "" ? `Input ${String(place)}, from ${source}` : `Input ${String(place)}, ${name2}, from ${source}`;
}
function VariadicInput({
  sources,
  input: input2,
  place,
  error,
  onChange,
  onCheck,
  onRemove
}) {
  const search = useSearch(sources, blockTexts, "No block can feed the block.");
  const what = `input ${String(place)}`;
  return /* @__PURE__ */ jsxs6("div", { className: "bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2", onPointerDownCapture: holdStill, children: [
    /* @__PURE__ */ jsx7(
      ComboBox2,
      {
        "aria-label": `Source of ${what}`,
        ...search,
        value: input2.from,
        placeholder: "Search blocks",
        isRequired: true,
        className: "bc:min-w-0 bc:flex-1",
        onChange: (key) => {
          if (key !== null && String(key) !== input2.from) onChange({ from: String(key) });
        },
        children: blockRow
      }
    ),
    /* @__PURE__ */ jsx7(
      TextField3,
      {
        "aria-label": `Name of ${what}`,
        value: input2.input,
        placeholder: "No name",
        isInvalid: error !== void 0,
        errorMessage: error,
        autoComplete: "off",
        ...identifier,
        className: "bc:w-1/3 bc:shrink-0",
        onChange: (value2) => {
          onChange({ input: value2 });
        },
        onBlur: onCheck
      }
    ),
    /* @__PURE__ */ jsx7(RemoveButton, { label: `Remove ${what}`, onPress: onRemove })
  ] });
}
function holdStill(event) {
  const item = event.currentTarget.closest('[role="row"][draggable="true"]');
  if (item === null) return;
  item.setAttribute("draggable", "false");
  const doc = item.ownerDocument;
  const release = () => {
    item.setAttribute("draggable", "true");
    doc.removeEventListener("pointerup", release, true);
    doc.removeEventListener("pointercancel", release, true);
  };
  doc.addEventListener("pointerup", release, true);
  doc.addEventListener("pointercancel", release, true);
}
function AddInput({ sources, onAdd }) {
  const search = useSearch(sources, blockTexts, "No block can feed the block.");
  return /* @__PURE__ */ jsx7(
    ComboBox2,
    {
      label: "Add input",
      ...search,
      value: null,
      placeholder: "Search blocks",
      onChange: (key) => {
        if (key !== null) onAdd(String(key));
      },
      children: blockRow
    }
  );
}
function RemoveButton({ label, onPress }) {
  return /* @__PURE__ */ jsx7(Button4, { variant: "quiet", "aria-label": label, onPress, className: "bc:size-7 bc:shrink-0 bc:justify-center bc:p-0", children: /* @__PURE__ */ jsx7("span", { title: label, className: "bc:flex bc:size-full bc:items-center bc:justify-center", children: /* @__PURE__ */ jsx7(Cross, {}) }) });
}
function Cross() {
  return /* @__PURE__ */ jsx7("svg", { width: "10", height: "10", viewBox: "0 0 10 10", fill: "none", stroke: "currentColor", strokeWidth: "1.4", strokeLinecap: "round", "aria-hidden": "true", children: /* @__PURE__ */ jsx7("path", { d: "M2.5 2.5l5 5M7.5 2.5l-5 5" }) });
}
var ROW = "bc:rounded-md bc:border bc:border-border bc:bg-muted bc:py-1 bc:data-dragging:opacity-60";
export {
  editInputs
};

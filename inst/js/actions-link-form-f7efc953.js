// srcts/actions/link-form.tsx
import { useState as useState8 } from "react";
import { flushSync as flushSync3 } from "react-dom";
import { Button as Button4, ComboBox as ComboBox2, Form as Form2, Select as Select2, SelectItem as SelectItem2, TextField as TextField3 } from "@blockr/ui";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
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
function namesInto(board, to, except) {
  return Object.entries(board.links).filter(([id2, link]) => id2 !== except && link.to === to && link.input !== "").map(([, link]) => link.input);
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
function once(ends2) {
  return new Map([...ends2].map(([id2, blocks]) => [id2, [...new Set(blocks)]]));
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
function takesLink(board, registry, to) {
  return (freeInputs(board, registry, to)?.length ?? 0) > 0;
}
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
function isVariadic(free) {
  return free.length === 1 && free[0] === "";
}
function focusFirstInvalid(form) {
  form.querySelector('[aria-invalid="true"]')?.focus();
}
function without2(errors, key) {
  return Object.fromEntries(Object.entries(errors).filter(([field]) => field !== key));
}

// srcts/actions/link-form.tsx
import { useBoard as useBoard2 } from "@blockr/core/react";
import { jsx as jsx7, jsxs as jsxs6 } from "react/jsx-runtime";
var ADD_LINK = "blockr.core-add_link";
var CHANGE_LINK = "blockr.core-change_link";
var createLinkForm = {
  render({ el, core, target: target2, picked, done }) {
    const given = picked === void 0 ? {} : startOf(picked);
    if ("input" in target2) {
      const input2 = target2.input === "" ? {} : { input: target2.input };
      const fixedInput = target2.input === "" ? {} : { fixedInput: target2.input };
      return drawForm(el, /* @__PURE__ */ jsx7(LinkForm, { core, start: { ...given, to: target2.block, ...input2 }, fixed: "to", ...fixedInput, done }));
    }
    if (!("block" in target2)) return drawForm(el, /* @__PURE__ */ jsx7(LinkForm, { core, start: given, done }));
    const { block } = target2;
    const fixed = given.to === block ? "to" : "from";
    const start = fixed === "to" ? { ...given, to: block } : { ...given, from: block };
    return drawForm(el, /* @__PURE__ */ jsx7(LinkForm, { core, start, fixed, done }));
  }
};
var editLinkForm = {
  render({ el, core, target: target2, done }) {
    if (!("link" in target2)) return void 0;
    return drawForm(el, /* @__PURE__ */ jsx7(EditLink, { core, id: target2.link, done }));
  }
};
function startOf(picked) {
  if (!isObject(picked)) return {};
  const { from, to, input: input2 } = picked;
  return {
    ...typeof from === "string" ? { from } : {},
    ...typeof to === "string" ? { to } : {},
    ...typeof input2 === "string" ? { input: input2 } : {}
  };
}
function fixedEnd(board, end, block, input2) {
  const name2 = block === void 0 ? "" : blockName(board, block);
  if (end === "from") return `A link from "${name2}".`;
  return input2 === void 0 ? `A link into "${name2}".` : `A link into the input "${input2}" of "${name2}".`;
}
function EditLink({ core, id: id2, done }) {
  const link = useBoard2(core, (board) => own(board.links, id2));
  return link === void 0 ? null : /* @__PURE__ */ jsx7(LinkForm, { core, editing: { id: id2, link }, done });
}
function LinkForm({
  core,
  start = {},
  fixed,
  fixedInput,
  editing,
  done
}) {
  const except = editing?.id;
  const [from, setFrom] = useState8(editing?.link.from ?? start.from);
  const [to, setTo] = useState8(editing?.link.to ?? start.to);
  const [input2, setInput] = useState8(editing?.link.input ?? start.input);
  const [name2, setName] = useState8(
    () => editing !== void 0 && isVariadicBlock(core.store.board(), core.blockRegistry(), editing.link.to) ? editing.link.input : ""
  );
  const [place, setPlace] = useState8(void 0);
  const [errors, setErrors] = useState8({});
  const [busy, setBusy] = useState8(false);
  const edits = useEdits(core);
  const { source, target: target2, fromOptions, toOptions, variadic, offered, chosen, hint, taken, among, fixedLine, targetName, few } = useBoard2(
    core,
    (board) => {
      const registry = core.blockRegistry();
      const others = withoutLink(board, except);
      const held = (block) => block !== void 0 && own(board.blocks, block) !== void 0;
      const source2 = held(from) ? from : void 0;
      const target3 = held(to) ? to : void 0;
      const free = target3 === void 0 ? [] : freeInputs(others, registry, target3) ?? [];
      const variadic2 = isVariadic(free);
      const offered2 = !variadic2 && editing !== void 0 && target3 === editing.link.to ? classInputs(board, registry, target3) : free;
      const chosen2 = variadic2 ? void 0 : input2 !== void 0 && offered2.includes(input2) ? input2 : free[0];
      return {
        source: source2,
        target: target3,
        ...ends(others, registry, source2, target3),
        variadic: variadic2,
        offered: offered2,
        chosen: chosen2,
        // What the form says below the input where another link feeds the one
        // picked, so that applying the form swaps the inputs of the two links.
        hint: editing !== void 0 && target3 === editing.link.to && chosen2 !== void 0 ? swapHint(board, registry, editing, chosen2) : void 0,
        taken: target3 === void 0 ? [] : namesInto(board, target3, except),
        // Where the link goes into a variadic block, the block's other links
        // and the place that the link falls in where nothing moves it, which
        // the form offers until the user picks another.
        among: target3 === void 0 || !variadic2 ? void 0 : placeAmong(board, target3, except),
        fixedLine: fixed === void 0 ? void 0 : fixedEnd(board, fixed, fixed === "from" ? from : to, fixedInput),
        targetName: target3 === void 0 ? "" : blockName(board, target3),
        // Whether the board holds fewer blocks than a link joins.
        few: Object.keys(board.blocks).length < 2
      };
    }
  );
  const fromSearch = useSearch(fromOptions, blockTexts);
  const toSearch = useSearch(toOptions, blockTexts);
  const position = among === void 0 ? void 0 : place !== void 0 && place <= among.others.length + 1 ? place : among.place;
  function check2() {
    return {
      ...source === void 0 ? { from: "Pick the block that the link comes from." } : {},
      ...target2 === void 0 ? { to: "Pick the block that the link goes to." } : {},
      ...variadic ? optional("input", inputNameError(name2, taken)) : {}
    };
  }
  function batch(source2, target3) {
    const linkInput = fixedInput ?? (variadic ? name2.trim() : chosen);
    const link = { from: source2, to: target3, ...linkInput === void 0 ? {} : { input: linkInput } };
    const moved = among !== void 0 && position !== void 0 && position !== among.place;
    const before = moved ? among.others[position - 1] ?? null : void 0;
    if (editing === void 0) {
      const id2 = core.mintId((minted) => Object.hasOwn(core.store.board().links, minted));
      const added = { id: ADD_LINK, args: { id: id2, ...link } };
      return before === void 0 ? [added] : [added, { id: CHANGE_LINK, args: { id: id2, before } }];
    }
    return [{ id: CHANGE_LINK, args: { id: editing.id, ...link, ...before === void 0 ? {} : { before } } }];
  }
  async function submit(event) {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check2();
    flushSync3(() => {
      setErrors(found);
      edits.clear();
    });
    if (Object.keys(found).length > 0 || source === void 0 || target2 === void 0 || busy) {
      focusFirstInvalid(form);
      return;
    }
    const board = core.store.board();
    const title = editing === void 0 ? "Could not add the link." : `Could not change the link from "${blockName(board, editing.link.from)}" to "${blockName(board, editing.link.to)}".`;
    const calls = batch(source, target2);
    setBusy(true);
    await edits.run(title, calls, done);
    setBusy(false);
  }
  return /* @__PURE__ */ jsxs6(
    Form2,
    {
      "aria-label": editing === void 0 ? "Add link" : "Change link",
      validationBehavior: "aria",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        fixedLine === void 0 ? null : /* @__PURE__ */ jsx7("p", { className: classes.hint, children: fixedLine }),
        /* @__PURE__ */ jsxs6("div", { className: fixed === void 0 ? "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2" : "bc:grid", children: [
          fixed === "from" ? null : /* @__PURE__ */ jsx7(
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
          fixed === "to" ? null : /* @__PURE__ */ jsx7(
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
        target2 === void 0 || fixedInput !== void 0 ? null : variadic ? /* @__PURE__ */ jsxs6("div", { className: among === void 0 ? "bc:grid" : "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx7(
            TextField3,
            {
              label: "Input name",
              value: name2,
              description: `Optional. The block "${targetName}" takes any number of links, each by its name or, where it has none, by its position.`,
              errorMessage: errors.input,
              isInvalid: errors.input !== void 0,
              autoComplete: "off",
              ...identifier,
              onChange: (value2) => {
                setName(value2);
                setErrors((now) => without2(now, "input"));
              },
              onBlur: () => {
                setErrors((now) => ({ ...now, ...optional("input", inputNameError(name2, taken)) }));
              }
            }
          ),
          among === void 0 ? null : /* @__PURE__ */ jsx7(
            Select2,
            {
              label: "Position",
              isRequired: true,
              items: Array.from({ length: among.others.length + 1 }, (_, i) => ({ id: String(i + 1) })),
              value: position === void 0 ? null : String(position),
              isDisabled: among.others.length === 0,
              onChange: (key) => {
                if (key !== null) setPlace(Number(key));
              },
              children: (item) => /* @__PURE__ */ jsx7(SelectItem2, { id: item.id, children: item.id })
            }
          )
        ] }) : /* @__PURE__ */ jsx7(
          Select2,
          {
            label: "Input",
            isRequired: true,
            items: offered.map((id2) => ({ id: id2 })),
            value: chosen ?? null,
            isDisabled: offered.length < 2,
            description: hint,
            onChange: (key) => {
              if (key !== null) setInput(String(key));
            },
            children: (item) => /* @__PURE__ */ jsx7(SelectItem2, { id: item.id, children: item.id })
          }
        ),
        editing === void 0 && few ? /* @__PURE__ */ jsx7("p", { className: classes.hint, children: "A link joins two blocks, so the board needs two blocks first." }) : null,
        /* @__PURE__ */ jsx7("div", { className: "bc:flex bc:gap-2", children: /* @__PURE__ */ jsx7(Button4, { type: "submit", variant: "primary", isDisabled: busy, children: editing === void 0 ? "Add link" : "Apply" }) }),
        /* @__PURE__ */ jsx7(Refusal, { refusal: edits.refusal })
      ]
    }
  );
}
function ends(board, registry, source, target2) {
  const graph = createGraph(board);
  const upstream = source === void 0 ? /* @__PURE__ */ new Set() : graph.withUpstream([source]);
  const downstream = target2 === void 0 ? /* @__PURE__ */ new Set() : graph.withDownstream([target2]);
  const ids = Object.keys(board.blocks);
  return {
    fromOptions: ids.filter((id2) => !downstream.has(id2)).map((id2) => blockItem(board, registry, id2)),
    toOptions: ids.filter((id2) => !upstream.has(id2) && takesLink(board, registry, id2)).map((id2) => blockItem(board, registry, id2))
  };
}
function swapHint(board, registry, editing, input2) {
  const other = linkFeeding(board, registry, editing.link.to, input2, editing.id);
  const from = other === void 0 ? void 0 : own(board.links, other)?.from;
  if (from === void 0) return void 0;
  return `The link from "${blockName(board, from)}" feeds "${input2}". Applying gives that link "${editing.link.input}", so the two links swap their inputs.`;
}
function placeAmong(board, to, id2) {
  const ids = Object.keys(board.links).filter((other) => other === id2 || own(board.links, other)?.to === to);
  const others = ids.filter((other) => other !== id2);
  return { others, place: id2 === void 0 ? others.length + 1 : ids.indexOf(id2) + 1 };
}
function withoutLink(board, id2) {
  if (id2 === void 0 || !Object.hasOwn(board.links, id2)) return board;
  const links = Object.fromEntries(Object.entries(board.links).filter(([other]) => other !== id2));
  return { ...board, links };
}
function optional(key, error) {
  return error === void 0 ? {} : { [key]: error };
}
export {
  createLinkForm,
  editLinkForm
};

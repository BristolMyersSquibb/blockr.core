// srcts/actions/stack-form.tsx
import { useState as useState7 } from "react";
import {
  Button as Button3,
  Checkbox,
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ComboBox,
  ComboBoxSection,
  Form,
  TextField as TextField2
} from "@blockr/ui";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}
function listing(items) {
  if (items.length <= 1) return items.join("");
  return `${items.slice(0, -1).join(", ")} and ${items.at(-1) ?? ""}`;
}
function quoted(items) {
  return listing(items.map((item) => `"${item}"`));
}

// srcts/util/blocks.ts
function blockName(board, block) {
  return own(board.blocks, block)?.name ?? block;
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
function suggestedName(stacks) {
  const names = new Set(stacks.map((stack) => stack.name));
  let n = 1;
  while (names.has(`Stack ${String(n)}`)) n++;
  return `Stack ${String(n)}`;
}

// srcts/actions/stack-form.tsx
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

// srcts/actions/palette.ts
var PALETTE = ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"];
function suggestedColor(stacks) {
  const used = new Set(stacks.map((stack) => stack.color?.toLowerCase()));
  return PALETTE.find((color2) => !used.has(color2)) ?? PALETTE[stacks.length % PALETTE.length] ?? PALETTE[0];
}

// srcts/actions/targets.ts
function idOf(target2, kind) {
  if (kind === "block") return "block" in target2 ? target2.block : void 0;
  if (kind === "link") return "link" in target2 ? target2.link : void 0;
  return "stack" in target2 ? target2.stack : void 0;
}
function held(board, target2, kind) {
  const objects = "selection" in target2 ? target2.selection : [target2];
  const kept = { block: board.blocks, link: board.links, stack: board.stacks }[kind];
  const ids = objects.flatMap((object) => {
    const id2 = idOf(object, kind);
    return id2 !== void 0 && Object.hasOwn(kept, id2) ? [id2] : [];
  });
  return [...new Set(ids)];
}
function heldBlocks(board, target2) {
  return held(board, target2, "block");
}

// srcts/actions/stack-form.tsx
import { jsx as jsx6, jsxs as jsxs5 } from "react/jsx-runtime";
var ADD_STACK = "blockr.core-add_stack";
var CHANGE_STACK = "blockr.core-change_stack";
var createStackForm = {
  render({ el, core, target: target2, done }) {
    return drawForm(el, /* @__PURE__ */ jsx6(StackForm, { core, given: heldBlocks(core.store.board(), target2), done }));
  }
};
var editStackForm = {
  render({ el, core, target: target2, done }) {
    if (!("stack" in target2)) return void 0;
    return drawForm(el, /* @__PURE__ */ jsx6(EditStack, { core, id: target2.stack, done }));
  }
};
function EditStack({ core, id: id2, done }) {
  const stack = useBoard(core, (board) => own(board.stacks, id2));
  return stack === void 0 ? null : /* @__PURE__ */ jsx6(StackForm, { core, editing: { id: id2, stack }, done });
}
function stackColor(color2) {
  return typeof color2 === "string" ? color2 : color2.toString("hex");
}
function sameColor(a, b) {
  return a === null || b === null ? a === b : a.toLowerCase() === b.toLowerCase();
}
function StackForm({
  core,
  given = [],
  editing,
  done
}) {
  const [name2, setName] = useState7(editing?.stack.name ?? "");
  const [color2, setColor] = useState7(
    () => editing === void 0 ? suggestedColor(Object.values(core.store.board().stacks)) : editing.stack.color
  );
  const [blocks, setBlocks] = useState7(editing?.stack.blocks ?? given);
  const fixed = editing === void 0 && given.length > 0;
  const [busy, setBusy] = useState7(false);
  const edits = useEdits(core);
  const { options, kept, fallbackName, line } = useBoard(core, (board) => {
    const registry = core.blockRegistry();
    const holder = /* @__PURE__ */ new Map();
    for (const [id2, stack] of Object.entries(board.stacks)) {
      if (id2 === editing?.id) continue;
      for (const block of stack.blocks) holder.set(block, stack.name);
    }
    const kept2 = blocks.filter((block) => Object.hasOwn(board.blocks, block));
    return {
      // The blocks, those in no stack first, and those in another stack, which
      // say that they leave it.
      options: Object.keys(board.blocks).map((id2) => {
        const item = blockItem(board, registry, id2);
        const other = holder.get(id2);
        return other === void 0 ? { ...item, free: true } : { ...item, free: false, detail: `${item.detail} \xB7 leaves the stack ${other}` };
      }),
      kept: kept2,
      fallbackName: editing === void 0 ? suggestedName(Object.values(board.stacks)) : editing.stack.name,
      // The line that names the new stack's blocks, where the target fixes
      // them.
      line: fixed ? stackOf(board, kept2) : void 0
    };
  });
  const search = useSearch(options, blockTexts, "The board holds no block.");
  const sections = [
    { id: "free", title: "In no stack", items: search.items.filter((item) => item.free) },
    { id: "held", title: "In another stack", items: search.items.filter((item) => !item.free) }
  ].filter((section) => section.items.length > 0);
  function batch() {
    const trimmed = name2.trim();
    const hex = color2 === null ? null : stackColor(color2);
    if (editing === void 0) {
      const id3 = core.mintId((minted) => Object.hasOwn(core.store.board().stacks, minted));
      return [{ id: ADD_STACK, args: { id: id3, blocks: kept, color: hex, ...trimmed === "" ? {} : { name: trimmed } } }];
    }
    const { id: id2, stack } = editing;
    const changed = {
      ...trimmed === "" || trimmed === stack.name ? {} : { name: trimmed },
      ...sameBlocks(kept, stack.blocks) ? {} : { blocks: kept },
      ...sameColor(hex, stack.color) ? {} : { color: hex }
    };
    return Object.keys(changed).length === 0 ? [] : [{ id: CHANGE_STACK, args: { id: id2, ...changed } }];
  }
  async function submit(event) {
    event.preventDefault();
    if (busy) return;
    const title = editing === void 0 ? "Could not add the stack." : `Could not change the stack "${editing.stack.name}".`;
    const calls = batch();
    setBusy(true);
    await edits.run(title, calls, done);
    setBusy(false);
  }
  return /* @__PURE__ */ jsxs5(
    Form,
    {
      "aria-label": editing === void 0 ? "Add stack" : "Change stack",
      validationBehavior: "aria",
      onSubmit: (event) => {
        void submit(event);
      },
      children: [
        line === void 0 ? null : /* @__PURE__ */ jsx6("p", { className: classes.hint, children: line }),
        /* @__PURE__ */ jsxs5("div", { className: "bc:grid bc:grid-cols-2 bc:items-start bc:gap-2", children: [
          /* @__PURE__ */ jsx6(
            TextField2,
            {
              label: "Name",
              value: name2,
              placeholder: fallbackName,
              description: editing === void 0 ? `Optional. "${fallbackName}" where left empty.` : void 0,
              autoComplete: "off",
              onChange: setName
            }
          ),
          /* @__PURE__ */ jsx6(StackColor, { color: color2, onChange: setColor })
        ] }),
        fixed ? null : /* @__PURE__ */ jsx6(
          ComboBox,
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
            children: (section) => /* @__PURE__ */ jsx6(ComboBoxSection, { id: section.id, title: section.title, items: section.items, children: blockRow })
          }
        ),
        /* @__PURE__ */ jsx6("div", { className: "bc:flex bc:gap-2", children: /* @__PURE__ */ jsx6(Button3, { type: "submit", variant: "primary", isDisabled: busy, children: editing === void 0 ? "Add stack" : "Apply" }) }),
        /* @__PURE__ */ jsx6(Refusal, { refusal: edits.refusal })
      ]
    }
  );
}
function StackColor({ color: color2, onChange }) {
  const [last, setLast] = useState7(color2 ?? PALETTE[0]);
  const shown = color2 ?? last;
  return /* @__PURE__ */ jsxs5("fieldset", { className: classes.fieldset, children: [
    /* @__PURE__ */ jsx6("legend", { className: classes.legend, children: "Colour" }),
    /* @__PURE__ */ jsxs5("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-x-3 bc:gap-y-1", children: [
      /* @__PURE__ */ jsx6(
        Checkbox,
        {
          isSelected: color2 !== null,
          onChange: (selected) => {
            onChange(selected ? last : null);
          },
          children: "A colour of its own"
        }
      ),
      /* @__PURE__ */ jsxs5(
        ColorPicker,
        {
          label: stackColor(shown).toUpperCase(),
          value: shown,
          isDisabled: color2 === null,
          onChange: (picked) => {
            setLast(picked);
            onChange(picked);
          },
          children: [
            /* @__PURE__ */ jsx6(ColorArea, { colorSpace: "hsb", xChannel: "saturation", yChannel: "brightness" }),
            /* @__PURE__ */ jsx6(ColorSlider, { colorSpace: "hsb", channel: "hue" }),
            /* @__PURE__ */ jsx6(ColorField, { label: "Hex" }),
            /* @__PURE__ */ jsx6(ColorSwatchPicker, { "aria-label": "Palette", className: "bc:max-w-36", children: PALETTE.map((hex) => /* @__PURE__ */ jsx6(ColorSwatchPickerItem, { color: hex }, hex)) })
          ]
        }
      )
    ] })
  ] });
}
function stackOf(board, blocks) {
  const names = quoted(blocks.map((block) => blockName(board, block)));
  return blocks.length === 1 ? `A stack of the block ${names}.` : `A stack of the blocks ${names}.`;
}
function sameBlocks(a, b) {
  return a.length === b.length && a.every((block, i) => block === b[i]);
}
export {
  createStackForm,
  editStackForm
};

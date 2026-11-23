// The block picker: the form that adds a block of a class from the block
// registry. It is the advanced action of core's four board actions that add a
// block: Add block, Append block, Prepend block and Insert block
// (../actions/add.tsx,
// https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions).
// So every place that shows one of them adds a block the same way, the Edit
// board plugin among them, which mounts the advanced action of Add block.
// The user fills in the form, and the block goes onto the board only as the
// user submits it, with these fields:
// - The block's class, which a select picks among the block registry's
//   entries, by their names, descriptions, keywords and registry ids, grouped
//   by category. Where the user opened the picker from a quick action, the
//   search starts from the text in the quick action's filter.
// - The input that the new link feeds, where the mode adds one (below).
// - The new block's id and its name, which the user may leave empty. Then the
//   form mints the id through CoreApi, and the command takes the name from the
//   block's class.
// Each of the four modes adds the block in one step:
// - On its own, with add_block.
// - After a block, taking that block's result, with a batch of add_block and
//   add_link from that block to the new one. The new block's class needs an
//   input for that.
// - Before a block, feeding an input of that block that no link feeds, with a
//   batch of add_block and add_link from the new block to that block. Where
//   the mode names the input, as Prepend block's on an input does, the link
//   goes into that input, which the form names in a line in place of the
//   field of the input.
// - On a link, between the two blocks that it joins, with a batch of
//   add_block, add_link from the block that the link comes from to the new
//   block, and change_link, which has the link come from the new block. So
//   the link keeps its id, its input and its place among the links. The new
//   block's class needs an input for that too.
// Added after or before a block, or on a link, the new block comes with a
// link:
// - Where the link could go into one of several inputs, the user picks the
//   input, since the input that a link feeds changes what a block computes,
//   as the left and the right table of a merge.
// - Where it goes into a variadic block, the user may name the input, as
//   `b2` in `rbind(a, b2 = b)`, or leave it empty for a positional one.
// - A batch hands no value from one command to the next, and add_link names
//   the new block. So the form fixes the block's id before it hands on the
//   batch, as every board action of core's that adds an object does.
// The form checks the id and the input's name as the user leaves each field
// and as they submit, before the commands do, so that the user learns what to
// change next to the field. It hands the batch to its `done`, which runs it as
// one step of the user, as the done() of an advanced action does. What the
// command registry or the store refuses all the same, the form says below its
// button, until the user submits it again, and it takes the notifications of
// the refusal, so that core raises none.

import { useMemo, useState } from "react";
import type { ReactElement, SyntheticEvent } from "react";
import { flushSync } from "react-dom";
import { defaultBlockName, freeInputs } from "../commands/index.ts";
import { ComboBox, ComboBoxItem, ComboBoxSection, Form, Select, SelectItem, Text, TextField, useBoard } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockId, Board, Link, LinkId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { classes, identifier, Refusal, useEdits, useSearch } from "../ui/index.ts";
import type { Done } from "../ui/index.ts";
import { own, packageFromId } from "../util/index.ts";

// Where the new block goes: on its own, after a block, before a block, or on
// a link. Before a block, it may feed the input given, "" for the next place
// of a variadic block, where its link goes without a name.
export type AddMode =
  | { readonly kind: "add" }
  | { readonly kind: "after"; readonly from: BlockId }
  | { readonly kind: "before"; readonly to: BlockId; readonly input?: string }
  | { readonly kind: "on"; readonly link: LinkId };

export interface PickerProps {
  // Through which the form reads the board and the block registry, and mints
  // the id of the block that it adds.
  readonly core: Pick<CoreApi, "store" | "blockRegistry" | "commands" | "mintId">;
  readonly mode: AddMode;
  // Runs the batch that adds the block as one step of the user, as the done()
  // of an advanced action does, and rejects with the refusal, whose
  // notifications the notifier takes.
  readonly done: Done;
  // The text that the search of block classes starts from, such as the one
  // that the user typed into the filter of a quick action.
  readonly filter?: string;
  // Whether the form puts the keyboard's focus in its search of block classes
  // as it shows.
  readonly autofocus?: boolean;
}

type Errors = Partial<Record<"registry" | "id" | "input", string>>;

export function BlockPicker({ core, mode, done, filter = "", autofocus = false }: PickerProps): ReactElement | null {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const edits = useEdits(core);
  const [picked, setPicked] = useState<string | undefined>(undefined);
  const [input, setInput] = useState<string | undefined>(undefined);
  const [inputName, setInputName] = useState("");
  const [id, setId] = useState("");
  const [name, setName] = useState("");
  const [errors, setErrors] = useState<Errors>({});
  const [busy, setBusy] = useState(false);

  // A block added after another or on a link takes a block's result, so its
  // class has an input.
  const takes = mode.kind === "after" || mode.kind === "on";
  const options = useMemo(() => blockClasses(Object.values(registry).filter((entry) => !takes || entry.inputs.length > 0)), [registry, takes]);
  const search = useSearch(
    options,
    (item) => [item.name, item.description, item.id, item.category, item.pkg ?? "", ...item.keywords],
    "No block class to pick.",
    filter,
  );
  const entry = picked === undefined ? undefined : own(registry, picked);
  const link = linkChoice(mode, board, registry, entry);
  const chosen = link.kind === "pick" ? (input !== undefined && link.inputs.includes(input) ? input : link.inputs[0]) : undefined;
  // The link that the new block goes on, which the board holds until core
  // ends the advanced action as the link leaves the board.
  const on = mode.kind === "on" ? own(board.links, mode.link) : undefined;
  if (mode.kind === "on" && on === undefined) return null;

  function check(): Errors {
    return {
      ...(entry === undefined ? { registry: "Pick the class of the new block." } : {}),
      ...optional("id", blockIdError(id, board)),
      ...(link.kind === "name" ? optional("input", inputNameError(inputName, link.taken)) : {}),
    };
  }

  async function submit(event: SyntheticEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check();
    flushSync(() => {
      setErrors(found);
      edits.clear();
    });
    if (Object.keys(found).length > 0 || entry === undefined || busy) {
      focusFirstInvalid(form);
      return;
    }
    const args: Record<string, Json> = {
      registry_id: entry.id,
      ...filled("id", id),
      ...filled("name", name),
    };
    const into: Record<string, Json> = {
      ...(chosen === undefined ? {} : { input: chosen }),
      ...(link.kind === "name" ? filled("input", inputName) : {}),
      ...(link.kind === "fixed" ? { input: link.input } : {}),
    };
    setBusy(true);
    // Once the batch has run, core ends the advanced action, and renders it
    // again where it shows in a plugin's room, so that the form starts afresh
    // for the next block.
    await edits.run("Could not add the block.", pickedBatch(core, mode, on, args, into), done);
    setBusy(false);
  }

  return (
    <Form
      aria-label="Add block"
      validationBehavior="aria"
      onSubmit={(event) => {
        void submit(event);
      }}
    >
      <ComboBox
        label="Block class"
        autoFocus={autofocus}
        defaultInputValue={filter}
        {...search}
        items={byCategory(search.items)}
        value={picked ?? null}
        placeholder={options.length === 0 ? "No block class to pick" : "Search by name, description or keyword"}
        isInvalid={errors.registry !== undefined}
        errorMessage={errors.registry}
        description={entry?.description === "" ? undefined : entry?.description}
        onChange={(key) => {
          setPicked(key === null ? undefined : String(key));
          // After a block or on a link, the inputs to pick among are those of
          // the class picked, while before a block they are that block's,
          // whatever the class.
          if (takes) setInput(undefined);
          setErrors((now) => without(now, "registry"));
        }}
      >
        {(category) => (
          <ComboBoxSection id={category.id} title={category.name} items={category.items}>
            {(item) => (
              <ComboBoxItem id={item.id} textValue={item.name}>
                <Text slot="label" className="bc:shrink-0 bc:truncate">
                  {item.name}
                </Text>
                {item.description === "" ? null : <Text slot="description">{item.description}</Text>}
                {item.pkg === null ? null : <span className={BADGE}>{item.pkg}</span>}
              </ComboBoxItem>
            )}
          </ComboBoxSection>
        )}
      </ComboBox>
      {link.kind === "pick" ? (
        <Select
          label={link.label}
          isRequired
          items={link.inputs.map((input) => ({ id: input }))}
          value={chosen ?? null}
          onChange={(key) => {
            if (key !== null) setInput(String(key));
          }}
        >
          {(item) => <SelectItem id={item.id}>{item.id}</SelectItem>}
        </Select>
      ) : null}
      {link.kind === "name" ? (
        <TextField
          label={link.label}
          value={inputName}
          description="Optional. Leave it empty for an input without a name, which the block takes in the order of its links."
          errorMessage={errors.input}
          isInvalid={errors.input !== undefined}
          autoComplete="off"
          onChange={(value) => {
            setInputName(value);
            setErrors((now) => without(now, "input"));
          }}
          onBlur={() => {
            setErrors((now) => ({ ...now, ...optional("input", inputNameError(inputName, link.taken)) }));
          }}
        />
      ) : null}
      {link.kind === "fixed" ? <p className={styles.hint}>{link.line}</p> : null}
      {link.kind === "no_free_inputs" ? <p className={styles.alert}>{link.why}</p> : null}
      <div className="bc:grid bc:grid-cols-2 bc:items-start bc:gap-2">
        <TextField
          label="ID"
          value={id}
          description="Optional. Made up where left empty."
          errorMessage={errors.id}
          isInvalid={errors.id !== undefined}
          autoComplete="off"
          {...identifier}
          onChange={(value) => {
            setId(value);
            setErrors((now) => without(now, "id"));
          }}
          onBlur={() => {
            setErrors((now) => ({ ...now, ...optional("id", blockIdError(id, board)) }));
          }}
        />
        <TextField
          label="Name"
          value={name}
          placeholder={entry === undefined ? undefined : defaultBlockName(entry.id)}
          description="Optional. The class's name where left empty."
          autoComplete="off"
          onChange={setName}
        />
      </div>
      <div>
        <button type="submit" className={classes.primary} disabled={busy || link.kind === "no_free_inputs"}>
          Add block
        </button>
      </div>
      <Refusal refusal={edits.refusal} />
    </Form>
  );
}

// What the form asks about the link that comes with the new block:
// - "pick": the input among several, where the input is fixed.
// - "name": an optional name, where the input is variadic, which the variadic
//   block's other links take in `taken`.
// - "fixed": nothing, before a block where the mode names the input, which
//   the form names in `line` instead.
// - "none": nothing, where the mode adds no link, where the user has not
//   picked the class of a block added after another or on a link yet, or
//   where the link has one input to go into, which the command then finds.
// - "no_free_inputs": nothing, before a block whose inputs all take a link
//   already, or where a link feeds the input that the mode names. The form
//   shows `why` instead, and adds no block.
// After a block and on a link alike, the link goes into an input of the new
// block, of which no link feeds any.
type LinkChoice =
  | { readonly kind: "pick"; readonly label: string; readonly inputs: readonly string[] }
  | { readonly kind: "name"; readonly label: string; readonly taken: readonly string[] }
  | { readonly kind: "fixed"; readonly input: string; readonly line: string }
  | { readonly kind: "none" }
  | { readonly kind: "no_free_inputs"; readonly why: string };

function linkChoice(
  mode: AddMode,
  board: Pick<Board, "blocks" | "links">,
  registry: Readonly<Record<string, RegistryEntry>>,
  entry: RegistryEntry | undefined,
): LinkChoice {
  if (mode.kind === "before") {
    const target = own(board.blocks, mode.to);
    const free = freeInputs(board, registry, mode.to) ?? [];
    const name = target?.name ?? mode.to;
    const of = `of "${name}"`;
    if (mode.input !== undefined) return fixedInput(free, mode.input, name);
    if (isVariadic(free)) return { kind: "name", label: `Input name ${of}`, taken: namesInto(board, mode.to) };
    if (free.length === 0) return { kind: "no_free_inputs", why: `Every input ${of} takes a link already.` };
    return free.length > 1 ? { kind: "pick", label: `Input ${of}`, inputs: free } : { kind: "none" };
  }
  if ((mode.kind === "after" || mode.kind === "on") && entry !== undefined) {
    if (entry.inputs.includes("...")) return { kind: "name", label: `Input name of the new ${entry.name}`, taken: [] };
    if (entry.inputs.length > 1) return { kind: "pick", label: `Input of the new ${entry.name}`, inputs: entry.inputs };
  }
  return { kind: "none" };
}

// What the form asks about a link into the input given, of the block of the
// name given, whose free inputs are those given. It asks nothing either way:
// - While no link feeds the input, the form names the input in a line.
// - Once a link feeds it, the form says why it adds no block.
// The input "" is the next place of a variadic block, into which the link
// goes without a name.
function fixedInput(free: readonly string[], input: string, name: string): LinkChoice {
  const named = input === "" ? `"${name}"` : `the input "${input}" of "${name}"`;
  if (free.includes(input)) return { kind: "fixed", input, line: `The new block feeds ${named}.` };
  return { kind: "no_free_inputs", why: input === "" ? `"${name}" takes no link without a name.` : `A link feeds ${named} already.` };
}

// The batch that adds the block of the mode in one step: add_block, and for a
// block after or before another or on a link, add_link, into the input that
// `into` gives, where it gives one, and on a link change_link too. The batch
// fixes the new block's id first, since add_link and change_link name the
// block, and mints it where `args` gives none. The link `on` is the one that
// the mode names, as the board holds it. Where the board no longer holds it,
// the batch is empty, so that done() only ends the advanced action.
function pickedBatch(
  core: Pick<CoreApi, "store" | "mintId">,
  mode: AddMode,
  on: Link | undefined,
  args: Readonly<Record<string, Json>>,
  into: Readonly<Record<string, Json>>,
): { readonly id: string; readonly args: Json }[] {
  const block = typeof args.id === "string" ? args.id : core.mintId((id) => Object.hasOwn(core.store.board().blocks, id));
  const add = { id: "blockr.core-add_block", args: { ...args, id: block } };
  const linked = (from: BlockId, to: BlockId) => ({ id: "blockr.core-add_link", args: { from, to, ...into } });
  switch (mode.kind) {
    case "add":
      return [add];
    case "after":
      return [add, linked(mode.from, block)];
    case "before":
      return [add, linked(block, mode.to)];
    case "on":
      return on === undefined
        ? []
        : [add, linked(on.from, block), { id: "blockr.core-change_link", args: { id: mode.link, from: block } }];
  }
}

// The names that the links into a variadic block give its inputs.
export function namesInto(board: Pick<Board, "links">, to: BlockId, except?: string): readonly string[] {
  return Object.entries(board.links)
    .filter(([id, link]) => id !== except && link.to === to && link.input !== "")
    .map(([, link]) => link.input);
}

// Why the name of a variadic block's input does not do, where it does not: a
// name that another of its links gives already would leave two arguments of
// one name. An empty name does, for an input without a name.
export function inputNameError(value: string, taken: readonly string[]): string | undefined {
  const trimmed = value.trim();
  if (trimmed === "") return undefined;
  if (taken.includes(trimmed)) return `Another link gives the input the name "${trimmed}" already.`;
  return undefined;
}

// Whether the free inputs are those of a variadic block, the one empty input
// that takes its links in order.
export function isVariadic(free: readonly string[]): boolean {
  return free.length === 1 && free[0] === "";
}

// A block class as the select and the quick actions that add a block offer
// it: by its name, with its description, its icon and its package, under its
// category, and found by those, by its keywords and by its registry id too.
export interface ClassItem {
  readonly id: string;
  readonly name: string;
  readonly description: string;
  readonly icon: string;
  readonly pkg: string | null;
  readonly category: string;
  readonly keywords: readonly string[];
}

// The block classes of the entries, in the order in which the picker and the
// quick actions that add a block list them (byGroupAndName(), below).
export function blockClasses(entries: readonly RegistryEntry[]): ClassItem[] {
  return entries.map(classItem).sort(byGroupAndName);
}

function classItem(entry: RegistryEntry): ClassItem {
  return {
    id: entry.id,
    name: entry.name,
    description: entry.description,
    icon: entry.icon,
    pkg: packageFromId(entry.id),
    category: entry.category === "" ? UNCATEGORIZED : entry.category,
    keywords: entry.keywords,
  };
}

// The category of a class that gives none, as R's registry names it.
const UNCATEGORIZED = "uncategorized";

// The classes by category and then by name, the categories by name, but for
// the classes without a category, which come last.
function byGroupAndName(a: ClassItem, b: ClassItem): number {
  const last = (item: ClassItem): number => (item.category.toLowerCase() === UNCATEGORIZED ? 1 : 0);
  return last(a) - last(b) || a.category.localeCompare(b.category) || a.name.localeCompare(b.name);
}

// The classes under their categories, in their order.
function byCategory(items: readonly ClassItem[]): { readonly id: string; readonly name: string; readonly items: ClassItem[] }[] {
  const categories = new Map<string, ClassItem[]>();
  for (const item of items) {
    const members = categories.get(item.category) ?? [];
    members.push(item);
    categories.set(item.category, members);
  }
  return [...categories].map(([name, members]) => ({ id: `category:${name}`, name, items: members }));
}

// The class of a package's badge after a block class's name.
const BADGE = "bc:ml-auto bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:text-text-muted";

// The arg of the name given, where the field holds more than blanks, and none
// otherwise, so that the command fills it in.
function filled(arg: string, value: string): Readonly<Record<string, string>> {
  const trimmed = value.trim();
  return trimmed === "" ? {} : { [arg]: trimmed };
}

function optional<K extends string>(key: K, error: string | undefined): Partial<Record<K, string>> {
  return error === undefined ? {} : ({ [key]: error } as Partial<Record<K, string>>);
}

// Puts the focus on the form's first field that holds an error, so that the
// user, and a screen reader, learns of it at once.
export function focusFirstInvalid(form: HTMLFormElement): void {
  form.querySelector<HTMLElement>('[aria-invalid="true"]')?.focus();
}

// Why the id that the user gives a new block does not do, where it does not.
// An empty id does, since the command then mints one. The store takes any id
// but one made of digits alone, which a JS object would put first, and one
// that the board holds already.
export function blockIdError(value: string, board: Pick<Board, "blocks">): string | undefined {
  return idError(value, "block", (id) => own(board.blocks, id) !== undefined);
}

function idError(value: string, what: string, taken: (id: string) => boolean): string | undefined {
  const id = value.trim();
  if (id === "") return undefined;
  if (/^\d+$/.test(id)) return `An ID needs a character other than a digit, such as "${what}_${id}".`;
  if (taken(id)) return `The board holds a ${what} "${id}" already.`;
  return undefined;
}

// The errors but the one of the field given, which the user is changing.
export function without<E extends Readonly<Record<string, string | undefined>>>(errors: E, key: keyof E): E {
  return Object.fromEntries(Object.entries(errors).filter(([field]) => field !== key)) as E;
}

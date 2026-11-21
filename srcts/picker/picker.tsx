// The form that adds a block of a class from the block registry, through
// core's commands. Core's Edit board plugin shows it in its add mode, and the
// menu of a card of core's basic frontend opens it in its other two modes, so
// that both add a block the same way.
// The user fills in the form, and the block goes onto the board only as the
// user submits it, with these fields:
// - The block's class, which a select picks among the block registry's
//   entries, by their names, descriptions, keywords and registry ids, grouped
//   by category.
// - The input that the new link feeds, where the mode adds one (below).
// - The new block's id and its name, which the user may leave empty. Then the
//   id is minted, and the name taken from the block's class.
// Each of the three modes adds the block in one step:
// - On its own, with add_block, which mints the id.
// - After a block, taking that block's result, with a batch of add_block and
//   add_link from that block to the new one. The new block's class needs an
//   input for that.
// - Before a block, feeding an input of that block that no link feeds, with a
//   batch of add_block and add_link from the new block to that block.
// Added after or before a block, the new block comes with a link:
// - Where the link could go into one of several inputs, the user picks the
//   input, since the input that a link feeds changes what a block computes,
//   as the left and the right table of a merge.
// - Where it goes into a variadic block, the user may name the input, as
//   `b2` in `rbind(a, b2 = b)`, or leave it empty for a positional one.
// - A batch hands no value from one command to the next, and add_link names
//   the new block. So where the user leaves the id empty, the form mints it
//   through CoreApi.
// The form checks the id and the input's name as the user leaves each field
// and as they submit, before the commands do, so that the user learns what to
// change next to the field. What the command registry or the store refuses
// all the same, the form of the Edit board plugin says below its button,
// until the user submits it again. In a dialog of core's basic frontend, the
// form leaves the refusal to core, which raises it as a notification, since
// the frontend draws every notification.

import { useMemo, useState } from "react";
import type { ReactElement, SyntheticEvent } from "react";
import { flushSync } from "react-dom";
import { defaultBlockName, freeInputs } from "../commands/index.ts";
import { ComboBox, ComboBoxItem, ComboBoxSection, Form, Select, SelectItem, Text, TextField, useBoard } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { classes, identifier, Refusal, useEdits, useSearch } from "../ui/index.ts";
import { own, packageFromId } from "../util/index.ts";

// Where the new block goes.
export type AddMode =
  | { readonly kind: "add" }
  | { readonly kind: "after"; readonly from: BlockId }
  | { readonly kind: "before"; readonly to: BlockId };

export interface PickerProps {
  // Through which the form runs its edits, as edits of the user, and mints the
  // id of a block that it adds with a link.
  readonly core: Pick<CoreApi, "store" | "blockRegistry" | "commands" | "mintId">;
  readonly mode: AddMode;
  // Called once the block is on the board, as where a dialog that shows the
  // form closes.
  readonly onAdded?: () => void;
  // Whether a dialog that shows the form puts the focus on its first field as
  // it opens.
  readonly autofocus?: boolean;
  // Whether the form says why the command registry or the store refused the
  // block, below its button, as the form of the Edit board plugin does. The
  // form then takes the notification of the refusal, so that core raises
  // none. Otherwise core raises it, as for the form in a dialog of core's
  // basic frontend.
  readonly showsRefusal?: boolean;
}

type Errors = Partial<Record<"registry" | "id" | "input", string>>;

export function BlockPicker({ core, mode, onAdded, autofocus = false, showsRefusal = false }: PickerProps): ReactElement {
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const edits = useEdits(core, showsRefusal);
  const [picked, setPicked] = useState<string | undefined>(undefined);
  const [input, setInput] = useState<string | undefined>(undefined);
  const [inputName, setInputName] = useState("");
  const [id, setId] = useState("");
  const [name, setName] = useState("");
  const [errors, setErrors] = useState<Errors>({});
  const [busy, setBusy] = useState(false);

  // A block added after another takes its result, so its class has an input.
  const entries = useMemo(
    () => Object.values(registry).filter((entry) => mode.kind !== "after" || entry.inputs.length > 0),
    [registry, mode.kind],
  );
  const options = useMemo(() => entries.map(classItem).sort(byGroupAndName), [entries]);
  const search = useSearch(
    options,
    (item) => [item.name, item.description, item.id, item.category, item.pkg ?? "", ...item.keywords],
    "No block class to pick.",
  );
  const entry = picked === undefined ? undefined : own(registry, picked);
  const link = linkChoice(mode, board, registry, entry);
  const chosen = link.kind === "pick" ? (input !== undefined && link.inputs.includes(input) ? input : link.inputs[0]) : undefined;

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
    };
    setBusy(true);
    const title = "Could not add the block.";
    const ran =
      mode.kind === "add" ? await edits.run(title, "blockr.core-add_block", args) : await edits.run(title, addAndLink(core, mode, args, into));
    setBusy(false);
    if (!ran) return;
    setPicked(undefined);
    setInput(undefined);
    setInputName("");
    setId("");
    setName("");
    setErrors({});
    onAdded?.();
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
        {...search}
        items={byCategory(search.items)}
        value={picked ?? null}
        placeholder={options.length === 0 ? "No block class to pick" : "Search by name, description or keyword"}
        isInvalid={errors.registry !== undefined}
        errorMessage={errors.registry}
        description={entry?.description === "" ? undefined : entry?.description}
        onChange={(key) => {
          setPicked(key === null ? undefined : String(key));
          // After a block, the inputs to pick among are those of the class
          // picked, while before a block they are that block's, whatever the
          // class.
          if (mode.kind === "after") setInput(undefined);
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
// - "none": nothing, where the mode adds no link, where the user has not
//   picked the class of a block added after another yet, or where the link
//   has one input to go into, which the command then finds.
// - "no_free_inputs": nothing, before a block whose inputs all take a link
//   already. The form shows `why` instead, and adds no block.
type LinkChoice =
  | { readonly kind: "pick"; readonly label: string; readonly inputs: readonly string[] }
  | { readonly kind: "name"; readonly label: string; readonly taken: readonly string[] }
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
    const of = `of "${target?.name ?? mode.to}"`;
    if (isVariadic(free)) return { kind: "name", label: `Input name ${of}`, taken: namesInto(board, mode.to) };
    if (free.length === 0) return { kind: "no_free_inputs", why: `Every input ${of} takes a link already.` };
    return free.length > 1 ? { kind: "pick", label: `Input ${of}`, inputs: free } : { kind: "none" };
  }
  if (mode.kind === "after" && entry !== undefined) {
    if (entry.inputs.includes("...")) return { kind: "name", label: `Input name of the new ${entry.name}`, taken: [] };
    if (entry.inputs.length > 1) return { kind: "pick", label: `Input of the new ${entry.name}`, inputs: entry.inputs };
  }
  return { kind: "none" };
}

// The batch that adds a block after or before the block of the mode and links
// the two, in one step: add_block, then add_link, into the input that `into`
// gives, where it gives one. The batch fixes the new block's id first, since
// add_link names the block. It mints the id where `args` gives none.
function addAndLink(
  core: Pick<CoreApi, "store" | "mintId">,
  mode: Exclude<AddMode, { readonly kind: "add" }>,
  args: Readonly<Record<string, Json>>,
  into: Readonly<Record<string, Json>>,
): { readonly id: string; readonly args: Json }[] {
  const block = typeof args.id === "string" ? args.id : core.mintId((id) => Object.hasOwn(core.store.board().blocks, id));
  const ends = mode.kind === "after" ? { from: mode.from, to: block } : { from: block, to: mode.to };
  return [
    { id: "blockr.core-add_block", args: { ...args, id: block } },
    { id: "blockr.core-add_link", args: { ...ends, ...into } },
  ];
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

// A block class as the select offers it: by its name, with its description
// and its package, under its category, and found by those, by its keywords
// and by its registry id too.
interface ClassItem {
  readonly id: string;
  readonly name: string;
  readonly description: string;
  readonly pkg: string | null;
  readonly category: string;
  readonly keywords: readonly string[];
}

function classItem(entry: RegistryEntry): ClassItem {
  return {
    id: entry.id,
    name: entry.name,
    description: entry.description,
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

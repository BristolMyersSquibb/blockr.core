// The form that adds a block of a class from the block registry, through
// core's commands. Core's Edit board plugin shows it in its add mode, and the
// menu of a card of core's basic frontend opens it in its other two modes, so
// that both add a block the same way.
// The user fills in the form, and the block goes onto the board only as the
// user submits it, with these fields:
// - The block's class, which a searchable dropdown picks among the block
//   registry's entries, by their names, descriptions, keywords and registry
//   ids, grouped by category.
// - The input that the new link feeds, where the mode adds one (below).
// - The new block's id and its name, which the user may leave empty, where
//   the command mints the id, or takes the name from the block's class.
// Each of the three modes runs a command of its own:
// - On its own, with add_block.
// - After a block, taking that block's result, with append_block. The new
//   block's class needs an input for that.
// - Before a block, feeding an input of that block that no link feeds, with
//   prepend_block.
// Added after or before a block, the new block comes with a link:
// - Where the link could go into one of several inputs, the user picks the
//   input, since the input that a link feeds changes what a block computes,
//   as the left and the right table of a merge.
// - Where it goes into a variadic block, the user may name the input, as
//   `b2` in `rbind(a, b2 = b)`, or leave it empty for a positional one.
// The form checks the id and the input's name as the user leaves each field
// and as they submit, before the command does, so that the user learns what
// to change next to the field. What the command or the store refuses all the
// same, the caller tells the user of.

import { useMemo, useState } from "react";
import type { ReactElement, SubmitEvent } from "react";
import { flushSync } from "react-dom";
import { defaultBlockName, freeInputs } from "../commands/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { classes, Combobox, TextField, useBoard } from "../ui/index.ts";
import type { ComboOption } from "../ui/index.ts";
import { own, packageFromId } from "../util/index.ts";

// Where the new block goes.
export type AddMode =
  | { readonly kind: "add" }
  | { readonly kind: "after"; readonly from: BlockId }
  | { readonly kind: "before"; readonly to: BlockId };

export interface PickerProps {
  readonly core: Pick<CoreApi, "store" | "blockRegistry">;
  readonly mode: AddMode;
  // Runs the command with the args, as one step of the user, and resolves
  // whether it ran. Where the command registry or the store refuses it, the
  // caller tells the user why.
  readonly run: (command: string, args: Json) => Promise<boolean>;
  // Called once the block is on the board, as where a dialog that shows the
  // form closes.
  readonly onAdded?: () => void;
  // Whether a dialog that shows the form puts the focus on its first field as
  // it opens.
  readonly autofocus?: boolean;
}

type Errors = Partial<Record<"registry" | "id" | "input", string>>;

export function BlockPicker({ core, mode, run, onAdded, autofocus = false }: PickerProps): ReactElement {
  const board = useBoard(core);
  const registry = core.blockRegistry();
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
  const options = useMemo(() => entries.map(classOption).sort(byGroupAndLabel), [entries]);
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

  async function submit(event: SubmitEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check();
    flushSync(() => {
      setErrors(found);
    });
    if (Object.keys(found).length > 0 || entry === undefined || busy) {
      focusFirstInvalid(form);
      return;
    }
    const args: Record<string, Json> = {
      registry_id: entry.id,
      ...filled("id", id),
      ...filled("name", name),
      ...(chosen === undefined ? {} : { input: chosen }),
      ...(link.kind === "name" ? filled("input", inputName) : {}),
    };
    setBusy(true);
    const ran =
      mode.kind === "after"
        ? await run("blockr.core-append_block", { ...args, from: mode.from })
        : mode.kind === "before"
          ? await run("blockr.core-prepend_block", { ...args, to: mode.to })
          : await run("blockr.core-add_block", args);
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
    <form
      aria-label="Add block"
      noValidate
      className="bc:grid bc:gap-3"
      onSubmit={(event) => {
        void submit(event);
      }}
    >
      <Combobox
        label="Block class"
        autofocus={autofocus}
        options={options}
        value={picked}
        placeholder={options.length === 0 ? "No block class to pick" : "Search by name, description or keyword"}
        empty="No block class matches."
        error={errors.registry}
        hint={entry?.description === "" ? undefined : entry?.description}
        onChange={(value) => {
          setPicked(value);
          // After a block, the inputs to pick among are those of the class
          // picked, while before a block they are that block's, whatever the
          // class.
          if (mode.kind === "after") setInput(undefined);
          setErrors((now) => without(now, "registry"));
        }}
      />
      {link.kind === "pick" ? (
        <label className={styles.field}>
          <span className={styles.label}>{link.label}</span>
          <select
            className={styles.box}
            value={chosen}
            onChange={(event) => {
              setInput(event.target.value);
            }}
          >
            {link.inputs.map((option) => (
              <option key={option} value={option}>
                {option}
              </option>
            ))}
          </select>
        </label>
      ) : null}
      {link.kind === "name" ? (
        <TextField
          label={link.label}
          value={inputName}
          hint="Optional. Leave it empty for an input without a name, which the block takes in the order of its links."
          error={errors.input}
          onChange={(value) => {
            setInputName(value);
            setErrors((now) => without(now, "input"));
          }}
          onLeave={() => {
            setErrors((now) => ({ ...now, ...optional("input", inputNameError(inputName, link.taken)) }));
          }}
        />
      ) : null}
      {link.kind === "none" && mode.kind === "before" ? <p className={styles.alert}>{link.why}</p> : null}
      <div className="bc:grid bc:grid-cols-2 bc:items-start bc:gap-2">
        <TextField
          label="ID"
          value={id}
          hint="Optional. Made up where left empty."
          error={errors.id}
          identifier
          onChange={(value) => {
            setId(value);
            setErrors((now) => without(now, "id"));
          }}
          onLeave={() => {
            setErrors((now) => ({ ...now, ...optional("id", blockIdError(id, board)) }));
          }}
        />
        <TextField
          label="Name"
          value={name}
          placeholder={entry === undefined ? undefined : defaultBlockName(entry.id)}
          hint="Optional. The class's name where left empty."
          onChange={setName}
        />
      </div>
      <div>
        <button type="submit" className={classes.primary} disabled={busy || (link.kind === "none" && mode.kind === "before")}>
          Add block
        </button>
      </div>
    </form>
  );
}

// What the form asks about the link that comes with the new block:
// - "pick": the input among several, where the input is fixed.
// - "name": an optional name, where the input is variadic, which the variadic
//   block's other links take in `taken`.
// - "none": nothing, where the input is the only one, or where the mode adds
//   no link, or, before a block, why no link can go into it.
type LinkChoice =
  | { readonly kind: "pick"; readonly label: string; readonly inputs: readonly string[] }
  | { readonly kind: "name"; readonly label: string; readonly taken: readonly string[] }
  | { readonly kind: "none"; readonly why?: string };

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
    if (free.length === 0) return { kind: "none", why: `Every input ${of} takes a link already.` };
    return free.length > 1 ? { kind: "pick", label: `Input ${of}`, inputs: free } : { kind: "none" };
  }
  if (mode.kind === "after" && entry !== undefined) {
    if (entry.inputs.includes("...")) return { kind: "name", label: `Input name of the new ${entry.name}`, taken: [] };
    if (entry.inputs.length > 1) return { kind: "pick", label: `Input of the new ${entry.name}`, inputs: entry.inputs };
  }
  return { kind: "none" };
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

// A block class as the dropdown offers it: by its name, with its description
// and its package, under its category.
function classOption(entry: RegistryEntry): ComboOption {
  const pkg = packageFromId(entry.id);
  return {
    value: entry.id,
    label: entry.name,
    detail: entry.description,
    ...(pkg === null ? {} : { badge: pkg }),
    group: entry.category === "" ? UNCATEGORIZED : entry.category,
    keywords: [...entry.keywords, entry.id],
  };
}

// The category of a class that gives none, as R's registry names it.
const UNCATEGORIZED = "uncategorized";

// The options by group and then by label, the groups by name, but for the
// classes without a category, which come last.
function byGroupAndLabel(a: ComboOption, b: ComboOption): number {
  const last = (option: ComboOption): number => (option.group?.toLowerCase() === UNCATEGORIZED ? 1 : 0);
  return last(a) - last(b) || (a.group ?? "").localeCompare(b.group ?? "") || a.label.localeCompare(b.label);
}

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

// Adding a block of a class from the block registry, through core's commands.
// Each of the three ways runs a command of its own:
// - On its own, with add_block.
// - After a block, taking that block's result, with append_block.
// - Before a block, feeding an input of that block, with prepend_block.
// Added after or before a block, the new block comes with a link. Where the
// link could go into one of several inputs, the user picks the input, since
// the input that a link feeds changes what a block computes, as the left and
// the right table of a merge.

import { useMemo, useState } from "react";
import type { ReactElement } from "react";
import { freeInputs } from "../../commands/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId } from "../../types/board.ts";
import type { Json } from "../../types/json.ts";
import type { RegistryEntry } from "../../types/registry.ts";
import { own } from "../../util/index.ts";
import { runCommand, useFrontEnd } from "./context.ts";
import { useBoard } from "./hooks.ts";
import { classes, Dialog } from "./ui.tsx";

// Where the new block goes.
export type AddMode =
  | { readonly kind: "add" }
  | { readonly kind: "after"; readonly from: BlockId }
  | { readonly kind: "before"; readonly to: BlockId };

export function AddBlockDialog({ mode, onClose }: { mode: AddMode; onClose: () => void }): ReactElement {
  const front = useFrontEnd();
  const { core } = front;
  const board = useBoard(core);
  const registry = core.blockRegistry();
  const [search, setSearch] = useState("");
  const [picked, setPicked] = useState<RegistryEntry | undefined>(undefined);
  const [input, setInput] = useState<string | undefined>(undefined);

  // A block added after another takes its result, so its class has an input.
  const entries = useMemo(
    () =>
      Object.values(registry)
        .filter((entry) => mode.kind !== "after" || entry.inputs.length > 0)
        .filter((entry) => matches(entry, search)),
    [registry, mode.kind, search],
  );
  const categories = useMemo(() => byCategory(entries), [entries]);
  const target = mode.kind === "add" ? undefined : own(board.blocks, mode.kind === "after" ? mode.from : mode.to);
  const title = target === undefined ? "Add block" : `Add block ${mode.kind} "${target.name}"`;

  // Before a block, the user picks among the free inputs of that block, the
  // same for every class. After a block, the user picks among the inputs of
  // the class picked, once it is picked.
  const before = mode.kind === "before" ? several(freeInputs(board, registry, mode.to) ?? []) : [];
  const after = picked === undefined ? [] : inputsOf(picked);

  function add(entry: RegistryEntry, into: string | undefined): void {
    onClose();
    const link: Readonly<Record<string, Json>> = into === undefined ? {} : { input: into };
    if (mode.kind === "add") void runCommand(front, "blockr.core-add_block", { registry_id: entry.id });
    else if (mode.kind === "after") void runCommand(front, "blockr.core-append_block", { registry_id: entry.id, from: mode.from, ...link });
    else void runCommand(front, "blockr.core-prepend_block", { registry_id: entry.id, to: mode.to, ...link });
  }

  function pick(entry: RegistryEntry): void {
    if (mode.kind === "after" && inputsOf(entry).length > 0) {
      setPicked(entry);
      setInput(undefined);
    } else {
      add(entry, before.length > 0 ? (input ?? before[0]) : undefined);
    }
  }

  return (
    <Dialog title={title} onClose={onClose}>
      <label className={styles.field}>
        <span className={styles.label}>Search</span>
        <input
          type="search"
          data-autofocus
          className={styles.box}
          value={search}
          placeholder="Name, description or keyword"
          onChange={(event) => {
            setSearch(event.target.value);
          }}
        />
      </label>
      {before.length > 0 ? (
        <InputSelect label={`Input of "${target?.name ?? ""}"`} inputs={before} value={input ?? before[0]} onChange={setInput} />
      ) : null}
      {picked !== undefined && after.length > 0 ? (
        <div className="bc:grid bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:p-2">
          <InputSelect label={`Input of the new ${picked.name}`} inputs={after} value={input ?? after[0]} onChange={setInput} />
          <div className="bc:flex bc:gap-2">
            <button
              type="button"
              className={classes.primary}
              onClick={() => {
                add(picked, input ?? after[0]);
              }}
            >
              Add {picked.name}
            </button>
            <button
              type="button"
              className={classes.button}
              onClick={() => {
                setPicked(undefined);
                setInput(undefined);
              }}
            >
              Back
            </button>
          </div>
        </div>
      ) : (
        <div className="bc:grid bc:max-h-96 bc:gap-3 bc:overflow-y-auto">
          {categories.length === 0 ? <p className={styles.hint}>No block class matches.</p> : null}
          {categories.map(([category, members]) => (
            <section key={category} aria-label={category} className="bc:grid bc:gap-1">
              <h3 className="bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted">{category}</h3>
              <ul className={classes.list}>
                {members.map((entry) => (
                  <li key={entry.id}>
                    <button
                      type="button"
                      className="bc:grid bc:w-full bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:p-2 bc:text-left bc:font-sans bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle"
                      onClick={() => {
                        pick(entry);
                      }}
                    >
                      <span className="bc:text-sm bc:font-medium">{entry.name}</span>
                      {entry.description === "" ? null : <span className="bc:text-xs bc:text-text-muted">{entry.description}</span>}
                    </button>
                  </li>
                ))}
              </ul>
            </section>
          ))}
        </div>
      )}
    </Dialog>
  );
}

function InputSelect({
  label,
  inputs,
  value,
  onChange,
}: {
  label: string;
  inputs: readonly string[];
  value: string | undefined;
  onChange: (input: string) => void;
}): ReactElement {
  return (
    <label className={styles.field}>
      <span className={styles.label}>{label}</span>
      <select
        className={styles.box}
        value={value}
        onChange={(event) => {
          onChange(event.target.value);
        }}
      >
        {inputs.map((input) => (
          <option key={input} value={input}>
            {input}
          </option>
        ))}
      </select>
    </label>
  );
}

// The inputs of a class among which the user picks the one that the link of
// a new block of the class goes into. There are none to pick among for a
// variadic class or a class with one input.
function inputsOf(entry: RegistryEntry): readonly string[] {
  return entry.inputs.includes("...") ? [] : several(entry.inputs);
}

// The inputs, where there are several to pick among, and none otherwise.
function several(inputs: readonly string[]): readonly string[] {
  return inputs.length > 1 ? inputs : [];
}

// Whether the entry's name, description, keywords or id hold the text,
// whatever its case.
function matches(entry: RegistryEntry, text: string): boolean {
  const needle = text.trim().toLowerCase();
  if (needle === "") return true;
  return [entry.name, entry.description, entry.id, ...entry.keywords].some((field) => field.toLowerCase().includes(needle));
}

// The entries by category, each sorted by name, the categories by name.
function byCategory(entries: readonly RegistryEntry[]): [string, RegistryEntry[]][] {
  const groups = new Map<string, RegistryEntry[]>();
  for (const entry of entries) {
    const category = entry.category === "" ? "Other" : entry.category;
    groups.set(category, [...(groups.get(category) ?? []), entry]);
  }
  return [...groups]
    .map(([category, members]): [string, RegistryEntry[]] => [category, members.sort((a, b) => a.name.localeCompare(b.name))])
    .sort(([a], [b]) => a.localeCompare(b));
}

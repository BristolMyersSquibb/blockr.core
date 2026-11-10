// The UI of the file browser block: it lists a directory of one of the
// block's volumes through the request list_dir, opens a directory the user
// clicks, and submits the path of a file the user clicks as the state arg
// file_path.
// Which volume and which directory in it the browser shows is the module's
// own.
// It starts at the directory of the block's file, where the file lies in a
// volume, and otherwise at the root of the first volume.

import { Fragment, useState } from "react";
import type { ReactElement } from "react";
import { Field, reactModule, Stack, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { Json } from "../../types/json.ts";
import { isObject, jsonEqual, own } from "../../util/index.ts";
import type { State } from "./state.ts";

export const api = 1;

// What list_dir answers: the names of the directories and of the files in
// the directory it lists.
interface Listing {
  readonly dirs: readonly string[];
  readonly files: readonly string[];
}

// Where the browser is: a volume, by its name,
// and the directories from its root down to the one it lists.
interface Place {
  readonly volume: string | undefined;
  readonly dirs: readonly string[];
}

// The classes of the buttons that name a directory or a file,
// those in the list stretching across it.
const link =
  "bc:cursor-pointer bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-0.5 bc:text-left bc:text-sm bc:text-inherit bc:[font-family:inherit] bc:hover:bg-bg-subtle bc:aria-current:font-semibold";
const entry = `${link} bc:justify-self-stretch`;

function FileBrowserBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const shared = useValue(host, "volumes").value;
  const volumes = asVolumes(shared);
  const file = useValue(host, "file_path");
  const held = typeof file.value === "string" && file.value !== "" ? file.value : undefined;

  // Where the volumes change, such as through the assistant,
  // a volume that is gone gives way to the first, at its root.
  const [place, setPlace] = useState<Place>(() => locate(file.value, volumes));
  const volume =
    place.volume !== undefined && own(volumes, place.volume) !== undefined ? place.volume : Object.keys(volumes)[0];
  const dirs = volume === place.volume ? place.dirs : [];
  const root = volume === undefined ? undefined : own(volumes, volume);

  // The host sends the state arg volumes from the block's state,
  // so the request is asked again where it changes.
  const listed = useRequest(host, "list_dir", {
    args: { ...(volume === undefined ? {} : { volume }), path: dirs.join("/") },
    reads: shared ?? null,
  });
  // While R lists the place, the last listing is of another place,
  // or of the place before the volumes changed.
  const loading = listed.loading;
  const listing = listed.error === null ? asListing(listed.answer) : { dirs: [], files: [] };
  const error = (loading ? null : listed.error) ?? file.error;

  // Shows another place, which it asks R to list.
  const go = (next: Place): void => {
    if (next.volume === volume && jsonEqual(next.dirs, dirs)) return;
    setPlace(next);
  };

  const names = Object.keys(volumes);
  return (
    <Stack>
      {names.length > 1 && (
        <Field label="Volume">
          <select
            className={styles.box}
            value={volume ?? ""}
            onChange={(event) => {
              go({ volume: event.target.value, dirs: [] });
            }}
          >
            {names.map((name) => (
              <option key={name} value={name}>
                {name}
              </option>
            ))}
          </select>
        </Field>
      )}
      <nav aria-label="Directory" className="bc:flex bc:flex-wrap bc:items-center bc:gap-1 bc:text-sm">
        {[volume ?? "", ...dirs].map((name, i) => (
          <Fragment key={i}>
            {i > 0 && (
              <span className="bc:text-text-muted" aria-hidden="true">
                /
              </span>
            )}
            <button
              type="button"
              className={link}
              aria-current={i === dirs.length ? "true" : undefined}
              onClick={() => {
                go({ volume, dirs: dirs.slice(0, i) });
              }}
            >
              {name}
            </button>
          </Fragment>
        ))}
      </nav>
      <ul className="bc:m-0 bc:grid bc:max-h-64 bc:list-none bc:gap-0.5 bc:overflow-y-auto bc:p-0">
        {loading ? (
          <Note text="Loading…" />
        ) : (
          <>
            {dirs.length > 0 && (
              <Entry
                text=".."
                onClick={() => {
                  go({ volume, dirs: dirs.slice(0, -1) });
                }}
              />
            )}
            {listing.dirs.map((dir) => (
              <Entry
                key={`dir ${dir}`}
                text={`${dir}/`}
                onClick={() => {
                  go({ volume, dirs: [...dirs, dir] });
                }}
              />
            ))}
            {listing.files.map((name) => {
              const at = root === undefined ? undefined : join(root, [...dirs, name]);
              return (
                <Entry
                  key={`file ${name}`}
                  text={name}
                  current={at !== undefined && at === held}
                  onClick={() => {
                    if (at !== undefined) void file.commit(at);
                  }}
                />
              );
            })}
            {listing.dirs.length + listing.files.length === 0 && <Note text="This directory is empty." />}
          </>
        )}
      </ul>
      <p className={styles.hint}>{held === undefined ? "No file chosen yet." : `Chosen: ${held}`}</p>
      {error !== null && (
        <p role="alert" className={styles.alert}>
          {error}
        </p>
      )}
    </Stack>
  );
}

function Entry({ text, current = false, onClick }: { text: string; current?: boolean; onClick: () => void }): ReactElement {
  return (
    <li className="bc:grid">
      <button type="button" className={entry} aria-current={current ? "true" : undefined} onClick={onClick}>
        {text}
      </button>
    </li>
  );
}

function Note({ text }: { text: string }): ReactElement {
  return (
    <li className="bc:grid">
      <p className={styles.hint}>{text}</p>
    </li>
  );
}

export default reactModule(FileBrowserBlock);

// Where the browser starts: the volume that holds the block's file,
// and the directories down to the file, or the root of the first volume.
function locate(file: Json | undefined, roots: Readonly<Record<string, string>>): Place {
  if (typeof file === "string") {
    for (const [name, root] of Object.entries(roots)) {
      const prefix = `${join(root, [])}/`;
      if (file.startsWith(prefix)) {
        return { volume: name, dirs: file.slice(prefix.length).split("/").slice(0, -1) };
      }
    }
  }
  return { volume: Object.keys(roots)[0], dirs: [] };
}

// The volumes by name, as the state arg volumes holds them, or none where
// nothing gives them. A value that is no object of paths fails, rather than
// lose the volumes that it holds.
export function asVolumes(volumes: Json | undefined): Readonly<Record<string, string>> {
  if (volumes === undefined) return {};
  if (isObject(volumes) && Object.values(volumes).every((root) => typeof root === "string")) {
    return volumes as Readonly<Record<string, string>>;
  }
  throw new TypeError(`The state arg volumes is ${JSON.stringify(volumes)}, where the file browser takes the path of each volume by its name.`);
}

// The path of an entry below a volume's root,
// such as "/data" below the root "/" and "./data" below the root ".".
function join(root: string, names: readonly string[]): string {
  return [root.replace(/\/+$/, ""), ...names].join("/");
}

// What R answered to list_dir, as the directories and the files it lists, or
// none before R answers. An answer of another shape fails, rather than list
// nothing.
export function asListing(answer: unknown): Listing {
  if (answer === undefined) return { dirs: [], files: [] };
  if (isObject(answer) && isStrings(answer.dirs) && isStrings(answer.files)) {
    return { dirs: answer.dirs, files: answer.files };
  }
  throw new TypeError(`R answered list_dir with ${JSON.stringify(answer)}, where the file browser takes the dirs and the files that it lists.`);
}

function isStrings(value: unknown): value is readonly string[] {
  return Array.isArray(value) && value.every((item) => typeof item === "string");
}

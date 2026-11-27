// The UI of the file browser block: it lists a directory of one of the
// block's volumes through the request list_dir, opens a directory the user
// clicks, and submits the path of a file the user clicks as the state arg
// file_path.
// It is drawn as file dialogs and RStudio's Files pane are, in one frame.
// A bar at the top holds a button that goes up a directory, and the path from
// the volume down to the directory, each part of which goes back there.
// Below it, a Table of blockr.ui lists the directories first, then the
// files, each with its icon, its size where it is a file, and the time it
// changed last, in the time zone of the user's browser.
// The block's file shows highlighted.
// The user moves through the list by the arrow keys too, and opens an entry by
// Enter, as React Aria's Table has it.
// Which volume and which directory in it the browser shows is the module's
// own.
// It starts at the directory of the block's file, where the file lies in a
// volume, and otherwise at the root of the first volume.
// The module draws only its own state, and why the host refused a path. Where
// R fails to list a directory, core raises a notification of each condition of
// R's error reply, so that the frontend says where it shows, and the module
// lists nothing.

import { useState } from "react";
import type { ReactElement } from "react";
import { Breadcrumb, Breadcrumbs, Button, Cell, Column, FormLayout, Row, Select, SelectItem, Table, TableBody, TableHeader } from "@blockr/ui";
import { reactModule, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { Json } from "../../types/json.ts";
import { isObject, jsonEqual, own } from "../../util/index.ts";
import type { State } from "./state.ts";

export const api = 1;

// What list_dir answers: the directories and the files in the directory it
// lists, each by its name, with the time it changed last, in UTC, and a file
// with its size in bytes.
// R sends null for a time or a size that it cannot read, as of a broken link.
interface DirEntry {
  readonly name: string;
  readonly modified: string | null;
}

interface FileEntry extends DirEntry {
  readonly size: number | null;
}

interface Listing {
  readonly dirs: readonly DirEntry[];
  readonly files: readonly FileEntry[];
}

// Where the browser is: a volume, by its name,
// and the directories from its root down to the one it lists.
interface Place {
  readonly volume: string | undefined;
  readonly dirs: readonly string[];
}

// An entry of the list, a directory or a file, with its id, what the list
// shows of it, whether it is the block's file, and what opening it does.
interface Entry {
  readonly id: string;
  readonly name: string;
  readonly dir: boolean;
  readonly size: number | null;
  readonly modified: string | null;
  readonly current: boolean;
  readonly open: () => void;
}

// The classes of the frame, of the bar at its top, of the headings of the
// list's columns, and of the row of the block's file.
// The columns of sizes and times keep their widths, so that they stay in
// place as the user moves from one directory to another.
const FRAME = "bc:overflow-hidden bc:rounded-md bc:border bc:border-border bc:bg-popover";
const BAR = "bc:flex bc:items-center bc:gap-1 bc:border-b bc:border-border bc:p-1 bc:text-sm";
const HEAD = "bc:text-xs bc:text-muted-foreground";
const CHOSEN = "bc:bg-primary/10 bc:font-semibold";
// The classes of the line below the list that names the block's file, in the
// colour of secondary text, and of the line that says why the host refused a
// file, in the colour of an error's text, as blockr.ui draws a field's
// description and its error.
const HINT = "bc:m-0 bc:text-sm bc:text-muted-foreground";
const ALERT = "bc:m-0 bc:text-sm bc:text-destructive-text bc:whitespace-pre-wrap";

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
  const failed = listed.error !== null;
  const listing = failed ? { dirs: [], files: [] } : asListing(listed.answer);

  // Shows another place, which it asks R to list.
  const go = (next: Place): void => {
    if (next.volume === volume && jsonEqual(next.dirs, dirs)) return;
    setPlace(next);
  };

  const entries: Entry[] = [
    ...listing.dirs.map((dir) => ({
      id: `dir ${dir.name}`,
      name: dir.name,
      dir: true,
      size: null,
      modified: dir.modified,
      current: false,
      open: () => {
        go({ volume, dirs: [...dirs, dir.name] });
      },
    })),
    ...listing.files.map((found) => {
      const at = root === undefined ? undefined : join(root, [...dirs, found.name]);
      return {
        id: `file ${found.name}`,
        name: found.name,
        dir: false,
        size: found.size,
        modified: found.modified,
        current: at !== undefined && at === held,
        open: () => {
          if (at !== undefined) void file.commit(at);
        },
      };
    }),
  ];

  const names = Object.keys(volumes);
  return (
    <FormLayout>
      {names.length > 1 && (
        <Select
          label="Volume"
          isRequired
          items={names.map((id) => ({ id }))}
          value={volume ?? null}
          onChange={(key) => {
            if (key !== null) go({ volume: String(key), dirs: [] });
          }}
        >
          {(item) => <SelectItem id={item.id}>{item.id}</SelectItem>}
        </Select>
      )}
      <div className={FRAME}>
        <div className={BAR}>
          <Button
            variant="quiet"
            aria-label="Up one directory"
            className="bc:disabled:cursor-default bc:disabled:opacity-40"
            isDisabled={dirs.length === 0}
            onPress={() => {
              go({ volume, dirs: dirs.slice(0, -1) });
            }}
          >
            <Up />
          </Button>
          <FolderIcon className="bc:shrink-0 bc:text-muted-foreground" />
          <nav aria-label="Directory" className="bc:min-w-0">
            <Breadcrumbs
              onAction={(key) => {
                go({ volume, dirs: dirs.slice(0, Number(key)) });
              }}
            >
              {[volume ?? "", ...dirs].map((name, i) => (
                <Breadcrumb key={i} id={i}>
                  {name}
                </Breadcrumb>
              ))}
            </Breadcrumbs>
          </nav>
        </div>
        {/* The frame draws the line below the last row. */}
        <Table
          aria-label="Files"
          className="bc:-mb-px bc:max-h-64"
          onRowAction={(key) => {
            entries.find((entry) => entry.id === key)?.open();
          }}
        >
          <TableHeader>
            <Column id="name" isRowHeader className={HEAD}>
              Name
            </Column>
            <Column id="size" align="end" className={`${HEAD} bc:w-24`}>
              Size
            </Column>
            <Column id="modified" className={`${HEAD} bc:w-36`}>
              Modified
            </Column>
          </TableHeader>
          {/* While R lists a place, the list holds no entry and says that it
              is loading. Where the listing fails, it says nothing. */}
          <TableBody
            items={loading ? [] : entries}
            renderEmptyState={() => (loading ? "Loading…" : failed ? null : "This directory is empty.")}
          >
            {(entry) => (
              <Row id={entry.id} data-current={entry.current || undefined} className={`bc:cursor-pointer ${entry.current ? CHOSEN : ""}`}>
                <Cell textValue={entry.name}>
                  <span className="bc:flex bc:items-center bc:gap-2">
                    {entry.dir ? <FolderIcon className="bc:shrink-0 bc:text-primary" /> : <FileIcon className="bc:shrink-0 bc:text-muted-foreground" />}
                    {entry.name}
                  </span>
                </Cell>
                <Cell className="bc:text-right bc:tabular-nums">
                  {entry.size === null ? <span className="bc:text-muted-foreground">—</span> : sizeText(entry.size)}
                </Cell>
                <Cell className="bc:tabular-nums bc:text-muted-foreground">{modifiedText(entry.modified)}</Cell>
              </Row>
            )}
          </TableBody>
        </Table>
      </div>
      {/* Before a file is chosen, the block's status says what is missing. */}
      {held !== undefined && <p className={HINT}>{`Chosen: ${held}`}</p>}
      {file.error !== null && (
        <p role="alert" className={ALERT}>
          {file.error}
        </p>
      )}
    </FormLayout>
  );
}

export default reactModule(FileBrowserBlock);

// The arrow of the button that goes up a directory.
function Up(): ReactElement {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
      <path d="M8 13V3M4 7l4-4 4 4" />
    </svg>
  );
}

function FolderIcon({ className }: { className: string }): ReactElement {
  return (
    <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor" aria-hidden="true" className={className}>
      <path d="M1.5 4a1 1 0 0 1 1-1h3.6l1.5 1.5h5.9a1 1 0 0 1 1 1V12a1 1 0 0 1-1 1h-11a1 1 0 0 1-1-1z" />
    </svg>
  );
}

function FileIcon({ className }: { className: string }): ReactElement {
  return (
    <svg width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.3" strokeLinejoin="round" aria-hidden="true" className={className}>
      <path d="M4 1.75h5.25L12.5 5v9.25H4z" />
      <path d="M9.25 1.75V5h3.25" />
    </svg>
  );
}

// A size in bytes as the list shows it: in bytes below a kilobyte, and
// otherwise in the largest unit that keeps it at 1 or more, with one decimal,
// where a kilobyte is 1,024 bytes, as blockr.io's path field shows sizes.
export function sizeText(bytes: number): string {
  if (bytes < 1024) return `${String(bytes)} B`;
  const units = ["KB", "MB", "GB", "TB"] as const;
  let value = bytes / 1024;
  let unit = 0;
  while (value >= 1024 && unit < units.length - 1) {
    value /= 1024;
    unit += 1;
  }
  return `${value.toFixed(1)} ${units[unit] ?? "TB"}`;
}

// The time an entry changed last, which R sends in UTC, as the list shows it:
// in the time zone of the user's browser, to the minute, as 2026-11-20 08:03.
// It shows nothing where R sent no time, and the text as R sent it where it
// reads as no time.
export function modifiedText(modified: string | null): string {
  if (modified === null) return "";
  const at = new Date(modified);
  if (Number.isNaN(at.getTime())) return modified;
  const two = (n: number): string => String(n).padStart(2, "0");
  return `${String(at.getFullYear())}-${two(at.getMonth() + 1)}-${two(at.getDate())} ${two(at.getHours())}:${two(at.getMinutes())}`;
}

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
  if (isObject(answer) && isEntries(answer.dirs, isDirEntry) && isEntries(answer.files, isFileEntry)) {
    return { dirs: answer.dirs, files: answer.files };
  }
  throw new TypeError(
    `R answered list_dir with ${JSON.stringify(answer)}, where the file browser takes the dirs and the files that it lists, each with its name and the time it changed last, and each file with its size.`,
  );
}

function isEntries<T>(value: unknown, is: (entry: unknown) => entry is T): value is readonly T[] {
  return Array.isArray(value) && value.every(is);
}

function isDirEntry(entry: unknown): entry is DirEntry {
  return isObject(entry) && typeof entry.name === "string" && (entry.modified === null || typeof entry.modified === "string");
}

function isFileEntry(entry: unknown): entry is FileEntry {
  return isDirEntry(entry) && isObject(entry) && (entry.size === null || typeof entry.size === "number");
}

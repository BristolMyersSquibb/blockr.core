// Ids, which the client mints for the objects and boards it creates and for
// each object of a board that R sends without one, from the words that R's
// rand_names() draws from as well (words.ts), so that an id looks the same
// whichever side minted it.

import { adjectives, animals } from "./words.ts";

// An id is any string that is neither empty nor made of digits alone, since a
// JS object puts keys that look like integers first, whatever the order they
// were inserted in.
export function isId(id: unknown): boolean {
  return typeof id === "string" && id !== "" && !/^\d+$/.test(id);
}

// A new id, such as "calm_heron", that `taken` does not hold.
export function mintId(taken: (id: string) => boolean): string {
  for (let i = 0; i < 100; i++) {
    const id = `${pick(adjectives)}_${pick(animals)}`;
    if (!taken(id)) return id;
  }

  const id = `${pick(adjectives)}_${pick(animals)}`;
  let n = 2;
  while (taken(`${id}_${String(n)}`)) n++;
  return `${id}_${String(n)}`;
}

// A name made from an id, "Curious otter" from "curious_otter", as R's
// id_to_sentence_case() makes one.
export function sentenceCase(id: string): string {
  const words = id
    .replace(/([A-Z])/g, " $1")
    .replace(/[_\-.]/g, " ")
    .trim()
    .split(" ");
  const [first = "", ...rest] = words;
  return [
    first.charAt(0).toUpperCase() + first.slice(1).toLowerCase(),
    ...rest.map((word) => word.toLowerCase()),
  ].join(" ");
}

function pick(words: readonly string[]): string {
  return words[Math.floor(Math.random() * words.length)] ?? "";
}

// Ids, which the client mints for the objects and boards it creates, in the
// shapes in which R's rand_names() draws them for what an app script leaves
// out, from the same words (words.ts), so that an id looks the same whichever
// side minted it.

import { adjectives, animals } from "./words.ts";

// The shapes that the blockr option rand_id names, which R's rand_id_shapes()
// gives as well.
export const RAND_IDS = [
  "adjective_animal",
  "adjective_adjective_animal",
  "letters_6",
  "letters_8",
  "letters_12",
  "bytes_4",
  "bytes_8",
  "uuid",
] as const;

export type RandId = (typeof RAND_IDS)[number];

export function isRandId(value: unknown): value is RandId {
  return typeof value === "string" && (RAND_IDS as readonly string[]).includes(value);
}

// An id is any string that is neither empty nor made of digits alone, since a
// JS object puts keys that look like integers first, whatever the order they
// were inserted in.
export function isId(id: unknown): boolean {
  return typeof id === "string" && id !== "" && !/^\d+$/.test(id);
}

// A new id, such as "calm_heron", that `taken` does not hold, in the shape that
// `randId` names, as R's rand_names() draws one: the first of up to 100 draws
// that is new and is an id, and otherwise a further draw with the lowest number
// from 2 that makes it new, as "calm_heron_2".
export function mintId(taken: (id: string) => boolean, randId: RandId = "adjective_animal"): string {
  const draw = draws[randId];

  for (let i = 0; i < 100; i++) {
    const id = draw();
    if (isId(id) && !taken(id)) return id;
  }

  const id = draw();
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

// One draw in each shape, which may be taken.
const draws: Readonly<Record<RandId, () => string>> = {
  adjective_animal: () => `${pick(adjectives)}_${pick(animals)}`,
  adjective_adjective_animal: () => {
    const [first, second] = pickTwo(adjectives);
    return `${first}_${second}_${pick(animals)}`;
  },
  letters_6: () => randomLetters(6),
  letters_8: () => randomLetters(8),
  letters_12: () => randomLetters(12),
  bytes_4: () => hex(randomBytes(4)),
  bytes_8: () => hex(randomBytes(8)),
  uuid: randomUuid,
};

function randomInt(n: number): number {
  return Math.floor(Math.random() * n);
}

function pick(words: readonly string[]): string {
  return words[randomInt(words.length)] ?? "";
}

// Two different words.
function pickTwo(words: readonly string[]): [string, string] {
  const i = randomInt(words.length);
  let j = randomInt(words.length - 1);
  if (j >= i) j++;
  return [words[i] ?? "", words[j] ?? ""];
}

function randomLetters(n: number): string {
  return Array.from({ length: n }, () => String.fromCharCode(97 + randomInt(26))).join("");
}

function randomBytes(n: number): number[] {
  return Array.from({ length: n }, () => randomInt(256));
}

function hex(bytes: readonly number[]): string {
  return bytes.map((byte) => byte.toString(16).padStart(2, "0")).join("");
}

// A version 4 UUID: 16 random bytes, of which the seventh holds the version, 4,
// and the ninth the variant, in hex, grouped by hyphens as 8-4-4-4-12. It is
// drawn as the other shapes are, since crypto.randomUUID() exists only on a
// page served over HTTPS.
function randomUuid(): string {
  const bytes = randomBytes(16);
  bytes[6] = ((bytes[6] ?? 0) & 0x0f) | 0x40;
  bytes[8] = ((bytes[8] ?? 0) & 0x3f) | 0x80;
  const text = hex(bytes);
  return [text.slice(0, 8), text.slice(8, 12), text.slice(12, 16), text.slice(16, 20), text.slice(20)].join("-");
}

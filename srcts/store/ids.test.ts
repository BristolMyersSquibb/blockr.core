import { expect, test, vi } from "vitest";
import { isId, isRandId, mintId, RAND_IDS, type RandId, sentenceCase } from "./ids.ts";
import { adjectives, animals } from "./words.ts";

test("an id is a string that is neither empty nor made of digits alone", () => {
  expect(isId("curious_otter")).toBe(true);
  expect(isId("a1")).toBe(true);
  expect(isId("constructor")).toBe(true);
  expect(isId("")).toBe(false);
  expect(isId("12")).toBe(false);
  expect(isId(12)).toBe(false);
});

test("a minted id is an adjective and an animal of the words R draws from too", () => {
  for (let i = 0; i < 500; i++) {
    const [adjective = "", animal = ""] = mintId(() => false).split("_");
    expect(adjectives).toContain(adjective);
    expect(animals).toContain(animal);
  }
});

test("the words are distinct words of at most six lowercase letters", () => {
  for (const words of [adjectives, animals]) {
    for (const word of words) expect(word).toMatch(/^[a-z]{2,6}$/);
    expect(new Set(words).size).toBe(words.length);
  }
});

test("each shape that rand_id names mints ids of that shape", () => {
  const shapes: Record<RandId, RegExp> = {
    adjective_animal: /^[a-z]+_[a-z]+$/,
    adjective_adjective_animal: /^[a-z]+_[a-z]+_[a-z]+$/,
    letters_6: /^[a-z]{6}$/,
    letters_8: /^[a-z]{8}$/,
    letters_12: /^[a-z]{12}$/,
    bytes_4: /^[0-9a-f]{8}$/,
    bytes_8: /^[0-9a-f]{16}$/,
    uuid: /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/,
  };
  for (const randId of RAND_IDS) {
    for (let i = 0; i < 200; i++) {
      const id = mintId(() => false, randId);
      expect(id).toMatch(shapes[randId]);
      expect(isId(id)).toBe(true);
    }
  }
});

test("an id of two adjectives and an animal has two different adjectives", () => {
  for (let i = 0; i < 500; i++) {
    const [first, second, animal = ""] = mintId(() => false, "adjective_adjective_animal").split("_");
    expect(first).not.toBe(second);
    expect(adjectives).toContain(first);
    expect(adjectives).toContain(second);
    expect(animals).toContain(animal);
  }
});

test("a draw made of digits alone is drawn again", () => {
  // Four bytes of 0 make "00000000", which is no id, and four of 0xab follow.
  const random = vi.spyOn(Math, "random");
  for (const value of [0, 0, 0, 0, 0xab / 256, 0xab / 256, 0xab / 256, 0xab / 256]) {
    random.mockReturnValueOnce(value);
  }
  expect(mintId(() => false, "bytes_4")).toBe("abababab");
  random.mockRestore();
});

test("the shapes that rand_id names are told apart from other values", () => {
  for (const randId of RAND_IDS) expect(isRandId(randId)).toBe(true);
  expect(isRandId("counter")).toBe(false);
  expect(isRandId(4)).toBe(false);
});

test("a minted id is none that is taken", () => {
  const taken = new Set<string>();
  for (let i = 0; i < 2000; i++) {
    const id = mintId((other) => taken.has(other));
    expect(taken.has(id)).toBe(false);
    taken.add(id);
  }
});

test("an id is counted up once every draw is taken", () => {
  const id = mintId((other) => !/_\d+$/.test(other) || other.endsWith("_2"));
  expect(id).toMatch(/^[a-z]+_[a-z]+_3$/);
});

test("a board without a name is named after its id in sentence case", () => {
  expect(sentenceCase("curious_otter")).toBe("Curious otter");
  expect(sentenceCase("myBoard")).toBe("My board");
  expect(sentenceCase("data.dir-x_y")).toBe("Data dir x y");
});

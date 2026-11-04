import { expect, test } from "vitest";
import { isId, mintId, sentenceCase } from "./ids.ts";
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

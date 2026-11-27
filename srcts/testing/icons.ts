// The icon that an element shows, for the tests of UIs that draw Lucide's
// icons with blockr.ui's Icon.

import type { IconNode } from "../icons/index.ts";

// The shape of the first svg in the element given, or of the element itself
// where it is one: each element of the svg as its tag with its attributes,
// as an icon of @blockr/ui/icons holds its shape. So a test compares it with
// the icon that the UI is to show, as
// `expect(shownIcon(button)).toEqual(Ellipsis)`.
// It throws where the element holds no svg.
export function shownIcon(el: Element): IconNode {
  const svg = el.localName === "svg" ? el : el.querySelector("svg");
  if (svg === null) throw new Error(`The element ${el.localName} shows no icon.`);
  return [...svg.children].map((child) => [child.localName, Object.fromEntries([...child.attributes].map(({ name, value }) => [name, value]))]);
}

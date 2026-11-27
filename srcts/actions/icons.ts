// The icons of core's board actions and of the rows of their quick actions,
// SVG markup in the colour of the text around it, as a menu shows a row's icon
// and a toolbar's button shows a board action's.
// Each icon is Lucide's, from the icons that core draws (../icons/index.ts),
// and blockr.ui's iconMarkup() writes it as markup
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#the-icons).
// The swatch of a stack's colour draws no icon, and stays core's own.
// A board action that adds something shows what it adds, so that a menu of a
// block tells Append block, Create link and Create stack apart: a box for a
// block, a link for a link and layers for a stack. Add to stack shows the
// layers as well. The board actions that remove show a bin.

import { iconMarkup } from "@blockr/ui";
import { Box, Layers, Link, Trash } from "../icons/index.ts";

// The icon of the board actions that add a block.
export const BLOCK = iconMarkup(Box);

// The icon of Create link.
export const LINK = iconMarkup(Link);

// The icon of Create stack and of Add to stack.
export const STACK = iconMarkup(Layers);

// The icon of the board actions that remove.
export const TRASH = iconMarkup(Trash);

// A square in the colour of a stack, as a row of a quick action shows a stack
// or a colour that a stack may take, as blockr.dock#550 had them, or the
// outline of one for a stack without a colour.
export function swatch(color: string | null): string {
  const fill = color === null ? 'fill="none" stroke="currentColor" stroke-width="1.25"' : `fill="${escaped(color)}"`;
  return `<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true"><rect x="2.5" y="2.5" width="11" height="11" rx="3" ${fill}/></svg>`;
}

// A colour as an attribute of SVG markup holds it, which a stack's colour
// from a board's file could break out of.
function escaped(color: string): string {
  return color.replace(/[&<>"']/g, (char) => `&#${String(char.charCodeAt(0))};`);
}

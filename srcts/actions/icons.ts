// The icons of core's board actions and of the rows of their quick actions,
// SVG markup in the colour of the text around it, as a menu shows a row's icon
// and a toolbar's button shows a board action's. They take the shapes of
// blockr.ui's where one fits, since blockr.ui builds on blockr.core
// (https://github.com/BristolMyersSquibb/blockr.ui/tree/e14e8310481e98b156871bf96669b41cb88c8435/inst/assets/icons).

// The plus of a board action that adds something, in the shape of blockr.ui's
// `plus`.
export const PLUS =
  '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.25" stroke-linecap="round" aria-hidden="true"><path d="M8 3v10M3 8h10"/></svg>';

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

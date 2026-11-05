// Core's plain renderer of text outputs, blockr.core-text_output, which draws
// every text output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It shows a result as print() shows it, at R's own width, in a box that
// scrolls rather than wraps its lines, and that is no taller than the element
// it draws in, where a front-end gives that element a height. It takes its
// colours from that element.

import { outputRenderer } from "../../react/index.ts";
import type { TextOutput } from "../../types/outputs.ts";

export const api = 1;

// A box that scrolls takes the focus, so that the keyboard scrolls it too.
export default outputRenderer<TextOutput>(({ output }) => (
  <pre
    tabIndex={0}
    className="bc:m-0 bc:min-h-0 bc:overflow-auto bc:rounded bc:bg-current/5 bc:p-2 bc:font-mono bc:text-xs bc:leading-snug bc:whitespace-pre"
  >
    {output.text}
  </pre>
));

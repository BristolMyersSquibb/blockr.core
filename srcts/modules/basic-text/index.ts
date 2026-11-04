// Core's plain renderer of text outputs, blockr.core-text_output, which draws
// every text output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It shows a result as print() shows it, at R's own width, in a box that
// scrolls rather than wraps its lines, and that is no taller than the element
// it draws in, where a front-end gives that element a height.

import type { OutputModule } from "../../types/hosts.ts";
import { isObject } from "../../util/index.ts";

export const api = 1;

const renderer: OutputModule = {
  render({ el, host }) {
    const alert = document.createElement("div");
    alert.setAttribute("role", "alert");
    alert.className =
      "bc:rounded bc:border bc:border-red-300 bc:bg-red-50 bc:p-2 bc:text-sm bc:text-red-800 bc:whitespace-pre-wrap bc:dark:border-red-800 bc:dark:bg-red-950 bc:dark:text-red-200";
    const pre = document.createElement("pre");
    // A box that scrolls takes the focus, so that the keyboard scrolls it too.
    pre.tabIndex = 0;
    pre.className =
      "bc:m-0 bc:min-h-0 bc:overflow-auto bc:rounded bc:bg-gray-50 bc:p-2 bc:font-mono bc:text-xs bc:leading-snug bc:whitespace-pre bc:text-gray-900 bc:dark:bg-gray-900 bc:dark:text-gray-100";
    const root = document.createElement("div");
    root.className = "bc:flex bc:max-h-full bc:flex-col bc:gap-2";
    root.append(alert, pre);
    el.replaceChildren(root);

    const draw = (): void => {
      const output = host.output();
      const text = isObject(output) ? output.text : undefined;
      pre.textContent = typeof text === "string" ? text : "";

      const loading = host.loading();
      root.setAttribute("aria-busy", String(loading));
      pre.classList.toggle("bc:opacity-60", loading);

      const error = host.error();
      alert.hidden = error === null;
      alert.textContent = error?.message ?? "";
    };

    draw();
    return host.on("output", draw);
  },
};

export default renderer;

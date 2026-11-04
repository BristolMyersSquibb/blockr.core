// Core's plain renderer of plot outputs, blockr.core-plot_output, which draws
// every plot output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It shows the PNG that the output's body holds at the size it was drawn at,
// in CSS pixels, which R took from the output's info, so that a PNG drawn for
// a display of twice the density shows sharp on it.

import type { OutputModule } from "../../types/hosts.ts";
import { isObject } from "../../util/index.ts";

export const api = 1;

const renderer: OutputModule = {
  render({ el, host }) {
    const alert = document.createElement("div");
    alert.setAttribute("role", "alert");
    alert.className =
      "bc:rounded bc:border bc:border-red-300 bc:bg-red-50 bc:p-2 bc:text-sm bc:text-red-800 bc:whitespace-pre-wrap bc:dark:border-red-800 bc:dark:bg-red-950 bc:dark:text-red-200";
    const img = document.createElement("img");
    img.alt = "Plot";
    // No class sets its display, which would show it where `hidden` hides it.
    img.className = "bc:max-w-none bc:align-top";
    const root = document.createElement("div");
    root.className = "bc:flex bc:flex-col bc:gap-2";
    root.append(alert, img);
    el.replaceChildren(root);

    // The body that the image shows, through a URL of its own, which goes once
    // another body or the unmount replaces it.
    let shown: Blob | null = null;
    let url: string | null = null;

    const draw = (): void => {
      const body = host.body();
      if (body !== shown) {
        if (url !== null) URL.revokeObjectURL(url);
        shown = body;
        url = body === null ? null : URL.createObjectURL(body);
      }
      if (url === null) img.removeAttribute("src");
      else img.src = url;
      img.hidden = url === null;

      const output = host.output();
      const { width, height } = isObject(output) ? output : {};
      if (typeof width === "number" && typeof height === "number") {
        img.width = width;
        img.height = height;
      }

      const loading = host.loading();
      root.setAttribute("aria-busy", String(loading));
      img.classList.toggle("bc:opacity-60", loading);

      const error = host.error();
      alert.hidden = error === null;
      alert.textContent = error?.message ?? "";
    };

    draw();
    const off = host.on("output", draw);
    return () => {
      off();
      if (url !== null) URL.revokeObjectURL(url);
    };
  },
};

export default renderer;

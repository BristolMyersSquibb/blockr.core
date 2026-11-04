// The UI modules of core's board options, in plain TypeScript with Tailwind
// CSS, each a field that shows the board option's value and submits a new one
// only when the user acts, as the contract has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// A field takes a value it did not set, such as one that undo sets back, and
// updates in place, but for a number the user is typing.

import type { OptionModule } from "../types/hosts.ts";
import { report } from "../util/index.ts";

// How long a number field waits after the user's last keystroke before it
// submits, so that typing 25 is one edit and one undo step.
export const PAUSE = 500;

// One of the values a field offers, with what the user sees for it.
export interface Choice {
  readonly value: string;
  readonly label: string;
}

const LABEL = "bc:text-sm bc:text-gray-700 bc:dark:text-gray-200";
const CONTROL =
  "bc:rounded-md bc:border bc:border-gray-300 bc:bg-white bc:px-2 bc:py-1 bc:text-sm bc:text-gray-900 bc:dark:scheme-dark bc:dark:border-gray-600 bc:dark:bg-gray-800 bc:dark:text-gray-100";

// A number field for a whole number of at least `min`, such as n_rows. It
// submits once the user pauses typing, presses Enter or leaves the field, and
// submits only a whole number of at least `min`. A field left with anything
// else shows the value again.
export function numberInput(label: string, min: number): OptionModule<number> {
  return {
    render({ el, host }) {
      const input = document.createElement("input");
      input.type = "number";
      input.min = String(min);
      input.step = "1";
      input.className = `${CONTROL} bc:w-24`;
      el.replaceChildren(labelled(label, input));

      // What the field showed last, so that it can tell what the user typed
      // and has not submitted, which it leaves.
      let shown = "";
      let timer: ReturnType<typeof setTimeout> | undefined;

      const draw = (): void => {
        if (input.value !== shown) return;
        const value: unknown = host.value();
        shown = typeof value === "number" ? String(value) : "";
        input.value = shown;
        input.removeAttribute("aria-invalid");
      };

      // Submits what the user typed where it is a whole number of at least
      // `min`. A pause leaves anything else to be typed on, while leaving the
      // field shows the value again.
      const commit = (leaving: boolean): void => {
        clearTimeout(timer);
        timer = undefined;
        const typed = input.value;
        const value = Number(typed);
        if (typed.trim() === "" || !Number.isInteger(value) || value < min) {
          if (leaving) {
            shown = input.value;
            draw();
          } else {
            input.setAttribute("aria-invalid", "true");
          }
          return;
        }
        input.removeAttribute("aria-invalid");
        shown = typed;
        if (value === host.value()) return;
        host.submit(value).catch((error: unknown) => {
          report(error);
          if (input.value === typed) draw();
        });
      };

      input.addEventListener("input", () => {
        clearTimeout(timer);
        timer = setTimeout(() => {
          commit(false);
        }, PAUSE);
      });
      input.addEventListener("blur", () => {
        commit(true);
      });
      input.addEventListener("keydown", (event) => {
        if (event.key === "Enter") commit(true);
      });

      draw();
      const off = host.on("value", draw);
      return () => {
        off();
        // An edit still pending when core unmounts the field is submitted.
        if (timer !== undefined) commit(false);
      };
    },
  };
}

// A checkbox for each choice, such as for each severity of show_conditions.
// Each click submits the choices then checked, in the order of the choices.
export function checkboxGroupInput(
  label: string,
  choices: readonly Choice[],
): OptionModule<string[]> {
  return {
    render({ el, host }) {
      const group = document.createElement("fieldset");
      group.className = "bc:m-0 bc:flex bc:min-w-0 bc:flex-col bc:gap-1 bc:border-0 bc:p-0";
      const legend = document.createElement("legend");
      legend.className = `${LABEL} bc:mb-1 bc:p-0`;
      legend.textContent = label;
      const boxes = choices.map((choice) => {
        const box = document.createElement("input");
        box.type = "checkbox";
        box.value = choice.value;
        box.className = "bc:accent-blue-600 bc:dark:scheme-dark";
        const text = document.createElement("span");
        text.textContent = choice.label;
        const item = document.createElement("label");
        item.className = "bc:flex bc:items-center bc:gap-2 bc:text-sm bc:text-gray-900 bc:dark:text-gray-100";
        item.append(box, text);
        return { box, item };
      });
      group.append(legend, ...boxes.map(({ item }) => item));
      el.replaceChildren(group);

      const draw = (): void => {
        const value: unknown = host.value();
        const checked = new Set(Array.isArray(value) ? value : []);
        for (const { box } of boxes) box.checked = checked.has(box.value);
      };

      group.addEventListener("change", () => {
        const value = boxes.filter(({ box }) => box.checked).map(({ box }) => box.value);
        host.submit(value).catch((error: unknown) => {
          report(error);
          draw();
        });
      });

      draw();
      return host.on("value", draw);
    },
  };
}

// A select of one of the choices, such as light or dark for dark_mode, which
// submits the choice as the user makes it.
export function selectInput(label: string, choices: readonly Choice[]): OptionModule<string> {
  return {
    render({ el, host }) {
      const select = document.createElement("select");
      select.className = CONTROL;
      for (const choice of choices) {
        const option = document.createElement("option");
        option.value = choice.value;
        option.textContent = choice.label;
        select.append(option);
      }
      el.replaceChildren(labelled(label, select));

      const draw = (): void => {
        const value: unknown = host.value();
        select.selectedIndex = choices.findIndex((choice) => choice.value === value);
      };

      select.addEventListener("change", () => {
        host.submit(select.value).catch((error: unknown) => {
          report(error);
          draw();
        });
      });

      draw();
      return host.on("value", draw);
    },
  };
}

// A field's control with its label beside it.
function labelled(label: string, control: HTMLElement): HTMLLabelElement {
  const text = document.createElement("span");
  text.className = LABEL;
  text.textContent = label;
  const field = document.createElement("label");
  field.className = "bc:flex bc:items-center bc:justify-between bc:gap-3";
  field.append(text, control);
  return field;
}

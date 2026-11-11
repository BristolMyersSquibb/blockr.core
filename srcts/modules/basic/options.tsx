// The dialog that sets the board options that the board holds.
// Core mounts the UI of each with mountOption(), or the renderer that the app
// names for it. The dialog groups them by the category that each definition
// gives.
// The board holds a board option once something sets one, as where core's
// basic frontend requires it or a mounted module names it.

import { useCallback } from "react";
import type { ReactElement } from "react";
import { Mounted } from "../../card/index.ts";
import { styles } from "../../styles/index.ts";
import type { OptionId } from "../../types/board.ts";
import { useFrontEnd } from "./context.ts";
import { useBoard } from "./hooks.ts";
import { Dialog } from "./ui.tsx";

export function OptionsDialog({ onClose }: { onClose: () => void }): ReactElement {
  const { core } = useFrontEnd();
  const board = useBoard(core);
  const registered = core.options.registered();

  const categories = new Map<string, OptionId[]>();
  for (const id of Object.keys(board.options)) {
    const def = Object.hasOwn(registered, id) ? registered[id] : undefined;
    if (def === undefined) continue;
    categories.set(def.category, [...(categories.get(def.category) ?? []), id]);
  }

  return (
    <Dialog title="Board options" onClose={onClose}>
      {categories.size === 0 ? <p className={styles.hint}>The board holds no board options.</p> : null}
      {[...categories].map(([category, ids]) => (
        <section key={category} aria-label={category} className="bc:grid bc:gap-2">
          <h3 className="bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted">{category}</h3>
          {ids.map((id) => (
            <OptionUi key={id} id={id} />
          ))}
        </section>
      ))}
    </Dialog>
  );
}

function OptionUi({ id }: { id: OptionId }): ReactElement {
  const { core } = useFrontEnd();
  const mount = useCallback((el: HTMLElement) => core.mountOption(el, id), [core, id]);
  return <Mounted mount={mount} />;
}

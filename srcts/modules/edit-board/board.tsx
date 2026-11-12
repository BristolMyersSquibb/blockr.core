// What the Edit board plugin draws: three sections, for the board's blocks,
// its links and its stacks, each listing what the board holds with the
// controls that edit it, as What it shows has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#what-it-shows).
// It draws nothing of a block's state, which the block's UI sets, nor the
// board's name or its board options, which the frontend draws.
// It follows the board through the store, so the next edit reads the board as
// the last one left it, a block that the user just added among it.
// Under the board lock, the store refuses every edit that the plugin makes,
// so the plugin draws no control there, as it reads from core.lock.board. It
// then lists only the board's links and stacks, since a frontend draws the
// blocks on their cards.

import type { ReactElement, ReactNode } from "react";
import type { CoreApi } from "../../types/core-api.ts";
import { useBoard } from "../../ui/index.ts";
import { BlocksSection } from "./blocks.tsx";
import { LinksSection } from "./links.tsx";
import { StacksSection } from "./stacks.tsx";

export function EditBoard({ core }: { core: CoreApi }): ReactElement {
  const board = useBoard(core);
  const locked = core.lock.board;
  return (
    <div className="bc:grid bc:gap-4 bc:font-sans bc:text-text-default">
      {locked ? null : <BlocksSection core={core} board={board} />}
      <LinksSection core={core} board={board} locked={locked} />
      <StacksSection core={core} board={board} locked={locked} />
    </div>
  );
}

// One of the plugin's sections, a region named by its title.
export function Section({ title, children }: { title: string; children: ReactNode }): ReactElement {
  return (
    <section aria-label={title} className="bc:grid bc:gap-2">
      <h3 className="bc:m-0 bc:font-sans bc:text-sm bc:font-semibold">{title}</h3>
      {children}
    </section>
  );
}

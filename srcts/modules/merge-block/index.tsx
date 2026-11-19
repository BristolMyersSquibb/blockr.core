// The UI of the merge block: a select of the columns its inputs share,
// which the request shared gives, to join by,
// and a select of the join, of the four that the state args all_x and all_y
// make together.
// While the request asks again, as after an input changed,
// and after an ask failed, the columns are those of older data,
// so the select marks no column.
// The select of the join sets both state args in one submit, so that a pick
// is one edit and one undo step, as from a left join to a right join.

import type { ReactElement } from "react";
import { strings } from "../../binding/index.ts";
import { ComboBox, ComboBoxItem, reactModule, Stack, Text, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

type Args = Pick<State, "all_x" | "all_y">;

interface Join {
  readonly id: string;
  readonly name: string;
  // Which rows the join keeps, as the list shows it after the name.
  readonly description: string;
  readonly args: Args;
}

const joins: readonly Join[] = [
  { id: "inner", name: "Inner", description: "Only the rows that match", args: { all_x: false, all_y: false } },
  { id: "left", name: "Left", description: "Every row of x", args: { all_x: true, all_y: false } },
  { id: "right", name: "Right", description: "Every row of y", args: { all_x: false, all_y: true } },
  { id: "full", name: "Full", description: "Every row of x and y", args: { all_x: true, all_y: true } },
];

function MergeBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const shared = useRequest(host, "shared");
  const x = useValue(host, "all_x").value === true;
  const y = useValue(host, "all_y").value === true;
  const join = joins.find(({ args }) => args.all_x === x && args.all_y === y);
  return (
    <Stack>
      <ComboBox
        bound={useValue(host, "by")}
        selectionMode="multiple"
        label="By columns"
        defaultItems={strings(shared.answer).map((id) => ({ id }))}
        renderEmptyState={() => "The inputs share no column."}
        outdated={shared.loading || shared.error !== null}
      >
        {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
      </ComboBox>
      <ComboBox
        label="Join type"
        defaultItems={joins}
        value={join?.id ?? null}
        isRequired
        onChange={(key) => {
          const picked = joins.find(({ id }) => id === key);
          if (picked !== undefined) submit(host, picked.args);
        }}
      >
        {(item) => (
          <ComboBoxItem id={item.id} textValue={item.name}>
            <Text slot="label">{item.name}</Text>
            <Text slot="description">{item.description}</Text>
          </ComboBoxItem>
        )}
      </ComboBox>
    </Stack>
  );
}

// Submits the state args of the join that the user picked, unless the block's
// state holds them already.
// Where the host refuses the submit, core tells the user why, through a
// notification about the block.
function submit(host: BlockHost<State>, args: Args): void {
  const state = host.state();
  if (state.all_x === args.all_x && state.all_y === args.all_y) return;
  host.submit(args).catch(() => undefined);
}

export default reactModule(MergeBlock);

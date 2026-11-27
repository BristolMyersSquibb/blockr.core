// The UI of the merge block: a select of the columns its inputs share,
// which the request shared gives, to join by,
// and a select of the join, of the four that the state args all_x and all_y
// make together.
// While the request asks again, as after an input changed,
// and after an ask failed, the columns are those of older data,
// so the select marks no column.
// The select of the join binds both state args, which a pick sets in one
// submit, so that it is one edit and one undo step, as from a left join to a
// right join.

import type { ReactElement } from "react";
import { ComboBox, ComboBoxItem, FormLayout, Select, SelectItem, Text } from "@blockr/ui";
import { strings } from "../../binding/index.ts";
import { reactModule, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { Json } from "../../types/json.ts";
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

// The join that all_x and all_y make, by its id.
function joinOf(state: State): string | undefined {
  return joins.find(({ args }) => args.all_x === state.all_x && args.all_y === state.all_y)?.id;
}

// The state args of the join, by its id, or none for an id of no join.
function argsOf(id: Json | undefined): Partial<State> {
  return joins.find((join) => join.id === id)?.args ?? {};
}

function MergeBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const shared = useRequest(host, "shared");
  return (
    <FormLayout>
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
      <Select bound={useValue(host, joinOf, argsOf)} label="Join type" items={joins} isRequired>
        {(item) => (
          <SelectItem id={item.id} textValue={item.name}>
            <Text slot="label">{item.name}</Text>
            <Text slot="description">{item.description}</Text>
          </SelectItem>
        )}
      </Select>
    </FormLayout>
  );
}

export default reactModule(MergeBlock);

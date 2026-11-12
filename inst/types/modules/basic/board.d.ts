import type { ReactElement } from "react";
import type { Condition } from "../../protocol/index.ts";
import type { Json } from "../../types/json.ts";
import type { FrontEnd } from "./context.ts";
export declare function BasicBoard({ front }: {
    front: FrontEnd;
}): ReactElement;
export declare function asSeverities(value: Json | undefined): readonly Condition["severity"][];

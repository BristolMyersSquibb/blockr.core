import type { ReactElement } from "react";
import type { TabListProps, TabPanelProps, TabProps, TabsProps } from "../aria/index.ts";
export declare function Tabs({ className, ...props }: TabsProps): ReactElement;
export declare function TabList<T extends object>({ className, ...props }: TabListProps<T>): ReactElement;
export declare function Tab({ className, children, ...props }: TabProps): ReactElement;
export declare function TabPanel({ className, ...props }: TabPanelProps): ReactElement;

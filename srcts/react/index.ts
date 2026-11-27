// The React SDK: what core offers modules written in React, which core's own
// UIs draw with and which core ships to packages as `@blockr/core/react`.
// It holds the hooks through which a component follows its host or CoreApi,
// among them useValue(), which binds a value for an input of blockr.ui, and
// the helpers that make a module or a renderer from a component.
// It also holds the list in which a UI shows notifications, as useNotified()
// gives them.
// The components themselves are blockr.ui's, which a module imports as
// `@blockr/ui` (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3).

export { useBoard, useNotified, useOutput, useRequest, useStatus, useValue } from "./hooks.ts";
export type { Answered, OutputSnapshot, RequestOptions } from "./hooks.ts";
export { outputRenderer, reactModule } from "./module.tsx";
export type { ModuleProps, OutputProps } from "./module.tsx";
export { NotificationList } from "./notifications.tsx";
export type { NotificationListProps } from "./notifications.tsx";

// The React SDK: what core offers modules written in React, which core's own
// UIs draw with and which core ships to packages as `@blockr/core/react`.
// It holds what draws nothing: the hooks through which a component follows
// its host or CoreApi, among them useValue(), which binds a value for an input
// of blockr.ui, and the helpers that make a module or a renderer from a
// component.
// It imports React and nothing of blockr.ui, so that a page whose UIs bind
// their values with the hooks, but draw neither with blockr.ui's components
// nor with React Aria's, loads no React Aria
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-bundle).
// What core draws, such as the list in which a UI shows notifications, is in
// `@blockr/core/components`.
// The components themselves are blockr.ui's, which a module imports as
// `@blockr/ui` (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3).

export { useBoard, useNotified, useOutput, useRequest, useStatus, useValue } from "./hooks.ts";
export type { Answered, OutputSnapshot, RequestOptions } from "./hooks.ts";
export { outputRenderer, reactModule } from "./module.tsx";
export type { ModuleProps, OutputProps } from "./module.tsx";

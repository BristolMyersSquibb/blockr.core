// The React SDK: what core offers modules written in React, which core's own
// UIs draw with and which core ships to packages as `@blockr/core/react`.
// It holds the hooks through which a component follows its host or CoreApi,
// the helpers that make a module or a renderer from a component, and the
// inputs, each on one value that useValue() binds.

export { useBoard, useNotified, useOutput, useRequest, useStatus, useValue } from "./hooks.ts";
export type { Answered, Bound, OutputSnapshot, RequestOptions } from "./hooks.ts";
export { outputRenderer, reactModule } from "./module.tsx";
export type { ModuleOptions, ModuleProps, OutputProps } from "./module.tsx";
export { Checkbox, Checklist, Field, NumberInput, Select, Stack, TextInput } from "../inputs/index.ts";

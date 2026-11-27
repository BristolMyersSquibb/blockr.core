// The components that core offers modules written in React, which core ships
// to packages as `@blockr/core/components`: what core draws, such as the list
// in which a UI shows notifications, as useNotified() of core's React SDK gives
// them.
// Core's React SDK, `@blockr/core/react`, holds what draws nothing, so that a
// page whose UIs use its hooks alone loads nothing that core draws
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-bundle).

export { NotificationList } from "./notifications.tsx";
export type { NotificationListProps } from "./notifications.tsx";

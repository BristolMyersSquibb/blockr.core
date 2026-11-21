// The tags of core's React SDK: React Aria's TagGroup, TagList and Tag
// (https://react-aria.adobe.com/TagGroup), in core's theme, after the
// TagGroup and the Tag of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/TagGroup.tsx),
// drawn as the chips of the ComboBox's field (./combobox.tsx).
// They take what those take: the props of React Aria's parts, and the label,
// the description, the error message, the items, what the list says where it
// has none, and the colour that the starter kit adds.
// A group holds its tags, each with its text as its children, which the user
// picks where the group's selectionMode picks, and removes by its button
// where the group takes onRemove.
// A tag group takes no `bound`: a caller that picks or removes with it holds
// what it picks itself.

import { createContext, useContext } from "react";
import type { ReactElement, ReactNode } from "react";
import { Button, composeRenderProps, Label, Tag as AriaTag, TagGroup as AriaTagGroup, TagList, Text } from "../aria/index.ts";
import type { TagGroupProps as AriaTagGroupProps, TagListProps, TagProps as AriaTagProps } from "../aria/index.ts";
import { styles } from "../styles/index.ts";
import { Cross } from "./icons.tsx";

// The colours of a tag: the chip's, "gray", or a tint of the accent, "accent".
const colors = {
  gray: "bc:border-border-default bc:bg-bg-subtle bc:text-text-default",
  accent: "bc:border-border-accent bc:bg-bg-subtle bc:text-text-default",
} as const;

type Color = keyof typeof colors;

const ColorContext = createContext<Color>("gray");

export interface TagGroupProps<T> extends Omit<AriaTagGroupProps, "children">, Pick<TagListProps<T>, "items" | "children" | "renderEmptyState"> {
  // What the starter kit adds.
  readonly color?: Color;
  readonly label?: string;
  readonly description?: string;
  readonly errorMessage?: string;
}

export function TagGroup<T extends object>({
  label,
  description,
  errorMessage,
  items,
  children,
  renderEmptyState,
  color = "gray",
  className,
  ...props
}: TagGroupProps<T>): ReactElement {
  return (
    <AriaTagGroup {...props} className={`${styles.field} ${className ?? ""}`}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <ColorContext.Provider value={color}>
        <TagList items={items} renderEmptyState={renderEmptyState} className="bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1">
          {children}
        </TagList>
      </ColorContext.Provider>
      {description === undefined ? null : (
        <Text slot="description" className={styles.hint}>
          {description}
        </Text>
      )}
      {errorMessage === undefined ? null : (
        <Text slot="errorMessage" className={styles.alert}>
          {errorMessage}
        </Text>
      )}
    </AriaTagGroup>
  );
}

export interface TagProps extends AriaTagProps {
  // What the starter kit adds, the group's colour unless given.
  readonly color?: Color;
  readonly children?: ReactNode;
}

// A tag shows its text, and the button that removes it where its group takes
// onRemove.
// A picked tag takes the accent colour.
export function Tag({ children, color, className, ...props }: TagProps): ReactElement {
  const textValue = typeof children === "string" ? children : undefined;
  const groupColor = useContext(ColorContext);
  return (
    <AriaTag
      textValue={textValue}
      {...props}
      className={composeRenderProps(className, (given) => `${TAG} ${colors[color ?? groupColor]} ${given ?? ""}`)}
    >
      {composeRenderProps(children, (children, { allowsRemoving }) => (
        <>
          <span className="bc:truncate">{children}</span>
          {allowsRemoving ? (
            <Button slot="remove" className={REMOVE}>
              <Cross />
            </Button>
          ) : null}
        </>
      ))}
    </AriaTag>
  );
}

// The classes of a tag and of its button, as of a chip of the ComboBox's
// field.
const TAG =
  "bc:inline-flex bc:max-w-full bc:min-w-0 bc:cursor-default bc:items-center bc:gap-0.5 bc:rounded bc:border bc:px-1.5 bc:py-0 bc:font-sans bc:text-xs bc:leading-5 bc:outline-none bc:data-allows-removing:pr-0.5 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-selected:border-border-accent bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-disabled:opacity-50";
const REMOVE =
  "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-inherit bc:data-hovered:bg-bg-subtle";

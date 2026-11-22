// The breadcrumbs of core's React SDK: React Aria's Breadcrumbs and
// Breadcrumb (https://react-aria.adobe.com/Breadcrumbs), in core's theme,
// after the Breadcrumbs of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Breadcrumbs.tsx).
// The breadcrumbs take the props of React Aria's Breadcrumbs, and a
// breadcrumb those of React Aria's Breadcrumb and those of the SDK's Link,
// in which it draws its text, in the colour of the text around it.
// The breadcrumbs lay out their breadcrumbs in a row, which wraps, with a
// chevron after each but the last, which is the current one.
// A breadcrumb with an `href` goes there. Without one, the breadcrumbs' `onAction`
// acts on it, given its `id`. The current breadcrumb does nothing.
// A UI puts the breadcrumbs in a landmark of the role navigation, which names
// what they lead through, as in <nav aria-label="Directory">.

import type { ReactElement } from "react";
import { Breadcrumb as AriaBreadcrumb, Breadcrumbs as AriaBreadcrumbs, composeRenderProps } from "../aria/index.ts";
import { ChevronRight } from "../inputs/index.ts";
import type { BreadcrumbProps as AriaBreadcrumbProps, BreadcrumbsProps } from "../aria/index.ts";
import { Link } from "./link.tsx";
import type { LinkProps } from "./link.tsx";

export function Breadcrumbs<T extends object>({ className, ...props }: BreadcrumbsProps<T>): ReactElement {
  return <AriaBreadcrumbs {...props} className={`${BREADCRUMBS} ${className ?? ""}`} />;
}

export type BreadcrumbProps = AriaBreadcrumbProps & Omit<LinkProps, "className" | "variant" | "id">;

// The breadcrumb's `id` names it to the breadcrumbs, as the key of an item of
// a collection, and goes on no element, so that two breadcrumbs of one id on
// the page do not share an id there.
export function Breadcrumb({ className, id, ...props }: BreadcrumbProps): ReactElement {
  return (
    <AriaBreadcrumb {...props} id={id} className={composeRenderProps(className, (given) => `${BREADCRUMB} ${given ?? ""}`)}>
      {({ isCurrent }) => (
        <>
          <Link variant="secondary" {...props} />
          {isCurrent ? null : <ChevronRight />}
        </>
      )}
    </AriaBreadcrumb>
  );
}

// The breadcrumbs are a list, which core's stylesheet leaves the browser's
// margin, padding and numbers, since it holds no reset of the page.
const BREADCRUMBS = "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:items-center bc:gap-1 bc:p-0 bc:font-sans bc:text-sm";
const BREADCRUMB = "bc:flex bc:items-center bc:gap-1";

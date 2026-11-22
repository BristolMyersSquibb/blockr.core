import type { ReactElement } from "react";
import type { BreadcrumbProps as AriaBreadcrumbProps, BreadcrumbsProps } from "../aria/index.ts";
import type { LinkProps } from "./link.tsx";
export declare function Breadcrumbs<T extends object>({ className, ...props }: BreadcrumbsProps<T>): ReactElement;
export type BreadcrumbProps = AriaBreadcrumbProps & Omit<LinkProps, "className" | "variant" | "id">;
export declare function Breadcrumb({ className, id, ...props }: BreadcrumbProps): ReactElement;

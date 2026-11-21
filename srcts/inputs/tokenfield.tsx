// The field of tokens of core's React SDK: React Aria's TokenField,
// TokenInput and Token (https://react-aria.adobe.com/TokenField), in core's
// theme, after the TokenField and the Token of React Aria's Tailwind starter
// kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/TokenField.tsx).
// They take what those take: the props of React Aria's parts, and the label
// and the description that the starter kit adds.
// The field holds text in which tokens sit, such as the names of columns in
// a formula, in the box of a text field, and draws each token through its
// children, a function of the token, as a Token.
// A field of tokens takes no `bound`: React Aria keeps its value as the text
// and the tokens it holds, which a caller turns into what it submits itself.

import type { ReactElement, Ref } from "react";
import { composeRenderProps, Label, Text, Token as AriaToken, TokenField as AriaTokenField, TokenInput } from "../aria/index.ts";
import type { TokenFieldProps as AriaTokenFieldProps, TokenFieldValue, TokenInputProps, TokenProps } from "../aria/index.ts";
import { styles } from "../styles/index.ts";

export interface TokenFieldProps<T extends TokenFieldValue = TokenFieldValue> extends Omit<AriaTokenFieldProps<T>, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string;
  readonly inputRef?: Ref<HTMLDivElement>;
  readonly children: TokenInputProps["children"];
}

export function TokenField<T extends TokenFieldValue = TokenFieldValue>({
  label,
  description,
  className,
  inputRef,
  children,
  ...props
}: TokenFieldProps<T>): ReactElement {
  return (
    <AriaTokenField {...props} className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <TokenInput ref={inputRef} className={INPUT}>
        {children}
      </TokenInput>
      {description === undefined ? null : (
        <Text slot="description" className={styles.hint}>
          {description}
        </Text>
      )}
    </AriaTokenField>
  );
}

// A token, drawn as a chip in the accent colour, which takes the accent
// itself where the user selects it.
export function Token({ className, ...props }: TokenProps): ReactElement {
  return <AriaToken {...props} className={composeRenderProps(className, (given) => `${TOKEN} ${given ?? ""}`)} />;
}

// The classes of the box, a text field's, and of a token.
const INPUT =
  "bc:box-border bc:min-h-[30px] bc:w-full bc:rounded-md bc:border bc:border-border-field bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focused:outline-2 bc:data-focused:outline-offset-1 bc:data-focused:outline-border-accent bc:data-disabled:opacity-50";
const TOKEN =
  "bc:mx-0.5 bc:inline-flex bc:h-5 bc:cursor-default bc:items-center bc:whitespace-nowrap bc:rounded bc:border bc:border-border-accent bc:bg-bg-subtle bc:px-1.5 bc:text-xs bc:text-text-default bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent";

// Modules drawn by a React component, which core mounts as it mounts any
// module (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// reactModule() makes a module of any kind, and outputRenderer() a renderer of
// an output class. A package may use them, or write a module's render()
// itself.

import { Component } from "react";
import type { ComponentType, ReactElement, ReactNode } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { Condition } from "../protocol/index.ts";
import type { OptionId } from "../types/board.ts";
import type { OutputHost, OutputModule } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { BlockOutput } from "../types/outputs.ts";
import { styles } from "../styles/index.ts";
import { errorMessage, report } from "../util/index.ts";
import { useOutput } from "./hooks.ts";

// What a module that reactModule() makes hands its component: the host that
// core mounts the module with, and the signal that core aborts as it unmounts
// the module.
export interface ModuleProps<H> {
  readonly host: H;
  readonly signal: AbortSignal;
}

// What a module names beside its render(): the board options it requires.
export interface ModuleOptions {
  readonly options?: readonly OptionId[];
}

// A module drawn by a React component. Core's render() call mounts the
// component in the element, drawing it before it returns, and the cleanup
// unmounts it, which empties the element.
export function reactModule<H>(
  Draw: ComponentType<ModuleProps<H>>,
  options: ModuleOptions = {},
): ModuleOptions & { render(args: { el: HTMLElement; host: H; signal: AbortSignal }): () => void } {
  return {
    ...(options.options === undefined ? {} : { options: options.options }),
    render({ el, host, signal }) {
      const root = createRoot(el);
      flushSync(() => {
        root.render(<Draw host={host} signal={signal} />);
      });
      return () => {
        root.unmount();
      };
    },
  };
}

// What outputRenderer() hands a renderer's component: what its output host
// holds, with the output and its parameters typed by the output class's
// contract, and the host itself, through which the renderer asks for other
// parameters, calls requests and reports what the user did.
export interface OutputProps<O extends BlockOutput = BlockOutput, P = Json> {
  readonly output: O;
  readonly body: Blob | null;
  readonly params: P;
  readonly options: Readonly<Record<OptionId, Json>>;
  readonly loading: boolean;
  readonly error: Condition | null;
  readonly host: OutputHost;
}

// A renderer of an output class, drawn by a React component, which gets the
// output as its class's contract types it, since R writes every output by its
// class's schema. Around the component, the renderer:
// - marks itself busy and dims what the component drew while core asks R for
//   a newer output, and
// - shows the error of a component that throws in place of what it drew, until
//   another output comes.
// It draws nothing of why the latest ask failed, which the component gets as
// `error`, since core raises it as a notification about the block, which the
// frontend places.
export function outputRenderer<O extends BlockOutput = BlockOutput, P = Json>(
  Draw: ComponentType<OutputProps<O, P>>,
  options: ModuleOptions = {},
): OutputModule {
  function Renderer({ host }: ModuleProps<OutputHost>): ReactElement {
    const now = useOutput(host);
    return (
      <div className="bc:flex bc:max-h-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default" aria-busy={now.loading}>
        <Boundary output={now.output}>
          <div className={`bc:flex bc:min-h-0 bc:flex-col ${now.loading ? "bc:opacity-60" : ""}`}>
            <Draw
              output={now.output as unknown as O}
              body={now.body}
              params={now.params as P}
              options={now.options}
              loading={now.loading}
              error={now.error}
              host={host}
            />
          </div>
        </Boundary>
      </div>
    );
  }
  return reactModule(Renderer, options);
}

function Alert({ message }: { message: string }): ReactElement {
  return (
    <p role="alert" className={styles.alert}>
      {message}
    </p>
  );
}

// Shows why a renderer's component threw, in place of what it drew, and draws
// the component again once another output comes. It reports what it caught,
// so that the page hears of it.
class Boundary extends Component<{ output: Json; children?: ReactNode }, { failed: string | null }> {
  override state: { failed: string | null } = { failed: null };

  static getDerivedStateFromError(error: unknown): { failed: string } {
    return { failed: errorMessage(error) };
  }

  override componentDidCatch(error: unknown): void {
    report(error);
  }

  override componentDidUpdate(before: { output: Json }): void {
    if (this.state.failed !== null && before.output !== this.props.output) {
      this.setState({ failed: null });
    }
  }

  override render(): ReactNode {
    const { failed } = this.state;
    return failed === null ? this.props.children : <Alert message={`The renderer failed to draw the output: ${failed}`} />;
  }
}

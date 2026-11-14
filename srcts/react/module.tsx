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
import { asError, errorMessage } from "../util/index.ts";
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
// class's schema. Around the component, the renderer does two things:
// - It marks itself busy and dims what the component drew while core asks R
//   for a newer output.
// - It draws nothing in place of a component that throws, and raises why
//   through the host's notify(), with the code "render_failed", until the
//   component draws another output or the renderer is unmounted. It logs what
//   the component threw through the host's log, at error.
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
        <Boundary host={host} output={now.output}>
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

// Draws nothing in place of a renderer's component that throws, and raises a
// notification of why through the host, which concerns the block. It draws the
// component again once another output comes, and withdraws the notification
// once the component draws, or replaces it where the component throws again.
// It logs what it caught too, at error, through the host's log.
class Boundary extends Component<{ host: OutputHost; output: Json; children?: ReactNode }, { failed: boolean }> {
  override state: { failed: boolean } = { failed: false };
  // What withdraws the notification of the latest throw, while one holds.
  private failure: (() => void) | undefined;

  static getDerivedStateFromError(): { failed: boolean } {
    return { failed: true };
  }

  override componentDidCatch(error: unknown): void {
    this.props.host.log.error(asError(error));
    const before = this.failure;
    this.failure = this.props.host.notify({
      severity: "error",
      code: "render_failed",
      message: `The renderer failed to draw the output: ${errorMessage(error)}`,
    });
    before?.();
  }

  // Another output draws the component again, but not the one that it threw
  // on, which may have come with the update in which it threw.
  override componentDidUpdate(before: { output: Json }, was: { failed: boolean }): void {
    if (this.state.failed) {
      if (was.failed && before.output !== this.props.output) this.setState({ failed: false });
    } else {
      this.withdrawFailure();
    }
  }

  override componentWillUnmount(): void {
    this.withdrawFailure();
  }

  private withdrawFailure(): void {
    const before = this.failure;
    this.failure = undefined;
    before?.();
  }

  override render(): ReactNode {
    return this.state.failed ? null : this.props.children;
  }
}

// The plugins that core's basic frontend shows, as Plugins has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#plugin-classes):
// each plugin whose class has a `ui` gets a section of its own, in which core
// mounts the module with mountPlugin(), as the frontend shows every block. The
// frontend reads no member of a plugin class beyond `ui`, so that a plugin
// needs only its `ui` to show under it.
// The user collapses and expands each section by the button that holds the
// plugin's name, and each section starts open. A collapsed section hides the
// plugin's UI and leaves it mounted, so that the UI keeps what it holds
// outside the plugin's state. Which sections are collapsed is the page's,
// which nothing saves or undoes. The plugin is on screen while the element of
// its UI is in view, which it is not in a collapsed section.
//
// A section shows the notifications about its plugin, of the severities that
// the board option show_conditions names, whoever raised them, as Core's
// basic frontend has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#cores-basic-frontend):
// - The title holds a pill for each severity beside the plugin's name, whether
//   the section is open or collapsed, as a card's pills count the
//   notifications about its block. The pill of the errors reads "error", and
//   those of the warnings and the messages count them, as "2 warnings".
// - Each pill opens the messages of its notifications below the title, as a
//   card's pill opens its banners, and they show while the section is
//   collapsed too. Each pill starts closed, since the plugin shows the
//   notifications in its UI. So the user can still read one that the plugin
//   does not show, such as why its UI cannot draw.
// The title is a heading that holds the button, with the pills beside it,
// rather than the summary of a <details>. A summary acts as a button itself,
// so the pills' buttons would sit inside it, where a screen reader may not
// reach them.

import { useCallback, useId, useLayoutEffect, useRef, useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { Banners, Mounted, Pills, usePills } from "../../card/index.ts";
import { useNotified } from "../../react/index.ts";
import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi, Plugin, Severity } from "../../types/core-api.ts";
import { classes } from "../../ui/index.ts";
import { useFrontEnd } from "./context.ts";

// Which plugins' sections the user collapsed, for the opening, which
// components follow as React's useSyncExternalStore follows a store, and
// through which show() opens a section.
export interface Sections {
  // Whether the user collapsed the plugin's section.
  readonly collapsed: (plugin: PluginClassId) => boolean;
  // Collapses the plugin's section, or expands it given false.
  readonly collapse: (plugin: PluginClassId, collapsed: boolean) => void;
  readonly subscribe: (fn: () => void) => () => void;
}

export function createSections(): Sections {
  const collapsed = new Set<PluginClassId>();
  const listeners = new Set<() => void>();
  return {
    collapsed: (plugin) => collapsed.has(plugin),
    collapse(plugin, collapse) {
      if (collapsed.has(plugin) === collapse) return;
      if (collapse) collapsed.add(plugin);
      else collapsed.delete(plugin);
      for (const fn of [...listeners]) fn();
    },
    subscribe(fn) {
      const wrapped = (): void => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
  };
}

// Whether the frontend shows the plugin in a section of its own, which shows
// the notifications about it: the app runs it, and its class has a `ui`.
export function hasSection(core: Pick<CoreApi, "plugins">, plugin: PluginClassId): boolean {
  return Object.hasOwn(core.plugins.appPlugins(), plugin) && core.plugins.pluginClass(plugin).ui !== undefined;
}

// The plugins that the frontend shows, in the order of the app's plugins: those
// that run whose class has a `ui`, each by the id of its class.
export function placed(core: Pick<CoreApi, "plugins">): readonly (readonly [PluginClassId, Plugin])[] {
  return Object.entries(core.plugins.appPlugins()).filter(([id]) => hasSection(core, id));
}

// The section of each plugin that the frontend shows, beside the board where
// the page is wide enough, in a column that takes a third of the page's width
// and no less than 28rem, staying in view as the board scrolls, and above it
// otherwise. It draws nothing where the frontend shows no plugin.
// Each section shows the notifications about its plugin of the severities
// given.
export function Plugins({ severities }: { severities: readonly Severity[] }): ReactElement | null {
  const { core } = useFrontEnd();
  const shown = placed(core);
  if (shown.length === 0) return null;
  return (
    <aside
      aria-label="Plugins"
      className="bc:grid bc:min-w-0 bc:content-start bc:gap-4 bc:md:sticky bc:md:top-4 bc:md:max-h-[calc(100dvh-2rem)] bc:md:w-1/3 bc:md:min-w-md bc:md:shrink-0 bc:md:overflow-auto"
    >
      {shown.map(([id, plugin]) => (
        <PluginSection key={id} id={id} plugin={plugin} severities={severities} />
      ))}
    </aside>
  );
}

function PluginSection({
  id,
  plugin,
  severities,
}: {
  id: PluginClassId;
  plugin: Plugin;
  severities: readonly Severity[];
}): ReactElement {
  const { core, inView, sections } = useFrontEnd();
  const notified = useNotified(core, { plugin: id });
  const pills = usePills(notified, severities);
  const collapsed = useSyncExternalStore(sections.subscribe, () => sections.collapsed(id));
  const body = useId();
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const el = ref.current;
    return el === null ? undefined : inView.watch({ plugin: id }, el);
  }, [inView, id]);
  const mount = useCallback((el: HTMLElement) => core.mountPlugin(el, id), [core, id]);
  return (
    <section
      aria-label={plugin.name}
      className={`${classes.card} bc:grid bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:p-3`}
    >
      <div className="bc:flex bc:flex-wrap bc:items-center bc:gap-2">
        <h3 className={classes.heading}>
          <button
            type="button"
            aria-expanded={!collapsed}
            aria-controls={body}
            onClick={() => {
              sections.collapse(id, !collapsed);
            }}
            className="bc:inline-flex bc:cursor-pointer bc:items-center bc:gap-1.5 bc:border-0 bc:bg-transparent bc:p-0 bc:text-inherit bc:[font:inherit]"
          >
            <Marker collapsed={collapsed} />
            {plugin.name}
          </button>
        </h3>
        <Pills {...pills} className="" />
      </div>
      <Banners {...pills} />
      <div id={body} ref={ref} hidden={collapsed} className="bc:min-w-0">
        <Mounted mount={mount} className="bc:min-w-0" />
      </div>
    </section>
  );
}

// The triangle before a section's name, as a browser draws one before the
// summary of a <details>: it points at the name while the section is
// collapsed, and down while it is open.
function Marker({ collapsed }: { collapsed: boolean }): ReactElement {
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 16 16"
      fill="currentColor"
      className={`bc:size-3 bc:shrink-0 bc:transition-transform bc:motion-reduce:transition-none ${collapsed ? "" : "bc:rotate-90"}`}
    >
      <path d="M5.5 3.5 11 8l-5.5 4.5Z" />
    </svg>
  );
}

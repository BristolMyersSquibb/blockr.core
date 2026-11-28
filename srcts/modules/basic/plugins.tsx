// The plugins that core's basic frontend shows, as Plugins has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#plugin-classes):
// each plugin whose class has a `ui` gets a section of its own, in which core
// mounts the module with mountPlugin(), as the frontend shows every block. The
// frontend reads no member of a plugin class beyond `ui`, so that a plugin
// needs only its `ui` to show under it.
// Each section is a Disclosure of blockr.ui, which the user collapses and
// expands by the button of its header, which holds the plugin's name, and
// each section starts open. A collapsed section hides the plugin's UI and
// leaves it mounted, so that the UI keeps what it holds outside the plugin's
// state. Which sections are collapsed is the page's, which nothing saves or
// undoes. The plugin is on screen while the element of its UI is in view,
// which it is not in a collapsed section, since that element hides with the
// panel rather than only from sight, as the panel of blockr.ui's Disclosure
// hides what it holds.
//
// A section shows the notifications about its plugin, of every severity,
// whoever raised them, as Core's basic frontend has it
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
// The header is a heading that holds the button, with the pills beside it,
// outside the button, so that a screen reader reaches the pills' buttons on
// their own.

import { useCallback, useLayoutEffect, useRef, useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { Disclosure, DisclosureHeader, DisclosurePanel } from "@blockr/ui";
import { Banners, Mounted, Pills, usePills } from "../../card/index.ts";
import { useNotified } from "../../react/index.ts";
import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi, Plugin } from "../../types/core-api.ts";
import { classes } from "../../ui/index.ts";
import { idNumber } from "../../util/index.ts";
import { useFrontEnd } from "./context.ts";

// Which plugins' sections the user collapsed, or which stacks' groups, for
// the opening, by the id of the plugin's class, as core's table holds it, or
// of the stack, which components follow as React's useSyncExternalStore
// follows a store, and through which show() opens a section or a group.
export interface Sections<K = string> {
  // Whether the user collapsed the section or the group.
  readonly collapsed: (id: K) => boolean;
  // Collapses the section or the group, or expands it given false.
  readonly collapse: (id: K, collapsed: boolean) => void;
  readonly subscribe: (fn: () => void) => () => void;
}

export function createSections<K = string>(): Sections<K> {
  const collapsed = new Set<K>();
  const listeners = new Set<() => void>();
  return {
    collapsed: (id) => collapsed.has(id),
    collapse(id, collapse) {
      if (collapsed.has(id) === collapse) return;
      if (collapse) collapsed.add(id);
      else collapsed.delete(id);
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
  return core.plugins.appPlugins().has(plugin) && core.plugins.pluginClass(plugin).ui !== undefined;
}

// The plugins that the frontend shows, in the order of the app's plugins: those
// that run whose class has a `ui`, each by the id of its class.
export function placed(core: Pick<CoreApi, "plugins">): readonly (readonly [PluginClassId, Plugin])[] {
  return [...core.plugins.appPlugins()].filter(([id]) => hasSection(core, id));
}

// The section of each plugin that the frontend shows, beside the board where
// the page is wide enough, in a column that takes a third of the page's width
// and no less than 28rem, staying in view as the board scrolls, and above it
// otherwise. It draws nothing where the frontend shows no plugin.
export function Plugins(): ReactElement | null {
  const { core } = useFrontEnd();
  const shown = placed(core);
  if (shown.length === 0) return null;
  return (
    <aside
      aria-label="Plugins"
      className="bc:grid bc:min-w-0 bc:content-start bc:gap-4 bc:md:sticky bc:md:top-4 bc:md:max-h-[calc(100dvh-2rem)] bc:md:w-1/3 bc:md:min-w-md bc:md:shrink-0 bc:md:overflow-auto"
    >
      {shown.map(([id, plugin]) => (
        <PluginSection key={idNumber(id)} id={id} plugin={plugin} />
      ))}
    </aside>
  );
}

function PluginSection({ id, plugin }: { id: PluginClassId; plugin: Plugin }): ReactElement {
  const { core, inView, sections } = useFrontEnd();
  const notified = useNotified(core, { plugin: id });
  const pills = usePills(notified);
  const collapsed = useSyncExternalStore(sections.subscribe, () => sections.collapsed(id));
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const el = ref.current;
    return el === null ? undefined : inView.watch({ plugin: id }, el);
  }, [inView, id]);
  const mount = useCallback((el: HTMLElement) => core.mountPlugin(el, id), [core, id]);
  return (
    <section
      aria-label={plugin.name}
      className={`${classes.card} bc:min-w-0 bc:rounded-lg bc:border bc:border-border bc:p-3`}
    >
      <Disclosure
        isExpanded={!collapsed}
        onExpandedChange={(expanded) => {
          sections.collapse(id, !expanded);
        }}
      >
        <div className="bc:grid bc:min-w-0 bc:gap-3">
          <div className="bc:flex bc:flex-wrap bc:items-center bc:gap-2">
            <DisclosureHeader>{plugin.name}</DisclosureHeader>
            <Pills {...pills} className="" />
          </div>
          <Banners {...pills} />
        </div>
        <DisclosurePanel className="bc:min-w-0">
          <div ref={ref} hidden={collapsed} className="bc:min-w-0 bc:pt-3">
            <Mounted mount={mount} className="bc:min-w-0" />
          </div>
        </DisclosurePanel>
      </Disclosure>
    </section>
  );
}

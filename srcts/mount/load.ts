// Loading the modules of packages: core imports each URL that R serves a
// module at once per page, with the stylesheet beside it, so that a module
// that core mounts many times, or for several blocks of its class, runs its
// imports once, as Packaging has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#packaging).
// What a module exports is checked where core mounts it (./block.ts).

import { messageOf, own, packageOf, report } from "../util/index.ts";

// The major of core's JS API. A module that core mounts declares the major it
// was built against, as `export const api = 1`, and core refuses one that
// declares another.
export const API = 1;

// Where R serves a module, and the stylesheet beside it, if any.
export interface Source {
  readonly module: string;
  readonly css?: string;
}

export interface Loader {
  // Imports the module, with its stylesheet, and resolves with what it
  // exports, or rejects with a ModuleError that names the module and the
  // package it comes from, or null for none.
  load(source: Source, pkg: string | null): Promise<unknown>;
  // A module another package offers, by id, as R's latest reply names it.
  module(id: string): Promise<unknown>;
}

export interface LoaderOptions {
  // The modules that packages offer each other, by id, from R's latest reply
  // to `hello` or to an opening.
  readonly modules?: () => Readonly<Record<string, Source>>;
  // Imports a module by its URL, resolved against the page, as `import()`
  // does on the page. Tests give their own.
  readonly importer?: (url: string) => Promise<unknown>;
  // Puts a stylesheet on the page, resolving once it has loaded.
  readonly stylesheet?: (url: string) => Promise<void>;
}

// Why a module could not be used: it failed to load, declares another major
// of core's JS API, or failed as core called it. It names the module, by its
// URL without the query that R adds, and its package.
export class ModuleError extends Error {
  readonly module: string;
  readonly package: string | null;

  constructor(module: string, pkg: string | null, what: string, options?: ErrorOptions) {
    const name = module.replace(/[?#].*$/, "");
    super(`The module ${name} ${pkg === null ? "from no package" : `of ${pkg}`} ${what}`, options);
    this.name = "ModuleError";
    this.module = name;
    this.package = pkg;
  }
}

export function createLoader(options: LoaderOptions = {}): Loader {
  const importer = options.importer ?? importModule;
  const stylesheet = options.stylesheet ?? linkStylesheet;
  const modules = options.modules ?? ((): Readonly<Record<string, Source>> => ({}));

  // What each URL resolved with, or why it failed, which a later load gets
  // too, as the page keeps a module it imported.
  const imported = new Map<string, Promise<unknown>>();
  const sheets = new Map<string, Promise<void>>();

  function once<T>(cache: Map<string, Promise<T>>, url: string, load: (url: string) => Promise<T>): Promise<T> {
    let loading = cache.get(url);
    if (loading === undefined) {
      loading = load(url);
      cache.set(url, loading);
    }
    return loading;
  }

  async function load(source: Source, pkg: string | null): Promise<unknown> {
    // A stylesheet that fails to load leaves its module unstyled rather than
    // unusable, so the failure is reported and the module loads regardless.
    const sheet =
      source.css === undefined
        ? Promise.resolve()
        : once(sheets, source.css, stylesheet).catch((error: unknown) => {
            report(new ModuleError(source.module, pkg, `has a stylesheet that failed to load: ${messageOf(error)}`, { cause: error }));
          });
    const exports = once(imported, source.module, importer);
    await sheet;
    try {
      return await exports;
    } catch (error) {
      throw new ModuleError(source.module, pkg, `failed to load: ${messageOf(error)}`, { cause: error });
    }
  }

  return {
    load,
    module(id) {
      const source = own(modules(), id);
      if (source === undefined) {
        return Promise.reject(new Error(`No package offers a module "${id}".`));
      }
      return load(source, packageOf(id));
    },
  };
}

// A module URL resolves against the page, as a module that R serves sits
// under the page's own path, rather than against core's runtime, which
// `import()` resolves a relative URL against.
function importModule(url: string): Promise<unknown> {
  return import(/* @vite-ignore */ new URL(url, document.baseURI).href) as Promise<unknown>;
}

function linkStylesheet(url: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = url;
    link.addEventListener("load", () => {
      resolve();
    }, { once: true });
    link.addEventListener("error", () => {
      reject(new Error(`${url} could not be loaded.`));
    }, { once: true });
    document.head.append(link);
  });
}

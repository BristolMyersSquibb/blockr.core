// TypeDoc writes the reference of core's TypeScript into
// vignettes/articles/typescript, as Markdown pages that pkgdown builds into
// articles of core's site.
// Run: npm run docs
import * as path from "node:path";
import { Comment, Converter, DeclarationReflection, OptionDefaults, Reflection, ReflectionKind } from "typedoc";
import type { Application, CommentDisplayPart, Context, RouterTarget, TypeDocOptions } from "typedoc";
import { MarkdownPageEvent, ModuleRouter } from "typedoc-plugin-markdown";
import type { MarkdownApplication, MarkdownThemeContext, PluginOptions } from "typedoc-plugin-markdown";

// The pages of the reference, in the order of its index, each under its title.
// As a page of core's R reference documents a few related functions, a page
// here documents what the entry exports from one module of srcts: one of its
// directories, which the other modules import through its index.ts, or one
// file of srcts/types, whose files they import one by one.
// TypeDoc warns about an export from a module that has no page here.
const pages: Record<string, string> = {
  "types/json.ts": "JSON",
  "types/board.ts": "Board",
  "types/registry.ts": "Registry",
  "types/log.ts": "Log",
  "types/outputs.ts": "Outputs",
  "types/hosts.ts": "Hosts",
  "types/core-api.ts": "Core API",
  store: "Store",
  commands: "Commands",
  actions: "Board actions",
  scheduler: "Scheduler",
  protocol: "Protocol",
  mount: "Mounting",
};

// TypeDoc writes a page for each module, so each page of the reference
// becomes a module that holds the page's declarations.
// They move there only once TypeDoc has resolved the links of comments, which
// it would otherwise look up among the declarations of the comment's page
// alone, and has sorted the declarations.
// So each page lists them in the order of their source, as each declaration
// lists its members, rather than in TypeDoc's groups by kind.
function referencePages(app: Application): void {
  app.converter.on(
    Converter.EVENT_RESOLVE_END,
    (context: Context) => {
      const project = context.project;
      const srcts = path.resolve("srcts");
      const moduleOf = (child: DeclarationReflection): string => {
        const file = path.relative(srcts, child.sources?.[0]?.fullFileName ?? "");
        return file.startsWith("types/") ? file : (file.split("/")[0] ?? "");
      };
      const children = project.children ?? [];
      const moved = new Set<DeclarationReflection>();
      const made: DeclarationReflection[] = [];
      for (const [module, title] of Object.entries(pages)) {
        const members = children.filter((child) => moduleOf(child) === module);
        if (members.length === 0) continue;
        const page = new DeclarationReflection(title, ReflectionKind.Module, project);
        project.registerReflection(page, undefined, undefined);
        // The summary of a page lists its declarations, at the top of the
        // page and beside the page in the index.
        const summary: CommentDisplayPart[] = [];
        for (const [i, member] of members.entries()) {
          member.parent = page;
          page.addChild(member);
          moved.add(member);
          if (i > 0) summary.push({ kind: "text", text: ", " });
          summary.push({ kind: "inline-tag", tag: "@link", text: member.name, target: member });
        }
        page.comment = new Comment(summary);
        made.push(page);
      }
      for (const child of children) {
        if (!moved.has(child)) {
          app.logger.warn(`No page of the reference holds ${child.name}, from srcts/${moduleOf(child)}.`);
        }
      }
      project.children = children.filter((child) => !moved.has(child));
      project.childrenIncludingDocuments = project.childrenIncludingDocuments?.filter(
        (child) => !(child instanceof DeclarationReflection && moved.has(child)),
      );
      // The groups that TypeDoc formed of the declarations go, so that the
      // index lists the pages under no heading.
      delete project.groups;
      delete project.categories;
      for (const page of made) project.addChild(page);
    },
    // After TypeDoc's own steps, the last of which resolves the links.
    -400,
  );
  app.renderer.defineRouter("reference", ReferenceRouter);
  // The index heads its column of pages "Page" rather than "Module", since
  // the pages are no modules that a package imports.
  app.internationalization.addTranslations("en", { kind_module: "Page" });
}

// The module router anchors each declaration of a page by its name, and the
// members of each declaration by theirs, numbering a name that comes again on
// the page, as render() does in BlockModule, OptionModule and OutputModule.
// So the anchor of a member would hang on what comes before it on the page,
// and the type Demand would take #demand-1, after the method demand() of
// Scheduler.
// This router keeps the anchor that the module router first gives a member,
// from its path on the page, as #blockhostsubmit for submit() of BlockHost.
class ReferenceRouter extends ModuleRouter {
  protected override buildAnchors(target: RouterTarget, pageTarget: RouterTarget): void {
    if (pageTarget instanceof Reflection && !this.hasOwnDocument(pageTarget)) return;
    super.buildAnchors(target, pageTarget);
  }
}

// Each page becomes an article, which pkgdown builds only from an .Rmd or
// .qmd file whose YAML header gives the article's title.
// So each page starts with such a header, which holds the title that
// typedoc-plugin-markdown gives the page, and is written as .Rmd.
// The links between the pages keep the extension .md, since pkgdown rewrites
// a relative link to an .md file into a link to the .html file, and does so
// for no other extension.
// The pages are .Rmd rather than .qmd for two reasons:
// 1. For each .qmd file, pkgdown reads the title through a call of
//    `quarto inspect`, which takes a second or more.
// 2. On a fresh build, pkgdown fails to write a Quarto article that sits in a
//    folder below vignettes/articles, as these pages do.
function pkgdownArticles(app: Application): void {
  const renderer = (app as MarkdownApplication).renderer;
  const header = (context: MarkdownThemeContext): string =>
    `---\ntitle: ${JSON.stringify(context.partials.pageTitle())}\n---`;
  renderer.markdownHooks.on("page.begin", header);
  renderer.markdownHooks.on("index.page.begin", header);
  renderer.on(MarkdownPageEvent.BEGIN, (page: MarkdownPageEvent) => {
    page.filename = page.filename.replace(/\.md$/, ".Rmd");
  });
}

const config: Partial<TypeDocOptions & PluginOptions> = {
  // The root entry, which holds the types that the modules of packages are
  // written against.
  entryPoints: ["srcts/index.ts"],
  // The solution file.
  // TypeDoc compiles each project that it references, and takes each entry
  // point from the project that includes it: srcts/index.ts from
  // tsconfig.client.json, and srcts/build from tsconfig.node.json.
  tsconfig: "tsconfig.json",
  name: "@blockr/core",
  // The README is the R package's.
  readme: "none",
  plugin: ["typedoc-plugin-markdown", referencePages, pkgdownArticles],
  // The router that referencePages defines, and the order of the source for
  // the declarations of each page and the members of each declaration.
  router: "reference",
  sort: ["source-order"],
  out: "vignettes/articles/typescript",
  // The index of the reference, articles/typescript/index.html, which the
  // navbar links.
  entryFileName: "index",
  // The navbar of pkgdown and the title of each article take the place of
  // TypeDoc's header, breadcrumbs and title.
  hidePageHeader: true,
  hideBreadcrumbs: true,
  hidePageTitle: true,
  // The title of each article is the name of its page, which pkgdown shows
  // as plain text.
  pageTitleTemplates: {
    index: "{projectName}",
    module: "{rawName}",
  },
  // Each heading carries its anchor, which pandoc takes in place of the one
  // it would derive from the heading.
  useCustomAnchors: true,
  // The index lists each page beside its declarations.
  indexFormat: "table",
  // The properties and parameters of a declaration form a table, as the
  // arguments of an R function do.
  interfacePropertiesFormat: "table",
  classPropertiesFormat: "table",
  typeAliasPropertiesFormat: "table",
  typeDeclarationFormat: "table",
  propertyMembersFormat: "table",
  parametersFormat: "table",
  enumMembersFormat: "table",
  // The annotations from which the build writes the JSON Schemas, which the
  // pages show beside TypeDoc's own tags.
  blockTags: [...OptionDefaults.blockTags, "@asType", "@minimum", "@pattern"],
  // The types that the exports of the entry points refer to, but which they do
  // not export themselves, so that the pages name them without a link.
  // TypeDoc warns about any other such type, and about a name here that it no
  // longer finds.
  intentionallyNotExported: [
    "BlockClassRequest",
    "BlockMod",
    "BoardOptionRequest",
    "HelloReply",
    "Inputs",
    "OnScreen",
    "OutputClassRequest",
    "OutputRequest",
    "PluginRequest",
    "RendererRequest",
    "Reply",
    "RequestOptions",
    "Values",
  ],
  treatWarningsAsErrors: true,
};

export default config;

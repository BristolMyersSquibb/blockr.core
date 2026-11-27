// TypeDoc writes the reference of core's TypeScript into
// vignettes/articles/typescript, as Markdown pages that pkgdown builds into
// articles of core's site.
// Run: npm run docs
import { Comment, Converter, DeclarationReflection, Reflection, ReflectionCategory, ReflectionKind } from "typedoc";
import type {
  Application,
  CommentDisplayPart,
  Context,
  ProjectReflection,
  RouterTarget,
  TypeDocOptions,
} from "typedoc";
import { MarkdownPageEvent, ModuleRouter } from "typedoc-plugin-markdown";
import type { MarkdownApplication, MarkdownThemeContext, PluginOptions } from "typedoc-plugin-markdown";

// A page of the reference documents related declarations, as a page of core's
// R reference documents related functions.
// It lists them in the order given, under the heading of each of its sections
// where it has sections.
interface Page {
  title: string;
  description: string;
  members: readonly string[] | Readonly<Record<string, readonly string[]>>;
}

// The pages of the reference, under the sections of its index, as the
// reference of _pkgdown.yml lists the topics of the R reference.
// TypeDoc warns about an export that no page holds, and about a name here
// that the entry does not export.
const reference: Record<string, readonly Page[]> = {
  "The board": [
    {
      title: "JSON",
      description: "The JSON values that the client and R exchange, and the JSON Schemas that describe some of them.",
      members: ["Json", "JsonSchema"],
    },
    {
      title: "Board",
      description: "The board that the client holds, as plain data, and the ids by which it names its objects.",
      members: {
        Objects: ["Board", "Block", "Link", "Stack", "Lock"],
        Ids: ["BoardObject", "BlockId", "LinkId", "StackId", "OptionId", "PluginClassId", "RegistryId"],
      },
    },
    {
      title: "Store",
      description: "The store, which holds the board, and the deltas in which every change to the board passes through it.",
      members: ["Store", "Change", "Origin", "Delta", "Patch", "StoreError"],
    },
    {
      title: "Saving and opening",
      description:
        "A board as the client saves and opens it: the JSON board, what opening it migrates and drops, " +
        "and the storage plugin that keeps it.",
      members: {
        "JSON boards": ["JsonBoard", "JsonBlock", "JsonOption", "JsonPlugin"],
        Opening: ["Migration", "Dropped"],
        "Storage plugins": ["Storage", "StorageControl"],
      },
    },
    {
      title: "Commands",
      description: "The command registry, through which every edit of the board runs, whoever makes it.",
      members: ["Commands", "Command", "CommandContext", "CommandError"],
    },
    {
      title: "Board actions",
      description:
        "What a user triggers from a menu, a button or a gesture, which changes the board through commands, " +
        "and the menus that offer it.",
      members: {
        Actions: ["BoardAction", "Target", "CommandCall", "Role"],
        "Quick actions": ["QuickLevel", "QuickRow"],
        Menus: ["MenuModule", "OpenMenu", "ShownLevel", "ShownRow"],
        "Advanced actions": ["AdvancedModule"],
      },
    },
    {
      title: "Evaluation",
      description: "The scheduler, which has R evaluate what is demanded, and the status that it keeps of each block.",
      members: ["Scheduler", "Demand", "BlockStatus", "Status", "KeptWaiting", "Condition", "Phase"],
    },
  ],
  Modules: [
    {
      title: "Blocks",
      description:
        "What the UI module of a block class exports, the host that core gives it, " +
        "and the block registry, which describes each block class.",
      members: {
        "UI modules": ["BlockModule", "Rendered", "BlockHost", "State", "BlockContext"],
        "Block registry": ["BlockRegistry", "RegistryEntry"],
      },
    },
    {
      title: "Outputs",
      description:
        "The outputs of core's output classes, what a renderer of them exports and gets, " +
        "and the fixtures that renderers are tested on.",
      members: {
        "Output classes": [
          "BlockOutput",
          "OutputRenderer",
          "TableOutput",
          "TableColumn",
          "TableParams",
          "TextOutput",
          "PlotOutput",
        ],
        Renderers: ["OutputModule", "OutputHost", "OutputInfo", "OutputEvents"],
        Fixtures: ["OutputFixtures", "OutputFixture"],
      },
    },
    {
      title: "Board options",
      description:
        "Board options, which packages register in the client, and what the UI module of a board option exports and gets.",
      members: ["Option", "Options", "OptionModule", "OptionHost"],
    },
    {
      title: "Plugins",
      description:
        "Plugin classes, which packages register in the client, and what the modules of their plugins and frontends export.",
      members: ["PluginClass", "Plugins", "Plugin", "PluginModule", "FrontendModule"],
    },
    {
      title: "Core API",
      description: "What core gives a frontend and a plugin, through which they work on the board.",
      members: ["CoreApi", "ModuleError"],
    },
    {
      title: "Requests",
      description: "What a module asks of R beside an evaluation: the options of a request, and why a request rejects.",
      members: ["RequestOpts", "HostRequestOpts", "OutputRequestOpts", "RequestError"],
    },
  ],
  "Notifications and logs": [
    {
      title: "Notifications",
      description: "The messages, warnings and errors that the user should see, and who raised each.",
      members: {
        Notifications: ["Notification", "Raised", "Severity"],
        Sources: [
          "NotificationSource",
          "RSource",
          "RRequest",
          "EvaluatePhase",
          "CoreSource",
          "CoreCode",
          "CoreFailure",
          "ModuleSource",
          "SourceModule",
        ],
      },
    },
    {
      title: "Log",
      description: "The log through which the client's code writes log lines.",
      members: ["Log", "LogLine", "LogLevel"],
    },
  ],
};

// TypeDoc writes a page for each module, so each page of the reference
// becomes a module that holds the page's declarations, and each section of a
// page or of the index a category.
// The declarations move only once TypeDoc has resolved the links of comments,
// which it would otherwise look up among the declarations of the comment's
// page alone.
function referencePages(app: Application): void {
  app.converter.on(
    Converter.EVENT_RESOLVE_END,
    (context: Context) => {
      const project = context.project;
      const children = project.children ?? [];
      const exported = new Map(children.map((child) => [child.name, child]));
      const placed = new Set<DeclarationReflection>();
      const place = (name: string, page: DeclarationReflection): DeclarationReflection[] => {
        const member = exported.get(name);
        if (!member) {
          app.logger.warn(`The page ${page.name} of the reference names ${name}, which the entry does not export.`);
          return [];
        }
        member.parent = page;
        page.addChild(member);
        placed.add(member);
        return [member];
      };
      const sections = Object.entries(reference).map(([title, pages]) => {
        const section = new ReflectionCategory(title);
        for (const { title, description, members } of pages) {
          const page = new DeclarationReflection(title, ReflectionKind.Module, project);
          project.registerReflection(page, undefined, undefined);
          page.comment = new Comment([{ kind: "text", text: description }]);
          if (isList(members)) {
            for (const name of members) place(name, page);
          } else {
            page.categories = Object.entries(members).map(([heading, names]) => {
              const category = new ReflectionCategory(heading);
              category.children = names.flatMap((name) => place(name, page));
              return category;
            });
          }
          section.children.push(page);
        }
        return section;
      });
      for (const child of children) {
        if (!placed.has(child)) app.logger.warn(`No page of the reference holds ${child.name}.`);
      }
      project.children = children.filter((child) => !placed.has(child));
      project.childrenIncludingDocuments = project.childrenIncludingDocuments?.filter(
        (child) => !(child instanceof DeclarationReflection && placed.has(child)),
      );
      // The index lists the pages under the headings of its sections, in
      // place of the groups by kind that TypeDoc formed of the declarations.
      delete project.groups;
      project.categories = sections;
      for (const section of sections) {
        for (const page of section.children) project.addChild(page);
      }
      linkNames(project, exported);
    },
    // After TypeDoc's own steps, the last of which resolves the links.
    -400,
  );
  app.renderer.defineRouter("reference", ReferenceRouter);
  // The index heads its column of pages "Page" rather than "Module", since
  // the pages are no modules that a package imports.
  app.internationalization.addTranslations("en", { kind_module: "Page" });
}

function isList(members: Page["members"]): members is readonly string[] {
  return Array.isArray(members);
}

// A comment links each declaration that it names, as downlit links each R
// function that an article names in code:
// - A name in code, as `CoreApi`, `CoreApi.log()` or `Store`, names a
//   declaration of the reference or its member.
// - A name in prose with two capitals or more, as CoreApi, names a
//   declaration, while one with a single capital, as Board or Store, may be a
//   word of the sentence and links nothing.
// A member's name alone, as `options`, links nothing, since it may name the
// member of another object, as of R's reply.
// A comment links nothing that it documents itself.
function linkNames(project: ProjectReflection, exported: ReadonlyMap<string, DeclarationReflection>): void {
  const within = (owner: Reflection, target: Reflection): boolean => {
    for (let reflection: Reflection | undefined = owner; reflection; reflection = reflection.parent) {
      if (reflection === target) return true;
    }
    return false;
  };
  const named = (name: string): Reflection | undefined => {
    const [first = "", ...rest] = name.split(".");
    const declaration = exported.get(first);
    return rest.length > 0 ? declaration?.getChildByName(rest) : declaration;
  };
  const link = (parts: readonly CommentDisplayPart[], owner: Reflection): CommentDisplayPart[] =>
    parts.flatMap((part): CommentDisplayPart[] => {
      if (part.kind === "code") {
        const name = /^`([A-Za-z_$][\w$]*(?:\.[A-Za-z_$][\w$]*)*)(?:\(\))?`$/.exec(part.text)?.[1];
        const target = name === undefined ? undefined : named(name);
        if (!target || within(owner, target)) return [part];
        return [{ kind: "inline-tag", tag: "@linkcode", text: part.text.slice(1, -1), target }];
      }
      if (part.kind !== "text") return [part];
      const linked: CommentDisplayPart[] = [];
      let last = 0;
      for (const match of part.text.matchAll(/https?:\/\/\S+|\b[A-Z][A-Za-z0-9]*[A-Z][A-Za-z0-9]*\b/g)) {
        const target = exported.get(match[0]);
        if (!target || within(owner, target)) continue;
        linked.push({ kind: "text", text: part.text.slice(last, match.index) });
        linked.push({ kind: "inline-tag", tag: "@link", text: match[0], target });
        last = match.index + match[0].length;
      }
      linked.push({ kind: "text", text: part.text.slice(last) });
      return linked;
    });
  for (const owner of Object.values(project.reflections)) {
    if (!owner.comment) continue;
    owner.comment.summary = link(owner.comment.summary, owner);
    for (const tag of owner.comment.blockTags) tag.content = link(tag.content, owner);
  }
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
  // Its header introduces the index of the reference, as its tag
  // @packageDocumentation has it.
  entryPoints: ["srcts/index.ts"],
  // The solution file.
  // TypeDoc compiles each project that it references, and takes each entry
  // point from the project that includes it: srcts/index.ts from
  // tsconfig.client.json, and srcts/build from tsconfig.node.json.
  tsconfig: "tsconfig.json",
  name: "@blockr/core",
  // The README is the R package's.
  readme: "none",
  // Core's TypeScript documents itself in comments of lines, and keeps blocks
  // for the annotations from which the build writes JSON Schemas.
  commentStyle: "line",
  plugin: ["typedoc-plugin-markdown", referencePages, pkgdownArticles],
  // The router that referencePages defines, and the order of the source for
  // the members of each declaration.
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
  // The index takes its title from the navbar's link to it.
  pageTitleTemplates: {
    index: "TypeScript reference",
    module: "{rawName}",
  },
  // Each heading carries its anchor, which pandoc takes in place of the one
  // it would derive from the heading.
  useCustomAnchors: true,
  // The index lists the pages of each of its sections in a table, beside
  // their descriptions.
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

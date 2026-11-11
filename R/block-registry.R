block_registry <- new.env()

#' Block registry
#'
#' Listing of blocks is available via a block registry, which holds each block
#' class under its registry id, with the metadata registration gives it, in
#' order to provide a browsable block directory and so that R evaluates a
#' block, and resolves its class's requests, without building a block. A block
#' class is registered by its constructor, made by [block_constructor()], from
#' which registration reads the class, and the registry keeps one entry per
#' class.
#'
#' A block class's registry id comes from the class alone.
#' It is `{pkg}-{id}`, such as `blockr.core-head_block`,
#' where `{pkg}` is the package whose code defines the class
#' and `{id}` is the class's id (see [new_block_class()]).
#' A class that an app script defines records no package,
#' and its registry id is its id alone.
#' So renaming a constructor leaves the registry id as it is,
#' while changing a class's id changes it.
#' The `registry_id_from_block()` function reads the registry id from a
#' block's class, whether or not the block registry holds the class.
#'
#' Which block classes an app offers is the app's choice, and loading a package
#' registers none.
#' Each method of [serve()] takes the packages that the app offers in
#' `packages`, core always among them.
#' As the app starts, R loads each, reads the block classes that its
#' `inst/registry/blocks.yml` lists, which the [block_registration_roclet()]
#' writes, and fills the block registry with them.
#' Core's own are narrowed by the blockr option `core_blocks`, which names the
#' classes to offer, as `"head_block"`, or is `"all"`.
#' A package that lists two constructors whose classes give one registry id
#' stops the app.
#' An app script registers the block classes that it defines itself, with
#' `register_block()` or `register_blocks()`.
#'
#' An entry holds, beside the metadata, the block class, its constructor, its
#' data inputs, its UI module and the JSON Schema of the block's state that the
#' module ships beside it, as `<name>.schema.json` for a module `<name>.js`,
#' decoded with an array of length one marked with [I()], so that R writes it
#' back as the module ships it, and whose properties must be the state args
#' where it ships one, a description of each request, with the inputs and
#' state args it names, the ids of the board options it reads, named by the
#' arguments that take their values, and whether it is volatile or an action,
#' and the state a new block starts with, from `expr` called without args. The
#' arg specs default to every state arg, and their examples are checked by
#' calling `expr` with them.
#'
#' R sends the client what it needs of each entry, in its reply to `hello`,
#' without building a block: the metadata, with `link` as `url`, the data
#' inputs, where R serves the UI module, the schema, the state a new block
#' starts with, the arg specs and the requests. R serves the
#' module, and the stylesheet beside it, at a URL that holds a hash of the
#' file's content, so that no browser runs a changed file from its cache. It
#' writes the starting state by the schema of the block's state, and the
#' example of each arg spec by the type it gives. So registration refuses a
#' class whose starting state is not a plain value, as a block's state has to
#' be (see [blockr_ser()]), and refuses an arg spec whose example is not one
#' either.
#'
#' Blocks can be registered (i.e. added to the registry) via `register_block()`
#' with scalar-valued arguments and `register_blocks()`, where arguments may be
#' vector-valued, while de-registration (or removal) is handled via
#' `unregister_blocks()`. A listing of all available blocks can be created as
#' `list_blocks()`, which will return registry IDs and `available_blocks()`,
#' which provides a set of (named) `block_registry_entry` objects. Finally,
#' block construction via a registry ID is available as `create_block()`.
#'
#' @param ctor Block constructor, made by [block_constructor()]
#' @param name,description Metadata describing the block
#' @param category Useful to sort blocks by topics. If not specified,
#'   blocks are uncategorized.
#' @param icon Icon
#' @param arguments Block argument specification, either a [new_arg_specs()]
#'   object or a (possibly empty) named character vector of argument
#'   descriptions
#' @param details Optional longer human-facing description (e.g. for a help
#'   popover), complementing the short `description`
#' @param link Optional URL to a block's help or documentation page
#' @param guidance Optional model-facing construction guidance (do/don't rules,
#'   enums, pitfalls), distinct from the human-facing `details`
#' @param examples Optional list of complete worked configurations (each a
#'   named list keyed by argument name); supersedes the per-argument example
#'   assembly
#' @param keywords Optional character vector of free-text search terms for block
#'   discovery
#' @param overwrite Overwrite existing entry
#'
#' @examples
#' new_identity_block <- block_constructor(
#'   new_block_class(
#'     "identity_block",
#'     inputs = "data",
#'     expr = function() bbquote(.(data)),
#'     ui = js_module("identity-block.js")
#'   )
#' )
#'
#' blks <- list_blocks()
#' register_block(new_identity_block, "Identity", "Returns its input")
#'
#' new <- setdiff(list_blocks(), blks)
#' new
#'
#' registry_id_from_block(new_identity_block())
#'
#' unregister_blocks(new)
#' setequal(list_blocks(), blks)
#'
#' @return `register_block()` and `register_blocks()` are invoked for their side
#' effects and return `block_registry_entry` object(s) invisibly, while
#' `unregister_blocks()` returns `NULL` (invisibly).
#' Listing via `list_blocks()` returns a character vector and a list of
#' `block_registry_entry` object(s) for `available_blocks()`. Block
#' construction via `create_block()` returns a newly instantiated `block`
#' object, and the lookup `registry_id_from_block()` returns the registry id
#' that a block's class gives, a string. A block's argument specification is
#' built and read with [new_arg_spec()] and friends.
#'
#' @export
register_block <- function(ctor, name, description, category = NULL,
                           icon = NULL, arguments = NULL, details = NULL,
                           link = NULL, guidance = NULL, examples = list(),
                           keywords = NULL, overwrite = FALSE) {

  if (is.null(category)) {
    category <- default_category()
  }

  if (!is_string(category)) {
    blockr_abort(
      "A block category is expected to be a string.",
      class = "block_category_invalid"
    )
  }

  if (!category %in% names(suggested_categories())) {
    blockr_warn(
      "Block category {category} is not among suggested categories ",
      "{names(suggested_categories())}. Consider choosing a different one.",
      class = "block_category_discouraged",
      frequency = "once",
      frequency_id = paste0("block_category_", category, "_discouraged")
    )
  }

  if (is.null(icon)) {

    icon <- default_icon(category)

  } else {

    if (!is_string(icon)) {
      blockr_abort(
        "A block icon is expected to be the name of a Bootstrap icon, as a ",
        "string.",
        class = "block_icon_invalid"
      )
    }

    if (grepl("-fill$", icon)) {
      blockr_warn(
        "Using block icons with 'fill' style, such as {icon} is discouraged.",
        class = "block_icon_fill_discouraged",
        frequency = "once",
        frequency_id = paste0("block_icon_", icon, "_discouraged")
      )
    }

    if (!icon %in% bsicon_icons()) {
      blockr_abort("Unknown icon {icon}.", class = "block_icon_invalid")
    }

    icon <- as.character(bsicons::bs_icon(icon))
  }

  cls <- constructor_class(ctor)
  id <- class_registry_id(cls)

  if (is.null(arguments)) {
    arguments <- default_arg_specs(class_state_args(cls))
  }

  user_spec <- is_arg_specs(arguments)

  norm <- normalize_arguments(arguments, guidance)
  arguments <- norm[["arguments"]]
  guidance <- norm[["guidance"]]

  if (user_spec || length(examples) > 0L) {
    validate_block_spec(arguments, examples, cls, check_names = user_spec)
  }

  if (id %in% list_blocks() && !isTRUE(overwrite)) {
    blockr_abort(
      "Block {id} already exists. Set `overwrite = TRUE` to replace it, ",
      "or remove it first with `unregister_blocks(\"{id}\")`.",
      class = "block_already_in_registry"
    )
  }

  state <- expr_state(cls, list())

  check_sent_values(cls, state, arguments)

  entry <- new_registry_entry(
    id = id,
    name = name,
    description = description,
    details = details,
    link = link,
    guidance = guidance,
    keywords = keywords,
    category = category,
    icon = icon,
    arguments = arguments,
    examples = examples,
    package = cls[["package"]],
    constructor = ctor,
    block_class = cls,
    inputs = class_inputs(cls),
    ui = cls[["ui"]],
    schema = class_schema(cls),
    requests = lapply(cls[["requests"]], request_entry, cls),
    state = state
  )

  assign(id, entry, envir = block_registry)

  invisible(entry)
}

# R sends the client the state a new block of a class starts with and the
# example of each of its arg specs, which a block's state may hold, so each has
# to be a plain value, as the JSON of a board holds it.
check_sent_values <- function(cls, state, arguments) {

  id <- cls[["id"]]

  plain_value(
    state,
    paste("The state a new block of class", id, "starts with")
  )

  for (arg in names(arguments)) {
    plain_value(
      arg_spec_example(arguments[[arg]]),
      paste("The example of arg", arg, "of block class", id)
    )
  }

  invisible()
}

# A block class's registry id, `{pkg}-{id}`, from the package that the class
# records and the class's id, or the id alone for a class that records no
# package, as one that an app script defines.
class_registry_id <- function(cls) {

  id <- cls[["id"]]
  package <- cls[["package"]]

  if (is.null(package)) {
    return(id)
  }

  paste0(package, "-", id)
}

class_schema <- function(cls) {

  path <- module_file(cls[["ui"]], ".schema.json")

  if (!nzchar(path)) {
    return(NULL)
  }

  schema <- decode_schema(path)
  props <- names(schema[["properties"]])
  args <- class_state_args(cls)

  if (!setequal(coal(props, character(), fail_all = FALSE), args)) {
    blockr_abort(
      "The schema of block class {cls[['class']][1L]} has properties ",
      "{props}, where the class's state args are {args}.",
      class = "block_schema_invalid"
    )
  }

  schema
}

#' @rdname register_block
#' @export
default_icon <- function(category = default_category()) {

  if (!is_string(category)) {
    blockr_abort(
      "A block category is expected to be a string.",
      class = "block_category_invalid"
    )
  }

  res <- switch(
    category,
    input = "upload",
    transform = "magic",
    structured = "asterisk",
    plot = "file-bar-graph",
    table = "table",
    model = "gear",
    output = "save",
    utility = "tools",
    "question-square"
  )

  as.character(bsicons::bs_icon(res))
}

#' @rdname register_block
#' @export
default_category <- function() "uncategorized"

bsicon_icons <- function() {
  get("icon_info", envir = asNamespace("bsicons"), mode = "list")$name
}

#' @rdname register_block
#' @export
suggested_categories <- function() {
  c(
    input = paste(
      "Data loading and import blocks, e.g. dataset selection, file uploads,",
      "API connections, database queries."
    ),
    transform = paste(
      "Data manipulation and transformation (incl. joins, binds), e.g. filter,",
      "select, mutate, arrange, summarize, join, bind_rows."
    ),
    structured = paste(
      "Processing of specialized structured data formats such as time series",
      "or spatial data, hierarchical structures and graph/network data"
    ),
    plot = paste(
      "Visualization and plotting blocks, e.g. ggplotz visualizations, scatter",
      "plots, bar charts, histograms."
    ),
    table = paste(
      "Tabular output and display blocks, e.g. data tables, formatted tables,",
      "interactive tables, table viewers."
    ),
    model = paste(
      "Statistical modeling and Al/ML blocks, e.g. linear models, GLMs,",
      "neural networks, LLMs, predictions, classifiers."
    ),
    output = paste(
      "Data export and output blocks, e.g. save to file, export tables,",
      "generate reports."
    ),
    utility = paste(
      "Utility and tool blocks, e.g. parse, format, text manipulation,",
      "helpers."
    ),
    uncategorized = "Blocks without assigned category."
  )
}

new_registry_entry <- function(...) {
  structure(list(...), class = "block_registry_entry")
}

is_registry_entry <- function(x) inherits(x, "block_registry_entry")

#' @export
format.block_registry_entry <- function(x, ...) {
  c(
    paste0("<block_registry_entry> ", x[["id"]]),
    paste0("Name: \"", x[["name"]], "\""),
    paste0("Description: \"", x[["description"]], "\""),
    paste0("Category: ", x[["category"]]),
    paste0("Block class: ", x[["block_class"]][["id"]])
  )
}

#' @export
print.block_registry_entry <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
board_options.block_registry_entry <- function(x, ...) {
  class_board_options(x[["block_class"]])
}

#' @rdname register_block
#' @export
list_blocks <- function() {
  ls(envir = block_registry)
}

#' @param block A block or a block class
#' @rdname register_block
#' @export
registry_id_from_block <- function(block) {

  cls <- if (is_block_class(block)) block else block_class(block)

  if (!is_block_class(cls)) {
    blockr_abort(
      "Expecting a block or a block class.",
      class = "block_class_invalid"
    )
  }

  class_registry_id(cls)
}

#' @param uid Registry ids of the entries to remove
#' @rdname register_block
#' @export
unregister_blocks <- function(uid = list_blocks()) {
  rm(list = uid, envir = block_registry, inherits = FALSE)
  invisible()
}

#' @param ... For `register_blocks()`, arguments forwarded to
#'   `register_block()`; for `new_arg_specs()`, named `new_arg_spec()` objects
#'   (or strings), one per constructor formal
#' @rdname register_block
#' @export
register_blocks <- function(...) {
  arg_processor <- function(ctor, ...) {
    wrap_list <- function(x) {
      if (length(x) > 1L) list(x) else x
    }

    if (length(ctor) > 1L) {
      return(c(list(ctor), list(...)))
    }

    c(list(ctor), lapply(list(...), wrap_list))
  }

  invisible(
    do.call(Map, c(register_block, arg_processor(...)))
  )
}

get_registry_entry <- function(uid) {

  res <- if (is_string(uid)) get0(uid, envir = block_registry, inherits = FALSE)

  if (!is_registry_entry(res)) {
    blockr_abort(
      "No block class is registered under the registry id {uid}.",
      class = "registry_id_unknown"
    )
  }

  res
}

#' @rdname register_block
#' @export
available_blocks <- function() {
  lapply(set_names(nm = list_blocks()), get_registry_entry)
}

#' @param id Block ID as reported by `list_blocks()`
#' @rdname register_block
#' @export
create_block <- function(id, ...) {
  get_registry_entry(id)[["constructor"]](...)
}

# Fills the block registry from the packages that an app offers, as `serve()`
# starts the app, and gives the packages, core first among them.
# R loads each package, and registers the block classes that it lists in its
# `inst/registry/blocks.yml`, those of core narrowed by the blockr option
# `core_blocks`.
# A package that is not installed stops the app before R loads any.
fill_block_registry <- function(packages = character()) {

  if (!is.character(packages) || anyNA(packages) || !all(nzchar(packages))) {
    blockr_abort(
      "The packages that an app offers are expected to be a character ",
      "vector of package names, as in `packages = \"blockr.dplyr\"`.",
      class = "serve_packages_invalid"
    )
  }

  packages <- unique(c("blockr.core", packages))
  missing <- packages[!lgl_ply(packages, is_installed_pkg)]

  if (length(missing)) {
    blockr_abort(
      "{qty(missing)}Package{?s} {missing}, which the app offers in ",
      "`packages`, {?is/are} not installed.",
      class = "serve_packages_missing"
    )
  }

  for (pkg in packages) {
    loadNamespace(pkg)
    register_yaml_blocks(
      pkg,
      if (identical(pkg, "blockr.core")) offered_core_blocks() else "all"
    )
  }

  packages
}

# Whether R finds the package, installed or loaded, as `pkgload::load_all()`
# loads one.
is_installed_pkg <- function(pkg) {
  length(find.package(pkg, quiet = TRUE)) > 0L
}

# The constructors of core's block classes that an app offers, by the blockr
# option `core_blocks`, which names the classes, as `"head_block"`, or is
# `"all"`.
offered_core_blocks <- function(which = blockr_option("core_blocks", "all")) {

  if (identical(which, "all")) {
    return(which)
  }

  paste0("new_", which)
}

# Registers the block classes that a package lists in its
# `inst/registry/blocks.yml`, which the roclet writes from its `@block` tags,
# or those of them whose constructors `which` names.
# A package that lists none registers none.
register_yaml_blocks <- function(package, which = "all") {

  file <- package_file("registry/blocks.yml", package)

  if (!nzchar(file)) {
    return(invisible())
  }

  entries <- yaml::read_yaml(file)

  if (identical(which, "all")) {

    which <- names(entries)

  } else {

    unknown <- setdiff(which, names(entries))

    if (length(unknown)) {
      blockr_abort(
        "Package {package} declares no {qty(unknown)}block constructor{?s} ",
        "{unknown}.",
        class = "package_blocks_invalid"
      )
    }

    dups <- unique(which[duplicated(which)])

    if (length(dups)) {
      blockr_abort(
        "Expecting `which` to name each block constructor once, but {dups} ",
        "{?is/are} named more than once.",
        class = "package_blocks_invalid"
      )
    }
  }

  entries <- entries[which]

  if (!length(entries)) {
    return(invisible())
  }

  ctors <- lapply(
    set_names(nm = names(entries)),
    get,
    envir = asNamespace(package),
    mode = "function"
  )

  check_package_registry_ids(ctors, package)

  register_blocks(
    ctors,
    name = chr_xtr(entries, "name"),
    description = chr_xtr(entries, "description"),
    details = lapply(entries, `[[`, "details"),
    link = lapply(entries, `[[`, "link"),
    category = chr_xtr(entries, "category"),
    icon = lapply(entries, `[[`, "icon"),
    guidance = lapply(entries, `[[`, "guidance"),
    keywords = lapply(entries, `[[`, "keywords"),
    arguments = lapply(entries, yaml_block_arguments),
    examples = lapply(entries, yaml_block_examples),
    overwrite = TRUE
  )
}

# Registering the block classes of a package refuses two constructors whose
# classes give one registry id, since the second would replace the first.
check_package_registry_ids <- function(ctors, package) {

  ids <- chr_ply(lapply(ctors, constructor_class), class_registry_id)
  id <- ids[duplicated(ids)][1L]
  same <- names(ctors)[ids %in% id]

  if (length(same)) {
    blockr_abort(
      "Package {package} declares block constructors {same}, whose classes ",
      "give one registry id, {id}.",
      class = "package_blocks_invalid"
    )
  }

  invisible()
}

yaml_block_examples <- function(entry) {

  types <- lapply(entry[["arguments"]], `[[`, "type")

  lapply(
    coal(entry[["examples"]], list(), fail_all = FALSE),
    function(example) {
      set_names(
        map(yaml_arg_example, example, types[names(example)]),
        names(example)
      )
    }
  )
}

yaml_block_arguments <- function(entry) {

  args <- entry[["arguments"]]

  if (!length(args)) {
    return(NULL)
  }

  do.call(
    new_arg_specs,
    lapply(
      args,
      function(a) {
        new_arg_spec(
          a[["description"]],
          yaml_arg_example(a[["example"]], a[["type"]]),
          a[["type"]]
        )
      }
    )
  )
}

# YAML reads an array of single values back as an atomic vector, where the R
# value of an arg spec's array type is a list
yaml_arg_example <- function(example, type) {

  if (identical(type[["type"]], "array") && is.atomic(example) &&
        !is.null(example)) {
    return(as.list(example))
  }

  example
}

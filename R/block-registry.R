block_registry <- new.env()

#' Block registry
#'
#' Listing of blocks is available via a block registry, which holds each block
#' class under its registry id, with the metadata registration gives it, in
#' order to provide a browsable block directory and so that R evaluates a
#' block, and resolves its class's requests, without building a block. A block
#' class is registered by its constructor, made by [block_constructor()], from
#' which registration reads the class, and the registry keeps one entry per
#' class, which `registry_id_from_block()` finds by the class a block refers
#' to.
#'
#' A registry id is `{pkg}-{name}`, such as `blockr.core-head_block`, for a
#' constructor registered by name, which comes with its package, where the
#' name defaults to the constructor's name without `new_` and `uid` sets it,
#' for a package that renames a constructor and wants its saved boards to keep
#' working. A constructor registered as a function is from no package, and its
#' id is its name alone, which defaults to the name of its class, and which may
#' not hold a `-`, so that it cannot equal a package's.
#'
#' An entry holds, beside the metadata, the block class, its constructor, its
#' data inputs, its UI module and the JSON Schema of the block's state that the
#' module ships beside it, as `<name>.schema.json` for a module `<name>.js`,
#' whose properties must be the state args where it ships one, a description
#' of each request, with the inputs, state args and board options it names and
#' whether it is volatile or an action, the ids of the class's board options
#' and the state a new block starts with, from `expr` called without args. The
#' arg specs default to every state arg, and their examples are checked by
#' calling `expr` with them.
#'
#' Blocks can be registered (i.e. added to the registry) via `register_block()`
#' with scalar-valued arguments and `register_blocks()`, where arguments may be
#' vector-valued, while de-registration (or removal) is handled via
#' `unregister_blocks()`. A listing of all available blocks can be created as
#' `list_blocks()`, which will return registry IDs and `available_blocks()`,
#' which provides a set of (named) `block_registry_entry` objects. Finally,
#' block construction via a registry ID is available as `create_block()`.
#'
#' @param ctor Block constructor, made by [block_constructor()], or its name
#' @param name,description Metadata describing the block
#' @param uid The name part of the registry id
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
#' @param package Package where constructor is defined (or `NULL`)
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
#' `unregister_blocks()` returns `NULL` (invisibly). `register_package_blocks()`
#' registers the blocks a package declares in its YAML registry (written by the
#' [block_registration_roclet()]), likewise returning their entries invisibly.
#' Listing via `list_blocks()` returns a character vector and a list of
#' `block_registry_entry` object(s) for `available_blocks()`. Block
#' construction via `create_block()` returns a newly instantiated `block`
#' object, and the lookup `registry_id_from_block()` returns the registry id of
#' a block's class, or an empty character vector for a class that is not
#' registered. A block's argument specification is built and
#' read with [new_arg_spec()] and friends.
#'
#' @export
register_block <- function(ctor, name, description, uid = NULL,
                           category = NULL, icon = NULL, arguments = NULL,
                           details = NULL, link = NULL, guidance = NULL,
                           examples = list(), keywords = NULL,
                           package = NULL, overwrite = FALSE) {

  if (is.null(category)) {
    category <- default_category()
  }

  stopifnot(is_string(category))

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

    stopifnot(is_string(icon))

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

  if (is_string(ctor)) {
    stopifnot(is_string(package))
    uid <- coal(uid, sub("^new_", "", ctor), fail_all = FALSE)
    ctor <- get(ctor, asNamespace(package), mode = "function")
  } else {
    stopifnot(is.function(ctor), is.null(package))
  }

  cls <- constructor_class(ctor)
  id <- registry_id(coal(uid, cls[["class"]][1L], fail_all = FALSE), package)

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

  other <- setdiff(registry_id_from_block(cls), id)

  if (length(other)) {
    blockr_abort(
      "Block class {cls[['class']][1L]} is registered already, as {other}.",
      class = "block_class_in_registry"
    )
  }

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
    package = package,
    constructor = ctor,
    block_class = cls,
    inputs = class_inputs(cls),
    ui = cls[["ui"]],
    schema = class_schema(cls),
    requests = lapply(cls[["requests"]], request_entry, cls),
    options = class_option_ids(cls),
    state = expr_state(cls, list())
  )

  assign(id, entry, envir = block_registry)

  invisible(entry)
}

registry_id <- function(name, package) {

  if (!is_string(name) || !nzchar(name)) {
    blockr_abort(
      "The name part of a registry id is expected to be a non-empty string.",
      class = "registry_id_invalid"
    )
  }

  if (not_null(package)) {
    return(paste0(package, "-", name))
  }

  if (grepl("-", name, fixed = TRUE)) {
    blockr_abort(
      "The registry id {name} of a block class from no package may not ",
      "hold a `-`.",
      class = "registry_id_invalid"
    )
  }

  name
}

class_schema <- function(cls) {

  ui <- cls[["ui"]]
  path <- paste0(sub("\\.[^./]*$", "", ui[["path"]]), ".schema.json")

  if (not_null(ui[["package"]])) {
    path <- system.file(path, package = ui[["package"]])
  }

  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }

  schema <- jsonlite::read_json(path)

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

  stopifnot(is_string(category))

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
    paste0("Block class: ", x[["block_class"]][["class"]][1L])
  )
}

#' @export
print.block_registry_entry <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
board_options.block_registry_entry <- function(x, ...) {
  as_board_options(class_board_options(x[["block_class"]]))
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

  if (is.null(cls)) {
    return(character())
  }

  ids <- list_blocks()
  hits <- lgl_ply(lst_xtr(lapply(ids, get_registry_entry), "block_class"),
                  identical, cls)

  ids[hits]
}

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
  res <- get(uid, envir = block_registry, inherits = FALSE)
  stopifnot(is_registry_entry(res))
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

#' @param which Character vector of block constructor names to register (or
#'   `"all"`), keying into the YAML registry generated by the
#'   [block_registration_roclet()]
#'
#' @rdname register_block
#' @export
register_package_blocks <- function(which = "all", package = pkg_name(),
                                    overwrite = TRUE) {

  file <- system.file("registry/blocks.yml", package = package)

  if (!nzchar(file)) {
    return(invisible())
  }

  entries <- yaml::read_yaml(file)

  if (identical(which, "all")) {
    which <- names(entries)
  } else {
    stopifnot(all(which %in% names(entries)), anyDuplicated(which) == 0L)
  }

  entries <- entries[which]

  register_blocks(
    names(entries),
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
    package = package,
    overwrite = overwrite
  )
}

yaml_block_examples <- function(entry) {
  coal(entry[["examples"]], list(), fail_all = FALSE)
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
      function(a) new_arg_spec(a[["description"]], a[["example"]], a[["type"]])
    )
  )
}

register_core_blocks <- function(which = blockr_option("core_blocks", "all")) {

  if (!identical(which, "all")) {
    which <- paste0("new_", which)
  }

  register_package_blocks(which = which)
}

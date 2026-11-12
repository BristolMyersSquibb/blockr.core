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
#' Which block classes an app offers is the app's choice.
#' The methods of [serve()] for a board and for a path take them in `blocks`,
#' which defaults to `available_blocks()`: a data frame with a row for each
#' block class of each available package, with the class's package in
#' `package` and its id in `id`.
#' A block that `serve()` serves alone offers none.
#' A package is available where it is installed or `pkgload::load_all()`
#' loaded it, and it lists block classes in its `inst/registry/blocks.yml`,
#' which the [block_registration_roclet()] writes, or where the block registry
#' of the process holds a class of it (below).
#' Reading the files loads no package.
#' A script narrows the rows with plain R, as
#' `subset(available_blocks(), package != "blockr.ggplot")` offers every class
#' but those of blockr.ggplot, or gives the path of a YAML file that lists
#' packages and, for each, the ids of the classes to offer, all of them where
#' it lists none:
#'
#' ```yaml
#' - blockr.core:
#'   - dataset_block
#'   - head_block
#' - blockr.dplyr
#' ```
#'
#' The file offers what the same rows would, and a package that it leaves out
#' offers nothing.
#' A row whose package is not available, or whose class its package does not
#' list, stops the app, naming the row, and so does a package that lists two
#' constructors whose classes give one registry id.
#' A package whose `inst/registry/blocks.yml` holds anything that R cannot read
#' stops `available_blocks()`, and every app that offers the package's classes,
#' with each fault of each entry (see [block_registration_roclet()]).
#'
#' Each app holds a block registry of its own, which `serve()` builds as it is
#' called, and which the app's sessions read alone.
#' R loads the packages of the rows and registers their classes in it.
#' The block registry of the R process holds the classes that
#' `register_block()` and `register_blocks()` register.
#' A package that writes no `inst/registry/blocks.yml` registers its own
#' classes there as it loads, and an app script may register a package's class
#' there in place of the one that the package lists.
#' Each class of a package that the process registry holds is a row of
#' `available_blocks()`, as a class that a package lists is, and an app offers
#' it where its rows hold it, from the process registry.
#' A class that an app script defines itself records no package, so no row
#' names it, and every app for a board or a path that the script serves after
#' registering it holds it.
#' So a later `serve()` or `register_block()` changes no app that `serve()`
#' built before.
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
#' `unregister_blocks()`, all in the block registry of the process. The
#' registry IDs of the registered classes come from `list_blocks()`, while
#' `available_blocks()` lists the block classes of the available packages,
#' which an app may offer. Finally, block construction via a registry ID is
#' available as `create_block()`.
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
#' Listing via `list_blocks()` returns a character vector, and
#' `available_blocks()` a data frame of `package` and `id` with a row for each
#' class. Block
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

  add_block_entry(
    block_registry, ctor, name, description, category, icon, arguments,
    details, link, guidance, examples, keywords, overwrite
  )
}

# Registers a block class in `registry`, as `register_block()` does in the
# block registry of the process, and `serve()` in an app's.
add_block_entry <- function(registry, ctor, name, description, category = NULL,
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

  if (exists(id, envir = registry, inherits = FALSE) && !isTRUE(overwrite)) {
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

  assign(id, entry, envir = registry)

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

get_registry_entry <- function(uid, registry = block_registry) {

  res <- if (is_string(uid)) get0(uid, envir = registry, inherits = FALSE)

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

  rows <- lapply(available_block_packages(), package_block_rows)

  res <- unique(
    do.call(rbind, c(list(block_rows()), rows, list(registered_block_rows())))
  )
  rownames(res) <- NULL

  res
}

# The block classes of packages that the block registry of the process holds,
# as rows of `available_blocks()`.
registered_block_rows <- function() {

  entries <- Filter(function(x) not_null(x[["package"]]), registry_entries())

  block_rows(
    package = chr_xtr(entries, "package"),
    id = chr_ply(entries, function(x) x[["block_class"]][["id"]])
  )
}

# The entries of the block registry of the process, by registry id, whose
# classes record `package`, or for `NULL`, those of the classes that an app
# script defined itself, which record none.
process_entries <- function(package = NULL) {
  Filter(function(x) identical(x[["package"]], package), registry_entries())
}

# Block classes as `available_blocks()` gives them, one row each, with the
# class's package in `package` and its id in `id`.
block_rows <- function(package = character(), id = character()) {
  data.frame(package = package, id = id)
}

#' @param id Block ID as reported by `list_blocks()`
#' @rdname register_block
#' @export
create_block <- function(id, ...) {
  get_registry_entry(id)[["constructor"]](...)
}

# The entries of a block registry by registry id, the process's by default.
registry_entries <- function(registry = block_registry) {
  lapply(set_names(nm = ls(envir = registry)), get_registry_entry, registry)
}

# The packages whose block classes an app may offer, by name: those that ship
# a `registry/blocks.yml`, core itself among them, whether installed or loaded
# by `pkgload::load_all()`, which puts a package in no library.
# A package whose file lists no block class gives `available_blocks()` no row.
available_block_packages <- function() {

  installed <- unlst(
    lapply(
      .libPaths(),
      function(lib) {
        pkgs <- list.files(lib)
        pkgs[file.exists(file.path(lib, pkgs, "registry", "blocks.yml"))]
      }
    )
  )

  # A package that `pkgload::load_all()` loaded is among the loaded ones,
  # whose files `pkg_file()` finds wherever they are
  loaded <- Filter(
    function(pkg) nzchar(package_file("registry/blocks.yml", pkg)),
    loadedNamespaces()
  )

  sort(unique(c(installed, loaded)))
}

# The block classes that a package lists, as rows of `available_blocks()`.
package_block_rows <- function(package) {

  entries <- package_yaml_blocks(package)

  block_rows(
    package = rep(package, length(entries)),
    id = chr_xtr(entries, "id")
  )
}

# The block classes that a package lists in its `inst/registry/blocks.yml`,
# which the roclet writes from its `@block` tags, by constructor, each with the
# id of its class, or none where it ships no such file.
# A file that R cannot read stops the app (see `check_yaml_blocks()`).
package_yaml_blocks <- function(package) {

  file <- package_file("registry/blocks.yml", package)

  if (!nzchar(file)) {
    return(list())
  }

  entries <- tryCatch(
    yaml::read_yaml(file),
    error = function(e) {
      blockr_abort(
        "The `inst/registry/blocks.yml` of package {package} holds no YAML: ",
        "{conditionMessage(e)}",
        class = c("package_blocks_yaml_invalid", "package_blocks_invalid")
      )
    }
  )

  entries <- coal(entries, list(), fail_all = FALSE)

  check_yaml_blocks(entries, package)

  entries
}

# A package's `inst/registry/blocks.yml` maps each of its block constructors,
# by name, to the registry metadata of the class that the constructor makes, as
# the block registration roclet writes it.
# R reads each entry's `id`, `name`, `description` and `category` as strings,
# so an entry that lacks one, or gives one as anything else, stops the app,
# with each such fault of each entry, while `register_block()` checks the
# entry's other fields as for any class.
check_yaml_blocks <- function(entries, package) {

  if (!is_yaml_mapping(entries)) {
    blockr_abort(
      "The `inst/registry/blocks.yml` of package {package} is expected to ",
      "map each block constructor to the registry metadata of its class, as ",
      "the block registration roclet writes it.",
      class = c("package_blocks_yaml_invalid", "package_blocks_invalid")
    )
  }

  faults <- unlst(Map(yaml_block_faults, entries, names(entries)))

  if (!length(faults)) {
    return(invisible())
  }

  no_id <- lgl_ply(
    entries,
    function(x) is_yaml_mapping(x) && is.null(x[["id"]])
  )

  hint <- if (any(no_id)) {
    paste(
      "",
      "The block registration roclet of this version of blockr.core writes",
      "the `id` of each class, which older versions leave out, so rebuilding",
      "the package's documentation, as `devtools::document()` does, adds it."
    )
  } else {
    ""
  }

  faults <- paste(faults, collapse = " ")

  blockr_abort(
    "The `inst/registry/blocks.yml` of package {package} holds registry ",
    "metadata that R cannot read. {faults} Each entry needs `id`, `name`, ",
    "`description` and `category`, each a string, as the block registration ",
    "roclet writes them.",
    hint,
    class = c("package_blocks_entry_invalid", "package_blocks_invalid")
  )
}

# The faults of the entry for the block constructor `ctor` in a package's
# `inst/registry/blocks.yml`, each as a sentence, where a field given as `null`
# counts as left out.
yaml_block_faults <- function(entry, ctor) {

  fault <- function(...) paste0("Its entry for ", ctor, " ", ..., ".")

  if (!is_yaml_mapping(entry)) {
    return(fault("is not a mapping"))
  }

  fields <- c("id", "name", "description", "category")

  lacks <- fields[lgl_ply(fields, function(x) is.null(entry[[x]]))]
  given <- setdiff(fields, lacks)
  strings <- given[!lgl_ply(entry[given], is_nonempty_string)]

  c(
    if (length(lacks)) {
      fault("lacks ", paste_enum(lacks))
    },
    if (length(strings)) {
      fault(
        "gives ", paste_enum(strings), " as ",
        if (length(strings) > 1L) "values other than" else "a value other than",
        " a non-empty string"
      )
    }
  )
}

# Whether YAML gave `x` as a mapping, a named list.
is_yaml_mapping <- function(x) {
  is.list(x) && (!length(x) || not_null(names(x)))
}

# The block registry of an app, which `serve()` builds as it starts the app,
# and which the app's sessions read alone.
# It holds the block classes that the app offers, as `blocks` gives them, whose
# packages R loads, the classes that the app script defined and registered
# itself, and the classes of the blocks on `board` that it lacks (see
# `register_board_classes()`).
# Building it reads the `blocks.yml` of each package that the app offers once,
# as it registers the package's classes, and no other package's.
app_block_registry <- function(blocks = available_blocks(), board = NULL) {

  offered <- offered_block_classes(blocks)
  registry <- new.env(parent = emptyenv())

  for (pkg in names(offered)) {
    loadNamespace(pkg)
    register_offered_classes(pkg, registry, offered[[pkg]])
  }

  list2env(process_entries(), registry)

  if (not_null(board)) {
    register_board_classes(board, registry)
  }

  registry
}

# The block classes that an app offers, as `serve()` takes them in `blocks`,
# by package: the ids of the package's classes to offer, or `NULL` for all of
# them, from the rows of a data frame (see `frame_class_rows()`) or from a YAML
# file at the path (see `yaml_block_rows()`).
# A package that is not available stops the app, and so does an id that its
# package does not list, as R registers the package's classes.
offered_block_classes <- function(blocks) {

  rows <- if (is_string(blocks)) {
    yaml_block_rows(blocks)
  } else {
    frame_class_rows(
      blocks,
      "block classes that an app offers",
      "available_blocks()",
      "block_classes_invalid",
      yaml = TRUE
    )
  }

  # A package that the rows name twice offers the classes of both rows, and
  # all of its classes where a row's id is `NA`
  offered <- lapply(
    set_names(nm = unique(rows[["package"]])),
    function(pkg) {
      ids <- rows[["id"]][rows[["package"]] == pkg]
      if (!anyNA(ids)) unique(ids)
    }
  )

  check_block_class_packages(names(offered))

  offered
}

# The rows of a YAML file that lists packages, each as its name, for all of its
# block classes, or as its name with the ids of the classes to offer, as the
# example in `?register_block` shows, where a package that the file lists
# without ids gives one row whose id is `NA`.
yaml_block_rows <- function(path) {

  if (is.na(path) || !file.exists(path) || dir.exists(path)) {
    blockr_abort(
      "There is no file {path} that lists the block classes of an app.",
      class = "block_classes_invalid"
    )
  }

  items <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      blockr_abort(
        "The file {path} holds no YAML: {conditionMessage(e)}",
        class = "block_classes_invalid"
      )
    }
  )

  # A list of package names alone reads as a character vector
  items <- as.list(coal(items, list(), fail_all = FALSE))

  listed <- if (is.null(names(items))) {
    lapply(items, yaml_block_item)
  }

  if (!is.list(listed) || !all(lgl_ply(listed, is.list))) {
    blockr_abort(
      "The file {path} is expected to list packages, each as its name or as ",
      "its name with the ids of the block classes to offer.",
      class = "block_classes_invalid"
    )
  }

  do.call(
    rbind,
    c(
      list(block_rows()),
      lapply(
        listed,
        function(x) {
          block_rows(
            package = x[["package"]],
            id = coal(x[["ids"]], NA_character_, fail_all = FALSE)
          )
        }
      )
    )
  )
}

# A package that a YAML file of block classes lists, as its name, or as its
# name with the ids of its classes to offer, or `NULL` for anything else.
yaml_block_item <- function(x) {

  if (is_nonempty_string(x)) {
    return(list(package = x, ids = NULL))
  }

  valid <- is.list(x) && length(x) == 1L && is_nonempty_string(names(x)) &&
    (is.null(x[[1L]]) || (is.character(x[[1L]]) && !anyNA(x[[1L]])))

  if (valid) list(package = names(x), ids = x[[1L]])
}

check_block_class_packages <- function(packages) {

  missing <- setdiff(
    packages,
    c(available_block_packages(), registered_block_rows()[["package"]])
  )

  if (length(missing)) {
    blockr_abort(
      "{qty(missing)}Package{?s} {missing}, whose block classes the app ",
      "offers, {?is/are} not available (see `available_blocks()`).",
      class = "block_classes_unavailable"
    )
  }

  invisible()
}

# Registers in `registry` the block classes of `package` that an app offers,
# those whose ids `ids` gives, or all of them for `NULL`: each from the block
# registry of the process where that holds it, in place of the entry of the
# package's `inst/registry/blocks.yml`, and from the file otherwise.
register_offered_classes <- function(package, registry, ids = NULL) {

  held <- process_entries(package)
  held_ids <- chr_ply(held, function(x) x[["block_class"]][["id"]])

  if (not_null(ids)) {
    held <- held[held_ids %in% ids]
    ids <- setdiff(ids, held_ids)
  }

  register_yaml_blocks(package, registry, ids)
  list2env(held, registry)

  invisible()
}

# Registers the block classes that a package lists in its
# `inst/registry/blocks.yml` in `registry`, or those of them whose ids `ids`
# gives, where an id that the package does not list stops the app.
# A package that lists none registers none.
register_yaml_blocks <- function(package, registry, ids = NULL) {

  entries <- package_yaml_blocks(package)

  if (not_null(ids)) {

    listed <- chr_xtr(entries, "id")
    unknown <- setdiff(ids, listed)

    if (length(unknown)) {
      blockr_abort(
        "The app offers {qty(unknown)}block class{?es} {unknown} of ",
        "{package}, which its package does not list.",
        class = "block_classes_unknown"
      )
    }

    entries <- entries[listed %in% ids]
  }

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

  invisible(
    Map(
      add_block_entry,
      ctor = ctors,
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
      MoreArgs = list(registry = registry, overwrite = TRUE)
    )
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

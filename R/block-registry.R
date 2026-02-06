block_registry <- new.env()

#' Block registry
#'
#' Listing of blocks is available via a block registry, which associates a block
#' constructor with metadata in order to provide a browsable block directory.
#' Every constructor is identified by a unique ID (uid), which by default is
#' generated from the class vector (first element). If the class vector is not
#' provided during registration, an object is instantiated (by calling the
#' constructor with arguments `ctor` and `ctor_pkg` only) to derive this
#' information. Block constructors therefore should be callable without block-
#' specific arguments.
#'
#' Due to current requirements for serialization/deserialization, we keep track
#' the constructor that was used for block instantiation. This works most
#' reliable whenever a block constructor is an exported function from a package
#' as this function is guaranteed to be available in a new session (give the
#' package is installed in an appropriate version). While it is possible to
#' register a block passing a "local" function as `ctor`, this may introduce
#' failure modes that are less obvious (for example when such a constructor
#' calls another function that is only defined within the scope of the session).
#' It is therefore encouraged to only rely on exported function constructors.
#' These can also be passed as strings and together with the value of `package`,
#' the corresponding function can easily be retrieved in any session.
#'
#' Blocks can be registered (i.e. added to the registry) via `register_block()`
#' with scalar-valued arguments and `register_blocks()`, where arguments may be
#' vector-valued, while de-registration (or removal) is handled via
#' `unregister_blocks()`. A listing of all available blocks can be created as
#' `list_blocks()`, which will return registry IDs and `available_blocks()`,
#' which provides a set of (named) `block_registry_entry` objects. Finally,
#' block construction via a registry ID is available as `create_block()`.
#'
#' @param ctor Block constructor
#' @param name,description Metadata describing the block
#' @param classes Block classes
#' @param uid Unique ID for a registry entry
#' @param category Useful to sort blocks by topics. If not specified,
#'   blocks are uncategorized.
#' @param icon Icon
#' @param arguments Block argument description
#' @param package Package where constructor is defined (or `NULL`)
#' @param overwrite Overwrite existing entry
#'
#' @examples
#' blks <- list_blocks()
#' register_block("new_dataset_block", "Test", "Registry test",
#'                uid = "test_block", package = "blockr.core")
#'
#' new <- setdiff(list_blocks(), blks)
#' unregister_blocks(new)
#' setequal(list_blocks(), blks)
#'
#' @return `register_block()` and `register_blocks()` are invoked for their side
#' effects and return `block_registry_entry` object(s) invisibly, while
#' `unregister_blocks()` returns `NULL` (invisibly). Listing via `list_blocks()`
#' returns a character vector and a list of `block_registry_entry` object(s) for
#' `available_blocks()`. Finally, `create_block()` returns a newly instantiated
#' `block` object.
#'
#' @export
register_block <- function(ctor, name, description, classes = NULL, uid = NULL,
                           category = NULL, icon = NULL, arguments = NULL,
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

  ctor_name <- NULL

  if (is_string(ctor)) {
    stopifnot(is_string(package))
    ctor_name <- ctor
    ctor <- get(ctor, asNamespace(package), mode = "function")
  } else {
    stopifnot(is.function(ctor), is.null(package))
    package <- pkg_name(environment(ctor))
  }

  if (is.null(classes) || is.null(arguments)) {

    if (is.null(ctor_name)) {
      obj <- ctor(ctor = ctor, ctor_pkg = NULL, block_metadata = FALSE)
    } else {
      obj <- ctor(ctor = ctor_name, ctor_pkg = package, block_metadata = FALSE)
    }

    if (is.null(classes)) {
      classes <- class(obj)
    }

    if (is.null(arguments)) {
      arguments <- block_external_ctrl(obj)
    }
  }

  if (is.null(uid)) {
    uid <- registry_uid(classes)
  }

  if (uid %in% list_blocks() && !isTRUE(overwrite)) {
    blockr_abort(
      "Block {uid} already exists. Try removing or `overwrite = FALSE`.",
      class = "block_already_in_registry"
    )
  }

  entry <- new_registry_entry(
    ctor,
    name = name,
    description = description,
    classes = classes,
    category = category,
    icon = icon,
    arguments = arguments,
    ctor_name = ctor_name,
    package = package
  )

  assign(uid, entry, envir = block_registry)

  invisible(entry)
}

registry_uid <- function(x) {

  if (is_block(x)) {
    x <- class(x)
  }

  stopifnot(is.character(x), length(x) >= 1L)

  x[1L]
}

#' @rdname register_block
#' @export
default_icon <- function(category) {

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

new_registry_entry <- function(ctor, ...) {
  structure(ctor, ..., class = "block_registry_entry")
}

is_registry_entry <- function(x) inherits(x, "block_registry_entry")

#' @export
board_options.block_registry_entry <- function(x, ...) {
  board_options(structure(list(), class = attr(x, "classes")), ...)
}

#' @rdname register_block
#' @export
list_blocks <- function() {
  ls(envir = block_registry)
}

#' @param block Block object
#' @rdname register_block
#' @export
registry_id_from_block <- function(block) {

  uid <- registry_uid(block)
  ids <- list_blocks()

  hit <- sum(uid == ids)

  if (!hit) {
    return(character())
  }

  stopifnot(identical(hit, 1L))

  uid
}

#' @rdname register_block
#' @export
unregister_blocks <- function(uid = list_blocks()) {
  rm(list = uid, envir = block_registry, inherits = FALSE)
  invisible()
}

#' @param ... Forwarded to `register_block()`
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

registry_metadata_fields <- c(
  "name",
  "description",
  "category",
  "icon",
  "arguments",
  "package"
)

#' @param blocks Character vector of registry IDs
#' @param fields Metadata fields
#'
#' @rdname register_block
#' @export
block_metadata <- function(blocks = list_blocks(), fields = "all") {

  stopifnot(is.character(blocks), is.character(fields))

  if (identical(fields, "all")) {
    fields <- registry_metadata_fields
  }

  fields <- match.arg(fields, registry_metadata_fields, several.ok = TRUE)

  reg <- lapply(blocks, get_registry_entry)
  fld <- setdiff(fields, c("package", "arguments"))

  res <- lapply(
    set_names(nm = fld),
    function(f) chr_ply(reg, attr, f)
  )

  if ("arguments" %in% fields) {
    res <- c(res, list(arguments = lapply(reg, attr, "arguments")))
  }

  if ("package" %in% fields) {
    res <- c(
      res,
      list(package = chr_ply(lapply(reg, attr, "package"), coal, "local"))
    )
  }

  if (length(fields) == 1L && length(blocks) == 1L) {
    return(res[[1L]])
  } else if (length(fields) == 1L) {
    return(set_names(res[[1L]], blocks))
  }

  list2DF(c(list(id = blocks), res))
}

#' @param id Block ID as reported by `list_blocks()`
#' @rdname register_block
#' @export
create_block <- function(id, ...) {
  ctor <- get_registry_entry(id)
  ctor(..., ctor = attr(ctor, "ctor_name"), ctor_pkg = attr(ctor, "package"))
}

register_core_blocks <- function(which = blockr_option("core_blocks", "all")) {
  blocks <- paste0(
    c(
      "dataset", "subset", "merge", "rbind", "scatter", "upload", "filebrowser",
      "csv", "static", "head", "glue"
    ),
    "_block"
  )

  if (identical(which, "all")) {
    which <- blocks
  } else {
    stopifnot(all(which %in% blocks), anyDuplicated(which) == 0L)
  }

  blocks <- match(which, blocks)

  register_blocks(
    c(
      "new_dataset_block",
      "new_subset_block",
      "new_merge_block",
      "new_rbind_block",
      "new_scatter_block",
      "new_upload_block",
      "new_filebrowser_block",
      "new_csv_block",
      "new_static_block",
      "new_head_block",
      "new_glue_block"
    )[blocks],
    name = c(
      "dataset block",
      "subset block",
      "merge block",
      "rbind block",
      "scatter plot block",
      "data upload block",
      "file browser block",
      "csv parser block",
      "static data block",
      "head/tail block",
      "glue string block"
    )[blocks],
    description = c(
      "Choose a dataset from a package",
      "Row and column subsetting",
      "Joining or datasets",
      "Row-binding of datasets",
      "Scatter plotting",
      "Upload data",
      "Browse local files",
      "Read CSV file",
      "Static data",
      "Data head/tail",
      "String interpolation using glue"
    )[blocks],
    category = c(
      "input",
      "transform",
      "transform",
      "transform",
      "plot",
      "input",
      "input",
      "utility",
      "input",
      "transform",
      "utility"
    )[blocks],
    icon = c(
      "database",
      "funnel",
      "union",
      "chevron-bar-expand",
      "dice-5",
      "upload",
      "folder2-open",
      "filetype-csv",
      "file-earmark-text",
      "eye",
      "braces"
    )[blocks],
    package = pkg_name(),
    overwrite = TRUE
  )
}

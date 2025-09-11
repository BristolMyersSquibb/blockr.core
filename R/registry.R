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
#' @param package Package where constructor is defined (or `NULL`)
#' @param overwrite Overwrite existing entry
#'
#' @examples
#' blks <- list_blocks()
#' register_block("new_dataset_block", "Test", "Registry test",
#'                uid = "test_block", package = "blockr.core")
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
                           category = "uncategorized", package = NULL,
                           overwrite = FALSE) {

  if (is.function(ctor)) {
    package <- pkg_name(environment(ctor))
  }

  ctor_name <- NULL

  if (is_string(ctor)) {
    stopifnot(is_string(package))
    ctor_name <- ctor
    ctor <- get(ctor, asNamespace(package), mode = "function")
  } else {
    stopifnot(is.function(ctor), is.null(package))
  }

  if (is.null(classes)) {

    if (is.null(ctor_name)) {
      obj <- ctor(ctor = ctor, ctor_pkg = NULL)
    } else {
      obj <- ctor(ctor = ctor_name, ctor_pkg = package)
    }

    classes <- class(obj)
  }

  if (is.null(uid)) {
    uid <- classes[1L]
  }

  if (uid %in% list_blocks() && !isTRUE(overwrite)) {
    abort(
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
    ctor_name = ctor_name,
    package = package
  )

  assign(uid, entry, envir = block_registry)

  invisible(entry)
}

new_registry_entry <- function(ctor, ...) {
  structure(ctor, ..., class = "block_registry_entry")
}

is_registry_entry <- function(x) inherits(x, "block_registry_entry")

#' @rdname register_block
#' @export
list_blocks <- function() {
  ls(envir = block_registry)
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

#' @param id Block ID as reported by `list_blocks()`
#' @rdname register_block
#' @export
create_block <- function(id, ...) {
  ctor <- get_registry_entry(id)
  ctor(..., ctor = attr(ctor, "ctor_name"), ctor_pkg = attr(ctor, "package"))
}

register_core_blocks <- function(which = blockr_option("blocks", "all")) {

  blocks <- paste0(
    c("dataset", "subset", "merge", "rbind", "scatter", "upload", "filebrowser",
      "csv", "static", "head", "glue"),
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
      "data",
      "transform",
      "transform",
      "transform",
      "plot",
      "file",
      "file",
      "parse",
      "data",
      "transform",
      "text"
    )[blocks],
    package = pkg_name(),
    overwrite = TRUE
  )
}

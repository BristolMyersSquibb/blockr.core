#' Block metadata
#'
#' Registry metadata for blocks is available both as a tabular overview and via
#' per-attribute accessors. `block_metadata()` returns a `data.frame` with one
#' row per block -- dispatching on a `block`, a `blocks` collection, a
#' `block_registry_entry` or a registry ID -- where scalar attributes are
#' atomic columns and the multi-valued ones (`arguments`, `examples`,
#' `keywords`) are list-columns. The `fields` argument selects a subset of
#' columns. Each attribute additionally has a dedicated getter
#' (`block_meta_name()`, `block_meta_guidance()`, ...) returning that attribute
#' for a single block. Missing fields are filled with display defaults in the
#' data.frame; the getters instead return the stored value (or `NA` / an empty
#' value).
#'
#' @param x A `block`, a `blocks` collection, a `block_registry_entry` or a
#'   registry ID
#' @param fields Metadata fields to include (defaults to `"all"`)
#' @param ... Generic consistency, passed on to methods
#'
#' @return `block_metadata()` returns a `data.frame`. The `block_meta_*()`
#'   getters return the named attribute: a string (or `NA`) for scalar fields, a
#'   character vector for `block_meta_keywords()`, an `arg_specs` object for
#'   `block_meta_arguments()`, and a list of worked configurations for
#'   `block_meta_examples()`.
#'
#' @rdname block_metadata
#' @export
block_metadata <- function(x, fields = "all", ...) {
  UseMethod("block_metadata")
}

#' @export
block_metadata.character <- function(x, fields = "all", ...) {
  build_block_catalog(
    lapply(x, block_metadata_record),
    fields,
    rep(NA_character_, length(x))
  )
}

#' @export
block_metadata.default <- function(x, fields = "all", ...) {

  blks <- as_blocks(x)

  build_block_catalog(
    lapply(blks, block_metadata_record),
    fields,
    chr_ply(blks, block_default_name),
    names(blks)
  )
}

block_default_name <- function(x) {
  gsub("_", " ", class(x)[1L])
}

block_metadata_fields <- function() {
  c(
    "id", "name", "description", "details", "link", "guidance", "keywords",
    "category", "icon", "arguments", "examples", "package"
  )
}

build_block_catalog <- function(records, fields, default_names,
                                row_names = NULL) {

  if (identical(fields, "all")) {
    fields <- block_metadata_fields()
  }

  fields <- match.arg(fields, block_metadata_fields(), several.ok = TRUE)

  cols <- lapply(
    set_names(nm = fields),
    catalog_column,
    records = records,
    default_names = default_names
  )

  res <- list2DF(cols)

  if (not_null(row_names) && length(row_names) == nrow(res)) {
    rownames(res) <- row_names
  }

  res
}

catalog_column <- function(field, records, default_names) {

  switch(
    field,
    id = scalar_column(records, "id", NA_character_),
    name = name_column(records, default_names),
    description = scalar_column(
      records, "description", "No description available."
    ),
    details = scalar_column(records, "details", NA_character_),
    link = scalar_column(records, "link", NA_character_),
    guidance = scalar_column(records, "guidance", NA_character_),
    category = scalar_column(records, "category", default_category()),
    icon = icon_column(records),
    package = scalar_column(records, "package", "local"),
    keywords = lapply(records, meta_default, "keywords", character()),
    arguments = lapply(records, meta_default, "arguments", new_arg_specs()),
    examples = lapply(records, record_examples)
  )
}

scalar_column <- function(records, field, default) {
  chr_ply(records, function(r) coal(r[[field]], default, fail_all = FALSE))
}

name_column <- function(records, default_names) {
  chr_mply(
    function(r, d) coal(r[["name"]], d, fail_all = FALSE),
    records,
    default_names
  )
}

icon_column <- function(records) {
  chr_ply(
    records,
    function(r) {
      category <- coal(r[["category"]], default_category())
      coal(r[["icon"]], default_icon(category), fail_all = FALSE)
    }
  )
}

meta_default <- function(record, field, default) {
  coal(record[[field]], default, fail_all = FALSE)
}

record_examples <- function(record) {
  block_examples_list(
    coal(record[["arguments"]], new_arg_specs(), fail_all = FALSE),
    coal(record[["examples"]], list(), fail_all = FALSE)
  )
}

block_record_fields <- function() {
  c(registry_metadata_fields, "examples")
}

block_metadata_record <- function(x, ...) {
  UseMethod("block_metadata_record")
}

#' @export
block_metadata_record.block <- function(x, ...) {

  record <- attr(x, "block_metadata")

  if (!is.list(record)) {
    blockr_abort(
      "Block {class(x)[1L]} carries no metadata.",
      class = "missing_block_metadata"
    )
  }

  record
}

#' @export
block_metadata_record.block_registry_entry <- function(x, ...) {
  c(
    list(id = registry_uid(attr(x, "classes"))),
    lapply(set_names(nm = block_record_fields()), get_attr, x)
  )
}

#' @export
block_metadata_record.character <- function(x, ...) {

  record <- block_metadata_record(get_registry_entry(x))
  record[["id"]] <- x

  record
}

scalar_meta <- function(x, field, ...) {
  coal(block_metadata_record(x, ...)[[field]], NA_character_, fail_all = FALSE)
}

#' @rdname block_metadata
#' @export
block_meta_id <- function(x, ...) {
  scalar_meta(x, "id", ...)
}

#' @rdname block_metadata
#' @export
block_meta_name <- function(x, ...) {
  scalar_meta(x, "name", ...)
}

#' @rdname block_metadata
#' @export
block_meta_description <- function(x, ...) {
  scalar_meta(x, "description", ...)
}

#' @rdname block_metadata
#' @export
block_meta_details <- function(x, ...) {
  scalar_meta(x, "details", ...)
}

#' @rdname block_metadata
#' @export
block_meta_link <- function(x, ...) {
  scalar_meta(x, "link", ...)
}

#' @rdname block_metadata
#' @export
block_meta_guidance <- function(x, ...) {
  scalar_meta(x, "guidance", ...)
}

#' @rdname block_metadata
#' @export
block_meta_category <- function(x, ...) {
  scalar_meta(x, "category", ...)
}

#' @rdname block_metadata
#' @export
block_meta_icon <- function(x, ...) {
  scalar_meta(x, "icon", ...)
}

#' @rdname block_metadata
#' @export
block_meta_package <- function(x, ...) {
  scalar_meta(x, "package", ...)
}

#' @rdname block_metadata
#' @export
block_meta_keywords <- function(x, ...) {
  coal(block_metadata_record(x, ...)[["keywords"]], character(),
       fail_all = FALSE)
}

#' @rdname block_metadata
#' @export
block_meta_arguments <- function(x, ...) {
  coal(block_metadata_record(x, ...)[["arguments"]], new_arg_specs(),
       fail_all = FALSE)
}

#' @rdname block_metadata
#' @export
block_meta_examples <- function(x, ...) {
  record_examples(block_metadata_record(x, ...))
}

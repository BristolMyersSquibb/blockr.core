#' @section Block vectors:
#' Multiple blocks can be combined into a `blocks` object, a container for
#' an (ordered) set of blocks. Block IDs are handled at the `blocks` level,
#' which ensures that those given are unique and, as every ID, strings that
#' are neither empty nor made of digits alone. A block passed without a name
#' has no ID, which the client mints when it opens the board.
#'
#' @examples
#' blks <- c(a = new_dataset_block(), b = new_subset_block())
#'
#' is_block(blks)
#' is_blocks(blks)
#'
#' names(blks)
#'
#' @rdname new_block
#' @export
blocks <- function(...) {

  args <- list(...)

  args <- lapply(args, as_block)

  names(args) <- ids_or_empty(names(args), length(args))

  validate_blocks(
    new_vctr(args, class = "blocks")
  )
}

#' @rdname new_block
#' @export
is_blocks <- function(x) {
  inherits(x, "blocks")
}

#' @rdname new_block
#' @export
as_blocks <- function(x, ...) {
  UseMethod("as_blocks")
}

#' @export
as_blocks.blocks <- function(x, ...) {
  validate_blocks(x)
}

#' @export
as_blocks.list <- function(x, ...) {
  do.call(blocks, x)
}

#' @export
as_blocks.block <- function(x, ...) {
  as_blocks(list(x))
}

#' @export
as_blocks.board <- function(x, ...) {
  board_blocks(x)
}

#' @export
as.list.blocks <- function(x, ...) {
  vec_data(x)
}

#' @export
format.blocks <- function(x, ...) {
  format_collection(x)
}

format_collection <- function(x) {

  out <- Map(
    function(id, lines) c(id[nzchar(id)], paste0(" ", lines)),
    ids_or_empty(names(x), length(x)),
    lapply(x, format)
  )
  out <- lapply(out, c, "")
  out <- unlst(out)

  c(
    paste0("<", class(x)[1L], "[", length(x), "]>"),
    if (length(x)) "",
    out[-length(out)]
  )
}

#' @export
print.blocks <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
str_value.blocks <- function(x, ...) {
  str_value_collection(x, chr_ply(x, str_value))
}

#' @export
str.blocks <- function(object, ...) {
  cat_str_value(object, ...)
}

#' @export
vec_restore.blocks <- function(x, to, ...) {
  validate_blocks(NextMethod())
}

harmonize_list_of_blocks <- function(x) {
  if (is_block(x)) {
    list(x)
  } else if (is_blocks(x)) {
    as.list(x)
  } else if (is.list(x) && all(lgl_ply(x, is_block))) {
    x
  } else {
    list(as_block(x))
  }
}

#' @export
c.blocks <- function(...) {

  res <- unlist(
    lapply(list(...), harmonize_list_of_blocks),
    recursive = FALSE
  )

  as_blocks(res)
}

validate_blocks <- function(x) {

  if (!is_blocks(x)) {
    blockr_abort(
      "Expecting blocks to inherit from \"blocks\".",
      class = "blocks_class_invalid"
    )
  }

  if (!is.list(x) || !all(lgl_ply(x, is_block))) {
    blockr_abort(
      "Expecting the board to contain a set of blocks.",
      class = "blocks_contains_invalid"
    )
  }

  ids <- ids_or_empty(names(x), length(x))
  ids <- ids[nzchar(ids)]

  validate_ids(ids, "Block", "blocks_names_invalid")

  if (anyDuplicated(ids) != 0) {
    blockr_abort(
      "Block IDs are required to be unique.",
      class = "blocks_names_invalid"
    )
  }

  x
}

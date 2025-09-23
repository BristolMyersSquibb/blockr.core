#' @section Block vectors:
#' Multiple blocks can be combined into a `blocks` object, a container for
#' an (ordered) set of blocks. Block IDs are handled at the `blocks` level
#' which will ensure uniqueness.
#'
#' @examples
#' blks <- c(a = new_dataset_block(), b = new_subset_block())
#'
#' is_block(blks)
#' is_blocks(blks)
#'
#' names(blks)
#'
#' tryCatch(
#'   names(blks["a"]) <- "b",
#'   error = function(e) conditionMessage(e)
#' )
#'
#' @rdname new_block
#' @export
blocks <- function(...) {

  args <- list(...)

  args <- lapply(args, as_block)

  if (is.null(names(args))) {
    names(args) <- rand_names(n = length(args))
  }

  miss <- is.na(names(args)) | names(args) == ""

  if (any(miss)) {
    names(args)[miss] <- rand_names(names(args)[!miss], sum(miss))
  }

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
as.list.blocks <- function(x, ...) {
  vec_data(x)
}

#' @export
`names<-.blocks` <- function(x, value) {

  if (is.null(value)) {
    value <- rep("", length(x))
  } else if (anyDuplicated(value) != 0L) {
    blockr_abort(
      "IDs are required to be unique.",
      class = "blocks_names_unique_invalid"
    )
  }

  attr(x, "names") <- value

  x
}

#' @export
format.blocks <- function(x, ...) {

  out <- lapply(x, format)
  out <- Map(c, names(x), lapply(out, function(x) paste0(" ", x)))
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

#' @export
`[<-.blocks` <- function(x, i, ..., value) {

  i <- vec_as_location(i, length(x), names(x))

  if (is.null(value)) {
    return(blocks_slice(x, -i))
  }

  value <- as_blocks(value)

  trg_ids <- names(x)[i]

  if (is.null(names(value))) {
    names(value) <- trg_ids
  }

  new_ids <- names(value)

  if (!setequal(new_ids, trg_ids)) {
    blockr_abort(
      paste0(
        "Replacing IDs ", paste_enum(trg_ids), " with ", paste_enum(new_ids),
        " is not allowed."
      ),
      class = "blocks_assignment_name_invalid"
    )
  }

  blocks_assign(x, i, value[trg_ids])
}

#' @export
`[[<-.blocks` <- function(x, i, ..., value) {

  i <- vec_as_location2(i, length(x), names(x))
  val <- set_names(list(as_block(value)), x$id[i])

  blocks_assign(x, i, as_blocks(val))
}

blocks_slice <- function(...) {
  validate_blocks(vec_slice(...))
}

blocks_assign <- function(...) {
  validate_blocks(vec_assign(...))
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

  ids <- names(x)

  if (length(ids) != length(x) || any(is.na(ids) | !nchar(ids))) {
    blockr_abort(
      "Block IDs are required to be nonempty strings.",
      class = "blocks_names_invalid"
    )
  }

  if (anyDuplicated(ids) != 0) {
    blockr_abort(
      "Block IDs are required to be unique.",
      class = "blocks_names_invalid"
    )
  }

  x
}

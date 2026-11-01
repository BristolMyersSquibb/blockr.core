#' Similar to [`blocks`](new_block()) and [`links`][new_link()], multiple
#' stacks can be handled using `stacks` container objects. These guarantee
#' that the IDs given (implemented as [base::names()]) are unique and, as every
#' ID, strings that are neither empty nor made of digits alone, while a stack
#' passed without a name has no ID, which the client mints when it opens the
#' board. They ensure further consistency criteria, such as disallowing
#' membership in multiple stacks at the same time for a given block.
#'
#' Stack container objects (`stacks` objects) can be created with `stacks()`
#' or `as_stacks()` and inheritance can be tested via `is_stacks()`. Further
#' basic operations such as concatenation and subsetting are available by means
#' of base R generics.
#'
#' @rdname new_stack
#' @export
stacks <- function(...) {

  args <- list(...)

  args <- lapply(args, as_stack)

  names(args) <- ids_or_empty(names(args), length(args))

  validate_stacks(
    new_vctr(args, class = "stacks")
  )
}

#' @rdname new_stack
#' @export
is_stacks <- function(x) {
  inherits(x, "stacks")
}

validate_stacks <- function(x) {

  if (!is_stacks(x)) {
    blockr_abort(
      "Expecting stacks to inherit from \"stacks\".",
      class = "stacks_class_invalid"
    )
  }

  if (!is.list(x)) {
    blockr_abort(
      "Expecting a `stacks` object to behave list-like.",
      class = "stacks_type_invalid"
    )
  }

  if (!all(lgl_ply(x, is_stack))) {
    blockr_abort(
      "Expecting a `stacks` object to contain a set of stacks.",
      class = "stacks_contains_invalid"
    )
  }

  ids <- ids_or_empty(names(x), length(x))
  ids <- ids[nzchar(ids)]

  validate_ids(ids, "Stack", "stacks_names_invalid")

  if (anyDuplicated(ids) != 0) {
    blockr_abort(
      "Stack IDs are required to be unique.",
      class = "stacks_names_invalid"
    )
  }

  if (!stack_blocks_unique(x)) {
    blockr_abort(
      "Blocks cannot be in mutliple stacks at the same time.",
      class = "stacks_blocks_invalid"
    )
  }

  x
}

stack_blocks_unique <- function(x) {

  seq <- seq_along(x)

  for (i in seq) {
    for (j in seq[seq > i]) {
      if (length(intersect(x[[i]], x[[j]]))) {
        return(FALSE)
      }
    }
  }

  TRUE
}

#' @export
format.stacks <- function(x, ...) {
  format_collection(x)
}

#' @export
print.stacks <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
str_value.stacks <- function(x, ...) {
  str_value_collection(x, chr_ply(x, str_value))
}

#' @export
str.stacks <- function(object, ...) {
  cat_str_value(object, ...)
}

#' @rdname new_stack
#' @export
as_stacks <- function(x, ...) {
  UseMethod("as_stacks")
}

#' @export
as_stacks.stacks <- function(x, ...) {
  validate_stacks(x)
}

#' @export
as_stacks.list <- function(x, ...) {
  do.call(stacks, x)
}

#' @export
as_stacks.character <- function(x, ...) {
  as_stacks(list(x))
}

#' @export
as_stacks.stack <- function(x, ...) {
  as_stacks(list(x))
}

#' @export
as_stacks.board <- function(x, ...) {
  board_stacks(x)
}

#' @export
as.list.stacks <- function(x, ...) {
  vec_data(x)
}

#' @export
vec_restore.stacks <- function(x, to, ...) {
  validate_stacks(NextMethod())
}

harmonize_list_of_stacks <- function(x) {
  if (is_stack(x)) {
    list(x)
  } else if (is_stacks(x)) {
    as.list(x)
  } else if (is.list(x) && all(lgl_ply(x, is_stack))) {
    x
  } else {
    list(as_stack(x))
  }
}

#' @export
c.stacks <- function(...) {

  res <- unlist(
    lapply(list(...), harmonize_list_of_stacks),
    recursive = FALSE
  )

  as_stacks(res)
}

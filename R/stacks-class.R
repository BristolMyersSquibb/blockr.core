#' Similar to [`blocks`](new_block()) and [`links`][new_link()], multiple
#' stacks can be handled using `stacks` container objects. These hand out
#' (and guarantee) unique IDs (implemented as [base::names()]) and ensure
#' further consistency criteria, such as disallowing membership in multiple
#' stacks at the same time for a given block.
#'
#' Stack container objects (`stacks` objects) can be created with `stacks()`
#' or `as_stacks()` and inheritance can be tested via `is_stacks()`. Further
#' basic operations such as concatenation, subsetting and sub-assignments is
#' available by means of base R generics.
#'
#' @rdname new_stack
#' @export
stacks <- function(...) {

  args <- list(...)

  args <- lapply(args, as_stack)

  if (is.null(names(args))) {
    names(args) <- rand_names(n = length(args))
  }

  miss <- is.na(names(args)) | names(args) == ""

  if (any(miss)) {
    names(args)[miss] <- rand_names(names(args)[!miss], sum(miss))
  }

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

  ids <- names(x)

  if (length(ids) != length(x) || any(is.na(ids) | !nchar(ids))) {
    blockr_abort(
      "Stack IDs are required to be nonempty strings.",
      class = "stacks_names_invalid"
    )
  }

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
print.stacks <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
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
`names<-.stacks` <- function(x, value) {

  if (is.null(value)) {
    value <- rep("", length(x))
  } else if (anyDuplicated(value) != 0L) {
    blockr_abort(
      "IDs are required to be unique.",
      class = "stacks_names_unique_invalid"
    )
  }

  attr(x, "names") <- value

  x
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

#' @export
`[<-.stacks` <- function(x, i, ..., value) {

  i <- vec_as_location(i, length(x), names(x))

  if (is.null(value)) {
    return(stacks_slice(x, -i))
  }

  value <- as_stacks(value)

  trg_ids <- names(x)[i]

  if (is.null(names(value))) {
    names(value) <- trg_ids
  }

  new_ids <- names(value)

  if (!setequal(new_ids, trg_ids)) {
    blockr_abort(
      "Replacing IDs {trg_ids} with {new_ids} is not allowed.",
      class = "stacks_assignment_name_invalid"
    )
  }

  stacks_assign(x, i, value[trg_ids])
}

#' @export
`[[<-.stacks` <- function(x, i, ..., value) {

  i <- vec_as_location2(i, length(x), names(x))
  val <- set_names(list(as_stack(value)), names(x)[i])

  stacks_assign(x, i, as_stacks(val))
}

stacks_slice <- function(...) {
  validate_stacks(vec_slice(...))
}

stacks_assign <- function(...) {
  validate_stacks(vec_assign(...))
}

#' Vector-valued links (i.e. `links` objects) manage IDs that are unique (with
#' empty strings being the only exception). These are either user-supplied or
#' generated automatically (via random strings) and operations like renaming,
#' concatenation and sub-assignments are guaranteed to not allow duplicated
#' IDs.
#'
#' In addition to unique IDs, `links` objects are guaranteed to be consistent
#' in that it is not possible to have multiple links pointing to the same
#' target (combination of `to` and `input` attributes). Furthermore, links
#' behave like edges in a directed acyclic graph (DAG) in that cycles are
#' detected and disallowed.
#'
#' @rdname new_link
#' @export
links <- function(...) {

  vals <- list_to_list_of_links(list(...))

  if (length(vals)) {

    if (is.null(names(vals))) {
      names(vals) <- rand_names(n = length(vals))
    }

    miss <- is.na(names(vals)) | names(vals) == ""

    if (any(miss)) {
      names(vals)[miss] <- rand_names(names(vals)[!miss], sum(miss))
    }

    ids <- names(vals)

  } else {

    vals <- list(lapply(new_link(), `[`, 0L))
    ids <- character()
  }

  res <- do.call(rbind, lapply(vals, as.data.frame))
  res <- cbind(id = ids, res)

  rownames(res) <- NULL

  validate_links(
    vctrs::new_rcrd(res, class = "links")
  )
}

harmonize_list_of_links <- function(x) {
  if (is_link(x)) {
    list(x)
  } else if (is_links(x)) {
    as.list(x)
  } else if (is.list(x) && all(lgl_ply(x, is_link))) {
    x
  } else {
    list(as_link(x))
  }
}

list_to_list_of_links <- function(x) {

  if (length(x) == 1L && is.character(x[[1L]])) {
    x <- list(as.list(x))
  }

  if (length(x) && all(lgl_ply(x, is.atomic))) {

    stopifnot(
      all(length(x[[1L]]) == lengths(x[-1L]))
    )

    vals <- lgl_ply(
      coal(names(x), rep("", length(x))),
      Negate(identical),
      "id"
    )

    res <- do.call(map, c(new_link, x[vals]))

    if ("id" %in% names(x)) {
      names(res) <- x[["id"]]
    } else {
      names(res) <- NULL
    }

    res

  } else {

    unlist(lapply(x, harmonize_list_of_links), recursive = FALSE)
  }
}

#' @rdname new_link
#' @export
is_links <- function(x) {
  inherits(x, "links")
}

#' @export
names.links <- function(x) {
  field(x, "id")
}

#' @export
`names<-.links` <- function(x, value) {

  if (is.null(value)) {
    value <- rep("", length(x))
  } else if (anyDuplicated(value) != 0L) {
    blockr_abort(
      "IDs are required to be unique.",
      class = "links_names_unique_invalid"
    )
  }

  field(x, "id") <- value

  x
}

#' @export
format.links <- function(x, ...) {
  res <- chr_ply(as.list(x), format, length = 2L, use_names = TRUE)
  res[2L, ]
}

#' @export
vec_restore.links <- function(x, to, ...) {
  validate_links(NextMethod())
}

#' @export
c.links <- function(...) {
  as_links(list_to_list_of_links(list(...)))
}

#' @export
`[.links` <- function(x, i, ...) {

  res <- links_slice(x, vec_as_location(i, length(x), names(x)))

  if (is.null(res)) {
    return(links())
  }

  res
}

#' @export
`[<-.links` <- function(x, i, ..., value) {

  i <- vec_as_location(i, length(x), names(x))

  if (is.null(value)) {
    return(links_slice(x, -i))
  }

  value <- list_to_list_of_links(value)

  trg_ids <- field(links_slice(x, i), "id")

  if (is.null(names(value))) {
    names(value) <- trg_ids
  }

  new_ids <- names(value)

  if (!setequal(new_ids, trg_ids)) {
    blockr_abort(
      "Replacing IDs {trg_ids} with {new_ids} is not allowed.",
      class = "links_assignment_name_invalid"
    )
  }

  links_assign(x, i, as_links(value[trg_ids]))
}

#' @export
`[[.links` <- function(x, i, ...) {

  res <- vec_proxy(
    links_slice(x, vec_as_location2(i, length(x), names(x)))
  )

  as_link(res[, colnames(res) != "id"])
}

#' @export
`[[<-.links` <- function(x, i, ..., value) {

  i <- vec_as_location2(i, length(x), names(x))
  val <- set_names(list(as_link(value)), x$id[i])

  links_assign(x, i, as_links(val))
}

#' @export
`$.links` <- function(x, name) {
  field(x, name)
}

#' @export
`$<-.links` <- function(x, name, value) {
  `field<-`(x, name, value)
}

#' @rdname new_link
#' @export
as_links <- function(x, ...) {
  UseMethod("as_links")
}

#' @export
as_links.links <- function(x, ...) {
  validate_links(x)
}

#' @export
as_links.link <- function(x, ...) {
  as_links(as.list(x))
}

#' @export
as_links.list <- function(x, ...) {
  do.call(links, x)
}

#' @export
as_links.NULL <- function(x, ...) {
  links()
}

#' @method as_links data.frame
#' @export
as_links.data.frame <- function(x, ...) {
  as_links(as.list(x))
}

#' @export
as_links.board <- function(x, ...) {
  board_links(x)
}

#' @method as.data.frame links
#' @export
as.data.frame.links <- function(x, ...) {
  vec_proxy(x)
}

#' @method as.matrix links
#' @export
as.matrix.links <- function(x, ...) {
  todo <- x$from != "" & x$to != ""
  as_adjacency_matrix(x$from[todo], x$to[todo])
}

#' @rdname topo_sort
#' @export
is_acyclic.links <- function(x) {
  is_acyclic(as.matrix(x))
}

#' @method as.list links
#' @export
as.list.links <- function(x, ...) {

  df <- vec_proxy(x)

  lst <- split(df, seq_len(nrow(df)))

  res <- map(
    function(x, cols) as_link(lapply(x[, cols], na_to_empty)),
    lst,
    lapply(lapply(lst, colnames), setdiff, "id")
  )

  names(res) <- chr_xtr(lst, "id")

  res
}

links_slice <- function(...) {
  validate_links(vec_slice(...))
}

links_assign <- function(...) {
  validate_links(vec_assign(...))
}

#' @rdname new_link
#' @export
validate_links <- function(x) {

  any_dup <- function(x) {
    anyDuplicated(filter_empty(x))
  }

  if (is_board(x)) {
    x <- board_links(x)
  }

  if (!is_links(x)) {
    blockr_abort(
      "Expecting a board links objects to inherit from \"links\".",
      class = "links_class_invalid"
    )
  }

  if (!is.list(x)) {
    blockr_abort(
      "Expecting a board links objects behave list-like.",
      class = "links_list_like_invalid"
    )
  }

  fields <- c("id", "from", "to", "input")

  if (!all(fields %in% fields(x))) {
    blockr_abort(
      "Expecting the links to contain at least fields {fields}.",
      class = "links_fields_invalid"
    )
  }

  for (y in as.list(x)) {
    validate_link(y)
  }

  if (any_dup(field(x, "id")) != 0L) {
    blockr_abort(
      "Links IDs are required to be unique.",
      class = "links_names_unique_invalid"
    )
  }

  dup_inp <- lapply(split(field(x, "input"), field(x, "to")), any_dup)
  dup_inp <- lgl_ply(dup_inp, Negate(identical), 0L, use_names = TRUE)

  if (any(dup_inp)) {
    blockr_abort(
      "Block{?s} {names(dup_inp)[dup_inp]} {?has/have} multiple identical ",
      "inputs.",
      class = "links_block_inputs_invalid"
    )
  }

  if (!is_acyclic(x)) {
    blockr_abort("Links form a cycle.", class = "links_acyclic_invalid")
  }

  x
}

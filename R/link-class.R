#' Board links
#'
#' Two blocks can be connected via a (directed) link. This means the result from
#' one block is passed as (data) input to the next. Source and destination are
#' identified by `from` and `to` attributes and in case of polyadic receiving
#' blocks, the `input` attribute identified which of the data inputs is the
#' intended destination. In principle, the `link` object may be extended via
#' sub-classing and passing further attributes, but this has not been properly
#' tested so far.
#'
#' A links is created via the `new_link()` constructor and for a vector of
#' links, the container object `links` is provided and a corresponding
#' constructor `links()` exported from the package. Testing whether an object
#' inherits from `link` (or `links`) is available via `is_link()` (or
#' `is_links()`, respectively). Coercion to `link` (and `links`) objects is
#' implemented as `as_link()` (and `as_links()`, respectively). Finally, links
#' can be validated by calling `validate_links()`.
#'
#' @param from,to Block ID(s)
#' @param input Block argument
#' @param ... Extensibility
#' @param class (Optional) link sub-class
#'
#' @examples
#' lnks <- links(from = c("a", "b"), to = c("b", "c"), input = c("x", "y"))
#' is_links(lnks)
#' names(lnks)
#'
#' tryCatch(
#'   c(lnks, new_link("a", "b", "x")),
#'   error = function(e) conditionMessage(e)
#' )
#' tryCatch(
#'   c(lnks, new_link("b", "a")),
#'   error = function(e) conditionMessage(e)
#' )
#'
#' lnks <- links(a = new_link("a", "b"), b = new_link("b", "c"))
#' names(lnks)
#'
#' tryCatch(
#'   c(lnks, a = new_link("a", "b")),
#'   error = function(e) conditionMessage(e)
#' )
#'
#' @return Both `new_link()`/`as_link()`, and `links()`/`as_links()` return
#' `link` and `links` objects, respectively. Testing for inheritance is
#' available as `is_link()`/`is_links()` and validation (for `links`) is
#' performed with `validate_links()`, which returns its input (`x`) or throws
#' an error.
#'
#' @export
new_link <- function(from = "", to = "", input = "", ..., class = character()) {

  stopifnot(length(from) == length(to))

  if (length(from) && is_empty(input)) {
    input <- ""
  }

  validate_link(
    new_vctr(
      list(from = from, to = to, input = input, ...),
      class = c(class, "link")
    )
  )
}

validate_link <- function(x) {

  if (!is_link(x)) {
    abort(
      "Expecting a link to inherit from \"link\".",
      class = "link_class_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting a link to be represented by a list.",
      class = "link_list_like_invalid"
    )
  }

  if (all_zero_len(x)) {
    return(x)
  }

  fields <- c("from", "to", "input")

  if (!all(fields %in% names(x))) {
    abort(
      paste0(
        "Expecting a link to contain attributes ", paste_enum(fields), "."
      ),
      class = "link_components_missing"
    )
  }

  if (!all(lgl_ply(x[fields], is_string))) {
    abort(
      paste0(
        "Expecting link attributes ", paste_enum(fields), " to be strings."
      ),
      class = "link_components_invalid"
    )
  }

  if (anyNA(x[fields])) {
    abort(
      paste0(
        "Missing values for ", paste_enum(fields), " are not allowed."
      ),
      class = "link_components_invalid"
    )
  }

  if (x[["from"]] == x[["to"]] && x[["to"]] != "") {
    abort(
      "Self-referencing links are not allowed.",
      class = "link_self_referencing"
    )
  }

  x
}

#' @rdname new_link
#' @export
is_link <- function(x) {
  inherits(x, "link")
}

#' @param x Links object
#' @rdname new_link
#' @export
as_link <- function(x) {
  UseMethod("as_link")
}

#' @export
as_link.link <- function(x) {
  x
}

#' @export
as_link.character <- function(x) {
  do.call(new_link, as.list(x))
}

#' @export
as_link.list <- function(x) {
  do.call(new_link, x)
}

#' @method as_link data.frame
#' @export
as_link.data.frame <- function(x) {
  do.call(new_link, x)
}

#' @export
as.character.link <- function(x, ...) {
  do.call(c, as.list(x))
}

#' @export
as.list.link <- function(x, ...) {
  vec_data(x)
}

#' @method as.data.frame link
#' @export
as.data.frame.link <- function(x, ...) {
  as.data.frame(as.list(x))
}

#' @export
`[.link` <- function(x, i, ...) {
  x <- vec_data(x)
  x[vec_as_location(i, length(x), names(x))]
}

#' @export
`[[.link` <- function(x, i, ...) {
  x <- vec_data(x)
  x[[vec_as_location2(i, length(x), names(x))]]
}

#' @export
format.link <- function(x, ...) {

  field_miss <- function(x) {
    if (is.na(x) || !nzchar(x)) "?" else x
  }

  out <- ""

  for (cl in rev(setdiff(class(x), c("list", "vctrs_vctr")))) {
    out <- paste0("<", cl, out, ">")
  }

  if (all_zero_len(x)) {
    return(out)
  }

  inp <- x["input"]

  if (is.na(inp) || !nzchar(inp)) {
    inp <- ""
  } else {
    inp <- paste0(" (", inp, ")")
  }

  c(out, paste0(field_miss(x["from"]), " -> ", field_miss(x["to"]), inp))
}

#' @export
print.link <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
c.link <- function(...) {
  as_links(list_to_list_of_links(list(...)))
}

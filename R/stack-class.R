#' Stacks
#'
#' Multiple (related) blocks can be grouped together into stacks. Such a
#' grouping has no functional implications, rather it is an organizational tool
#' to help users manage more complex pipelines. Stack objects constitute a set
#' of attributes, the most important of which is `blocks` (a character vector
#' of block IDs). Each `stack` may have an arbitrary `name` and the class can
#' be extended by adding further attributes, maybe something like `color`,
#' coupled with sub-classing.
#'
#' Individual stacks can be created using `new_stack()` or `as_stack()` and
#' inheritance can be tested with `is_stack()`. Attributes can be retrieved
#' (and modified) with `stack_blocks()`/`stack_blocks<-()` and
#' `stack_name()`/`stack_name<-()`, while validation is available as
#' (generic) `validate_stack()`.
#'
#' @param blocks Set of blocks
#' @param name Stack name
#' @param ... Extensibility
#' @param ctor,pkg Constructor information (used for serialization)
#' @param class (Optional) stack sub-class
#'
#' @examples
#' stk <- new_stack(letters[1:5], "Alphabet 1")
#'
#' stack_blocks(stk)
#' stack_name(stk)
#' stack_name(stk) <- "Alphabet start"
#'
#' stks <- c(start = stk, cont = new_stack(letters[6:10], "Alphabet cont."))
#' names(stks)
#'
#' tryCatch(
#'   stack_blocks(stks[[2]]) <- letters[4:8],
#'   error = function(e) conditionMessage(e)
#' )
#'
#' @return Construction and coercion via `new_stack()`/`as_stack()` and
#' `stacks()`/`as_stacks()` results in `stack` and `stacks` objects,
#' respectively, while inheritance testing via `is_stack()` and `is_stacks()`
#' returns scalar logicals. Attribute getters `stack_name()` and
#' `stack_blocks()` return scalar and vector-valued character vectors while
#' setters `stack_name()<-` and `stack_blocks()<-` return modified stack
#' objects.
#'
#' @export
new_stack <- function(blocks = character(), name = default_stack_name, ...,
                      ctor = "new_stack", pkg = pkg_name(),
                      class = character()) {

  if (is_blocks(blocks)) {
    blocks <- names(blocks)
  }

  if (!length(blocks)) {
    blocks <- character()
  }

  if (is.null(name)) {
    name <- default_stack_name
  }

  if (is.function(name)) {
    name <- name()
  }

  stopifnot(is_string(name))

  validate_stack(
    structure(
      blocks,
      name = name,
      ...,
      ctor = new_blockr_ctor(NULL, ctor, pkg),
      class = c(class, "stack")
    )
  )
}

#' @rdname new_stack
#' @export
default_stack_name <- function() {
  "Stack"
}

#' @param x Stack object
#' @rdname new_stack
#' @export
is_stack <- function(x) {
  inherits(x, "stack")
}

#' @rdname new_stack
#' @export
stack_blocks <- function(x) {
  as.character(x)
}

#' @param value Replacement value
#' @rdname new_stack
#' @export
`stack_blocks<-` <- function(x, value) {
  vec_restore(new_stack(value), x)
}

#' @rdname new_stack
#' @export
stack_name <- function(x, name) {
  attr(x, "name")
}

#' @rdname new_stack
#' @export
`stack_name<-` <- function(x, value) {
  attr(x, "name") <- value
  x
}

#' @rdname new_stack
#' @export
validate_stack <- function(x) {
  UseMethod("validate_stack")
}

#' @export
validate_stack.stack <- function(x) {

  if (!is_stack(x)) {
    blockr_abort(
      "Expecting a stack to inherit from `stack`.",
      class = "stack_class_invalid"
    )
  }

  if (!is_string(attr(x, "name"))) {
    blockr_abort(
      "Expecting the stack name to be a string.",
      class = "stack_name_invalid"
    )
  }

  if (!is.character(x)) {
    blockr_abort(
      "Expecting stack blocks to be character-like.",
      class = "stack_type_invalid"
    )
  }

  if (anyNA(x) || !all(nzchar(x))) {
    blockr_abort(
      "Expecting the stack blocks to be strings.",
      class = "stack_blocks_invalid"
    )
  }

  nme <- names(x)

  if (!is.null(nme) || any(nzchar(nme))) {
    blockr_warn(
      "Names are ignored in stack objects.",
      class = "named_stack_obejct_warn"
    )
  }

  if (anyDuplicated(x) != 0L) {
    blockr_abort(
      "Stack blocks have to be unique.",
      class = "stack_block_duplicates"
    )
  }

  x
}

#' @export
validate_stack.default <- function(x) {
  blockr_abort(
    "Expecting a stack to inherit from `stack`.",
    class = "stack_class_invalid"
  )
}

#' @export
format.stack <- function(x, ...) {

  out <- paste0("[", length(x), "]")

  for (cl in rev(setdiff(class(x), c("list", "vctrs_vctr")))) {
    out <- paste0("<", cl, out, ">")
  }

  if (length(x)) {
    blks <- paste0("Blocks: ", paste_enum(as.character(x), quotes = "\""))
  } else {
    blks <- "No blocks"
  }

  c(
    out,
    paste0("Name: \"", attr(x, "name"), "\""),
    strwrap(blks, exdent = 2)
  )
}

#' @export
print.stack <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
duplicated.stack <- function(x, incomparables = FALSE, ...) {
  duplicated(as.character(x), incomparables = incomparables, ...)
}

#' @export
anyDuplicated.stack <- function(x, incomparables = FALSE, ...) {
  anyDuplicated(as.character(x), incomparables = incomparables, ...)
}

#' @export
as.character.stack <- function(x, ...) c(unclass(x))

#' @export
as.list.stack <- function(x, ...) {
  list(
    blocks = as.character(x),
    name = stack_name(x)
  )
}

#' @rdname new_stack
#' @export
as_stack <- function(x) UseMethod("as_stack")

#' @export
as_stack.stack <- function(x) validate_stack(x)

#' @export
as_stack.character <- function(x) new_stack(x)

#' @export
as_stack.list <- function(x) {
  do.call(new_stack, x[c("blocks", "name")])
}

#' @export
vec_restore.stack <- function(x, to, ...) {
  validate_stack(NextMethod())
}

#' @importFrom generics intersect
#' @export
intersect.stack <- function(x, y, ...) {
  vec_restore(intersect(as.character(x), as.character(y), ...), x)
}

#' @importFrom generics union
#' @export
union.stack <- function(x, y, ...) {
  vec_restore(union(as.character(x), as.character(y), ...), x)
}

#' @importFrom generics setdiff
#' @export
setdiff.stack <- function(x, y, ...) {
  vec_restore(setdiff(as.character(x), as.character(y), ...), x)
}

#' @importFrom generics setequal
#' @export
setequal.stack <- function(x, y, ...) {
  setequal(as.character(x), as.character(y), ...)
}

#' @importFrom generics is.element
#' @export
is.element.stack <- function(el, set, ...) {
  is.element(as.character(el), as.character(set), ...)
}

#' @export
`[.stack` <- function(x, i, ...) {
  vec_slice(as.character(x), vec_as_location(i, length(x)))
}

#' @export
`[[.stack` <- function(x, i, ...) {
  vec_slice(as.character(x), vec_as_location2(i, length(x)))
}

#' @export
`[<-.stack` <- function(x, i, ..., value) {
  blockr_abort(
    "Subassignment of stack objects is not suported. Use set operations ",
    "instead.",
    class = "stack_subassignment_invalid"
  )
}

#' @export
`[[<-.stack` <- function(x, i, ..., value) {
  blockr_abort(
    "Subassignment of stack objects is not suported. Use set operations",
    "instead.",
    class = "stack_subassignment_invalid"
  )
}

#' @export
c.stack <- function(...) {

  res <- unlist(
    lapply(list(...), harmonize_list_of_stacks),
    recursive = FALSE
  )

  as_stacks(res)
}

reactives <- function(...) {

  args <- list(...)

  keys <- names(args)

  if (is.null(keys)) {
    keys <- character(length(args))
  }

  unnamed <- !nzchar(keys)
  keys[unnamed] <- positional_key(which(unnamed))

  if (anyDuplicated(keys)) {
    blockr_abort(
      "Each `reactives()` entry needs a distinct name.",
      class = "reactives_key_collision"
    )
  }

  res <- structure(
    list(
      store = new.env(parent = emptyenv()),
      keys = reactiveVal(character())
    ),
    class = "reactives"
  )

  for (i in seq_along(args)) {
    set_reactive(res, keys[[i]], args[[i]])
  }

  res
}

# An unnamed slot still needs a store key; it gets a hidden synthetic one,
# prefixed with a control character so `names()` can blank it while a real
# name (even "1") survives.
positional_marker <- function() {
  intToUtf8(1L)
}

positional_key <- function(i) {
  paste0(positional_marker(), i)
}

is_positional_key <- function(keys) {
  startsWith(keys, positional_marker())
}

is_reactives <- function(x) {
  inherits(x, "reactives")
}

raw_keys <- function(x) {
  .subset2(x, "keys")()
}

display_names <- function(keys) {

  pos <- is_positional_key(keys)

  if (all(pos)) {
    return(NULL)
  }

  replace(keys, pos, "")
}

set_reactive <- function(x, key, fun) {

  store <- .subset2(x, "store")

  if (exists(key, store, inherits = FALSE)) {
    rm(list = key, envir = store)
  }

  makeActiveBinding(key, fun, store)

  keys <- .subset2(x, "keys")

  if (!key %in% isolate(keys())) {
    keys(c(isolate(keys()), key))
  }

  invisible(x)
}

drop_reactive <- function(x, key) {

  rm(list = key, envir = .subset2(x, "store"))

  keys <- .subset2(x, "keys")
  keys(setdiff(isolate(keys()), key))

  invisible(x)
}

reorder_reactives <- function(x, order) {

  keys <- .subset2(x, "keys")
  cur <- isolate(keys())

  if (is.numeric(order)) {
    order <- cur[vec_as_location(order, length(cur))]
  }

  if (!setequal(order, cur) || anyDuplicated(order)) {
    blockr_abort(
      "`order` must list each slot exactly once; unnamed slots by position.",
      class = "reactives_bad_reorder"
    )
  }

  keys(order)

  invisible(x)
}

get_reactive <- function(x, key) {

  store <- .subset2(x, "store")

  if (is_string(key) && exists(key, store, inherits = FALSE)) {
    get(key, envir = store)
  } else {
    NULL
  }
}

#' @export
`$.reactives` <- function(x, name) {
  get_reactive(x, name)
}

#' @export
`[[.reactives` <- function(x, i) {

  if (is.numeric(i)) {
    i <- raw_keys(x)[[vec_as_location2(i, length(x))]]
  }

  get_reactive(x, i)
}

#' @export
names.reactives <- function(x) {
  display_names(raw_keys(x))
}

#' @export
length.reactives <- function(x) {
  length(raw_keys(x))
}

#' @export
as.list.reactives <- function(x, ...) {

  keys <- raw_keys(x)

  set_names(lapply(keys, get_reactive, x = x), display_names(keys))
}

#' @export
print.reactives <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.reactives <- function(x, ...) {
  paste0("<reactives[", length(isolate(raw_keys(x))), "]>")
}

assign_value <- function(x, key, value) {

  if (!key %in% isolate(raw_keys(x))) {
    blockr_abort(
      "Cannot assign to unknown slot {key}.",
      class = "reactives_unknown_slot"
    )
  }

  store <- .subset2(x, "store")
  store[[key]] <- value

  invisible(x)
}

#' @export
`$<-.reactives` <- function(x, name, value) {
  assign_value(x, name, value)
}

#' @export
`[[<-.reactives` <- function(x, i, value) {
  assign_value(x, if (is.numeric(i)) raw_keys(x)[[i]] else i, value)
}

#' @export
`[<-.reactives` <- function(x, i, value) {

  keys <- if (is.numeric(i)) raw_keys(x)[vec_as_location(i, length(x))] else i

  value <- if (is.list(value)) value else list(value)

  for (j in seq_along(keys)) {
    set_reactive(x, keys[[j]], value[[j]])
  }

  invisible(x)
}

#' @export
`[.reactives` <- function(x, i) {

  keys <- if (is.numeric(i)) raw_keys(x)[vec_as_location(i, length(x))] else i

  store <- .subset2(x, "store")

  res <- reactives()

  for (key in keys) {
    set_reactive(res, key, activeBindingFunction(key, store))
  }

  res
}

#' @export
`names<-.reactives` <- function(x, value) {

  keys <- raw_keys(x)

  stopifnot(length(value) == length(keys))

  unnamed <- !nzchar(value)
  value[unnamed] <- positional_key(which(unnamed))

  if (anyDuplicated(value)) {
    blockr_abort(
      "Each `reactives()` entry needs a distinct name.",
      class = "reactives_key_collision"
    )
  }

  store <- .subset2(x, "store")
  funs <- lapply(keys, activeBindingFunction, env = store)

  for (key in keys) {
    rm(list = key, envir = store)
  }

  for (j in seq_along(value)) {
    makeActiveBinding(value[[j]], funs[[j]], store)
  }

  .subset2(x, "keys")(value)

  invisible(x)
}

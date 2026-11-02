declared_front_ends <- new.env()

#' Front-ends
#'
#' A front-end is the module that draws a board, which a package declares in
#' R, from its `.onLoad()`, with `register_front_end()`: under a name, which
#' gives it the ID `{pkg}-{name}`, as a registry id is, with its module, made
#' by [js_module()], whose package is the one that declares it, and the board
#' class it draws. A module made outside a package declares no front-end.
#' Core declares its basic front-end, `blockr.core-basic`, for `board`, and a
#' package may declare several front-ends, each for a class of its own, since
#' two front-ends declared for one class are an error naming both. Declaring a
#' front-end again under the same ID replaces it, as when a package is loaded
#' again.
#'
#' R picks a board's front-end wherever it holds the board as an R object, the
#' one an app script builds and the one [blockr_deser()] builds from a file of
#' today's stack: `board_front_end()` gives the ID of the front-end declared
#' for the most specific class in the board's class vector that has one, so
#' that a board of a class extending `dock_board` that no package declares a
#' front-end for is drawn by the dock, and every board by core's basic
#' front-end at least. A board given as JSON keeps the front-end it names in
#' its `front_end.id`, since it carries no class.
#'
#' @param name The name part of the front-end's ID
#' @param module The front-end's module, made by [js_module()]
#' @param class The board class the front-end draws
#'
#' @examples
#' \dontrun{
#' # In the `.onLoad()` of a package such as blockr.dock
#' register_front_end(
#'   "dock",
#'   js_module("js/dock.js"),
#'   class = "dock_board"
#' )
#' }
#'
#' board_front_end(new_board(name = "My board"))
#'
#' @return The declaration `register_front_end()` returns the front-end's
#' entry invisibly, a list holding its `id`, `name`, `package`, `module` and
#' `class`, while `board_front_end()` returns a front-end's ID.
#'
#' @export
register_front_end <- function(name, module, class) {

  if (!is_js_module(module)) {
    blockr_abort(
      "A front-end's module is expected to be made by `js_module()`.",
      class = "front_end_invalid"
    )
  }

  id <- declared_id(name, module[["package"]], "front-end", "front_end_invalid")

  if (!is_nonempty_string(class)) {
    blockr_abort(
      "The class that front-end {id} draws is expected to be a non-empty ",
      "string.",
      class = "front_end_invalid"
    )
  }

  taken <- front_end_classes()
  others <- setdiff(names(taken)[taken == class], id)

  if (length(others)) {
    blockr_abort(
      "Front-ends {others} and {id} are both declared for board class ",
      "{class}.",
      class = "front_end_class_taken"
    )
  }

  entry <- list(
    id = id,
    name = name,
    package = module[["package"]],
    module = module,
    class = class
  )

  assign(id, entry, envir = declared_front_ends)

  invisible(entry)
}

#' @param x Board
#' @rdname register_front_end
#' @export
board_front_end <- function(x) {

  stopifnot(is_board(x))

  res <- most_specific(class(x), front_end_classes())

  if (is.null(res)) {
    blockr_abort(
      "No front-end is declared for board class{?es} {class(x)}.",
      class = "front_end_missing"
    )
  }

  res
}

# Of `taken`, the class that each front-end or renderer is for, by its id, the
# id for the most specific class in `class` that has one, as S3 dispatch picks
# a method, or `NULL` where no class has one.
most_specific <- function(class, taken) {

  hit <- names(taken)[match(class, taken)]
  hit <- hit[!is.na(hit)]

  if (length(hit)) {
    hit[1L]
  }
}

front_end_classes <- function() {
  ids <- list_front_ends()
  chr_xtr(lapply(set_names(nm = ids), get_front_end), "class", use_names = TRUE)
}

list_front_ends <- function() {
  ls(envir = declared_front_ends)
}

get_front_end <- function(id) {
  get_declared(id, declared_front_ends, "front-end", "front_end_missing")
}

declared_id <- function(name, package, what, class) {

  check_declaring_package(package, what, class)

  if (!is_nonempty_string(name)) {
    blockr_abort(
      "The name of a {what} of package {package} is expected to be a ",
      "non-empty string.",
      class = class
    )
  }

  paste0(package, "-", name)
}

get_declared <- function(id, env, what, class) {

  res <- get0(id, envir = env, inherits = FALSE)

  if (is.null(res)) {
    blockr_abort("No {what} is declared as {id}.", class = class)
  }

  res
}

check_declaring_package <- function(package, what, class) {

  if (!is_nonempty_string(package)) {
    blockr_abort(
      "A {what} is declared by a package, from its `.onLoad()`.",
      class = class
    )
  }

  invisible(package)
}

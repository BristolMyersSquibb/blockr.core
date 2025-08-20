#' Board options
#'
#' User settings at the board level are managed by a `board_options` object.
#' This can be constructed via `new_board_options()` and in case the set of
#' user options is to be extended, the constructor is designed with sub-classing
#' in mind. Consequently, the associated validator `validate_board_options()`
#' is available as S3 generic. Inheritance checking is available as
#' `is_board_options()` and coercion as `as_board_options()`. The currently set
#' options for a board object can be retrieved with `board_options()` and option
#' names are available as `list_board_options()`, which is short for
#' `names(board_options(.))`. Finally, in order to extract the value of a
#' specific option, `board_option()` can be used.
#'
#' @param ... Options passed as individual arguments
#'
#' @examples
#' opt <- new_board_options(
#'    new_board_name_option(),
#'    new_page_size_option()
#'  )
#'
#' is_board_options(opt)
#' list_board_options(opt)
#'
#' board_option("page_size", opt)
#'
#' @return All of `new_board_options()`, `as_board_options()` and
#' `board_options()` return a `board_options` object, as does the validator
#' `validate_board_options()`, which is typically called for side effects of
#' throwing errors is validation does not pass. Inheritance checking as
#' `is_board_options()` returns a scalar logical, while `list_board_options()`
#' returns a character vector of option names. Finally, `board_option()` returns
#' the current value for a specific board option, which in principle may be any
#' R object, but typically we have values such as strings or scalar integers
#' and logicals.
#'
#' @export
new_board_options <- function(...) {

  res <- structure(
    lapply(list(...), as_board_option),
    class = "board_options"
  )

  validate_board_options(res)

  res
}

#' @rdname new_board_options
#' @export
default_board_options <- function() {
  new_board_options(
    new_board_name_option(),
    new_n_rows_option(),
    new_page_size_option(),
    new_filter_rows_option(),
    new_thematic_option(),
    new_dark_mode_option()
  )
}

#' @param x Board options object
#' @rdname new_board_options
#' @export
is_board_options <- function(x) {
  inherits(x, "board_options")
}

#' @rdname new_board_options
#' @export
as_board_options <- function(x) {
  UseMethod("as_board_options", x)
}

#' @rdname new_board_options
#' @export
as_board_options.board_options <- function(x) {
  x
}

#' @rdname new_board_options
#' @export
as_board_options.board_option <- function(x) {
  new_board_options(x)
}

#' @rdname new_board_options
#' @export
as_board_options.list <- function(x) {
  do.call(new_board_options, x)
}

#' @rdname new_board_options
#' @export
as_board_options.board <- function(x) {
  board_options(x)
}

#' @rdname new_board_options
#' @export
validate_board_options <- function(x) {
  UseMethod("validate_board_options", x)
}

#' @rdname new_board_options
#' @export
validate_board_options.board_options <- function(x) {

  if (!is_board_options(x)) {
    abort(
      "Expecting board options to inherit from `board_options`.",
      class = "board_options_inheritance_invalid"
    )
  }

  for (opt in x) {
    validate_board_option(opt)
  }

  ids <- chr_ply(x, board_option_id)

  if (length(unique(ids)) != length(x)) {
    abort(
      "Non-unique board options IDs are being used.",
      class = "board_options_duplicated_ids"
    )
  }

  invisible(x)
}

#' @export
as.list.board_options <- function(x, ...) {
  unclass(x)
}

#' @rdname new_board_options
#' @export
list_board_options <- function(x) {

  if (is_board(x)) {
    x <- board_options(x)
  }

  if (!is_board_options(x)) {
    abort(
      "Expecting a `board_options` object to be passed as `x`.",
      class = "board_option_x_invalid"
    )
  }

  chr_ply(x, board_option_id)
}

#' @rdname new_board_options
#' @export
board_option_values <- function(x) {

  if (is_board(x)) {
    x <- board_options(x)
  }

  set_names(
    lapply(x, board_option_value),
    chr_ply(x, board_option_id)
  )
}

#' @param opt Board option
#' @rdname new_board_options
#' @export
board_option <- function(opt, x) {

  x <- as_board_options(x)

  if (!is_board_options(x)) {
    abort(
      "Expecting a `board_options` object to be passed as `x`.",
      class = "board_option_x_invalid"
    )
  }

  if (!is_string(opt)) {
    abort(
      "Expecting a string to be passed as `opt`.",
      class = "board_option_opt_invalid"
    )
  }

  if (!opt %in% list_board_options(x)) {
    abort(
      "Expecting `opt` to be a component of `x`.",
      class = "board_option_opt_invalid"
    )
  }

  x[[match(opt, chr_ply(x, board_option_id))]]
}

#' @rdname board_ui
#' @export
board_ui.board_options <- function(id, x, ...) {
  do.call(tagList, lapply(x, board_option_ui, id))
}

board_option_to_userdata <- function(x, board, input, session = get_session()) {

  stopifnot(is_board_option(x), is_board(board))

  id <- board_option_id(x)
  rv <- reactiveVal(board_option_value(x))

  res <- board_option_server(x, board, session)
  obs <- observeEvent(
    input[[id]],
    {
      new <- board_option_value(x, input[[id]])

      if (!identical(new, rv())) {
        log_debug("setting option ", id)
        rv(new)
      }
    }
  )

  attr(rv, "observers") <- c(
    list(obs),
    if (is.list(res) && all(lgl_ply(res, inherits, "Observer"))) {
      res
    } else if (inherits(res, "Observer")) {
      list(res)
    } else {
      abort(
        paste0(
          "Expecting a `board_option` server function to return either a ",
          "single or list of observers."
        ),
        class = "invalid_board_option_server_return_value"
      )
    }
  )

  env <- session$userData

  if (!exists("board_options", envir = env, inherits = FALSE)) {
    assign("board_options", list(), envir = env, inherits = FALSE)
  }

  env$board_options[[id]] <- rv

  invisible()
}

board_options_to_userdata <- function(board, input,
                                      options = board_options(board),
                                      session = get_session()) {

  stopifnot(is_board(board), is_board_options(options))

  for (opt in options) {
    board_option_to_userdata(opt, board, input, session)
  }

  invisible()
}

clear_board_options <- function(session) {

  env <- session$userData

  if (exists("board_options", envir = env, inherits = FALSE)) {
    for (opt in env$board_options) {
      for (obs in attr(opt, "observer")) {
        obs$destroy()
      }
    }
  }

  assign("board_options", list(), envir = env, inherits = FALSE)

  invisible()
}

update_board_options <- function(new, session = get_session()) {

  new <- as_board_options(new)

  val <- set_names(
    lapply(new, board_option_value),
    chr_ply(new, board_option_id)
  )

  env <- session$userData

  stopifnot(
    exists("board_options", envir = env, inherits = FALSE),
    all(names(val) %in% names(env$board_options))
  )

  for (i in names(val)) {
    env$board_options[[i]](val[[i]])
  }

  invisible()
}

#' @param session Shiny session
#' @rdname new_board_options
#' @export
get_board_option_value <- function(opt, session = get_session()) {

  env <- session$userData

  stopifnot(is_string(opt), is.environment(env))

  if (!exists("board_options", envir = env, inherits = FALSE)) {
    assign("board_options", list(), envir = env, inherits = FALSE)
  }

  if (!opt %in% names(env$board_options)) {
    abort(
      paste0("Could not find option `", opt, "`."),
      class = "board_option_not_found"
    )
  }

  env$board_options[[opt]]()
}

get_board_option_values <- function(...,
  if_not_found = c("error", "default", "null"),
  session = get_session()) {

  fun <- switch(
    match.arg(if_not_found),
    error = get_board_option_value,
    default = get_board_option_or_default,
    null = get_board_option_or_null
  )

  lapply(set_names(nm = c(...)), fun, session = session)
}

#' @param opts Board options
#' @rdname new_board_options
#' @export
get_board_option_or_default <- function(opt, opts = default_board_options(),
                                        session = get_session()) {
  tryCatch(
    get_board_option_value(opt, session),
    board_option_not_found = function(e) {
      board_option_value(board_option(opt, opts))
    }
  )
}

#' @rdname new_board_options
#' @export
get_board_option_or_null <- function(opt, session = get_session()) {
  tryCatch(
    get_board_option_value(opt, session),
    board_option_not_found = function(e) NULL
  )
}

#' @export
format.board_options <- function(x, ...) {
  c(
    paste0("<", class(x)[1L], "[", length(x), "]>"),
    if (length(x)) paste0("  ", chr_ply(x, format))
  )
}

#' @export
print.board_options <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Board options
#'
#' User settings at the board level are managed by a `board_options` object.
#' This can be constructed via `new_board_options()` and in case the set of
#' user options is to be extended, the constructor is designed with sub-classing
#' in mind. Consequently, the associated validator `validate_board_options()`
#' is available as S3 generic. Inheritance checking is available as
#' `is_board_options()` and coercion as `as_board_options()`.
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
#' names(opt)
#'
#' opt[["page_size"]]
#'
#' @return All of `new_board_options()` and `as_board_options()` return a
#' `board_options` object, as does the validator `validate_board_options()`,
#' which is typically called for side effects of throwing errors is validation
#' does not pass. Inheritance checking as `is_board_options()` returns a scalar
#' logical, while `board_option_values()` returns a named list of option values.
#'
#' @export
new_board_options <- function(...) {

  res <- new_vctr(
    list_to_list_of_opts(list(...)),
    class = "board_options"
  )

  validate_board_options(res)

  res
}

#' @rdname new_board_options
#' @export
default_board_options <- function(...) {
  new_board_options(
    new_board_name_option(...),
    new_n_rows_option(...),
    new_page_size_option(...),
    new_filter_rows_option(...),
    new_thematic_option(...),
    new_dark_mode_option(...)
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
  validate_board_options(x)
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
  validate_board_options(x[["options"]])
}

#' @rdname new_board_options
#' @export
validate_board_options <- function(x) {

  if (!is_board_options(x)) {
    abort(
      "Expecting board options to inherit from `board_options`.",
      class = "board_options_inheritance_invalid"
    )
  }

  for (opt in x) {
    validate_board_option(opt)
  }

  ids <- names(x)

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

  res <- unclass(x)

  names(res) <- chr_ply(res, board_option_id)

  res
}

#' @export
names.board_options <- function(x) {
  chr_ply(x, board_option_id)
}

#' @rdname new_board_options
#' @export
board_option_values <- function(x) {

  x <- as_board_options(x)

  set_names(
    lapply(x, board_option_value),
    names(x)
  )
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
  trg <- board_option_trigger(x)
  exp <- as.call(
    c(
      quote(`{`),
      lapply(trg, function(v) substitute(input[[v]], list(v = v)))
    )
  )

  obs <- observeEvent(
    exp,
    {
      if (is_scalar(trg)) {
        new <- board_option_value(x, input[[trg]])
      } else {
        new <- board_option_value(
          x,
          lapply(set_names(nm = trg), function(v) input[[v]])
        )
      }

      if (!identical(new, rv())) {
        log_debug("setting option ", id)
        rv(new)
      }
    },
    event.quoted = TRUE
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
                                      options = as_board_options(board),
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

#' @param opt Option name
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

get_board_option_values <- function(..., if_not_found = "error",
                                    session = get_session()) {

  fun <- switch(
    match.arg(if_not_found, c("error", "default", "null")),
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
      board_option_value(opts[[opt]])
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

#' @export
c.board_options <- function(...) {
  as_board_options(list_to_list_of_opts(list(...)))
}

#' @export
`[.board_options` <- function(x, i, ...) {

  res <- opts_slice(x, vec_as_location(i, length(x), names(x)))

  if (!length(res)) {
    return(new_board_options())
  }

  res
}

#' @export
`[<-.board_options` <- function(x, i, ..., value) {
  validate_board_options(NextMethod())
}

#' @export
`[[.board_options` <- function(x, i, ...) {
  validate_board_option(
    .subset2(x, vec_as_location2(i, length(x), names(x)))
  )
}

#' @export
`[[<-.board_options` <- function(x, i, ..., value) {
  validate_board_options(NextMethod())
}

opts_slice <- function(...) {
  validate_board_options(vec_slice(...))
}

harmonize_list_of_opts <- function(x) {
  if (is_board_option(x)) {
    list(x)
  } else if (is_board_options(x)) {
    as.list(x)
  } else if (is.list(x) && all(lgl_ply(x, is_board_option))) {
    x
  } else if (is.list(x) && any(lgl_ply(x, is_board_options))) {
    c(
      x[lgl_ply(x, Negate(is_board_options))],
      unlst(lapply(x[lgl_ply(x, is_board_options)], as.list))
    )
  } else {
    list(as_board_option(x))
  }
}

list_to_list_of_opts <- function(x) {
  unlst(lapply(x, harmonize_list_of_opts))
}

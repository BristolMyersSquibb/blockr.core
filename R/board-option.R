#' @param id Board option ID
#' @param default Default value
#' @param ui Option UI
#' @param server (Optional) option server
#' @param update_trigger Shiny `input` entry/entries that trigger an update
#' @param transform (Optional) transform function
#' @param ctor,pkg Constructor information (used for serialization)

#' @rdname new_board_options
#' @export
new_board_option <- function(id, default, ui,
                             server = function(board, session) {},
                             update_trigger = id,
                             transform = identity,
                             ctor = sys.parent(), pkg = NULL, ...) {

  res <- structure(
    list(ui = ui, server = server, transform = transform),
    default = default,
    id = id,
    trigger = update_trigger,
    ctor = resolve_ctor(ctor, pkg),
    class = c(paste0(id, "_option"), "board_option"),
    ...
  )

  validate_board_option(res)

  res
}

#' @rdname new_board_options
#' @export
is_board_option <- function(x) {
  inherits(x, "board_option")
}

#' @rdname new_board_options
#' @export
validate_board_option <- function(x) {
  UseMethod("validate_board_option", x)
}

#' @rdname new_board_options
#' @export
as_board_option <- function(x, ...) {
  UseMethod("as_board_option", x)
}

#' @rdname new_board_options
#' @export
as_board_option.board_option <- function(x, ...) {
  x
}

#' @rdname new_board_options
#' @export
board_option_id <- function(x) {
  stopifnot(is_board_option(x))
  attr(x, "id")
}

#' @rdname new_board_options
#' @export
board_option_trigger <- function(x) {
  stopifnot(is_board_option(x))
  attr(x, "trigger")
}

#' @rdname new_board_options
#' @export
board_option_default <- function(x) {
  stopifnot(is_board_option(x))
  attr(x, "default")
}

#' @rdname new_board_options
#' @export
board_option_ui <- function(x, id = NULL) {

  stopifnot(is_board_option(x))

  fun <- x[["ui"]]

  if (is.null(id)) {
    return(fun)
  }

  fun(id)
}

#' @rdname new_board_options
#' @export
board_option_server <- function(x, ...) {

  stopifnot(is_board_option(x))

  fun <- x[["server"]]

  if (...length()) {
    return(fun(...))
  }

  fun
}

#' @rdname new_board_options
#' @export
board_option_transform <- function(x) {
  stopifnot(is_board_option(x))
  x[["transform"]]
}

#' @param value Option value
#' @rdname new_board_options
#' @export
board_option_value <- function(x, value = board_option_default(x)) {
  trans <- board_option_transform(x)
  trans(value)
}

#' @rdname new_board_options
#' @export
validate_board_option.default <- function(x) {

  if (!is_board_option(x)) {
    abort(
      "Expecting a board option to inherit from `board_option`.",
      class = "board_option_inheritance_invalid"
    )
  }

  if (!is_string(board_option_id(x))) {
    abort(
      "Expecting a board option ID to be string-valued.",
      class = "board_option_component_invalid"
    )
  }

  trigger <- board_option_trigger(x)

  if (!is.null(trigger) && (!is.character(trigger) || !length(trigger))) {
    abort(
      "Expecting a board option trigger to be a non-zero length character",
      "vector or `NULL`.",
      class = "board_option_component_invalid"
    )
  }

  ui <- board_option_ui(x)

  if (!is.function(ui) || !identical(names(formals(ui)), "id")) {
    abort(
      "Expecting a board option UI function to have a single argument `id`.",
      class = "board_option_component_invalid"
    )
  }

  srv <- board_option_server(x)
  arg <- names(formals(srv))

  if (!is.function(srv) || !identical(arg, c("board", "session"))) {
    abort(
      "Expecting a board option server function to have argument{?s} {arg}.",
      class = "board_option_component_invalid"
    )
  }

  tfm <- board_option_transform(x)

  if (!is.function(tfm) || !identical(names(formals(tfm)), "x")) {
    abort(
      "Expecting a board option transform to have a single argument `x`.",
      class = "board_option_component_invalid"
    )
  }

  invisible(x)
}

#' @export
format.board_option <- function(x, session = get_session(), ...) {

  format_val <- function(x, n = 3L) {

    if (length(x) > 1) {

      res <- paste(chr_ply(utils::head(x, n = n), format), collapse = ", ")

      if (length(x) > n) {
        res <- paste0(res, ", ...")
      }

      return(res)
    }

    format(x)
  }

  id <- board_option_id(x)
  vl <- board_option_value(x)

  paste0(
    id, ": ",
    if (is.null(session)) {
      format_val(vl, 5L)
    } else {
      curr <- get_board_option_or_null(id, session)
      if (is.null(curr)) {
        format_val(vl, 5L)
      } else {
        paste0(format_val(curr, 3L), " (", format_val(vl, 3L), ")")
      }
    }
  )
}

#' @export
print.board_option <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
c.board_option <- function(...) {
  as_board_options(list_to_list_of_opts(list(...)))
}

#' @rdname new_board_options
#' @export
new_board_name_option <- function(value = "Board", ...) {

  new_board_option(
    id = "board_name",
    default = value,
    ui = function(id) {
      textInput(
        NS(id, "board_name"),
        "Board name",
        value
      )
    },
    server = function(board, session) {
      observeEvent(
        get_board_option_or_null("board_name", session),
        {
          updateTextInput(
            session,
            "board_name",
            value = get_board_option_value("board_name", session)
          )
        }
      )
    },
    ...
  )
}

#' @export
validate_board_option.board_name_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_string(val)) {
    abort(
      "Expecting `board_name` to be string-valued.",
      class = "board_name_option_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
new_n_rows_option <- function(value = blockr_option("n_rows", 50L), ...) {

  new_board_option(
    id = "n_rows",
    default = value,
    ui = function(id) {
      numericInput(
        NS(id, "n_rows"),
        "Preview rows",
        value,
        min = 1L,
        step = 1L
      )
    },
    server = function(board, session) {
      observeEvent(
        get_board_option_or_null("n_rows", session),
        {
          updateNumericInput(
            session,
            "n_rows",
            value = get_board_option_value("n_rows", session)
          )
        }
      )
    },
    transform = function(x) as.integer(x),
    ...
  )
}

#' @export
validate_board_option.n_rows_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_count(val)) {
    abort(
      "Expecting `n_rows` to represent a count.",
      class = "board_options_n_rows_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
new_page_size_option <- function(value = blockr_option("page_size", 5L), ...) {

  new_board_option(
    id = "page_size",
    default = value,
    ui = function(id) {
      selectInput(
        NS(id, "page_size"),
        "Preview page size",
        c(5, 10, 25, 50, 100),
        value
      )
    },
    server = function(board, session) {
      observeEvent(
        get_board_option_or_null("page_size", session),
        {
          updateSelectInput(
            session,
            "page_size",
            selected = get_board_option_value("page_size", session)
          )
        }
      )
    },
    transform = function(x) as.integer(x),
    ...
  )
}

#' @export
validate_board_option.page_size_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_count(val)) {
    abort(
      "Expecting `page_size` to represent a count.",
      class = "board_options_page_size_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
new_filter_rows_option <- function(value = blockr_option("filter_rows",
                                                         FALSE),
                                   ...) {

  new_board_option(
    id = "filter_rows",
    default = value,
    ui = function(id) {
      bslib::input_switch(
        NS(id, "filter_rows"),
        "Enable preview search",
        value
      )
    },
    server = function(board, session) {
      observeEvent(
        get_board_option_or_null("filter_rows", session),
        {
          bslib::toggle_switch(
            "filter_rows",
            value = get_board_option_value("filter_rows", session),
            session = session
          )
        }
      )
    },
    ...
  )
}

#' @export
validate_board_option.filter_rows_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_bool(val)) {
    abort(
      "Expecting `filter_rows` to represent a boolean.",
      class = "board_options_filter_rows_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
new_thematic_option <- function(value = blockr_option("thematic", NULL), ...) {

  new_board_option(
    id = "thematic",
    default = value,
    ui = function(id) {
      if (is_pkg_avail("thematic")) {
        bslib::input_switch(
          NS(id, "thematic"),
          "Enable thematic",
          coal(value, FALSE)
        )
      }
    },
    server = function(board, session) {
      observeEvent(
        get_board_option_or_null("thematic", session),
        {
          bslib::toggle_switch(
            "thematic",
            value = get_board_option_value("thematic", session),
            session = session
          )
        }
      )
    },
    ...
  )
}

#' @export
validate_board_option.thematic_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!(is.null(val) || is_bool(val))) {
    abort(
      "Expecting `thematic` to be `NULL` or a boolean.",
      class = "board_options_thematic_invalid"
    )
  }

  if (isTRUE(val) && !is_pkg_avail("thematic")) {
    abort(
      "Please install `thematic` to enable auto theming of plots.",
      class = "thematic_not_installed"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
new_dark_mode_option <- function(value = blockr_option("dark_mode", NULL),
                                 ...) {

  if (isTRUE(value)) {
    value <- "dark"
  }

  if (isFALSE(value)) {
    value <- "light"
  }

  if (length(value) && is.na(value)) {
    value <- NULL
  }

  new_board_option(
    id = "dark_mode",
    default = value,
    ui = function(id) {
      span(
        bslib::input_dark_mode(
          id = NS(id, "dark_mode"),
          mode = value
        ),
        tags$label(
          "Light/dark mode",
          style = "vertical-align: top; margin-top: 3px;"
        )
      )
    },
    server = function(board, session) {
      observeEvent(
        get_board_option_or_null("dark_mode", session),
        {
          bslib::toggle_dark_mode(
            mode = get_board_option_value("dark_mode", session),
            session = session
          )
        }
      )
    },
    ...
  )
}

#' @export
validate_board_option.dark_mode_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!(is.null(val) || (is_string(val) && val %in% c("light", "dark")))) {
    abort(
      "Expecting `dark_mode` to be either `NULL`, \"light\" or \"dark\".",
      class = "board_options_dark_mode_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
new_show_conditions_option <- function(value = blockr_option("show_conditions",
                                                             c("warning",
                                                               "error")),
                                       ...) {

  new_board_option(
    id = "show_conditions",
    default = value,
    ui = function(id) {
      selectInput(
        NS(id, "show_conditions"),
        "Message types to display",
        c("message", "warning", "error"),
        value,
        multiple = TRUE
      )
    },
    server = function(board, session) {
      observeEvent(
        get_board_option_or_null("show_conditions", session),
        {
          updateSelectInput(
            session,
            "show_conditions",
            selected = get_board_option_value("show_conditions", session)
          )
        }
      )
    },
    ...
  )
}

#' @export
validate_board_option.show_conditions_option <- function(x) {

  val <- board_option_value(NextMethod())

  opt <- c("message", "warning", "error")

  if (!(is.character(val) || all(val %in% opt))) {
    abort(
      "Expecting `show_conditions` to be any of {opt}.",
      class = "board_options_show_conditions_invalid"
    )
  }

  invisible(x)
}

default_chat <- function(system_prompt = NULL, params = NULL) {
  ellmer::chat_openai(
    system_prompt = system_prompt,
    params = params
  )
}

#' @param enable Enable (i.e. include) the llm model option
#' @rdname new_board_options
#' @export
need_llm_cfg_opts <- local(
  {
    state <- FALSE
    function(enable) {

      if (missing(enable)) {
        return(state)
      }

      stopifnot(is_bool(enable))

      if (isTRUE(enable) && !is_pkg_avail("ellmer")) {
        abort(
          "The ellmer package is required for including LLM options.",
          class = "ellmer_required_not_available"
        )
      }

      state <<- enable

      invisible(state)
    }
  }
)

#' @rdname new_board_options
#' @export
new_llm_model_option <- function(value = NA_character_, ...) {

  options <- blockr_option("chat_function", default_chat)

  if (!is.function(options) && length(options) == 1L) {

    if (is_string(value) && identical(value, names(options))) {
      value <- NA_character_
    }

    options <- options[[1L]]
  }

  if (is.na(value) && !is.function(options)) {
    value <- names(options)[1L]
  }

  new_board_option(
    id = "llm_model",
    default = value,
    ui = function(id) {
      if (is.function(options)) {
        return(NULL)
      }
      selectInput(
        NS(id, "llm_model"),
        "LLM Model",
        names(options),
        value
      )
    },
    server = function(board, session) {
      if (!is.function(options)) {
        observeEvent(
          get_board_option_or_null("llm_model", session),
          {
            opt <- get_board_option_value("llm_model", session)
            nme <- attr(opt, "chat_name")

            stopifnot(is_string(nme), nme %in% names(options))

            updateSelectInput(session, "llm_model", selected = nme)
          }
        )
      }
    },
    transform = function(x) {

      if (is.function(options)) {
        return(options)
      }

      structure(options[[x]], chat_name = x)
    },
    trigger = if (is.function(options)) NULL else "llm_model",
    ...
  )
}

#' @export
validate_board_option.llm_model_option <- function(x) {

  val <- board_option_value(NextMethod())
  arg <- list(system_prompt = NULL, params = NULL)

  if (!is.function(val) || !identical(arg, as.list(formals(val)))) {
    abort(
      "Expecting `llm_model` to be a function with arguments ",
      "{paste0(names(arg), \" = \", arg)}.",
      class = "board_options_llm_model_invalid"
    )
  }

  nme <- attr(val, "chat_name")
  opt <- blockr_option("chat_function", default_chat)

  is_loo <- is.list(opt) &&
    all(lgl_ply(opt, is.function)) &&
    length(unique(names(opt))) == length(opt)

  if (!(is.function(opt) || is_loo)) {
    abort(
      "Expecting the blockr option `chat_function` to be either a function ",
      "or a list of functions with unique names.",
      class = "board_options_llm_model_invalid"
    )
  }

  is_fun <- is.function(opt) || (is.list(opt) && length(opt) == 1L)

  if (is_fun && !is.null(nme)) {
    abort(
      "Expecting `llm_model` name contain no attribute `chat_name`.",
      class = "board_options_llm_model_invalid"
    )
  }

  nms <- names(opt)

  if (is.list(opt) && length(opt) > 1L && !(is_string(nme) && nme %in% nms)) {
    abort(
      "Expecting `llm_model` name contain a string-valued attribute ",
      "`chat_name` among options {nms}.",
      class = "board_options_llm_model_invalid"
    )
  }

  invisible(x)
}

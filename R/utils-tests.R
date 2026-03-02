#' Testing utilities
#'
#' Several utilities for unit testing, mainly with [shiny::testServer()] that
#' have proven themselves useful for testing this package are exported for
#' re-use in other packages.
#'
#' @param board A board object
#' @param mode Edit plugins, such as `manage_blocks` get an additional argument
#' `update` over read plugins such as `preserve_board`.
#'
#' @return For testing plugins, `generate_plugin_args()` returns objects that
#' mimic how plugins are called in the board server, `sink_msg()` is called
#' mainly for the side-effect of muting shiny messages (and returns them
#' invisibly), `with_mock_session()` returns `NULL` (invisibly) and
#' `with_mock_context()` returns the result of a call to
#' [shiny::withReactiveDomain()]. Finally, `get_s3_method()` returns a
#' class-specific implementation of the specified generic (and throws an error
#' if none is found).
#'
#' @rdname testing
#' @export
generate_plugin_args <- function(board, ..., mode = c("edit", "read")) {

  withr::local_envvar(BLOCKR_LOG_LEVEL = "")
  withr::local_options(blockr.log_level = "warn")

  session <- edit_plugin_args <- read_plugin_args <- NULL

  mode <- match.arg(mode)

  res_plugin_args <- list()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()
      res_plugin_args <<- switch(
        mode,
        edit = edit_plugin_args,
        read = read_plugin_args
      )
    },
    args = list(x = board, ...)
  )

  res_plugin_args
}

#' @param ... Forwarded to [utils::capture.output()]
#' @rdname testing
#' @export
sink_msg <- function(...) {
  invisible(utils::capture.output(..., type = "message"))
}

#' @rdname testing
#' @export
new_mock_session <- function() MockShinySession$new()

#' @inheritParams shiny::testServer
#' @rdname testing
#' @export
with_mock_session <- function(expr, session = new_mock_session()) {

  empty_module <- function() {
    moduleServer(rand_names(), function(input, output, session) { })
  }

  on.exit(if (!session$isClosed()) session$close())

  quosure <- rlang::enquo(expr)

  with_mock_context(session, empty_module())

  parent_clone <- rlang::env_clone(parent.env(session$env))
  clone <- rlang::env_clone(session$env, parent_clone)
  mask <- rlang::new_data_mask(clone, parent_clone)

  with_mock_context(
    session,
    rlang::eval_tidy(quosure, mask, rlang::caller_env())
  )

  invisible()
}

#' @rdname testing
#' @export
with_mock_context <- function(session, expr) {
  isolate(
    withReactiveDomain(
      session,
      {
        withr::with_options(
          list(shiny.allowoutputreads = TRUE),
          {
            shinyOptions(cache = session$appcache)
            expr
          }
        )
      }
    )
  )
}

#' @param generic Generic function name (passed as string)
#' @param object S3 Object
#' @rdname testing
#' @export
get_s3_method <- function(generic, object) {

  for (cls in class(object)) {
    res <- utils::getS3method(generic, cls, optional = TRUE)
    if (is.function(res)) {
      return(res)
    }
  }

  blockr_abort(
    "No function found for generic `{generic}()` and classes {class(object)}.",
    class = "generic_method_not_found"
  )
}

#' @param x Reactive object to use in [shiny::exportTestValues()]
#' @rdname testing
#' @export
export_safely <- function(x) {
  # https://github.com/rstudio/shiny/issues/3768
  r_quo <- rlang::enquo(x)
  rlang::inject(
    reactive(
      tryCatch(!!r_quo, error = function(e) e)
    )
  )
}

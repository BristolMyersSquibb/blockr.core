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
#' [shiny::withReactiveDomain()].
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

#' @param expr Expression
#' @param session Shiny session object
#' @rdname testing
#' @export
with_mock_session <- function(expr, session = MockShinySession$new()) {

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


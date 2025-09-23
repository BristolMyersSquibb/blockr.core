sink_msg <- function(...) {
  invisible(capture.output(..., type = "message"))
}

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

band_members <- data.frame(
  name = c("Mick", "John", "Paul"),
  band = c("Stones", "Beatles", "Beatles")
)

band_instruments <- data.frame(
  name = c("John", "Paul", "Keith"),
  plays = c("guitar", "bass", "guitar")
)

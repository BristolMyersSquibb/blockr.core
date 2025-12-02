#' Plot block constructors
#'
#' Blocks for data visualization using base R graphics can be created via
#' `new_plot_block()`.
#'
#' Due to the current block evaluation procedure, where block evaluation is
#' separated from block "rendering" (via  [shiny::renderPlot()]) integration of
#' base R graphics requires some mechanism to achieve this decoupling. This
#' is implemented by adding a `plot` attribute to the result of
#' [block_eval()], generated with [grDevices::recordPlot()] and containing the
#' required information to re-create the plot at a later time. As part of
#' [block_output()], the attribute is retrieved and passed to
#' [grDevices::replayPlot()]. Consequently, any block that inherits from
#' `plot_block` is required to support this type of decoupling.
#'
#' @param ... Forwarded to `new_plot_block()` and [new_block()]
#' @inheritParams new_block
#'
#' @return All blocks constructed via `new_plot_block()` inherit from
#' `plot_block`.
#'
#' @export
new_plot_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "plot_block"), ctor, ...)
}

#' @export
block_output.plot_block <- function(x, result, session) {
  renderPlot(evaluate::replay(result))
}

#' @export
block_ui.plot_block <- function(id, x, ...) {
  tagList(
    plotOutput(NS(id, "result"))
  )
}

#' @export
block_eval.plot_block <- function(x, expr, env, ...) {

  inherits_rec_plt <- function(x) {
    inherits(x, "recordedplot")
  }

  res <- evaluate::evaluate(expr, env, stop_on_error = 1L)

  show_condition(res)

  Filter(inherits_rec_plt, res)
}

#' @noRd
#' @export
block_eval_trigger.plot_block <- function(x, session = get_session()) {

  invisible(
    get_board_option_values(
      c("thematic", "dark_mode"),
      if_not_found = "null",
      session = session
    )
  )
}

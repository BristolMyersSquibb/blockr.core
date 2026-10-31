#' Plot block constructors
#'
#' Blocks for data visualization using base R graphics can be created via
#' `new_plot_block()`.
#'
#' Base R graphics draw as a side effect of evaluation, which is separated
#' from showing a block's result. The [block_eval()] method for `plot_block`
#' therefore evaluates the block expression with [evaluate::evaluate()] and
#' returns the plots recorded along the way, which contain the required
#' information to re-create a plot at a later time. Consequently, any block
#' that inherits from `plot_block` is required to support this type of
#' decoupling.
#'
#' @param ... Forwarded to `new_plot_block()` and [new_block()]
#' @inheritParams new_block
#'
#' @return All blocks constructed via `new_plot_block()` inherit from
#' `plot_block`.
#'
#' @seealso Real-world plot blocks built on `new_plot_block()` are provided by
#' the [blockr.ggplot](https://bristolmyerssquibb.github.io/blockr.ggplot/)
#' package.
#'
#' @export
new_plot_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "plot_block"), ctor, ...)
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

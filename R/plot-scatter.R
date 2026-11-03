#' @include block-class.R output-plot.R
#' @rdname new_scatter_block
#' @format NULL
#' @export
scatter_block <- new_block_class(
  "scatter_block",
  inputs = function(data) {
    if (!is.data.frame(data)) {
      blockr_abort(
        "The scatter block plots columns of a data frame, not of an object of ",
        "class {class(data)[1L]}.",
        class = "scatter_block_inputs_invalid"
      )
    }
  },
  expr = function(x, y) {
    if (!isTRUE(nzchar(x)) || !isTRUE(nzchar(y))) {
      blockr_invalid("Choose a column for each axis.")
    }
    bbquote(plot(.(data)[[.(x)]], .(data)[[.(y)]], xlab = .(x), ylab = .(y)))
  },
  ui = js_module("js/scatter-block.js"),
  requests = list(columns = function(data) colnames(data)),
  output = as_plot_output
)

#' Scatter block
#'
#' The scatter block draws a scatter plot of two columns of a data frame with
#' [graphics::plot()]. The block class `scatter_block` defines it, and its
#' constructor `new_scatter_block()` makes a block of the class (see
#' [new_block_class()]).
#'
#' @details
#' Mainly for demonstration purposes, this block draws a scatter plot using
#' [graphics::plot()]. In its current simplistic implementation, apart from axis
#' labels (fixed to the corresponding column names), no further plotting
#' options are available and for any "production" application, a more
#' sophisticated (set of) block(s) for data visualization will most likely be
#' required.
#'
#' @section Block class:
#' The block's data input `data` is checked to be a data frame.
#' Its state args `x` and `y` have no defaults, so a new block leaves them
#' unset and is `invalid` until something sets both. Its UI asks R for the
#' input's columns through the request `columns`. The block's [block_eval()]
#' method runs the block expression with [capture_plot()], so that its result
#' is the plot it draws, which shows through [as_plot_output()].
#'
#' @param x,y Columns to place on respective axes
#' @inheritParams new_head_block
#'
#' @block scatter plot block
#' @blockDescr Scatter plotting
#' @blockCategory plot
#' @blockIcon dice-5
#' @blockGuidance Draws a scatter plot of `x` against `y` with `base::plot()`.
#'   `x` and `y` are column names from the input data; axis labels follow the
#'   column names.
#' @blockKeywords scatter, scatter plot, chart, visualization, points
#' @blockArg x Column for the horizontal axis.
#'   [example] "Sepal.Length"
#'   [type] arg_string()
#' @blockArg y Column for the vertical axis.
#'   [example] "Sepal.Width"
#'   [type] arg_string()
#'
#' @examples
#' new_scatter_block(x = "Time", y = "demand")
#'
#' res <- evaluate_block(scatter_block, list(x = "Time", y = "demand"),
#'                       list(data = "a"), list(a = datasets::BOD))
#' res
#'
#' as_plot_output(res$result, info = list(width = 320, height = 240))
#'
#' @return The block class `scatter_block` is an object of class
#' `block_class`, and its constructor `new_scatter_block()` returns a block of
#' that class.
#'
#' @export
new_scatter_block <- block_constructor(scatter_block)

#' @export
block_eval.scatter_block <- function(x, expr, env, ...) {
  capture_plot(expr, env)
}

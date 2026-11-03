#' @include block-class.R output-table.R
#' @rdname new_fixed_block
#' @format NULL
#' @export
fixed_block <- new_block_class(
  "fixed_block",
  inputs = "data",
  expr = function(expr) str2lang(expr),
  ui = js_module("js/fixed-block.js"),
  output = as_table_output
)

#' Fixed block
#'
#' The fixed block applies a fixed transformation, R code that refers to its
#' data input as `data`, given as a string, such as `"utils::head(data, 3)"`.
#' The block class `fixed_block` defines it, and its constructor
#' `new_fixed_block()` makes a block of the class (see [new_block_class()]).
#'
#' @details
#' Mainly useful for testing and examples, this block applies a fixed
#' transformation to its data input. No UI elements are exposed and the
#' transformation consequently cannot be parametrized.
#'
#' @section Block class:
#' The block's state arg `expr` holds the code as a string, which `expr`
#' parses with [base::str2lang()], so that the constructor fails on a string
#' that does not parse and on anything but a string. The code writes the data
#' input bare, so the block expression calls a function of `data` with the
#' upstream block's ID, as
#' `(function(data) utils::head(data, 3))(data = a)` (see [interpolate()]).
#' Its module draws nothing, and its results show as a table, through
#' [as_table_output()].
#'
#' @param expr R code as a string
#' @inheritParams new_head_block
#'
#' @examples
#' new_fixed_block("utils::head(data, 3)")
#'
#' evaluate_block(fixed_block, list(expr = "utils::head(data, 3)"),
#'                list(data = "a"), list(a = datasets::iris))
#'
#' @return The block class `fixed_block` is an object of class `block_class`,
#' and its constructor `new_fixed_block()` returns a block of that class.
#'
#' @export
new_fixed_block <- block_constructor(fixed_block)

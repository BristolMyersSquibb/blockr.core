#' @include block-class.R output-table.R
#' @rdname new_rbind_block
#' @format NULL
#' @export
rbind_block <- new_block_class(
  "rbind_block",
  inputs = "...",
  expr = function() bbquote(rbind(...)),
  ui = js_module("js/rbind-block.js"),
  output = as_table_output
)

#' Row-bind block
#'
#' The row-bind block stacks the rows of any number of data frames, as
#' [base::rbind()] does. The block class `rbind_block` defines it, and its
#' constructor `new_rbind_block()` makes a block of the class (see
#' [new_block_class()]).
#'
#' @details
#' Row-wise concatenation of an arbitrary number of `data.frame`s, as performed
#' by [base::rbind()] is available as an `rbind_block`. This mainly serves as
#' an example for a variadic block, which takes its data inputs as `...`.
#'
#' @section Block class:
#' The block is variadic, so it runs once it has one link, and its block
#' expression binds the upstream blocks in link order, each named by its link's
#' input, such as `rbind(a, b2 = b)`. It has no state, and its results show as
#' a table, through [as_table_output()].
#'
#' @inheritParams new_head_block
#'
#' @block rbind block
#' @blockDescr Row-binding of datasets
#' @blockCategory transform
#' @blockIcon chevron-bar-expand
#' @blockGuidance Row-binds two or more inputs with `base::rbind()`; the inputs
#'   must share the same columns. Takes a variable number of data inputs and has
#'   no configurable arguments.
#' @blockKeywords rbind, bind, concatenate, rows, append, union
#'
#' @examples
#' new_rbind_block()
#'
#' evaluate_block(rbind_block, list(), list(... = list("a", "b")),
#'                list(a = datasets::BOD, b = datasets::BOD))
#'
#' @return The block class `rbind_block` is an object of class `block_class`,
#' and its constructor `new_rbind_block()` returns a block of that class.
#'
#' @export
new_rbind_block <- block_constructor(rbind_block)

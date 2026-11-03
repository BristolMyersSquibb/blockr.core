#' @include block-class.R output-table.R
#' @rdname new_static_block
#' @format NULL
#' @export
static_block <- new_block_class(
  "static_block",
  expr = function(expression) {

    if (!is_string(expression) || is.na(expression)) {
      blockr_abort(
        "The static block takes an R expression as a string, such as ",
        "\"datasets::iris\".",
        class = "block_code_invalid"
      )
    }

    str2lang(expression)
  },
  ui = js_module("js/static-block.js"),
  output = as_table_output
)

#' Static block
#'
#' The static block evaluates one R expression, given as a string, such as
#' `"datasets::iris"`, for its result. The block class `static_block` defines
#' it, and its constructor `new_static_block()` makes a block of the class (see
#' [new_block_class()]).
#'
#' @details
#' Mainly useful for testing and examples, this block returns what an R
#' expression evaluates to, such as a dataset. It holds the expression and not
#' the data, so its code reproduces the data wherever the expression can be
#' evaluated.
#'
#' @section Block class:
#' The block has no data inputs. Its state arg `expression` holds the
#' expression as a string, which `expr` parses with [base::str2lang()]. So the
#' constructor fails on a string that does not parse, and on anything but a
#' string, such as a data frame. The block's module draws nothing, and its
#' results show as a table, through [as_table_output()].
#'
#' @param expression An R expression as a string
#' @inheritParams new_head_block
#'
#' @examples
#' new_static_block("datasets::iris")
#'
#' evaluate_block(static_block, list(expression = "utils::head(datasets::BOD)"))
#'
#' try(new_static_block(datasets::iris))
#'
#' @return The block class `static_block` is an object of class `block_class`,
#' and its constructor `new_static_block()` returns a block of that class.
#'
#' @export
new_static_block <- block_constructor(static_block)

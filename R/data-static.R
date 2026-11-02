#' @include block-class.R output-table.R
#' @rdname new_static_block
#' @format NULL
#' @export
static_block <- new_block_class(
  "static_block",
  expr = function(data) {

    data <- code_string(data, "static block")
    code <- str2lang(data)

    if (!is.call(code)) {
      code <- call("(", code)
    }

    bbquote(.(code))
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
#' The block has no data inputs. Its state arg `data` holds the expression as a
#' string, which the block parses with [base::str2lang()], so that the
#' constructor fails on a string that does not parse and on anything but a
#' string, such as a data frame, while a script may give the expression quoted,
#' as `quote(datasets::iris)`, which the constructor turns into a string. The
#' block expression is the expression itself, in parentheses where it is a name
#' or a constant, such as `(iris)`. Its module draws nothing, and its results
#' show as a table, through [as_table_output()].
#'
#' @param data An R expression as a string
#' @inheritParams new_head_block
#'
#' @examples
#' new_static_block("datasets::iris")
#'
#' evaluate_block(static_block, list(data = "utils::head(datasets::iris)"))
#'
#' try(new_static_block(datasets::iris))
#'
#' @return The block class `static_block` is an object of class `block_class`,
#' and its constructor `new_static_block()` returns a block of that class.
#'
#' @export
new_static_block <- block_constructor(static_block)

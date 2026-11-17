#' @include block-class.R output-table.R
#' @rdname new_head_block
#' @format NULL
#' @export
head_block <- new_block_class(
  "head_block",
  inputs = function(data) {
    if (!is.data.frame(data)) {
      blockr_abort(
        "The head block takes a data frame, not an object of class ",
        "{class(data)[1L]}.",
        class = "head_block_inputs_invalid"
      )
    }
  },
  expr = function(n = 6L, direction = c("head", "tail")) {
    direction <- match.arg(direction)
    fn <- if (direction == "tail") quote(utils::tail) else quote(utils::head)
    bbquote(.(fn)(.(data), n = .(n)))
  },
  ui = js_module("js/head-block.js"),
  requests = list(n_max = function(data) nrow(data)),
  output = as_table_output
)

#' Head block
#'
#' The head block keeps the first or last `n` rows of a data frame, as
#' [utils::head()] and [utils::tail()] do. The block class `head_block`
#' defines it, and its constructor `new_head_block()` makes a block of the
#' class (see [new_block_class()]).
#'
#' @details
#' Row-subsetting the first or last `n` rows of a `data.frame` (as provided by
#' [utils::head()] and [utils::tail()]) is implemented as `head_block`. This
#' is an example of a block that takes a single `data.frame` as input and
#' produces a single `data.frame` as output.
#'
#' @section Block class:
#' The block's data input `data` is checked to be a data frame. Its UI asks R
#' for the number of rows of that input through the request `n_max`, which is
#' the maximum of its numeric input, and its results show as a table, through
#' [as_table_output()].
#'
#' @param n Number of rows
#' @param direction Either "head" or "tail"
#' @param block_name Name of the block, which defaults to one made from the
#'   name of its class, such as "Head"
#' @param locked The block's lock, which fixes its state where the board lock
#'   is on: `TRUE` locks the block, `FALSE` leaves it open to readers, and
#'   `NA`, the default, follows the board lock, which then locks it (see
#'   [locked-board])
#'
#' @block head/tail block
#' @blockDescr Data head/tail
#' @blockCategory transform
#' @blockIcon eye
#' @blockGuidance Keeps the first or last `n` rows of the input with
#'   `utils::head()` / `utils::tail()`. `direction` chooses the end: "head" for
#'   the first rows, "tail" for the last.
#' @blockKeywords head, tail, first, last, rows, limit
#' @blockArg n Number of rows to keep.
#'   [example] 10L
#'   [type] arg_integer()
#' @blockArg direction Row end to keep, either head (first) or tail (last).
#'   [example] "tail"
#'   [type] arg_enum(c("head", "tail"))
#'
#' @examples
#' new_head_block(n = 10L, direction = "tail")
#'
#' evaluate_block(head_block, list(n = 3L), list(data = "a"),
#'                list(a = datasets::iris))
#'
#' run_block_request(head_block, "n_max", list(data = datasets::iris))
#'
#' @return The block class `head_block` is an object of class `block_class`,
#' and its constructor `new_head_block()` returns a block of that class.
#'
#' @export
new_head_block <- block_constructor(head_block)

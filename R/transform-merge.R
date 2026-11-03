#' @include block-class.R output-table.R
#' @rdname new_merge_block
#' @format NULL
#' @export
merge_block <- new_block_class(
  "merge_block",
  inputs = function(x, y) {
    if (!is.data.frame(x) || !is.data.frame(y)) {
      blockr_abort(
        "The merge block joins two data frames.",
        class = "merge_block_inputs_invalid"
      )
    }
  },
  expr = function(by, all_x = FALSE, all_y = FALSE) {
    if (!length(by)) {
      blockr_invalid("Choose at least one column to join by.")
    }
    bbquote(merge(.(x), .(y), by = .(by), all.x = .(all_x), all.y = .(all_y)))
  },
  ui = js_module("js/merge-block.js"),
  requests = list(shared = function(x, y) intersect(names(x), names(y))),
  output = as_table_output
)

#' Merge block
#'
#' The merge block joins two data frames on a set of columns, as
#' [base::merge()] does. The block class `merge_block` defines it, and its
#' constructor `new_merge_block()` makes a block of the class (see
#' [new_block_class()]).
#'
#' @details
#' Joining together two `data.frame`s, based on a set of index columns, using
#' [base::merge()] is available as `merge_block`. Depending on values passed
#' as `all_x`/`all_y` the result will correspond to an "inner", "outer", "left"
#' or "right" join. See [base::merge()] for details. This block class serves
#' as an example for a transform block that takes exactly two data inputs `x`
#' and `y` to produce a single `data.frame` as output.
#'
#' @section Block class:
#' The block's data inputs `x` and `y` are checked to be data frames. Its state
#' arg `by` has no default, so a new block leaves it unset and is `invalid`
#' until something sets it, as it is when `by` names no column. Its UI asks R
#' for the columns its inputs share through the request `shared`, and its
#' results show as a table, through [as_table_output()].
#'
#' @param by Column(s) to join on
#' @param all_x,all_y Join type, see [base::merge()]
#' @inheritParams new_head_block
#'
#' @block merge block
#' @blockDescr Joining or merging datasets
#' @blockCategory transform
#' @blockIcon union
#' @blockGuidance Joins the two inputs with `base::merge()`. The required
#'   `by` names the shared key column(s). The flags all_x and all_y set the
#'   join type: both FALSE for an inner join, both TRUE for a full outer join,
#'   all_x alone for a left join, all_y alone for a right join.
#' @blockKeywords merge, join, inner join, outer join, left join, right join
#' @blockArg by Shared key column(s) to join on.
#'   [type] arg_array(arg_string())
#' @blockArg all_x Keep unmatched rows from the first input.
#'   [type] arg_boolean()
#' @blockArg all_y Keep unmatched rows from the second input.
#'   [type] arg_boolean()
#' @blockExamples list(
#'   list(by = "Time", all_x = TRUE, all_y = FALSE),
#'   list(by = "Time", all_x = TRUE, all_y = TRUE)
#' )
#'
#' @examples
#' new_merge_block(by = "Time")
#'
#' evaluate_block(merge_block, list(by = "Time"), list(x = "a", y = "b"),
#'                list(a = datasets::BOD, b = datasets::BOD))
#'
#' run_block_request(merge_block, "shared",
#'                   list(x = datasets::BOD, y = datasets::BOD))
#'
#' @return The block class `merge_block` is an object of class `block_class`,
#' and its constructor `new_merge_block()` returns a block of that class.
#'
#' @export
new_merge_block <- block_constructor(merge_block)

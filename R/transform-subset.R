#' @include block-class.R output-table.R
#' @rdname new_subset_block
#' @format NULL
#' @export
subset_block <- new_block_class(
  "subset_block",
  inputs = function(data) {
    if (!is.data.frame(data)) {
      blockr_abort(
        "The subset block takes a data frame, not an object of class ",
        "{class(data)[1L]}.",
        class = "subset_block_inputs_invalid"
      )
    }
  },
  expr = function(subset = "", select = "") {
    args <- list(
      if (nzchar(trimws(subset))) str2lang(subset),
      select = if (nzchar(trimws(select))) str2lang(select)
    )
    args <- Filter(not_null, args)
    bbquote(subset(.(data), ..(args)), splice = TRUE)
  },
  ui = js_module("js/subset-block.js"),
  output = as_table_output
)

#' Subset block
#'
#' The subset block keeps the rows and columns of a data frame that two R
#' expressions select, as [base::subset()] does. The block class
#' `subset_block` defines it, and its constructor `new_subset_block()` makes a
#' block of the class (see [new_block_class()]).
#'
#' @details
#' This block allows to perform row and column subsetting on `data.frame`
#' objects via [base::subset()]. Using non-standard evaluation, strings passed
#' as `subset`/`select` arguments or entered via the UI are parsed into R
#' expressions.
#'
#' @section Block class:
#' The block's data input `data` is checked to be a data frame. Each of its
#' state args is a string that holds one R expression, which the block parses
#' with [base::str2lang()] and puts in its block expression as code, or an
#' empty string, which leaves that argument of [base::subset()] out. A string
#' that does not parse makes the block fail.
#' Its results show as a table, through [as_table_output()].
#'
#' @param subset,select Expressions (passed as strings)
#' @inheritParams new_head_block
#'
#' @block subset block
#' @blockDescr Row and column subsetting
#' @blockCategory transform
#' @blockIcon funnel
#' @blockGuidance Row- and column-subsets the input with `base::subset()`.
#'   `subset` is a logical expression over the columns keeping rows where it is
#'   TRUE; `select` picks columns. Both are R expressions written as strings and
#'   evaluated with the data columns in scope.
#' @blockKeywords filter, subset, select, rows, columns, where
#' @blockArg subset Logical row-filter expression, as a string.
#'   [example] "Sepal.Width > 3"
#'   [type] arg_string()
#' @blockArg select Column-selection expression, as a string.
#'   [example] "Species"
#'   [type] arg_string()
#'
#' @examples
#' new_subset_block(subset = "Sepal.Width > 4")
#'
#' evaluate_block(subset_block, list(subset = "Sepal.Width > 4"),
#'                list(data = "a"), list(a = datasets::iris))
#'
#' @return The block class `subset_block` is an object of class `block_class`,
#' and its constructor `new_subset_block()` returns a block of that class.
#'
#' @export
new_subset_block <- block_constructor(subset_block)

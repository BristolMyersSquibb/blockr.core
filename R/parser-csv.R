#' @include block-class.R output-table.R
#' @rdname new_csv_block
#' @format NULL
#' @export
csv_block <- new_block_class(
  "csv_block",
  inputs = function(file) stopifnot(is_string(file), file.exists(file)),
  expr = function(sep = ",", quote = "\"") {
    bbquote(
      utils::read.table(.(file), header = TRUE, sep = .(sep), quote = .(quote),
                        dec = ".", fill = TRUE, comment.char = "")
    )
  },
  ui = js_module("js/csv-block.js"),
  output = as_table_output
)

#' CSV block
#'
#' The CSV block reads a delimited text file into a data frame, as
#' [utils::read.table()] does. The block class `csv_block` defines it, and its
#' constructor `new_csv_block()` makes a block of the class (see
#' [new_block_class()]).
#'
#' @details
#' Files in CSV format provided for example by a file browser or an upload
#' block may be parsed into `data.frame` by CSV blocks.
#'
#' @section Block class:
#' The block's data input `file` is checked to be the path of a file that
#' exists, as the file browser and the upload block give it. Its results show
#' as a table, through [as_table_output()].
#'
#' @param sep,quote Forwarded to [utils::read.table()]
#' @inheritParams new_head_block
#'
#' @block csv parser block
#' @blockDescr Read CSV file
#' @blockCategory utility
#' @blockIcon filetype-csv
#' @blockGuidance Parses a delimited text file (typically from a file or upload
#'   block) into a data.frame with `utils::read.table()`. `sep` is the field
#'   separator and `quote` the quoting character.
#' @blockKeywords csv, parse, read, delimited, import
#' @blockArg sep Field separator character.
#'   [example] ","
#'   [type] arg_string()
#' @blockArg quote Quoting character.
#'   [type] arg_string()
#'
#' @examples
#' new_csv_block(sep = ";")
#'
#' csv <- tempfile(fileext = ".csv")
#' write.csv(datasets::BOD, csv, row.names = FALSE)
#'
#' evaluate_block(csv_block, list(), list(file = "a"), list(a = csv))
#'
#' @return The block class `csv_block` is an object of class `block_class`,
#' and its constructor `new_csv_block()` returns a block of that class.
#'
#' @export
new_csv_block <- block_constructor(csv_block)

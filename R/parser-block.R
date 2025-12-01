#' Parser block constructors
#'
#' Operating on results from blocks created via [new_file_block()], parser
#' blocks read (i.e. "parse") a file and make the contents available to
#' subsequent blocks for further analysis and visualization.
#'
#' If using the default validator for a parser block sub-class (i.e. not
#' overriding the `dat_valid` argument in the call to `new_parser_block()`),
#' the data argument corresponding to the input file name must be `file` in
#' order to match naming conventions in the validator function.
#'
#' @param ... Forwarded to `new_parser_block()` and [new_block()]
#' @inheritParams new_block
#'
#' @return All blocks constructed via `new_parser_block()` inherit from
#' `parser_block`.
#'
#' @export
new_parser_block <- function(server, ui, class, ctor = sys.parent(),
                             dat_valid = is_file, ...) {

  new_block(server, ui, c(class, "parser_block"), ctor, dat_valid = dat_valid,
            ...)
}

#' @export
block_output.parser_block <- function(x, result, session) {
  dt_result(result, x, session)
}

#' @export
block_ui.parser_block <- function(id, x, ...) {
  tagList(
    DT::dataTableOutput(NS(id, "result"))
  )
}

is_file <- function(file) {
  stopifnot(is_string(file), file.exists(file))
}

#' @export
#' @include utils-dt.R
block_render_trigger.parser_block <- dt_render_trigger

#' @export
board_options.parser_block <- function(x, ...) {
  combine_board_options(
    new_n_rows_option(...),
    new_page_size_option(...),
    new_filter_rows_option(...),
    NextMethod()
  )
}

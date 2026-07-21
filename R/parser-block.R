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
#' @seealso The [blockr.io](https://bristolmyerssquibb.github.io/blockr.io/)
#' package provides real-world blocks for parsing external data formats (e.g.
#' `csv`, `xpt`).
#'
#' @export
new_parser_block <- function(server, ui, class, ctor = sys.parent(),
                             dat_valid = is_file, ...) {

  new_block(server, ui, c(class, "parser_block"), ctor, dat_valid = dat_valid,
            ...)
}

#' @export
block_output.parser_block <- function(x, result, session) {
  tabular_output(tabular_display(), result, x, session)
}

#' @export
block_ui.parser_block <- function(id, x, ...) {
  tagList(
    tabular_ui(tabular_display(), NS(id, "result"))
  )
}

is_file <- function(file) {
  stopifnot(is_string(file), file.exists(file))
}

#' @export
#' @include tabular-display.R
block_render_trigger.parser_block <- function(x, session = get_session()) {
  tabular_trigger(tabular_display(), session)
}

#' @export
board_options.parser_block <- function(x, ...) {
  combine_board_options(
    tabular_options(tabular_display(), ...),
    NextMethod()
  )
}

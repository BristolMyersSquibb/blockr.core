#' Text outputs
#'
#' A text output shows a result as [print()] shows it, at R's own width, in
#' lines that its renderer scrolls. The constructor `new_text_output()` makes
#' one. The output family `text_output()` makes one from a result, by the
#' result's class, and its default method prints any result.
#' A block class that names no other family in its `output` property shows its
#' results through it (see [new_block_class()]).
#'
#' @include output-class.R output-renderer.R block-class.R
#' @param text The text to show, as a string
#' @param ... For `new_text_output()`, further properties, which a subclass
#'   adds, and otherwise passed on to methods
#' @param class The classes of a subclass, which come before `text_output`
#' @inheritParams new_table_output
#'
#' @examples
#' new_text_output("Hello")
#'
#' out <- text_output(summary(datasets::iris$Sepal.Length))
#' cat(out$text)
#'
#' @return Both `new_text_output()` and `text_output()` return an object of
#' class `text_output`.
#'
#' @export
new_text_output <- function(text, ..., class = character()) {

  if (!is_string(text) || is.na(text)) {
    blockr_abort(
      "The text of a text output is expected to be a string.",
      class = "text_output_invalid"
    )
  }

  new_block_output(list(text = text, ...), c(class, "text_output"))
}

#' @rdname new_text_output
#' @export
text_output <- new_output_family(
  function(result, options = list(), info = list(), params = list(), ...) {
    UseMethod("text_output")
  },
  "text_output",
  renderer = new_renderer("text_output", js_module("js/basic-text.js"))
)

#' @rdname new_text_output
#' @export
text_output.default <- function(result, options = list(), info = list(),
                                params = list(), ...) {
  new_text_output(paste(utils::capture.output(print(result)), collapse = "\n"))
}

#' @export
output_schema.text_output <- function(x, ...) {
  read_json_schema("outputs/text-output.schema.json")
}

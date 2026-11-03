#' @include block-class.R
#' @rdname new_glue_block
#' @format NULL
#' @export
glue_block <- new_block_class(
  "glue_block",
  inputs = "...",
  expr = function(text = "") {
    function(...) {
      args <- list(...)
      nms <- if (is.null(names(args))) character(length(args)) else names(args)
      nms[!nzchar(nms)] <- paste0("...", which(!nzchar(nms)))
      names(args) <- nms
      glue::glue(.(text), .envir = list2env(args, parent = baseenv()))
    }
  },
  ui = js_module("js/glue-block.js")
)

#' Glue block
#'
#' The glue block fills in a text template with values computed from its data
#' inputs, as [glue::glue()] does. The block class `glue_block` defines it,
#' and its constructor `new_glue_block()` makes a block of the class (see
#' [new_block_class()]).
#'
#' @details
#' Using [glue::glue()], this block allows evaluation of a text string in the
#' context of datasets to produce text as block result. The text refers to an
#' input by the name of its link's input, as in `"{nrow(data)} rows"`. An input
#' whose link has no name goes by its position among the inputs, as `...1`,
#' `...2` and so on.
#'
#' @section Block class:
#' The block is variadic, so it waits for one link before it runs. Its `expr`
#' returns a function of `...`, so that the block expression calls it with the
#' upstream blocks in link order, an input whose link has no name as an
#' unnamed argument. The function names each such input by its position and
#' evaluates the template in an environment that binds every input by its
#' name. Its results show as text, through [as_text_output()].
#'
#' @param text String evaluated using [glue::glue()]
#' @inheritParams new_head_block
#'
#' @block glue string block
#' @blockDescr String interpolation using glue
#' @blockCategory utility
#' @blockIcon braces
#' @blockGuidance Interpolates a template against the input data with
#'   `glue::glue()` to produce text. Reference an input in `text` with braces,
#'   by the name of its link's input, e.g. "{nrow(data)} rows", or, where the
#'   link has no name, by its position among the inputs, as `...1`.
#' @blockKeywords glue, template, text, string, interpolate, markdown
#' @blockArg text Template string evaluated with glue::glue().
#'   [example] "{nrow(data)} rows"
#'   [type] arg_string()
#'
#' @examples
#' new_glue_block("{nrow(data)} rows")
#'
#' evaluate_block(glue_block, list(text = "{nrow(data)} rows"),
#'                list(... = list(data = "a")), list(a = datasets::iris))
#'
#' @return The block class `glue_block` is an object of class `block_class`,
#' and its constructor `new_glue_block()` returns a block of that class.
#'
#' @export
new_glue_block <- block_constructor(glue_block)

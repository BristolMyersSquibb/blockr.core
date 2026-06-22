#' @details
#' Block constructor arguments can be documented with a structured
#' specification: each argument via `block_arg()` (a `description`, a single
#' worked `example`, and an optional machine-readable `type`), collected with
#' `block_args()`. A bare named character vector of descriptions, and the empty
#' `character()`, are also accepted and normalized into this form, so existing
#' registrations are unaffected.
#'
#' The complete worked configuration of a block is the assembly of its
#' per-argument examples, keyed by argument name -- this is what
#' `block_examples()` returns. When arguments interact, or several few-shot
#' examples are wanted, complete configurations are instead supplied as a list
#' via `examples` and supersede that assembly; combining multiple per-argument
#' examples is intentionally not supported, as there is no safe way to form
#' coherent whole-block configurations from them.
#'
#' @param example A single worked value for an argument (or `NULL`)
#' @param type Optional machine-readable type (e.g. an `ellmer::type_*`),
#'   stored opaquely and not interpreted by blockr.core
#' @rdname register_block
#' @export
block_arg <- function(description = NULL, example = NULL, type = NULL) {

  if (not_null(description) && !is_string(description)) {
    blockr_abort(
      "A block argument `description` must be a string or `NULL`.",
      class = "block_arg_invalid"
    )
  }

  structure(
    list(description = description, example = example, type = type),
    class = "block_arg"
  )
}

#' @rdname register_block
#' @export
block_args <- function(...) {

  args <- list(...)

  nms <- names(args)

  if (length(args) && (is.null(nms) || any(!nzchar(nms)))) {
    blockr_abort(
      "Every block argument must be named after a constructor formal.",
      class = "block_args_unnamed"
    )
  }

  new_block_args(lapply(args, as_block_arg))
}

new_block_args <- function(x) {
  structure(x, class = "block_args")
}

is_block_args <- function(x) inherits(x, "block_args")

as_block_arg <- function(x) {

  if (inherits(x, "block_arg")) {
    return(x)
  }

  if (is_string(x)) {
    return(block_arg(description = x))
  }

  blockr_abort(
    "A block argument must be a string (its description) or a `block_arg()`.",
    class = "block_arg_invalid"
  )
}

normalize_arguments <- function(arguments, guidance) {

  if (is_block_args(arguments)) {
    return(list(arguments = arguments, guidance = guidance))
  }

  stopifnot(is.character(arguments))

  examples <- attr(arguments, "examples")
  prompt <- attr(arguments, "prompt")

  if (not_null(examples) || not_null(prompt)) {
    deprecate_legacy_arg_attrs()
  }

  nms <- names(arguments)

  if (is.null(nms)) {
    nms <- character()
  }

  spec <- set_names(
    lapply(
      nms,
      function(nm) {
        block_arg(description = arguments[[nm]], example = examples[[nm]])
      }
    ),
    nms
  )

  list(
    arguments = new_block_args(spec),
    guidance = coal(guidance, prompt, fail_all = FALSE)
  )
}

assemble_example <- function(args) {

  ex <- lst_xtr(args, "example")
  ex <- ex[!lgl_ply(ex, is.null)]

  if (!length(ex)) {
    return(NULL)
  }

  ex
}

block_examples_list <- function(args, examples) {

  if (length(examples)) {
    return(examples)
  }

  assembled <- assemble_example(args)

  if (is.null(assembled)) {
    return(list())
  }

  list(assembled)
}

validate_block_spec <- function(args, examples, obj, ctor, ctor_ref, ctor_pkg,
                                check_names = TRUE) {

  inputs <- block_ctor_inputs(obj)

  if (check_names) {

    unknown <- setdiff(names(args), inputs)

    if (length(unknown)) {
      blockr_abort(
        "Block argument(s) {unknown} are not constructor formals ({inputs}).",
        class = "block_arg_unknown"
      )
    }
  }

  for (cfg in block_examples_list(args, examples)) {

    unknown <- setdiff(names(cfg), inputs)

    if (length(unknown)) {
      blockr_abort(
        "Example field(s) {unknown} are not constructor formals ({inputs}).",
        class = "block_example_unknown"
      )
    }

    res <- try(
      do.call(
        ctor,
        c(
          cfg,
          list(ctor = ctor_ref, ctor_pkg = ctor_pkg, block_metadata = FALSE)
        )
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error")) {
      blockr_abort(
        "A registered block example does not construct: ",
        "{conditionMessage(attr(res, 'condition'))}",
        class = "block_example_invalid"
      )
    }
  }

  invisible()
}

as_legacy_arguments <- function(args, guidance, examples_list) {

  desc <- chr_ply(
    args,
    function(a) coal(a[["description"]], "", fail_all = FALSE)
  )

  example <- if (length(examples_list)) examples_list[[1L]] else NULL

  structure(set_names(desc, names(args)), examples = example, prompt = guidance)
}

deprecate_legacy_arg_attrs <- function() {
  blockr_warn(
    "Passing block metadata as `examples`/`prompt` attributes on `arguments` ",
    "is deprecated; use `block_args()` / `block_arg()` and the `guidance` ",
    "argument of `register_block()` instead.",
    class = "deprecated_arg_attrs",
    frequency = "once",
    frequency_id = "blockr_deprecated_arg_attrs"
  )
}

#' @rdname register_block
#' @export
block_arg_specs <- function(id) {
  attr(get_registry_entry(id), "arguments")
}

#' @rdname register_block
#' @export
block_examples <- function(id) {

  entry <- get_registry_entry(id)

  block_examples_list(attr(entry, "arguments"), attr(entry, "examples"))
}

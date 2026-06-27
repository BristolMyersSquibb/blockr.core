#' @details
#' Block constructor arguments can be documented with a structured
#' specification: each argument via `new_block_arg()` (a `description`, a single
#' worked `example`, and an optional machine-readable `type`), collected with
#' `new_block_args()`. A bare named character vector of descriptions, and the
#' empty `character()`, are also accepted and normalized into this form, so
#' existing registrations are unaffected.
#'
#' For a registered block, the structured construction metadata is retrieved
#' with the accessor generics `block_args()` (the specification),
#' `block_examples()` (complete worked configurations), `block_guidance()`
#' (model-facing construction notes) and `block_keywords()` (discovery terms).
#' Each accepts a `block`, a `block_registry_entry` or a registry ID -- a block
#' instance is read from its own attached metadata, the registry is consulted
#' only for a bare ID or entry -- so the same call works whether one holds an
#' instance or only its type. The
#' flat, scalar catalog metadata (name, description, category, ...) is instead
#' tabulated across many blocks via [block_metadata()].
#'
#' The complete worked configuration of a block is the assembly of its
#' per-argument examples, keyed by argument name. When arguments interact, or
#' several few-shot examples are wanted, complete configurations are instead
#' supplied as a list via the `examples` argument of `register_block()` and
#' supersede that assembly; combining multiple per-argument examples is
#' intentionally not supported, as there is no safe way to form coherent
#' whole-block configurations from them.
#'
#' @param example A single worked value for an argument (or `NULL`)
#' @param type Optional machine-readable type for the argument, built with the
#'   [arg-type] descriptor constructors (`arg_string()`, `arg_object()`, ...). A
#'   plain JSON-Schema-subset list; worked examples are validated against it
#' @rdname register_block
#' @export
new_block_arg <- function(description = NULL, example = NULL, type = NULL) {

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
new_block_args <- function(...) {
  as_block_args(list(...))
}

block_args_obj <- function(x) {
  structure(x, class = "block_args")
}

is_block_args <- function(x) inherits(x, "block_args")

as_block_arg <- function(x, ...) {
  UseMethod("as_block_arg")
}

#' @export
as_block_arg.block_arg <- function(x, ...) {
  x
}

#' @export
as_block_arg.character <- function(x, ...) {
  new_block_arg(description = x)
}

as_block_args <- function(x, ...) {
  UseMethod("as_block_args")
}

#' @export
as_block_args.block_args <- function(x, ...) {
  x
}

#' @export
as_block_args.list <- function(x, ...) {

  nms <- names(x)

  if (length(x) && (is.null(nms) || any(!nzchar(nms)))) {
    blockr_abort(
      "Every block argument must be named after a constructor formal.",
      class = "block_args_unnamed"
    )
  }

  block_args_obj(lapply(x, as_block_arg))
}

#' @export
as_block_args.character <- function(x, ...) {

  examples <- attr(x, "examples")

  nms <- names(x)

  if (is.null(nms)) {
    nms <- character()
  }

  spec <- set_names(
    lapply(
      nms,
      function(nm) {
        new_block_arg(description = x[[nm]], example = examples[[nm]])
      }
    ),
    nms
  )

  block_args_obj(spec)
}

normalize_arguments <- function(arguments, guidance) {

  prompt <- if (is.character(arguments)) attr(arguments, "prompt") else NULL

  if (is.character(arguments) &&
        (not_null(attr(arguments, "examples")) || not_null(prompt))) {
    deprecate_legacy_arg_attrs()
  }

  list(
    arguments = as_block_args(arguments),
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

  for (nm in names(args)) {

    type <- args[[nm]][["type"]]

    if (not_null(type) && !valid_descriptor(type)) {
      blockr_abort(
        "The `type` of block argument {nm} is not a valid descriptor.",
        class = "block_arg_type_invalid"
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

    for (nm in intersect(names(cfg), names(args))) {

      type <- args[[nm]][["type"]]

      if (not_null(type) && !conforms_to(cfg[[nm]], type)) {
        blockr_abort(
          "Example value for block argument {nm} does not conform to its ",
          "declared `type`.",
          class = "block_example_nonconforming"
        )
      }
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
  prompt <- if (length(guidance)) paste(guidance, collapse = "\n\n") else NULL

  structure(set_names(desc, names(args)), examples = example, prompt = prompt)
}

deprecate_legacy_arg_attrs <- function() {
  blockr_warn(
    "Passing block metadata as `examples`/`prompt` attributes on `arguments` ",
    "is deprecated; use `new_block_args()` / `new_block_arg()` and the ",
    "`guidance` argument of `register_block()` instead.",
    class = "deprecated_arg_attrs",
    frequency = "once",
    frequency_id = "blockr_deprecated_arg_attrs"
  )
}

#' @param x A `block_arg` object, or a bare string taken as its description
#' @rdname register_block
#' @export
block_arg_description <- function(x, ...) {
  UseMethod("block_arg_description")
}

#' @export
block_arg_description.block_arg <- function(x, ...) {
  x[["description"]]
}

#' @export
block_arg_description.default <- function(x, ...) {
  block_arg_description(as_block_arg(x), ...)
}

#' @rdname register_block
#' @export
block_arg_example <- function(x, ...) {
  UseMethod("block_arg_example")
}

#' @export
block_arg_example.block_arg <- function(x, ...) {
  x[["example"]]
}

#' @export
block_arg_example.default <- function(x, ...) {
  block_arg_example(as_block_arg(x), ...)
}

#' @rdname register_block
#' @export
block_arg_type <- function(x, ...) {
  UseMethod("block_arg_type")
}

#' @export
block_arg_type.block_arg <- function(x, ...) {
  x[["type"]]
}

#' @export
block_arg_type.default <- function(x, ...) {
  block_arg_type(as_block_arg(x), ...)
}

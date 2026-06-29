#' Block argument specification
#'
#' Block constructor arguments can be documented with a structured
#' specification: each argument via `new_block_arg()` (a `description`, a single
#' worked `example`, and an optional machine-readable `type`), collected with
#' `new_block_args()`. A bare named character vector of descriptions, and the
#' empty `character()`, are also accepted and normalized into this form, so
#' existing registrations are unaffected. A single argument's fields are read
#' back with `block_arg_description()`, `block_arg_example()` and
#' `block_arg_type()`; block-level metadata (the whole argument set, worked
#' examples, guidance and keywords) is tabulated across blocks via
#' [block_metadata()].
#'
#' An argument's `type` is described with a small, dependency-free subset of
#' JSON Schema, built with the `arg_*()` constructors: `arg_string()`,
#' `arg_number()`, `arg_integer()` and `arg_boolean()` for scalars, `arg_enum()`
#' for a fixed set of string values, `arg_array()` for a homogeneous list and
#' `arg_object()` for a closed record of named fields (`additionalProperties:
#' false`). Each returns a plain nested list mirroring the schema it denotes,
#' consumed directly -- blockr.ai binds it via `ellmer::type_from_schema()`, an
#' MCP or raw tool schema reads the JSON as-is -- and worked examples registered
#' alongside an argument are validated against it (see [register_block()]).
#' Semantic intent (e.g. "a column in the upstream data", "an R expression") is
#' carried in `description`, not in the type vocabulary.
#'
#' The complete worked configuration of a block is the assembly of its
#' per-argument examples, keyed by argument name. When arguments interact, or
#' several few-shot examples are wanted, complete configurations are instead
#' supplied as a list via the `examples` argument of [register_block()] and
#' supersede that assembly; combining multiple per-argument examples is
#' intentionally not supported, as there is no safe way to form coherent
#' whole-block configurations from them.
#'
#' @param description Human- and model-facing description of an argument value
#' @param example A single worked value for an argument (or `NULL`)
#' @param type Optional machine-readable type for the argument, built with the
#'   `arg_*()` descriptor constructors. A plain JSON-Schema-subset list; worked
#'   examples are validated against it
#' @param x A `block_arg` object, or a bare string taken as its description
#' @param values Allowed string values, for `arg_enum()`
#' @param items Element descriptor, for `arg_array()`
#' @param required Whether the field is required, when nested in an
#'   `arg_object()`
#' @param ... For `new_block_args()`, the per-argument `block_arg` objects (or
#'   bare description strings); for `arg_object()`, the named field descriptors;
#'   ignored by the `block_arg_*()` getters
#'
#' @return
#' `new_block_arg()` returns a `block_arg` and `new_block_args()` a `block_args`
#' collection. The `arg_*()` constructors each return a plain JSON-Schema node
#' (a list). The `block_arg_*()` getters return the corresponding field of a
#' single argument (resolving a bare description string too).
#'
#' @examples
#' new_block_args(
#'   n = new_block_arg(
#'     "Number of rows to return",
#'     example = 5L,
#'     type = arg_integer()
#'   )
#' )
#'
#' arg_object(
#'   conditions = arg_array(
#'     arg_object(column = arg_string(), value = arg_string())
#'   ),
#'   operator = arg_enum(c("&", "|"))
#' )
#'
#' @name new_block_arg
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

#' @rdname new_block_arg
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

#' @rdname new_block_arg
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

#' @rdname new_block_arg
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

#' @rdname new_block_arg
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

#' @rdname new_block_arg
#' @export
arg_string <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "string", description = description), required)
}

#' @rdname new_block_arg
#' @export
arg_number <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "number", description = description), required)
}

#' @rdname new_block_arg
#' @export
arg_integer <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "integer", description = description), required)
}

#' @rdname new_block_arg
#' @export
arg_boolean <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "boolean", description = description), required)
}

#' @rdname new_block_arg
#' @export
arg_enum <- function(values, description = NULL, required = TRUE) {

  if (!is.character(values) || !length(values)) {
    blockr_abort(
      "`arg_enum()` requires a non-empty character vector of values.",
      class = "arg_enum_invalid"
    )
  }

  arg_node(
    list(type = "string", enum = I(values), description = description),
    required
  )
}

#' @rdname new_block_arg
#' @export
arg_array <- function(items, description = NULL, required = TRUE) {
  arg_node(
    list(type = "array", items = items, description = description),
    required
  )
}

#' @rdname new_block_arg
#' @export
arg_object <- function(..., description = NULL, required = TRUE) {

  fields <- list(...)
  nms <- names(fields)

  if (length(fields) && (is.null(nms) || any(!nzchar(nms)))) {
    blockr_abort(
      "Every `arg_object()` field must be named.",
      class = "arg_object_unnamed"
    )
  }

  req <- nms[lgl_ply(fields, function(x) isTRUE(attr(x, "blockr_required")))]

  arg_node(
    list(
      type = "object",
      description = description,
      properties = fields,
      required = I(req),
      additionalProperties = FALSE
    ),
    required
  )
}

arg_node <- function(fields, required) {
  structure(
    fields[!lgl_ply(fields, is.null)],
    blockr_required = required
  )
}

valid_descriptor <- function(x) {

  if (!is.list(x) || !is_string(x[["type"]])) {
    return(FALSE)
  }

  type <- x[["type"]]
  kinds <- c("string", "number", "integer", "boolean", "array", "object")

  if (!type %in% kinds) {
    return(FALSE)
  }

  if (not_null(x[["enum"]]) &&
        (!identical(type, "string") || !is.character(x[["enum"]]) ||
           !length(x[["enum"]]))) {
    return(FALSE)
  }

  if (identical(type, "array")) {
    return(valid_descriptor(x[["items"]]))
  }

  if (identical(type, "object")) {
    return(valid_object_descriptor(x))
  }

  TRUE
}

valid_object_descriptor <- function(x) {

  props <- x[["properties"]]
  nms <- names(props)

  if (!is.list(props) ||
        (length(props) && (is.null(nms) || any(!nzchar(nms))))) {
    return(FALSE)
  }

  req <- x[["required"]]

  if (not_null(req) && (!is.character(req) || length(setdiff(req, nms)))) {
    return(FALSE)
  }

  all(lgl_ply(props, valid_descriptor))
}

conforms_to <- function(value, schema) {

  if (not_null(schema[["enum"]])) {
    return(is_string(value) && value %in% schema[["enum"]])
  }

  switch(
    schema[["type"]],
    string = is_string(value),
    number = is_number(value),
    integer = is_number(value) && value == round(value),
    boolean = is_bool(value),
    array = conforms_array(value, schema[["items"]]),
    object = conforms_object(value, schema),
    FALSE
  )
}

conforms_array <- function(value, items) {
  is.list(value) && all(lgl_ply(value, conforms_to, items))
}

conforms_object <- function(value, schema) {

  if (!is.list(value)) {
    return(FALSE)
  }

  nms <- names(value)

  if (length(value) && (is.null(nms) || any(!nzchar(nms)))) {
    return(FALSE)
  }

  if (is.null(nms)) {
    nms <- character()
  }

  props <- schema[["properties"]]

  if (length(setdiff(nms, names(props)))) {
    return(FALSE)
  }

  if (length(setdiff(schema[["required"]], nms))) {
    return(FALSE)
  }

  all(
    lgl_ply(
      seq_along(value),
      function(i) conforms_to(value[[i]], props[[nms[[i]]]])
    )
  )
}

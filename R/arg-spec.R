#' Argument specifications
#'
#' The named arguments of a constructor can be documented with a structured
#' specification: each argument via `new_arg_spec()` (a `description`, a single
#' worked `example`, and an optional machine-readable `type`), collected with
#' `new_arg_specs()`. A bare named character vector of descriptions, and the
#' empty `character()`, are also accepted and normalized into this form, so
#' existing specifications are unaffected. A single argument's fields are read
#' back with `arg_spec_description()`, `arg_spec_example()` and
#' `arg_spec_type()`. Blocks are the primary consumer -- a block's constructor
#' arguments are specified via the `arguments` parameter of [register_block()]
#' and tabulated across blocks via [block_metadata()] -- but nothing here is
#' block-specific, and the same machinery documents the named formals of any
#' constructor.
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
#' For a block, the complete worked configuration is the assembly of its
#' per-argument examples, keyed by argument name. When arguments interact, or
#' several few-shot examples are wanted, complete configurations are instead
#' supplied as a list via the `examples` argument of [register_block()] and
#' supersede that assembly; combining multiple per-argument examples is
#' intentionally not supported, as there is no safe way to form coherent
#' whole-block configurations from them.
#'
#' The former block-prefixed names -- `new_block_arg()`, `new_block_args()`,
#' `block_arg_description()`, `block_arg_example()` and `block_arg_type()` --
#' remain as deprecated wrappers that warn once and forward to the functions
#' above; update calls to the `arg_spec` family.
#'
#' @param description Human- and model-facing description of an argument value
#' @param example A single worked value for an argument (or `NULL`)
#' @param type Optional machine-readable type for the argument, built with the
#'   `arg_*()` descriptor constructors. A plain JSON-Schema-subset list; worked
#'   examples are validated against it
#' @param x An `arg_spec` / `arg_specs` object, or a bare (optionally named)
#'   character vector or list of descriptions to coerce; `is_arg_specs()`
#'   accepts any object
#' @param values Allowed string values, for `arg_enum()`
#' @param items Element descriptor, for `arg_array()`
#' @param required Whether the field is required, when nested in an
#'   `arg_object()`
#' @param ... For `new_arg_specs()`, the per-argument `arg_spec` objects (or
#'   bare description strings); for `arg_object()`, the named field descriptors;
#'   ignored by the `arg_spec_*()` getters
#'
#' @return
#' `new_arg_spec()` returns an `arg_spec` and `new_arg_specs()` an `arg_specs`
#' collection. The `arg_*()` constructors each return a plain JSON-Schema node
#' (a list). The `arg_spec_*()` getters return the corresponding field of a
#' single argument (resolving a bare description string too). `is_arg_specs()`
#' returns a boolean and `as_arg_specs()` coerces a named character vector or
#' list to an `arg_specs`; a consuming package should test and coerce through
#' these rather than reaching for the class name directly.
#'
#' @examples
#' new_arg_specs(
#'   n = new_arg_spec(
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
#' @name new_arg_spec
#' @export
new_arg_spec <- function(description = NULL, example = NULL, type = NULL) {

  if (not_null(description) && !is_string(description)) {
    blockr_abort(
      "An argument `description` must be a string or `NULL`.",
      class = "arg_spec_invalid"
    )
  }

  structure(
    list(description = description, example = example, type = type),
    class = "arg_spec"
  )
}

#' @rdname new_arg_spec
#' @export
new_arg_specs <- function(...) {
  as_arg_specs(list(...))
}

arg_specs_obj <- function(x) {
  structure(x, class = "arg_specs")
}

#' @rdname new_arg_spec
#' @export
is_arg_specs <- function(x) inherits(x, "arg_specs")

as_arg_spec <- function(x, ...) {
  UseMethod("as_arg_spec")
}

#' @export
as_arg_spec.arg_spec <- function(x, ...) {
  x
}

#' @export
as_arg_spec.character <- function(x, ...) {
  new_arg_spec(description = x)
}

#' @rdname new_arg_spec
#' @export
as_arg_specs <- function(x, ...) {
  UseMethod("as_arg_specs")
}

#' @export
as_arg_specs.arg_specs <- function(x, ...) {
  x
}

#' @export
as_arg_specs.list <- function(x, ...) {

  nms <- names(x)

  if (length(x) && (is.null(nms) || any(!nzchar(nms)))) {
    blockr_abort(
      "Every argument must be named after a constructor formal.",
      class = "arg_specs_unnamed"
    )
  }

  arg_specs_obj(lapply(x, as_arg_spec))
}

#' @export
as_arg_specs.character <- function(x, ...) {

  examples <- attr(x, "examples")

  nms <- names(x)

  if (is.null(nms)) {
    nms <- character()
  }

  spec <- set_names(
    lapply(
      nms,
      function(nm) {
        new_arg_spec(description = x[[nm]], example = examples[[nm]])
      }
    ),
    nms
  )

  arg_specs_obj(spec)
}

normalize_arguments <- function(arguments, guidance) {

  prompt <- if (is.character(arguments)) attr(arguments, "prompt") else NULL

  if (is.character(arguments) &&
        (not_null(attr(arguments, "examples")) || not_null(prompt))) {
    deprecate_legacy_arg_attrs()
  }

  list(
    arguments = as_arg_specs(arguments),
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
    "is deprecated; use `new_arg_specs()` / `new_arg_spec()` and the ",
    "`guidance` argument of `register_block()` instead.",
    class = "deprecated_arg_attrs",
    frequency = "once",
    frequency_id = "blockr_deprecated_arg_attrs"
  )
}

#' @rdname new_arg_spec
#' @export
arg_spec_description <- function(x, ...) {
  UseMethod("arg_spec_description")
}

#' @export
arg_spec_description.arg_spec <- function(x, ...) {
  x[["description"]]
}

#' @export
arg_spec_description.default <- function(x, ...) {
  arg_spec_description(as_arg_spec(x), ...)
}

#' @rdname new_arg_spec
#' @export
arg_spec_example <- function(x, ...) {
  UseMethod("arg_spec_example")
}

#' @export
arg_spec_example.arg_spec <- function(x, ...) {
  x[["example"]]
}

#' @export
arg_spec_example.default <- function(x, ...) {
  arg_spec_example(as_arg_spec(x), ...)
}

#' @rdname new_arg_spec
#' @export
arg_spec_type <- function(x, ...) {
  UseMethod("arg_spec_type")
}

#' @export
arg_spec_type.arg_spec <- function(x, ...) {
  x[["type"]]
}

#' @export
arg_spec_type.default <- function(x, ...) {
  arg_spec_type(as_arg_spec(x), ...)
}

#' @rdname new_arg_spec
#' @export
arg_string <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "string", description = description), required)
}

#' @rdname new_arg_spec
#' @export
arg_number <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "number", description = description), required)
}

#' @rdname new_arg_spec
#' @export
arg_integer <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "integer", description = description), required)
}

#' @rdname new_arg_spec
#' @export
arg_boolean <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "boolean", description = description), required)
}

#' @rdname new_arg_spec
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

#' @rdname new_arg_spec
#' @export
arg_array <- function(items, description = NULL, required = TRUE) {
  arg_node(
    list(type = "array", items = items, description = description),
    required
  )
}

#' @rdname new_arg_spec
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

#' @rdname new_arg_spec
#' @export
new_block_arg <- function(description = NULL, example = NULL, type = NULL) {
  deprecate_arg_spec("new_block_arg", "new_arg_spec")
  new_arg_spec(description = description, example = example, type = type)
}

#' @rdname new_arg_spec
#' @export
new_block_args <- function(...) {
  deprecate_arg_spec("new_block_args", "new_arg_specs")
  new_arg_specs(...)
}

#' @rdname new_arg_spec
#' @export
block_arg_description <- function(x, ...) {
  deprecate_arg_spec("block_arg_description", "arg_spec_description")
  arg_spec_description(x, ...)
}

#' @rdname new_arg_spec
#' @export
block_arg_example <- function(x, ...) {
  deprecate_arg_spec("block_arg_example", "arg_spec_example")
  arg_spec_example(x, ...)
}

#' @rdname new_arg_spec
#' @export
block_arg_type <- function(x, ...) {
  deprecate_arg_spec("block_arg_type", "arg_spec_type")
  arg_spec_type(x, ...)
}

deprecate_arg_spec <- function(old, new) {
  blockr_warn(
    "`{old}()` is deprecated; use `{new}()` instead.",
    class = "deprecated_arg_spec",
    frequency = "once",
    frequency_id = paste0("blockr_deprecated_", old)
  )
}

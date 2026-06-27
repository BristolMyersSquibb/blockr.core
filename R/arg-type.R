#' Argument type descriptors
#'
#' The optional `type` of a block argument (see [new_block_arg()]) is described
#' with a small, dependency-free subset of JSON Schema, built with these
#' constructors. Each returns a plain nested list mirroring the schema it
#' denotes: `arg_string()` / `arg_number()` / `arg_integer()` / `arg_boolean()`
#' for scalars, `arg_enum()` for a fixed set of string values, `arg_array()` for
#' a homogeneous list and `arg_object()` for a closed record of named fields
#' (`additionalProperties: false`). The descriptor is consumed directly --
#' blockr.ai binds it via `ellmer::type_from_schema()`, an MCP or raw tool
#' schema reads the JSON as-is -- and worked examples registered alongside an
#' argument are validated against it (see [register_block()]).
#'
#' Semantic intent (e.g. "a column in the upstream data", "an R expression") is
#' carried in `description`, not in the type vocabulary.
#'
#' @param description Human- and model-facing description of the value
#' @param required Whether the field is required, when nested in an
#'   `arg_object()`
#' @param values Allowed string values, for `arg_enum()`
#' @param items Element descriptor, for `arg_array()`
#' @param ... Named field descriptors, for `arg_object()`
#'
#' @return A plain list describing the argument value as a JSON-Schema node.
#'
#' @examples
#' arg_object(
#'   conditions = arg_array(
#'     arg_object(column = arg_string(), value = arg_string())
#'   ),
#'   operator = arg_enum(c("&", "|"))
#' )
#'
#' @name arg-type
#' @export
arg_string <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "string", description = description), required)
}

#' @rdname arg-type
#' @export
arg_number <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "number", description = description), required)
}

#' @rdname arg-type
#' @export
arg_integer <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "integer", description = description), required)
}

#' @rdname arg-type
#' @export
arg_boolean <- function(description = NULL, required = TRUE) {
  arg_node(list(type = "boolean", description = description), required)
}

#' @rdname arg-type
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

#' @rdname arg-type
#' @export
arg_array <- function(items, description = NULL, required = TRUE) {
  arg_node(
    list(type = "array", items = items, description = description),
    required
  )
}

#' @rdname arg-type
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

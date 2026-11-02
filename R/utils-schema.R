# Shapes a value by a JSON Schema, so that `encode_message()` writes it in the
# shape the schema gives. R's writer alone writes a vector of length one bare
# and an empty list as an array. Here, a value that the schema has as an array
# stays an array, marked with `I()`. A data frame becomes an array of its rows.
# Each row is an array where the schema's items are arrays, and an object
# otherwise. A value that the schema has as an object becomes a named list, an
# empty one included. Scalars, values that the schema allows in several forms,
# with `anyOf` or `oneOf`, and values it says nothing about stay as they are. A
# `$ref` is resolved within `root`, the schema as read from its file.
shape_by_schema <- function(x, schema, root = schema) {

  schema <- resolve_schema_ref(schema, root)

  switch(
    coal(schema_type(schema), "", fail_all = FALSE),
    array = shape_array(x, schema, root),
    object = shape_object(x, schema, root),
    x
  )
}

# The type a schema gives a value, "array", "object" or a scalar's. It is `NULL`
# where the schema allows several, as `anyOf`, `oneOf` and a vector of types do,
# or says nothing.
schema_type <- function(schema) {

  if (!is.list(schema) || not_null(schema[["anyOf"]]) ||
        not_null(schema[["oneOf"]])) {
    return(NULL)
  }

  type <- schema[["type"]]

  if (is.character(type) && length(type) == 1L) {
    return(as.character(type))
  }

  if (is.null(type) && not_null(schema[["properties"]])) {
    return("object")
  }

  if (is.null(type) && not_null(schema[["items"]])) {
    return("array")
  }

  NULL
}

resolve_schema_ref <- function(schema, root) {

  seen <- character()

  while (is.list(schema) && is_string(schema[["$ref"]])) {

    ref <- schema[["$ref"]]

    if (ref %in% seen) {
      blockr_abort(
        "The JSON Schema reference {ref} refers to itself.",
        class = "json_schema_ref_invalid"
      )
    }

    seen <- c(seen, ref)
    schema <- root

    for (part in strsplit(sub("^#/?", "", ref), "/", fixed = TRUE)[[1L]]) {
      schema <- if (is.list(schema)) schema[[part]]
    }

    if (is.null(schema)) {
      blockr_abort(
        "The JSON Schema reference {ref} points to nothing in its schema.",
        class = "json_schema_ref_invalid"
      )
    }
  }

  schema
}

shape_array <- function(x, schema, root) {

  if (is.null(x)) {
    return(list())
  }

  items <- resolve_schema_ref(schema[["items"]], root)

  if (is.data.frame(x)) {
    return(shape_rows(x, items, root))
  }

  if (is.list(x)) {
    return(lapply(unname(x), shape_by_schema, items, root))
  }

  I(x)
}

shape_object <- function(x, schema, root) {

  if (!is.list(x) || is.data.frame(x)) {
    return(x)
  }

  if (!length(x)) {
    return(set_names(list(), character()))
  }

  props <- schema[["properties"]]
  extra <- schema[["additionalProperties"]]

  res <- lapply(
    seq_along(x),
    function(i) {
      prop <- coal(props[[names(x)[i]]], extra, fail_all = FALSE)
      shape_by_schema(x[[i]], prop, root)
    }
  )

  set_names(res, names(x))
}

# The rows of a data frame, each an array of its cells where the schema's items
# are arrays, and otherwise an object of them by column name. An object leaves
# out a missing value of a property that the schema does not require, as JSON
# has no missing value but an absent property.
shape_rows <- function(x, items, root) {

  if (length(x)) {
    cells <- lapply(seq_along(x), function(j) column_cells(x[[j]]))
    rows <- .mapply(list, cells, NULL)
  } else {
    rows <- rep(list(list()), nrow(x))
  }

  if (identical(schema_type(items), "array")) {

    cell <- resolve_schema_ref(items[["items"]], root)

    if (is.null(schema_type(cell))) {
      return(rows)
    }

    return(lapply(rows, lapply, shape_by_schema, cell, root))
  }

  required <- items[["required"]]

  lapply(
    rows,
    function(row) {
      row <- set_names(row, names(x))
      gone <- !names(row) %in% required & lgl_ply(row, is_missing_value)
      shape_object(row[!gone], items, root)
    }
  )
}

column_cells <- function(x) {

  if (is.list(x) && !is.data.frame(x)) {
    return(unclass(x))
  }

  if (is.object(x) || is.array(x)) {
    return(lapply(seq_len(vec_size(x)), function(i) vec_slice(x, i)))
  }

  as.list(x)
}

is_missing_value <- function(x) {
  is.atomic(x) && length(x) == 1L && is.na(x)
}

# Shapes each key of the front-end's state by its schema, where the state holds
# a value at the key's path: a collection's entries each by an entry's schema,
# and a value by the value's.
shape_state_keys <- function(x, keys) {

  for (key in keys) {

    value <- value_at(x, key[["path"]])

    if (is.null(value)) {
      next
    }

    if (identical(key[["kind"]], "collection")) {
      value <- shape_collection(value, key[["schema"]])
    } else {
      value <- shape_by_schema(value, key[["schema"]])
    }

    x <- set_value_at(x, key[["path"]], value)
  }

  x
}

# The entries of a collection, by ID, each shaped by the schema of an entry.
shape_collection <- function(x, schema) {

  if (!is.list(x)) {
    return(x)
  }

  if (!length(x)) {
    return(set_names(list(), character()))
  }

  set_names(lapply(x, shape_by_schema, schema), names(x))
}

shape_option_values <- function(x, schemas) {

  for (id in intersect(names(x), names(schemas))) {
    x[id] <- list(shape_by_schema(x[[id]], schemas[[id]]))
  }

  x
}

value_at <- function(x, path) {

  for (key in path) {

    if (!is.list(x) || !key %in% names(x)) {
      return(NULL)
    }

    x <- x[[key]]
  }

  x
}

set_value_at <- function(x, path, value) {

  if (length(path) == 1L) {
    x[path] <- list(value)
    return(x)
  }

  x[[path[1L]]] <- set_value_at(x[[path[1L]]], path[-1L], value)

  x
}

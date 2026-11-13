# Shapes a value by a JSON Schema, so that `encode_message()` writes it in the
# shape the schema gives. R's writer alone writes a vector of length one bare
# and an empty list as an array. Here, a value that the schema has as an array
# stays an array, marked with `I()`. A data frame becomes an array of its rows.
# Each row is an array where the schema's items are arrays, and an object
# otherwise.
# A value that the schema has as an object becomes a named list, an empty one
# included, and so does a named vector, such as the file browser's volumes,
# `c(home = "~")`, whose names R's writer would drop.
# A value that the schema allows in several forms, with `anyOf` or `oneOf`, is
# shaped by the branch that it fits, as `shape_by_branch()` gives it.
# Scalars and values that the schema says nothing about stay as they are.
# A `$ref` is resolved within `root`, the schema as read from its file.
shape_by_schema <- function(x, schema, root = schema) {

  schema <- resolve_schema_ref(schema, root)

  if (not_null(schema_branches(schema))) {
    return(shape_by_branch(x, schema, root))
  }

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

  if (!is.list(schema) || not_null(schema_branches(schema))) {
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

# The branches of a schema that allows a value in several forms, with `anyOf`
# or `oneOf`.
# It is `NULL` for any other schema.
schema_branches <- function(schema) {

  if (!is.list(schema)) {
    return(NULL)
  }

  coal(schema[["anyOf"]], schema[["oneOf"]], fail_all = FALSE)
}

# Shapes a value under `anyOf` or `oneOf` by the branch that it fits.
# A value fits a branch where the branch is an object schema, and the value is
# a named list whose names include each property that the branch requires.
# Its names must be strings of their own, none of them empty.
# An empty list fits no branch, so that `list()` stays an array.
# A data frame or another list with a class fits no branch either.
# A list that fits no branch or several stays as it is, and so does any other
# value.
# So a vector of length one stays bare where the schema allows a string or an
# array of strings.
# In the dock's grids, a split requires `children` and `sizes`, and a tab group
# requires `panels` and `active`.
# So each node of a grid fits one branch.
# A named vector fits no branch, though an object schema alone turns it into an
# object.
# Core's `Json` type allows any object.
# If a named vector fit it, the vector would turn from an array into an object
# in the state of each block class without a schema.
shape_by_branch <- function(x, schema, root) {

  if (!length(x) || !is_named_list(x)) {
    return(x)
  }

  branch <- fitting_branch(schema, names(x), root)

  if (is.null(branch)) {
    return(x)
  }

  shape_object(x, branch, root)
}

# The one branch of a schema's `anyOf` or `oneOf` that is an object schema
# whose required properties are all among `props`.
# It is `NULL` where no branch or several are.
fitting_branch <- function(schema, props, root) {

  branches <- lapply(schema_branches(schema), resolve_schema_ref, root)

  fits <- lgl_ply(
    branches,
    function(branch) {
      identical(schema_type(branch), "object") &&
        all(branch[["required"]] %in% props)
    }
  )

  if (sum(fits) != 1L) {
    return(NULL)
  }

  branches[[which(fits)]]
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

  if (is.atomic(x) && !is.object(x) && not_null(names(x))) {
    x <- as.list(x)
  }

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
# Where the items allow several forms, the rows are shaped by the branch that
# the data frame's columns fit, as `fitting_branch()` gives it.
# Where the columns fit no branch or several, each row is an object of its
# cells as they are.
# A cell under `anyOf` or `oneOf` stays as it is.
# The cells of a table output are such cells, of core's `Json` type.
# Each is a single value or its summary, so shaping each would only cost time.
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

  if (not_null(schema_branches(items))) {
    items <- coal(fitting_branch(items, names(x), root), items)
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

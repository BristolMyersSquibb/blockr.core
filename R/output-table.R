#' Table outputs
#'
#' A table output shows a table: its columns, the rows of one page and the
#' total row count. The constructor `new_table_output()` makes one, and the
#' coercion `as_table_output()` makes one from a result, by the result's class.
#' Its method for data frames pages and sorts the result. A package that knows
#' another kind of table, such as a lazy table that it pages in a database,
#' adds a method that returns a table output. Every renderer of table outputs
#' then draws it.
#'
#' A coercion takes the result and what the renderer asks with: the board
#' option values in `options`, what the client measured for the output's
#' element in `info`, and the output's parameters in `params`. The method for
#' data frames reads three parameters. It shows page `params$page`, the first
#' by default. A page holds `params$page_size` rows, or else as many as the
#' board option `n_rows` gives. The rows are sorted by the column that
#' `params$sort$column` names, from the largest value where `params$sort$desc`
#' is `TRUE`. Asking for a page past the last one is an error, so that the
#' renderer asks again.
#'
#' The rows hold values that JSON can carry. A number, a string or a logical
#' stays as it is, and a missing value becomes `null`. An infinite number and
#' `NaN` show as their names, such as `"Inf"`. A factor shows its labels. A
#' column of another class, such as a date, shows the text that [format()]
#' gives. A list-column shows each cell that is a single number, string or
#' logical as it is. Any other cell shows as a summary, as [result_summary()]
#' gives it, such as `<lm>` or `<data.frame> [3 × 2]`. The same goes for each
#' row of a matrix or data frame column. Each column keeps its name, its first
#' class and its label attribute, where it has one.
#'
#' @param columns The table's columns: a data frame with the character columns
#'   `name` and `class`, and optionally `label`, which is `NA` for a column
#'   without a label
#' @param rows One page of the table's rows, as a data frame with a column for
#'   each of `columns`
#' @param total The table's total row count
#' @param ... For `new_table_output()`, further fields, which a subclass adds,
#'   and otherwise passed on to methods
#' @param class The classes of a subclass, which come before `table_output`
#' @param result The result to make an output from
#' @param options The values of the board options, by board option id
#' @param info What the client measured for the output's element
#' @param params The output's parameters
#'
#' @examples
#' new_table_output(
#'   columns = data.frame(name = "x", class = "integer"),
#'   rows = data.frame(x = 1:3),
#'   total = 10L
#' )
#'
#' out <- as_table_output(
#'   datasets::iris,
#'   options = list(n_rows = 5L),
#'   params = list(page = 2L, sort = list(column = "Sepal.Length"))
#' )
#'
#' out$total
#' out$rows
#'
#' @return Both `new_table_output()` and `as_table_output()` return an object
#' of class `table_output`.
#'
#' @export
new_table_output <- function(columns, rows, total, ..., class = character()) {

  if (!is_table_columns(columns)) {
    blockr_abort(
      "The columns of a table output are expected to be a data frame with ",
      "the character columns `name` and `class`, and optionally `label`.",
      class = "table_output_invalid"
    )
  }

  if (!is.data.frame(rows) || ncol(rows) != nrow(columns)) {
    blockr_abort(
      "The rows of a table output are expected to be a data frame with a ",
      "column for each of its {nrow(columns)} column{?s}.",
      class = "table_output_invalid"
    )
  }

  if (!is_count(total) || total < nrow(rows)) {
    blockr_abort(
      "The total row count of a table output is expected to be a count, at ",
      "least as large as the rows it holds.",
      class = "table_output_invalid"
    )
  }

  new_block_output(
    list(columns = columns, rows = table_rows(rows), total = total, ...),
    c(class, "table_output")
  )
}

is_table_columns <- function(x) {

  if (!is.data.frame(x) || !all(c("name", "class") %in% names(x)) ||
        !all(names(x) %in% c("name", "class", "label"))) {
    return(FALSE)
  }

  all(lgl_ply(x, is.character)) && !anyNA(x[["name"]]) &&
    !anyNA(x[["class"]])
}

#' @rdname new_table_output
#' @export
as_table_output <- function(result, options = list(), info = list(),
                            params = list(), ...) {
  UseMethod("as_table_output")
}

#' @rdname new_table_output
#' @export
as_table_output.data.frame <- function(result, options = list(),
                                       info = list(), params = list(), ...) {

  size <- coal(
    params[["page_size"]],
    options[["n_rows"]],
    board_option_value(new_n_rows_option()),
    fail_all = FALSE
  )

  page <- coal(params[["page"]], 1L, fail_all = FALSE)

  if (!is_count(size, allow_zero = FALSE) ||
        !is_count(page, allow_zero = FALSE)) {
    blockr_abort(
      "A table output's page and page size are expected to be positive ",
      "counts.",
      class = "table_output_params_invalid"
    )
  }

  total <- nrow(result)
  last <- max(1L, ceiling(total / size))

  if (page > last) {
    blockr_abort(
      "The table has {last} page{?s}, so it has no page {page}.",
      class = "table_output_params_invalid"
    )
  }

  cols <- lapply(seq_along(result), function(j) result[[j]])
  idx <- table_order(result, cols, params[["sort"]])
  idx <- idx[(page - 1L) * size + seq_len(size)]
  idx <- idx[!is.na(idx)]

  new_table_output(
    columns = table_columns(cols, names(result)),
    rows = new_data_frame(
      set_names(lapply(cols, vec_slice, idx), names(result)),
      n = length(idx)
    ),
    total = total
  )
}

table_order <- function(result, cols, sort) {

  if (!length(sort)) {
    return(seq_len(nrow(result)))
  }

  column <- sort[["column"]]
  desc <- coal(sort[["desc"]], FALSE, fail_all = FALSE)

  if (!is_string(column) || !column %in% names(result) || !is_bool(desc)) {
    blockr_abort(
      "A table output is sorted by the name of one of its columns, ",
      "`params$sort$column`, from the largest value where ",
      "`params$sort$desc` is `TRUE`.",
      class = "table_output_params_invalid"
    )
  }

  col <- cols[[match(column, names(result))]]

  if (is.list(col) || is.array(col)) {
    blockr_abort(
      "A table output cannot be sorted by column {column}, whose cells are ",
      "not single values.",
      class = "table_output_params_invalid"
    )
  }

  order(col, decreasing = desc, na.last = TRUE, method = "radix")
}

table_columns <- function(cols, names) {
  data.frame(
    name = names,
    class = chr_ply(cols, function(col) class(col)[1L]),
    label = chr_ply(cols, column_label)
  )
}

column_label <- function(x) {

  res <- attr(x, "label", exact = TRUE)

  if (is_string(res)) res else NA_character_
}

table_rows <- function(x) {
  new_data_frame(
    set_names(lapply(seq_along(x), function(j) table_cells(x[[j]])), names(x)),
    n = nrow(x)
  )
}

# The cells of a column, as values that JSON can carry. A column is turned into
# such values only once, and a column of them stays as it is.
table_cells <- function(x) {

  if (is.data.frame(x) || is.array(x)) {
    return(
      chr_ply(seq_len(vec_size(x)), function(i) result_summary(vec_slice(x, i)))
    )
  }

  if (is.list(x)) {
    return(lapply(unname(unclass(x)), table_cell))
  }

  if (is.factor(x)) {
    return(as.character(x))
  }

  if (is.object(x)) {
    res <- trimws(format(x))
    res[is.na(x)] <- NA_character_
    return(res)
  }

  if (is.double(x) && any(is_special_number(x))) {
    return(lapply(as.vector(x), table_cell))
  }

  if (is.complex(x) || is.raw(x)) {
    return(as.character(x))
  }

  as.vector(x)
}

table_cell <- function(x) {

  if (!is.atomic(x) || length(x) != 1L || is.object(x) ||
        !typeof(x) %in% c("logical", "integer", "double", "character")) {
    return(result_summary(x))
  }

  if (is_special_number(x)) {
    return(as.character(x))
  }

  x
}

is_special_number <- function(x) {
  is.double(x) & (is.infinite(x) | is.nan(x))
}

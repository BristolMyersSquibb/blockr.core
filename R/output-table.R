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
#' board option `n_rows` gives, whose value the client sends with every output
#' request, the default of its definition where the board holds none. The rows
#' are sorted by the column that `params$sort$column` names, from the largest
#' value where `params$sort$desc` is `TRUE`. Asking for a page past the last
#' one, or for a sort by a column that the table lacks, is an error, so that
#' the renderer asks again.
#'
#' The rows hold the values as the JSON writer writes them, so that numbers,
#' strings, logicals, factors and dates show as R prints them. A missing value
#' becomes `null`, and so do an infinite number and `NaN`, which JSON cannot
#' hold. A cell that is not a single value shows its summary instead, as
#' [result_summary()] gives it, such as `<lm>` or `<data.frame> [3 × 2]`. So
#' the table stays flat, whatever a list-column or a matrix or data frame
#' column holds. Values of other classes go to the writer as they are, which
#' writes a time in UTC and fails on a complex number. Each column keeps its
#' name and its first class. Row names are dropped, as tibble drops them. A
#' table that needs more takes a renderer of its own.
#'
#' The total row count may be unknown, as for a lazy table, whose count would
#' run its whole query. A renderer then shows no page count, and a page with
#' fewer rows than a page holds is the last.
#'
#' @param columns The table's columns: a data frame with the character columns
#'   `name` and `class`
#' @param rows One page of the table's rows, as a data frame with a column for
#'   each of `columns`
#' @param total The table's total row count, or `NA` where it is unknown
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
      "the character columns `name` and `class`.",
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

  if (!is_unknown(total) && (!is_count(total) || total < nrow(rows))) {
    blockr_abort(
      "The total row count of a table output is expected to be a count, at ",
      "least as large as the rows it holds, or `NA` where it is unknown.",
      class = "table_output_invalid"
    )
  }

  new_block_output(
    list(columns = columns, rows = table_rows(rows), total = total, ...),
    c(class, "table_output")
  )
}

is_table_columns <- function(x) {

  if (!is.data.frame(x) || !setequal(names(x), c("name", "class"))) {
    return(FALSE)
  }

  all(lgl_ply(x, is.character)) && !anyNA(x[["name"]]) &&
    !anyNA(x[["class"]])
}

is_unknown <- function(x) {
  is.atomic(x) && is_scalar(x) && is.na(x)
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

  size <- coal(params[["page_size"]], options[["n_rows"]], fail_all = FALSE)

  if (is.null(size)) {
    blockr_abort(
      "A table output takes its page size from the parameter `page_size` or ",
      "the board option `n_rows`, but neither is given.",
      class = "table_output_params_invalid"
    )
  }

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
  sort <- params[["sort"]]
  idx <- seq_len(total)

  if (length(sort)) {

    column <- sort[["column"]]

    if (is_string(column) && !column %in% names(result)) {
      blockr_abort(
        "The table has no column {column} to sort by.",
        class = "table_output_params_invalid"
      )
    }

    idx <- order(
      result[[column]],
      decreasing = isTRUE(sort[["desc"]]),
      method = "radix"
    )
  }

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

table_columns <- function(cols, names) {
  data.frame(
    name = names,
    class = chr_ply(cols, function(col) class(col)[1L])
  )
}

table_rows <- function(x) {
  new_data_frame(
    set_names(lapply(seq_along(x), function(j) table_cells(x[[j]])), names(x)),
    n = nrow(x)
  )
}

# The cells of a column, which the JSON writer writes as they are, but for a
# cell that is not a single value. That shows its summary, so that the table
# stays flat.
table_cells <- function(x) {

  if (is.data.frame(x) || is.array(x)) {
    return(
      chr_ply(seq_len(vec_size(x)), function(i) result_summary(vec_slice(x, i)))
    )
  }

  if (is.list(x)) {
    return(lapply(unname(unclass(x)), table_cell))
  }

  x
}

# A cell of a list-column: a single number, string or logical as it is, and
# anything else as its summary.
table_cell <- function(x) {

  if (is.atomic(x) && length(x) == 1L && !is.object(x) &&
        typeof(x) %in% c("logical", "integer", "double", "character")) {
    return(x)
  }

  result_summary(x)
}

#' @export
output_schema.table_output <- function(x, ...) {
  read_json_schema("outputs/table-output.schema.json")
}

#' @export
output_renderer.table_output <- function(x, ...) {
  chosen_renderer(
    "table_output",
    new_renderer("table_output", js_module("js/basic-table.js"))
  )
}

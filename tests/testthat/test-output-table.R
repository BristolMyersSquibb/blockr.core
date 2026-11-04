# The value of the board option n_rows that the client sends with every output
# request, the default of its definition where the board holds none
n_rows <- list(n_rows = 50L)

table_json <- function(x) {
  as.character(encode_message(shape_output(x)$output))
}

test_that("a table output holds columns, a page of rows and a total", {

  out <- new_table_output(
    columns = data.frame(name = c("x", "y"), class = c("integer", "factor")),
    rows = data.frame(x = 1:2, y = factor(c("a", "b"))),
    total = 5L
  )

  expect_s3_class(out, c("table_output", "block_output"), exact = TRUE)
  expect_identical(out$rows$y, factor(c("a", "b")))

  expect_identical(
    table_json(out),
    paste0(
      "{\"columns\":[{\"name\":\"x\",\"class\":\"integer\"},",
      "{\"name\":\"y\",\"class\":\"factor\"}],",
      "\"rows\":[[1,\"a\"],[2,\"b\"]],\"total\":5}"
    )
  )

  sub <- new_table_output(
    columns = data.frame(name = "x", class = "integer"),
    rows = data.frame(x = 1L),
    total = 1L,
    sql = "SELECT x FROM t",
    class = "lazy_table_output"
  )

  expect_s3_class(sub, c("lazy_table_output", "table_output", "block_output"),
                  exact = TRUE)
  expect_identical(sub$sql, "SELECT x FROM t")
})

test_that("a table output checks its fields", {

  cols <- data.frame(name = "x", class = "integer")
  rows <- data.frame(x = 1:2)

  expect_error(new_table_output(list(name = "x"), rows, 2L),
               class = "table_output_invalid")
  expect_error(new_table_output(data.frame(name = "x"), rows, 2L),
               class = "table_output_invalid")
  expect_error(new_table_output(data.frame(name = "x", class = 1), rows, 2L),
               class = "table_output_invalid")
  expect_error(
    new_table_output(data.frame(name = "x", class = "a", other = "b"), rows,
                     2L),
    class = "table_output_invalid"
  )
  expect_error(
    new_table_output(data.frame(name = NA_character_, class = "a"), rows, 2L),
    class = "table_output_invalid"
  )
  expect_error(new_table_output(cols, list(x = 1:2), 2L),
               class = "table_output_invalid")
  expect_error(new_table_output(cols, data.frame(x = 1, y = 2), 2L),
               class = "table_output_invalid")
  expect_error(new_table_output(cols, rows, 1L),
               class = "table_output_invalid")
  expect_error(new_table_output(cols, rows, -1L),
               class = "table_output_invalid")
  expect_error(new_table_output(cols, rows, 2.5),
               class = "table_output_invalid")
  expect_error(new_table_output(cols, rows, c(NA, NA)),
               class = "table_output_invalid")
  expect_error(
    new_table_output(data.frame(name = "x", class = "a", label = "b"), rows,
                     2L),
    class = "table_output_invalid"
  )
})

test_that("a table output's total row count may be unknown", {

  out <- new_table_output(
    columns = data.frame(name = "x", class = "integer"),
    rows = data.frame(x = 1:2),
    total = NA
  )

  expect_identical(out$total, NA)
  expect_match(table_json(out), "\"total\":null", fixed = TRUE)
})

test_that("a data frame's table output pages by n_rows and its parameters", {

  iris <- datasets::iris

  out <- as_table_output(utils::head(iris, 7L), options = list(n_rows = 3L),
                         params = list(page = 2L))

  expect_identical(out$total, 7L)
  expect_identical(out$rows$Sepal.Length, iris$Sepal.Length[4:6])
  expect_identical(out$columns$name, names(iris))
  expect_identical(out$columns$class, c(rep("numeric", 4L), "factor"))

  out <- as_table_output(utils::head(iris, 7L), options = list(n_rows = 3L),
                         params = list(page = 3L))

  expect_identical(out$rows$Sepal.Length, iris$Sepal.Length[7L])

  out <- as_table_output(iris, options = list(n_rows = 3L),
                         params = list(page = 2L, page_size = 10L))

  expect_identical(out$rows$Sepal.Length, iris$Sepal.Length[11:20])

  # The client sends n_rows with every output request, so R reads no blockr
  # option for it
  withr::local_options(blockr.n_rows = 4L)

  expect_error(
    as_table_output(iris),
    "neither is given",
    class = "table_output_params_invalid"
  )

  expect_error(as_table_output(iris, params = list(page = 0L, page_size = 5L)),
               class = "table_output_params_invalid")
  expect_error(as_table_output(iris, params = list(page_size = 1.5)),
               class = "table_output_params_invalid")
  expect_error(
    as_table_output(iris, options = n_rows, params = list(page = "2")),
    class = "table_output_params_invalid"
  )
  expect_error(
    as_table_output(utils::head(iris, 7L), options = list(n_rows = 3L),
                    params = list(page = 4L)),
    class = "table_output_params_invalid"
  )
})

test_that("an empty table has one page, which holds no rows", {

  out <- as_table_output(datasets::iris[0L, ], options = n_rows)

  expect_identical(out$total, 0L)
  expect_identical(nrow(out$rows), 0L)
  expect_identical(nrow(out$columns), 5L)

  expect_match(table_json(out), "\"rows\":[]", fixed = TRUE)

  expect_error(
    as_table_output(datasets::iris[0L, ], options = n_rows,
                    params = list(page = 2L)),
    class = "table_output_params_invalid"
  )
})

test_that("a data frame's table output sorts by its parameters", {

  df <- data.frame(x = c(2, NA, 3, 1), y = c("b", "D", "a", "C"))

  asc <- as_table_output(df, options = n_rows,
                         params = list(sort = list(column = "x")))
  expect_identical(asc$rows$x, c(1, 2, 3, NA))

  desc <- as_table_output(df, options = n_rows,
                          params = list(sort = list(column = "x", desc = TRUE)))
  expect_identical(desc$rows$x, c(3, 2, 1, NA))

  # Strings sort by their bytes, whatever the locale, so that "D" comes before
  # "a".
  chr <- as_table_output(df, options = n_rows,
                         params = list(sort = list(column = "y")))
  expect_identical(chr$rows$y, c("C", "D", "a", "b"))

  paged <- as_table_output(
    df,
    options = list(n_rows = 2L),
    params = list(page = 2L, sort = list(column = "x", desc = TRUE))
  )
  expect_identical(paged$rows$x, c(1, NA))
  expect_identical(paged$total, 4L)

  # Sorting is order() as it is, so a column it cannot sort fails with R's own
  # error.
  df$l <- list(1, 2, 3, 4)

  expect_error(as_table_output(df, options = n_rows,
                               params = list(sort = list(column = "l"))))

  # A column that the table lacks fails as a page past the last one does.
  expect_error(
    as_table_output(df, options = n_rows,
                    params = list(sort = list(column = "z"))),
    "no column z to sort by",
    class = "table_output_params_invalid"
  )
})

test_that("a table output's cells hold values that JSON can carry", {

  df <- data.frame(
    num = c(1.5, Inf, -Inf, NaN, NA),
    int = c(1L, NA, 3L, 4L, 5L),
    lgl = c(TRUE, FALSE, NA, TRUE, TRUE),
    chr = c("a", NA, "c", "d", "e"),
    fct = factor(c("u", "v", NA, "u", "v")),
    date = as.Date("2024-01-31") + 0:4,
    time = as.POSIXct("2024-01-31 12:30:00", tz = "UTC") + 0:4 * 3600
  )

  df$lst <- list(1:3, "one", NULL, datasets::BOD, TRUE)
  df$mat <- matrix(1:10, ncol = 2L)
  df$sub <- data.frame(a = 1:5, b = letters[1:5])

  attr(df$num, "label") <- "A number"
  df$date[2L] <- NA

  out <- as_table_output(df, options = n_rows)

  expect_identical(
    out$columns$class,
    c("numeric", "integer", "logical", "character", "factor", "Date",
      "POSIXct", "list", "matrix", "data.frame")
  )

  json <- read_json_strictly(table_json(out))

  expect_identical(
    json$rows[[1L]],
    list(1.5, 1, TRUE, "a", "u", "2024-01-31", "2024-01-31 12:30:00",
         "<integer> [3]", "<integer> [1 \u00d7 2]",
         "<data.frame> [1 \u00d7 2]")
  )

  expect_identical(
    json$rows[[2L]][c(1L, 2L, 4L, 6L, 8L)],
    list(NULL, NULL, NULL, NULL, "one")
  )

  # JSON holds no infinite number or NaN, so they become null, as NA does.
  expect_identical(lapply(json$rows, `[[`, 1L),
                   list(1.5, NULL, NULL, NULL, NULL))

  # A factor's missing value is null too.
  expect_null(json$rows[[3L]][[5L]])

  expect_identical(lapply(json$rows, `[[`, 8L),
                   list("<integer> [3]", "one", "<NULL>",
                        "<data.frame> [6 \u00d7 2]", TRUE))

  # A column's attributes, such as its label, stay in R.
  expect_identical(names(json$columns[[1L]]), c("name", "class"))
})

test_that("a table's cells are turned into JSON values only once", {

  df <- data.frame(
    t = as.POSIXct("2024-01-31 12:30:00", tz = "Europe/Berlin") + 0:1
  )
  df$l <- list(1:2, "a")

  out <- as_table_output(df, options = n_rows)
  again <- new_table_output(out$columns, out$rows, out$total)

  expect_identical(again$rows, out$rows)
})

test_that("a table output keeps names that R would change", {

  df <- data.frame(`2019` = 1L, a = 2L, a = 3L, check.names = FALSE)

  out <- as_table_output(df, options = n_rows)

  expect_identical(out$columns$name, c("2019", "a", "a"))
  expect_match(table_json(out), "\"rows\":[[1,2,3]]", fixed = TRUE)
})

test_that("tables of other data frame classes page as data frames do", {

  skip_if_not_installed("tibble")

  tbl <- tibble::as_tibble(datasets::mtcars)
  out <- as_table_output(tbl, options = list(n_rows = 2L),
                         params = list(sort = list(column = "mpg")))

  expect_identical(out$rows$mpg, sort(datasets::mtcars$mpg)[1:2])
  expect_identical(out$total, 32L)
})

test_that("a result without a method has no table output", {

  expect_error(
    as_table_output(list(1)),
    "no applicable method for 'as_table_output' applied to an object of class"
  )
})

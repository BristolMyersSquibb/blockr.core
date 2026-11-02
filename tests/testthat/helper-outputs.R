# The sample outputs of core's output classes, in inst/outputs/fixtures, which
# every renderer of a class is tested on. R makes them here, with the coercions
# of the classes. To write the fixtures again after a change, run
# write_output_fixtures() with the package loaded by pkgload::load_all().

output_fixture_dir <- function() {
  system.file("outputs", "fixtures", package = "blockr.core")
}

output_fixtures <- function() {
  sub("\\.json$", "", list.files(output_fixture_dir(), pattern = "\\.json$"))
}

output_fixture <- function(name) {
  file <- file.path(output_fixture_dir(), paste0(name, ".json"))
  paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# The R values of each fixture, by its file name: a description and the sample
# outputs, each with what it was asked with.
output_fixture_values <- function() {

  withr::local_options(width = 80L)

  list(
    `table-output` = list(
      description = paste(
        "Sample table outputs, as the method of as_table_output() for data",
        "frames makes them, with the renderer that core declares for table",
        "outputs."
      ),
      outputs = list(
        output_sample(
          paste(
            "The second page of the first seven rows of iris, at three rows",
            "to a page, as the board option n_rows gives."
          ),
          as_table_output,
          utils::head(datasets::iris, 7L),
          options = list(n_rows = 3L),
          params = list(page = 2L)
        ),
        output_sample(
          paste(
            "The last page of airquality, sorted by Ozone from the largest,",
            "at fifty rows to a page, where the rows without Ozone come last",
            "and a missing value is null."
          ),
          as_table_output,
          datasets::airquality,
          options = list(n_rows = 50L),
          params = list(page = 4L, sort = list(column = "Ozone", desc = TRUE))
        ),
        output_sample(
          paste(
            "Cells of many kinds: an infinite number and NaN as their names,",
            "a factor as its labels, a date and a time as format() gives",
            "them, a list-column as a summary of each cell that is not a",
            "single value, a column with a label, and names that R would",
            "change, such as 2019 and a name that repeats."
          ),
          as_table_output,
          output_fixture_cells(),
          options = list(n_rows = 50L)
        ),
        output_sample(
          "A table without rows, which has one page, holding no rows.",
          as_table_output,
          datasets::iris[0L, ],
          options = list(n_rows = 50L)
        )
      )
    ),
    `text-output` = list(
      description = paste(
        "Sample text outputs, as as_text_output() prints a result, at R's",
        "width of 80, with the renderer that core declares for text outputs."
      ),
      outputs = list(
        output_sample(
          "Thirty integers, as print() wraps them at a width of 80.",
          as_text_output,
          1:30
        ),
        output_sample(
          "A named character vector, as print() quotes and aligns it.",
          as_text_output,
          c(first = "a", second = "bb")
        )
      )
    ),
    `plot-output` = list(
      description = paste(
        "Sample plot outputs, as as_plot_output() draws a captured base",
        "plot, with the renderer that core declares for plot outputs. The",
        "PNG of each is the file its body_file names, beside this one."
      ),
      outputs = list(
        output_sample(
          paste(
            "A scatter plot of iris, drawn at 320 by 200 CSS pixels and a",
            "pixel ratio of 2, so that its PNG has 640 by 400 pixels."
          ),
          as_plot_output,
          capture_plot(
            quote(
              plot(a$Sepal.Length, a$Sepal.Width, xlab = "Sepal.Length",
                   ylab = "Sepal.Width")
            ),
            list2env(list(a = datasets::iris))
          ),
          info = list(width = 320L, height = 200L, pixel_ratio = 2L,
                      bg = "#ffffff", fg = "#212529", accent = "#0d6efd",
                      font = "Inter"),
          body_file = "plot-output-scatter.png"
        )
      )
    )
  )
}

output_fixture_cells <- function() {

  res <- data.frame(
    number = c(1.5, Inf, -Inf, NaN, NA),
    count = c(1L, NA, 3L, 4L, 5L),
    flag = c(TRUE, FALSE, NA, TRUE, TRUE),
    text = c("a", NA, "c", "d", "e"),
    level = factor(c("low", "high", NA, "low", "high")),
    date = as.Date(c("2024-01-31", NA, "2024-02-02", "2024-02-03",
                     "2024-02-04")),
    time = as.POSIXct("2024-01-31 12:30:00", tz = "UTC") + 0:4 * 3600,
    `2019` = 1:5,
    text = c("v", "w", "x", "y", "z"),
    check.names = FALSE
  )

  res$cells <- list(1:3, "one", NULL, datasets::BOD,
                    stats::lm(dist ~ speed, datasets::cars))

  attr(res$number, "label") <- "A number with a label"

  res
}

# A sample output: its description, what it was asked with, and the output as R
# sends it, after the renderer that core declares for its class. An output
# with a body announces it, and names the file that holds its bytes.
output_sample <- function(description, coercion, result, options = list(),
                          info = list(), params = list(), body_file = NULL) {

  out <- coercion(result, options = options, info = info, params = params)
  parts <- shape_output(out)

  res <- list(
    description = description,
    options = as_json_object(options),
    params = as_json_object(params),
    output = c(list(renderer = core_renderer(out)), parts$output)
  )

  if (not_null(body_file)) {
    res$body <- body_announcement(parts$body)
    res$body_file <- body_file
    res$bytes <- parts$body
  }

  res
}

core_renderer <- function(x) {
  switch(
    class(x)[1L],
    table_output = "blockr.core-basic_table",
    text_output = "blockr.core-basic_text",
    plot_output = "blockr.core-basic_plot"
  )
}

# A fixture's JSON, without the bytes of the bodies, which go to files of their
# own.
output_fixture_json <- function(value) {

  value$outputs <- lapply(value$outputs, function(x) x[names(x) != "bytes"])

  json_str(value, auto_unbox = TRUE, pretty = TRUE)
}

write_output_fixtures <- function(dir = file.path("inst", "outputs",
                                                  "fixtures")) {

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  values <- output_fixture_values()

  for (name in names(values)) {

    value <- values[[name]]

    writeLines(output_fixture_json(value),
               file.path(dir, paste0(name, ".json")), useBytes = TRUE)

    for (sample in value$outputs) {
      if (not_null(sample$bytes)) {
        writeBin(reply_body_bytes(sample$bytes),
                 file.path(dir, sample$body_file))
      }
    }
  }

  invisible(dir)
}

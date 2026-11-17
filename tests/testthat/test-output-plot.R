png_signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))

is_png <- function(x) {
  identical(utils::head(reply_body_bytes(x), 8L), png_signature)
}

dark <- list(width = 320, height = 240, pixel_ratio = 2, bg = "#212529",
             fg = "#f8f9fa", accent = "#0d6efd", font = "Inter")

test_that("a plot output holds the size of its PNG body", {

  body <- new_reply_body(as.raw(1:3), "image/png")
  out <- new_plot_output(body, 640, 400)

  expect_s3_class(out, c("plot_output", "block_output"), exact = TRUE)
  expect_identical(out$body, body)
  expect_identical(
    as.character(encode_message(shape_output(out)$output)),
    "{\"width\":640.0,\"height\":400.0}"
  )

  expect_error(new_plot_output(as.raw(1:3), 640, 400),
               class = "plot_output_invalid")
  expect_error(new_plot_output(body, 0, 400), class = "plot_output_invalid")
  expect_error(new_plot_output(body, 640, c(1, 2)),
               class = "plot_output_invalid")
  expect_error(new_plot_output(body, 640, NA), class = "plot_output_invalid")
})

test_that("a base plot is captured as the block expression runs", {

  env <- list2env(list(a = data.frame(x = 1:3, y = 3:1)))
  dev <- grDevices::dev.cur()

  plt <- capture_plot(quote(plot(a$x, a$y)), env)

  expect_s3_class(plt, "captured_plot")
  expect_s3_class(plt$cache$recording, "recordedplot")
  expect_null(plt$cache$theme)
  expect_identical(grDevices::dev.cur(), dev)
  expect_identical(format(plt), "<captured_plot>")
  expect_identical(result_summary(plt), "<captured_plot>")

  expect_error(capture_plot(quote(stop("broken")), env), "broken")
  expect_identical(grDevices::dev.cur(), dev)

  # The function that draws the plot again keeps no recording alive.
  expect_setequal(ls(environment(plt$redraw)), c("expr", "env"))
})

test_that("capturing leaves the current device current", {

  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  withr::defer(grDevices::dev.off(dev))

  capture_plot(quote(plot(1:3)), new.env())

  expect_identical(grDevices::dev.cur(), dev)
})

test_that("an expression that closes the device leaves no recording", {

  plt <- capture_plot(quote(grDevices::dev.off()), new.env())

  expect_null(plt$cache$recording)
})

test_that("a captured plot prints as it was recorded", {

  plt <- capture_plot(quote(plot(1:3)), new.env())

  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())

  expect_identical(print(plt), plt)
})

test_that("a block_eval() method makes a captured plot the block's result", {

  cls <- new_block_class(
    "captured_scatter_block",
    inputs = "data",
    expr = function(x = "x", y = "y") {
      bbquote(plot(.(data)[[.(x)]], .(data)[[.(y)]]))
    },
    ui = js_module("scatter.js"),
    output = as_plot_output
  )

  registerS3method(
    "block_eval",
    "captured_scatter_block",
    function(x, expr, env, ...) capture_plot(expr, env),
    envir = asNamespace("blockr.core")
  )

  res <- evaluate_block(cls, list(), list(data = "a"),
                        list(a = data.frame(x = 1:3, y = 3:1)))

  expect_identical(res$status, "ok")
  expect_identical(res$expr, quote(plot(a[["x"]], a[["y"]])))
  expect_s3_class(res$result, "captured_plot")
  expect_identical(res$summary, "<captured_plot>")

  out <- cls$output(res$result, list(), list(width = 200, height = 200),
                    list())

  expect_s3_class(out, "plot_output")
  expect_true(is_png(out$body))
})

test_that("a captured plot is drawn again only for new colors", {

  skip_if_not_installed("thematic")

  runs <- 0L

  env <- list2env(
    list(
      a = data.frame(x = 1:3, y = 3:1),
      counted = function() runs <<- runs + 1L
    )
  )

  plt <- capture_plot(quote({
    counted()
    plot(a$x, a$y)
  }), env)

  expect_identical(runs, 1L)

  out <- as_plot_output(plt, info = dark)

  expect_s3_class(out, "plot_output")
  expect_identical(out$width, 320)
  expect_identical(out$height, 240)
  expect_true(is_png(out$body))
  expect_identical(out$body$type, "image/png")
  expect_identical(runs, 2L)
  expect_identical(plt$cache$theme$bg, "#212529")

  as_plot_output(plt, info = utils::modifyList(dark, list(width = 640)))
  expect_identical(runs, 2L)

  as_plot_output(plt, info = utils::modifyList(dark, list(bg = "#ffffff")))
  expect_identical(runs, 3L)
})

test_that("without thematic, a captured plot is replayed at any size", {

  local_mocked_bindings(pkg_avail = function(...) FALSE)

  runs <- 0L
  env <- list2env(list(counted = function() runs <<- runs + 1L))

  plt <- capture_plot(quote({
    counted()
    plot(1:3)
  }), env)

  out <- as_plot_output(plt, info = dark)

  expect_true(is_png(out$body))
  expect_identical(runs, 1L)
})

test_that("a captured plot without a recording is drawn again", {

  runs <- 0L
  counted <- function() runs <<- runs + 1L

  plt <- new_captured_plot(NULL, function() {
    counted()
    plot(1:3)
  })

  out <- as_plot_output(plt, info = list(width = 200, height = 200))

  expect_true(is_png(out$body))
  expect_identical(runs, 1L)
  expect_s3_class(plt$cache$recording, "recordedplot")

  as_plot_output(plt, info = list(width = 300, height = 200))
  expect_identical(runs, 1L)
})

test_that("a replay that fails gives way to drawing the plot again", {

  runs <- 0L
  env <- list2env(list(counted = function() runs <<- runs + 1L))

  plt <- capture_plot(quote({
    counted()
    plot(1:3)
  }), env)

  local_mocked_bindings(plot_theme = function(info) NULL)

  # Too small for the plot's margins, which a replay reports as an invalid
  # graphics state, while drawing the plot names the margins.
  expect_error(
    as_plot_output(plt, info = list(width = 200, height = 100)),
    "figure margins too large"
  )
  expect_identical(runs, 2L)
})

test_that("a ggplot is printed into a plot output", {

  skip_if_not_installed("ggplot2")

  plt <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point()

  out <- as_plot_output(plt, info = dark)

  expect_s3_class(out, "plot_output")
  expect_true(is_png(out$body))
  expect_identical(c(out$width, out$height), c(320, 240))
})

test_that("a plot is drawn at the size that info gives", {

  plt <- capture_plot(quote(plot(1:3)), new.env())

  expect_error(as_plot_output(plt), class = "plot_output_info_invalid")
  expect_error(as_plot_output(plt, info = list(width = 100)),
               class = "plot_output_info_invalid")
  expect_error(
    as_plot_output(plt, info = list(width = 100, height = 100,
                                    pixel_ratio = 0)),
    class = "plot_output_info_invalid"
  )

  expect_identical(
    plot_size(list(width = 100, height = 50)),
    list(width = 100, height = 50, ratio = 1)
  )
})

test_that("the theme takes the colors of info, and its font where it exists", {

  local_mocked_bindings(
    pkg_avail = function(...) TRUE,
    font_available = function(family) identical(family, "Inter")
  )

  expect_null(plot_theme(list(width = 1)))
  expect_identical(plot_theme(list(bg = "#fff")), list(bg = "#fff"))
  expect_identical(
    plot_theme(dark),
    list(bg = "#212529", fg = "#f8f9fa", accent = "#0d6efd", font = "Inter")
  )
  expect_identical(
    plot_theme(utils::modifyList(dark, list(font = "Comic Sans"))),
    list(bg = "#212529", fg = "#f8f9fa", accent = "#0d6efd")
  )
})

test_that("the font is the first family of a CSS font list", {

  expect_identical(first_font_family("Inter"), "Inter")
  expect_identical(
    first_font_family("\"Source Sans 3\", system-ui, sans-serif"),
    "Source Sans 3"
  )
  expect_identical(first_font_family("'Fira Sans' , serif"), "Fira Sans")
  expect_null(first_font_family(""))
  expect_null(first_font_family(NULL))
  expect_null(first_font_family(1))
})

test_that("a font is available where the system has it", {

  expect_false(font_available(NULL))

  skip_if_not_installed("systemfonts")

  family <- systemfonts::system_fonts()$family[1L]

  expect_true(font_available(family))
  expect_false(font_available("No Such Font Family"))
})

test_that("a PNG is drawn without ragg too", {

  local_mocked_bindings(pkg_avail = function(...) FALSE)

  size <- list(width = 200, height = 200, ratio = 1)
  png <- draw_png(function() plot(1:3), size, NULL)

  expect_identical(utils::head(png$png, 8L), png_signature)
  expect_null(png$recording)
})

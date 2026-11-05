#' Plot outputs
#'
#' A plot output shows a plot as a PNG image. The image travels as the
#' output's binary body, and the output gives the size the plot was drawn at,
#' in CSS pixels. The constructor `new_plot_output()` makes one. The coercion
#' `as_plot_output()` makes one from a result, by the result's class. It draws
#' the plot at the size that `info` gives, at its pixel ratio. Where the
#' thematic package is installed, it draws with the colors and the font that
#' `info` gives, so that the plot follows the page's theme. A change of theme
#' then draws the plot again, without evaluating the block again.
#'
#' Core's methods draw two kinds of results. A ggplot is printed again. A base
#' plot is different: it is drawn while the block expression runs, and the
#' expression returns nothing to print. So a block class whose expression draws
#' a base plot captures the plot, with a [block_eval()] method that calls
#' `capture_plot()`:
#'
#' ```r
#' block_eval.scatter_block <- function(x, expr, env, ...) {
#'   capture_plot(expr, env)
#' }
#' ```
#'
#' The function `capture_plot()` runs the block expression on an off-screen
#' device. It returns a plot object of class `captured_plot`, which becomes the
#' block's result. The object holds a recording of the plot and a function that
#' runs the expression again. The block expression itself stays a plain call,
#' such as `plot(a$x, a$y)`, so that an exported script draws the plot. When a
#' plot is drawn with the colors of its recording, the coercion replays the
#' recording at the new size. When the colors change, it runs the expression
#' again, since a recording keeps the colors it was drawn with. It then keeps
#' the new recording for the next size. It also runs the expression again where
#' a replay fails, such as at a size too small for the plot's margins, so that
#' the error is the one that drawing the plot gives.
#'
#' @param body The plot's PNG, as a reply body made by [new_reply_body()]
#' @param width,height The size the plot was drawn at, in CSS pixels
#' @param ... For `new_plot_output()`, further fields, which a subclass adds,
#'   and otherwise passed on to methods
#' @param class The classes of a subclass, which come before `plot_output`
#' @param info What the client measured for the output's element: its `width`
#'   and `height` in CSS pixels, its `pixel_ratio`, its colors `bg`, `fg` and
#'   `accent`, and its `font`
#' @param expr The block expression
#' @param env The environment that the block expression runs in, where each
#'   upstream block's result is bound to its id
#' @inheritParams new_table_output
#'
#' @examples
#' png <- tempfile(fileext = ".png")
#' grDevices::png(png)
#' plot(1:10)
#' invisible(grDevices::dev.off())
#'
#' new_plot_output(new_reply_body(png, "image/png"), 480, 480)
#'
#' plt <- capture_plot(quote(plot(a$x, a$y)),
#'                     list2env(list(a = data.frame(x = 1:3, y = 3:1))))
#' plt
#'
#' out <- as_plot_output(plt, info = list(width = 320, height = 240))
#' out
#'
#' @return Both `new_plot_output()` and `as_plot_output()` return an object of
#' class `plot_output`, while `capture_plot()` returns one of class
#' `captured_plot`.
#'
#' @export
new_plot_output <- function(body, width, height, ..., class = character()) {

  if (!is_reply_body(body)) {
    blockr_abort(
      "The body of a plot output is expected to be a reply body, made by ",
      "`new_reply_body()`, such as the plot's PNG.",
      class = "plot_output_invalid"
    )
  }

  if (!is_size(width) || !is_size(height)) {
    blockr_abort(
      "The width and height of a plot output are expected to be positive ",
      "numbers.",
      class = "plot_output_invalid"
    )
  }

  new_block_output(
    list(body = body, width = width, height = height, ...),
    c(class, "plot_output")
  )
}

is_size <- function(x) {
  is_number(x) && x > 0
}

#' @rdname new_plot_output
#' @export
as_plot_output <- function(result, options = list(), info = list(),
                           params = list(), ...) {
  UseMethod("as_plot_output")
}

#' @rdname new_plot_output
#' @export
as_plot_output.ggplot <- function(result, options = list(), info = list(),
                                  params = list(), ...) {

  size <- plot_size(info)
  png <- draw_png(function() print(result), size, plot_theme(info))

  new_plot_output(new_reply_body(png$png, "image/png"), size$width,
                  size$height)
}

#' @rdname new_plot_output
#' @export
as_plot_output.captured_plot <- function(result, options = list(),
                                         info = list(), params = list(),
                                         ...) {

  size <- plot_size(info)
  theme <- plot_theme(info)
  cache <- result[["cache"]]
  png <- NULL

  if (not_null(cache$recording) && identical(cache$theme, theme)) {
    png <- tryCatch(
      draw_png(
        function() grDevices::replayPlot(cache$recording),
        size,
        theme,
        apply_theme = FALSE
      ),
      error = function(e) NULL
    )
  }

  if (is.null(png)) {

    png <- draw_png(result[["redraw"]], size, theme, record = TRUE)

    cache$recording <- png$recording
    cache$theme <- theme
  }

  new_plot_output(new_reply_body(png$png, "image/png"), size$width,
                  size$height)
}

#' @rdname new_plot_output
#' @export
capture_plot <- function(expr, env) {

  recording <- with_device(
    function() grDevices::pdf(NULL),
    function(dev) {
      grDevices::dev.control("enable")
      eval(expr, env)
      if (dev %in% grDevices::dev.list()) {
        grDevices::dev.set(dev)
        grDevices::recordPlot()
      }
    }
  )

  new_captured_plot(recording, redraw_function(expr, env))
}

# A function that runs the expression again. Its environment holds the
# expression and the environment it runs in, and nothing else, such as a
# recording that the plot has replaced since.
redraw_function <- function(expr, env) {
  force(expr)
  force(env)
  function() eval(expr, env)
}

new_captured_plot <- function(recording, redraw) {

  cache <- new.env(parent = emptyenv())
  cache$recording <- recording
  cache$theme <- NULL

  structure(list(redraw = redraw, cache = cache), class = "captured_plot")
}

#' @export
format.captured_plot <- function(x, ...) {
  "<captured_plot>"
}

#' @export
print.captured_plot <- function(x, ...) {

  recording <- x[["cache"]]$recording

  if (not_null(recording)) {
    grDevices::replayPlot(recording)
  }

  invisible(x)
}

# The size a plot is drawn at, from the output's info.
plot_size <- function(info) {

  width <- info[["width"]]
  height <- info[["height"]]
  ratio <- coal(info[["pixel_ratio"]], 1, fail_all = FALSE)

  if (!is_size(width) || !is_size(height) || !is_size(ratio)) {
    blockr_abort(
      "A plot output is drawn at the size that `info` gives, as positive ",
      "numbers `width` and `height`, and optionally `pixel_ratio`.",
      class = "plot_output_info_invalid"
    )
  }

  list(width = width, height = height, ratio = ratio)
}

# The theme a plot is drawn with: the colors and the font of the output's
# info, which thematic applies. It is `NULL` where thematic is not installed or
# the info gives no colors. The font is left out where the system lacks it.
plot_theme <- function(info) {

  colors <- Filter(is_string, info[c("bg", "fg", "accent")])

  if (!length(colors) || !pkg_avail("thematic")) {
    return(NULL)
  }

  font <- first_font_family(info[["font"]])

  c(colors, if (font_available(font)) list(font = font))
}

# The first family of a CSS font-family list, such as "Inter" of
# '"Inter", system-ui, sans-serif'.
first_font_family <- function(x) {

  if (!is_string(x)) {
    return(NULL)
  }

  res <- trimws(strsplit(x, ",", fixed = TRUE)[[1L]][1L])
  res <- gsub("^[\"']|[\"']$", "", res)

  if (is.na(res) || !nzchar(res)) NULL else res
}

font_available <- function(family) {

  if (is.null(family) || !pkg_avail("systemfonts")) {
    return(FALSE)
  }

  family %in% c(
    systemfonts::system_fonts()[["family"]],
    systemfonts::registry_fonts()[["family"]]
  )
}

thematic_plot_theme <- function(theme) {

  color <- function(x) coal(theme[[x]], NA, fail_all = FALSE)
  font <- NA

  if (not_null(theme[["font"]])) {
    font <- thematic::font_spec(theme[["font"]], install = FALSE)
  }

  thematic::thematic_theme(
    bg = color("bg"),
    fg = color("fg"),
    accent = color("accent"),
    font = font
  )
}

# Draws a plot into a PNG at the given size and pixel ratio, by calling `draw`.
# The theme applies through thematic, unless `apply_theme` is `FALSE`, as for a
# recording that holds the theme already. Where `record` is `TRUE`, the plot is
# recorded as well, for a later replay.
draw_png <- function(draw, size, theme, record = FALSE, apply_theme = TRUE) {

  file <- tempfile(fileext = ".png")
  on.exit(unlink(file))

  recording <- with_device(
    function() open_png(file, size, coal(theme[["bg"]], "white")),
    function(dev) {

      if (record) {
        grDevices::dev.control("enable")
      }

      if (apply_theme && not_null(theme)) {
        thematic::thematic_with_theme(thematic_plot_theme(theme), draw())
      } else {
        draw()
      }

      if (record && dev %in% grDevices::dev.list()) {
        grDevices::dev.set(dev)
        grDevices::recordPlot()
      }
    }
  )

  list(png = readBin(file, "raw", file.size(file)), recording = recording)
}

open_png <- function(file, size, bg) {

  width <- round(size$width * size$ratio)
  height <- round(size$height * size$ratio)
  res <- 72 * size$ratio

  if (pkg_avail("ragg")) {
    ragg::agg_png(file, width = width, height = height, units = "px",
                  res = res, background = bg)
  } else {
    grDevices::png(file, width = width, height = height, units = "px",
                   res = res, bg = bg)
  }
}

# Calls `code` with a device that `open` opens, and closes that device again.
# The device that was current before is current again afterwards.
with_device <- function(open, code) {

  prev <- grDevices::dev.cur()

  open()
  dev <- grDevices::dev.cur()

  on.exit({
    if (dev %in% grDevices::dev.list()) {
      grDevices::dev.off(dev)
    }
    if (prev != 1L && prev %in% grDevices::dev.list()) {
      grDevices::dev.set(prev)
    }
  })

  code(dev)
}

#' @rdname output_schema
#' @export
output_schema.plot_output <- function(x, ...) {
  read_json_schema("outputs/plot-output.schema.json")
}

#' @rdname new_renderer
#' @export
output_renderer.plot_output <- function(x, ...) {
  chosen_renderer(
    "plot_output",
    new_renderer("plot_output", js_module("js/basic-plot.js"))
  )
}

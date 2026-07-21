#' Tabular result display
#'
#' How a data, parser or transform block previews its result is governed by a
#' *tabular display*: an S3 object selecting a coherent set of methods for the
#' output container (`tabular_ui()`), the render function (`tabular_output()`),
#' the render trigger (`tabular_trigger()`) and the board options the
#' preview relies on (`tabular_options()`). Bundling all four on one
#' object keeps them in sync: a display cannot declare a `page_size` option it
#' then fails to honor, or render through DT while its container is plain text.
#'
#' The active display is resolved from the `blockr.tabular_display` option (via
#' [blockr_option()]) and defaults to `minimal_display`, a compact preview of
#' the top rows (tibble-formatted when the suggested tibble package is
#' installed, base `print()` otherwise). `dt_display` reinstates the paginated,
#' searchable DT table. Downstream packages provide further displays by
#' defining methods for the four generics on their own `tabular_display`
#' sub-class and having users opt in with
#' `options(blockr.tabular_display = <display>)`.
#'
#' @param subclass Display sub-class string
#' @param x Tabular display object
#' @param id Namespace ID for the output container
#' @param result Block result
#' @param block Block object (used to read board option defaults)
#' @param session Shiny session object
#' @param ... Forwarded to board option constructors
#'
#' @return `new_tabular_display()` and `tabular_display()` return a
#' `tabular_display` object and `is_tabular_display()` a boolean.
#' `tabular_ui()` returns shiny UI, `tabular_output()` the result of a shiny
#' render function, `tabular_trigger()` is called for its reactive side
#' effect (invisibly returning the option values it depends on) and
#' `tabular_options()` a [board_options][new_board_options] set.
#'
#' @name tabular-display
#' @export
new_tabular_display <- function(subclass) {
  structure(list(), class = c(subclass, "tabular_display"))
}

#' @rdname tabular-display
#' @format NULL
#' @export
minimal_display <- new_tabular_display("minimal_display")

#' @rdname tabular-display
#' @format NULL
#' @export
dt_display <- new_tabular_display("dt_display")

#' @rdname tabular-display
#' @export
is_tabular_display <- function(x) {
  inherits(x, "tabular_display")
}

#' @rdname tabular-display
#' @export
tabular_display <- function() {

  res <- blockr_option("tabular_display", minimal_display)

  if (is_tabular_display(res)) res else minimal_display
}

#' @rdname tabular-display
#' @export
tabular_ui <- function(x, id) {
  UseMethod("tabular_ui")
}

#' @rdname tabular-display
#' @export
tabular_output <- function(x, result, block, session) {
  UseMethod("tabular_output")
}

#' @rdname tabular-display
#' @export
tabular_trigger <- function(x, session) {
  UseMethod("tabular_trigger")
}

#' @rdname tabular-display
#' @export
tabular_options <- function(x, ...) {
  UseMethod("tabular_options")
}

#' @export
tabular_ui.minimal_display <- function(x, id) {
  tagList(
    tagAppendAttributes(
      verbatimTextOutput(id),
      class = "blockr-minimal-preview"
    ),
    htmltools::htmlDependency(
      "blockr-minimal-preview",
      pkg_version(),
      src = pkg_file("assets", "js"),
      script = "minimalPreviewWidth.js"
    )
  )
}

#' @export
tabular_output.minimal_display <- function(x, result, block, session) {

  rows <- get_board_option_or_default("n_rows", board_options(block), session)

  renderText(
    {
      dat <- as.data.frame(utils::head(result, rows))

      val <- if (tibble_available()) {
        tibble::as_tibble(dat, .name_repair = "minimal")
      } else {
        dat
      }

      old <- options(width = minimal_preview_width(session))
      on.exit(options(old), add = TRUE)

      paste(utils::capture.output(print(val)), collapse = "\n")
    }
  )
}

#' @export
tabular_trigger.minimal_display <- function(x, session) {
  invisible(
    get_board_option_values("n_rows", if_not_found = "null", session = session)
  )
}

#' @export
tabular_options.minimal_display <- function(x, ...) {
  combine_board_options(new_n_rows_option(...))
}

#' @export
tabular_ui.dt_display <- function(x, id) {
  DT::dataTableOutput(id)
}

#' @export
tabular_output.dt_display <- function(x, result, block, session) {
  dt_result(result, block, session)
}

#' @export
tabular_trigger.dt_display <- function(x, session) {
  invisible(
    get_board_option_values(
      "n_rows",
      "page_size",
      "filter_rows",
      if_not_found = "null",
      session = session
    )
  )
}

#' @export
tabular_options.dt_display <- function(x, ...) {
  combine_board_options(
    new_n_rows_option(...),
    new_page_size_option(...),
    new_filter_rows_option(...)
  )
}

minimal_preview_width <- function(session, default = getOption("width", 80L)) {

  cols <- session$input[["result_cols"]]

  if (is.null(cols)) default else max(20L, as.integer(cols))
}

tibble_available <- function() {
  requireNamespace("tibble", quietly = TRUE)
}

dt_result <- function(result, block, session) {

  info_string <- function(total) {
    paste0(
      "\"Showing \" + start + \" to \" + end + \" of ",
      if (is.null(total)) "0" else if (is.na(total)) "??" else total,
      " entries\""
    )
  }

  need_pagination <- function(dat_row, show_row, page) {
    if (is.na(dat_row)) rows > page else min(dat_row, rows) > page
  }

  default_opts <- board_options(block)

  rows <- get_board_option_or_default("n_rows", default_opts, session)
  page <- get_board_option_or_default("page_size", default_opts, session)

  dom <- "rti"

  if (not_null(result) && need_pagination(nrow(result), rows, page)) {
    dom <- paste0(dom, "p")
  }

  if (get_board_option_or_default("filter_rows", default_opts, session)) {
    dom <- paste0("f", dom)
  }

  opts <- list(
    processing = FALSE,
    infoCallback = DT::JS(
      "function(settings, start, end, max, total, pre) {",
      "let res = ", info_string(nrow(result)), ";",
      "return res;",
      "}"
    ),
    dom = dom,
    pageLength = page,
    ordering = FALSE
  )

  DT::renderDT(
    {
      dat <- as.data.frame(utils::head(result, rows))
      dt <- DT::datatable(
        dat,
        selection = "none",
        options = opts
      )

      # formatStyle with columns = character(0) breaks DT in DAG boards
      if (ncol(dat) > 0) {
        DT::formatStyle(
          dt,
          columns = names(dat),
          whiteSpace = "pre-wrap"
        )
      } else {
        dt
      }
    },
    server = TRUE
  )
}

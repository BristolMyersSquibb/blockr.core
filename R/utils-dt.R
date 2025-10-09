dt_result <- function(result, session) {

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

  rows <- get_board_option_or_default("n_rows", session = session)
  page <- get_board_option_or_default("page_size", session = session)

  dom <- "rti"

  if (not_null(result) && need_pagination(nrow(result), rows, page)) {
    dom <- paste0(dom, "p")
  }

  if (get_board_option_or_default("filter_rows", session = session)) {
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
      DT::formatStyle(
        DT::datatable(
          dat,
          selection = "none",
          options = opts
        ),
        columns = names(dat),
        whiteSpace = "pre-wrap"
      )
    },
    server = TRUE
  )
}

dt_render_trigger <- function(x, session = get_session()) {

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

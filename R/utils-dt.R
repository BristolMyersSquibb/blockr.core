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

  rows <- get_board_option_or_default("n_rows", session)
  page <- get_board_option_or_default("page_size", session)

  dom <- "rti"

  if (not_null(result) && need_pagination(nrow(result), rows, page)) {
    dom <- paste0(dom, "p")
  }

  if (get_board_option_or_default("filter_rows", session)) {
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
    DT::datatable(
      as.data.frame(utils::head(result, rows)),
      selection = "none",
      options = opts
    ),
    server = TRUE
  )
}

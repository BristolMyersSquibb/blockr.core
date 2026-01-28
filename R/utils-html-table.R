#' HTML Table Preview for Data Frames
#'
#' Replaces DT output with a lightweight HTML table for data blocks,
#' transform blocks, and parser blocks.
#'
#' @param result The data frame result to display
#' @param block The block object
#' @param session Shiny session object
#'
#' @return A shiny renderUI object containing the HTML table
#'
#' @keywords internal
html_table_result <- function(result, block, session) {

  page_size <- tryCatch(
    get_board_option_or_default(
      "page_size",
      board_options(block),
      session
    ),
    error = function(e) 5L
  )

  ns <- session$ns

  renderUI({
    tryCatch({
      total_rows <- if (is.null(result)) 0L else nrow(result)

      # Show first page only (no pagination yet)
      end_row <- min(page_size, total_rows)
      dat <- if (end_row > 0) {
        as.data.frame(dplyr::slice(result, 1:end_row))
      } else {
        as.data.frame(result)
      }

      build_html_table(dat, total_rows, ns = ns, page_size = page_size)
    }, error = function(e) {
      tags$div(
        class = "blockr-table-error",
        style = "color: red; padding: 12px;",
        paste("Error rendering table:", conditionMessage(e))
      )
    })
  })
}

#' Build HTML Table
#'
#' Constructs the HTML table structure with type indicators and row numbers.
#'
#' @param dat Data frame to display (already subset)
#' @param total_rows Total number of rows in original data
#' @param ns Shiny namespace function
#' @param page_size Number of rows per page (default 5)
#'
#' @return A shiny tagList containing the table HTML
#'
#' @keywords internal
build_html_table <- function(dat, total_rows, ns = NULL, page_size = 5L) {
  n_showing <- nrow(dat)
  n_cols <- ncol(dat)

  # Handle empty data frame
  if (n_cols == 0) {
    return(
      tagList(
        tags$div(
          class = "blockr-table-container",
          tags$div(
            class = "blockr-table-footer",
            tags$span(class = "blockr-table-range", "Empty data frame (0 columns)")
          )
        ),
        table_preview_css()
      )
    )
  }

  col_names <- names(dat)

  # Pre-compute column metadata
  col_is_numeric <- vapply(dat, is.numeric, logical(1))
  col_types <- vapply(dat, col_type_label, character(1))

  # Pre-format all columns
  old_opts <- options(cli.num_colors = 1)
  on.exit(options(old_opts), add = TRUE)

  formatted <- lapply(dat, format_column_inner)

  # Pre-compute NA and negative masks
  col_na <- lapply(dat, is.na)
  col_neg <- Map(function(vec, is_num) {
    if (is_num) !is.na(vec) & vec < 0 else rep(FALSE, length(vec))
  }, dat, col_is_numeric)

  # Build header row
  header_cells <- vector("list", n_cols + 1L)
  header_cells[[1L]] <- tags$th(class = "blockr-row-number", "")
  for (j in seq_along(col_names)) {
    col_name <- col_names[j]

    header_cells[[j + 1L]] <- tags$th(
      tags$span(class = "blockr-col-name", col_name),
      tags$span(
        class = "blockr-type-row",
        tags$span(class = "blockr-type-label", col_types[j])
      )
    )
  }

  # Build body rows
  body_rows <- vector("list", n_showing)

  for (i in seq_len(n_showing)) {
    row_cells <- vector("list", n_cols + 1L)
    row_cells[[1L]] <- tags$td(class = "blockr-row-number", i)

    for (j in seq_along(col_names)) {
      is_na <- col_na[[j]][i]
      is_neg <- col_neg[[j]][i]

      cell_class <- if (col_is_numeric[j]) {
        if (is_neg) "blockr-td-numeric blockr-negative" else "blockr-td-numeric"
      } else {
        NULL
      }

      content <- if (is_na) {
        tags$span(class = "blockr-na", "NA")
      } else {
        formatted[[j]][i]
      }

      row_cells[[j + 1L]] <- tags$td(class = cell_class, content)
    }

    body_rows[[i]] <- do.call(tags$tr, row_cells)
  }

  # Build footer
  end_row <- min(page_size, total_rows)
  range_text <- if (total_rows == 0) {
    "No rows"
  } else {
    sprintf("1\u2013%d of %d", end_row, total_rows)
  }

  footer <- tags$div(
    class = "blockr-table-footer",
    tags$span(class = "blockr-table-range", range_text)
  )

  tagList(
    tags$div(
      class = "blockr-table-container",
      tags$div(
        class = "blockr-table-wrapper",
        tags$table(
          class = "blockr-table",
          tags$thead(
            do.call(tags$tr, header_cells)
          ),
          do.call(tags$tbody, body_rows)
        )
      ),
      footer
    ),
    table_preview_css()
  )
}

#' Column Type Label
#'
#' Returns tibble-style type labels for column types.
#'
#' @param x A vector
#'
#' @return A character string like "<chr>", "<int>", "<dbl>", etc.
#'
#' @keywords internal
col_type_label <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    "<dttm>"
  } else if (inherits(x, "Date")) {
    "<date>"
  } else if (is.factor(x)) {
    "<fct>"
  } else if (is.logical(x)) {
    "<lgl>"
  } else if (is.integer(x)) {
    "<int>"
  } else if (is.numeric(x)) {
    "<dbl>"
  } else if (is.character(x)) {
    "<chr>"
  } else if (is.list(x)) {
    "<list>"
  } else {
    paste0("<", class(x)[1], ">")
  }
}

#' Format Column
#'
#' Formats an entire column vector for display using pillar's formatting.
#'
#' @param x A vector (column from data frame)
#' @param max_chars Maximum characters before truncation (default 50)
#'
#' @return A character vector of formatted values
#'
#' @keywords internal
format_column <- function(x, max_chars = 50) {
  old_opts <- options(cli.num_colors = 1)
  on.exit(options(old_opts), add = TRUE)
  format_column_inner(x, max_chars)
}

#' @rdname format_column
#' @keywords internal
format_column_inner <- function(x, max_chars = 50) {
  shaft <- pillar::pillar_shaft(x)
  trimws(format(shaft, width = max_chars))
}

#' Table Preview CSS
#'
#' Returns inline CSS for table styling. Uses CSS variables from blockr.dock
#' when available, with fallback values for standalone use.
#'
#' @return A shiny tags$style element
#'
#' @keywords internal
table_preview_css <- function() {
  tags$style(HTML("
    .blockr-table-container {
      background: white;
      width: 100%;
    }

    .blockr-table-wrapper {
      max-height: 400px;
      overflow-y: auto;
    }

    .blockr-table {
      border-collapse: collapse;
      width: 100%;
      font-size: var(--blockr-font-size-base, 0.875rem);
    }

    .blockr-table thead {
      position: sticky;
      top: 0;
      background: white;
      z-index: 1;
    }

    .blockr-table thead tr {
      border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
    }

    .blockr-table th {
      text-align: left;
      padding: 10px 16px;
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-color-text-primary, #111827);
      vertical-align: bottom;
    }

    .blockr-table th.blockr-row-number {
      width: 64px;
      text-align: center;
    }

    .blockr-col-name {
      display: block;
      font-size: 14px;
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-color-text-primary, #111827);
    }

    .blockr-type-row {
      display: flex;
      align-items: center;
      gap: 4px;
      margin-top: 2px;
    }

    .blockr-type-label {
      font-size: 11px;
      font-weight: var(--blockr-font-weight-normal, 400);
      color: var(--blockr-color-text-subtle, #9ca3af);
    }

    .blockr-table tbody tr {
      border-bottom: 1px solid var(--blockr-grey-100, #f3f4f6);
      transition: background-color 0.15s ease;
    }

    .blockr-table tbody tr:hover {
      background-color: var(--blockr-color-bg-subtle, #f9fafb);
    }

    .blockr-table td {
      padding: 10px 16px;
      font-size: var(--blockr-font-size-base, 0.875rem);
      color: var(--blockr-color-text-primary, #111827);
      max-width: 200px;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }

    .blockr-table .blockr-row-number {
      text-align: center;
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-color-text-subtle, #9ca3af);
    }

    .blockr-table .blockr-td-numeric {
      text-align: right;
      font-variant-numeric: tabular-nums;
    }

    .blockr-table .blockr-negative {
      color: var(--blockr-color-negative, #F43F5E);
    }

    .blockr-table .blockr-na {
      color: var(--blockr-color-text-subtle, #9ca3af);
      font-size: var(--blockr-font-size-xs, 0.75rem);
    }

    .blockr-table-footer {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 12px 16px;
      border-top: 1px solid var(--blockr-color-border, #e5e7eb);
    }

    .blockr-table-range {
      font-size: 12px;
      color: #6B7280;
    }
  "))
}

#' HTML Table Render Trigger
#'
#' Trigger function for re-rendering HTML table when board options change.
#'
#' @param x The block object
#' @param session Shiny session object
#'
#' @return Invisible NULL (side effect: reads board options to trigger reactivity)
#'
#' @keywords internal
html_table_render_trigger <- function(x, session = get_session()) {
  invisible(
    get_board_option_values(
      "page_size",
      if_not_found = "null",
      session = session
    )
  )
}

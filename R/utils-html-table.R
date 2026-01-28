#' Apply Table Sort
#'
#' Sorts data using dplyr::arrange(). Works with both local data frames
#' and remote database tables (dbplyr).
#'
#' @param data A data frame or tbl object
#' @param sort_col Column name to sort by (NULL for no sorting)
#' @param sort_dir Sort direction: "asc", "desc", or "na" (NA first, then asc)
#'
#' @return The sorted data
#'
#' @keywords internal
apply_table_sort <- function(data, sort_col, sort_dir) {
  if (is.null(sort_col) || is.null(sort_dir) || sort_dir == "none") {
    return(data)
  }
  if (!sort_col %in% names(data)) {
    return(data)
  }
  if (sort_dir == "desc") {
    dplyr::arrange(data, dplyr::desc(.data[[sort_col]]))
  } else if (sort_dir == "na") {
    dplyr::arrange(data, !is.na(.data[[sort_col]]), .data[[sort_col]])
  } else {
    dplyr::arrange(data, .data[[sort_col]])
  }
}

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
  key <- paste0("blockr_table_state_", ns(""))

  # Initialize state once per namespace
  if (is.null(session$userData[[key]])) {
    sort_state <- reactiveVal(list(col = NULL, dir = "none"))

    session$userData[[key]] <- list(
      sort_state = sort_state
    )

    # Observer for sort input from header clicks
    observeEvent(session$input$blockr_table_sort, {
      sort_input <- session$input$blockr_table_sort
      if (!is.null(sort_input)) {
        sort_state(list(col = sort_input$col, dir = sort_input$dir))
      }
    })
  }

  sort_state <- session$userData[[key]]$sort_state

  renderUI({
    tryCatch({
      current_sort <- sort_state()
      total_rows <- if (is.null(result)) 0L else nrow(result)

      # Apply sorting
      sorted_result <- apply_table_sort(
        result,
        current_sort$col,
        current_sort$dir
      )

      # Show first page only (no pagination yet)
      end_row <- min(page_size, total_rows)
      dat <- if (end_row > 0) {
        as.data.frame(dplyr::slice(sorted_result, 1:end_row))
      } else {
        as.data.frame(sorted_result)
      }

      build_html_table(
        dat,
        total_rows,
        sort_state = current_sort,
        ns = ns,
        page_size = page_size
      )
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
#' @param sort_state List with col (column name) and dir ("asc", "desc", or "none")
#' @param ns Shiny namespace function
#' @param page_size Number of rows per page (default 5)
#'
#' @return A shiny tagList containing the table HTML
#'
#' @keywords internal
build_html_table <- function(dat, total_rows, sort_state = NULL, ns = NULL,
                             page_size = 5L) {
  n_showing <- nrow(dat)
  n_cols <- ncol(dat)

  sort_col <- sort_state$col
  sort_dir <- sort_state$dir

  sort_input_id <- if (!is.null(ns)) ns("blockr_table_sort") else "blockr_table_sort"

  # Handle empty data frame
  if (n_cols == 0) {
    return(
      tagList(
        tags$div(
          class = "blockr-table-container",
          `data-sort-input` = sort_input_id,
          tags$div(
            class = "blockr-table-footer",
            tags$span(class = "blockr-table-range", "Empty data frame (0 columns)")
          )
        ),
        table_preview_css(),
        table_sort_js()
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

    # Determine sort class for this column
    header_class <- "blockr-sortable"
    sort_icon_class <- "blockr-sort-icon"
    if (!is.null(sort_col) && sort_col == col_name && sort_dir != "none") {
      sort_class_suffix <- switch(
        sort_dir,
        asc = " blockr-sort-asc",
        desc = " blockr-sort-desc",
        na = " blockr-sort-na",
        ""
      )
      header_class <- paste0(header_class, sort_class_suffix)
      icon_class_suffix <- switch(
        sort_dir,
        asc = " blockr-sort-icon-asc",
        desc = " blockr-sort-icon-desc",
        na = " blockr-sort-icon-na",
        ""
      )
      sort_icon_class <- paste0(sort_icon_class, icon_class_suffix)
    }

    header_cells[[j + 1L]] <- tags$th(
      class = header_class,
      `data-column` = col_name,
      tags$span(class = "blockr-col-name", col_name),
      tags$span(
        class = "blockr-type-row",
        tags$span(class = "blockr-type-label", col_types[j]),
        tags$span(class = sort_icon_class)
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
      `data-sort-input` = sort_input_id,
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
    table_preview_css(),
    table_sort_js()
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

    .blockr-table th.blockr-sortable {
      cursor: pointer;
      user-select: none;
      transition: background-color 0.15s ease;
    }

    .blockr-table th.blockr-sortable:hover {
      background-color: var(--blockr-color-bg-subtle, #f9fafb);
    }

    .blockr-sort-icon {
      display: inline-block;
      width: 12px;
      height: 12px;
      font-size: 10px;
      line-height: 12px;
      text-align: center;
    }

    .blockr-sort-icon-asc::after {
      content: '\\2191';
      color: #374151;
    }

    .blockr-sort-icon-desc::after {
      content: '\\2193';
      color: #374151;
    }

    .blockr-sort-icon-na::after {
      content: '\\2205';
      color: #374151;
    }
  "))
}

#' Table Sort JavaScript
#'
#' Returns JavaScript for handling column header clicks to toggle sort.
#' Reads the input ID from the parent container's data-sort-input attribute.
#'
#' @return A shiny tags$script element
#'
#' @keywords internal
table_sort_js <- function() {
  tags$script(HTML("
    if (!window.blockrSortInit) {
      window.blockrSortInit = true;
      document.addEventListener('click', function(e) {
        var header = e.target.closest('.blockr-sortable');
        if (!header) return;
        e.preventDefault();
        e.stopPropagation();
        var container = header.closest('.blockr-table-container');
        var inputId = container ? container.dataset.sortInput : null;
        if (!inputId) return;

        var col = header.dataset.column;
        var currentDir = header.classList.contains('blockr-sort-asc') ? 'asc' :
                         header.classList.contains('blockr-sort-desc') ? 'desc' :
                         header.classList.contains('blockr-sort-na') ? 'na' : 'none';
        // Cycle: none -> asc -> desc -> na -> none
        var newDir = currentDir === 'none' ? 'asc' :
                     currentDir === 'asc' ? 'desc' :
                     currentDir === 'desc' ? 'na' : 'none';
        Shiny.setInputValue(inputId, {col: col, dir: newDir}, {priority: 'event'});
      });
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

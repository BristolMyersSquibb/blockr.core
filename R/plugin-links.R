#' Plugin module for managing board links
#'
#' Logic and user experience for adding new, removing and modifying existing
#' links to/from the board can be customized or enhanced by providing an
#' alternate version of this plugin. The default implementation provides a
#' table-based UI, presented in a modal.
#'
#' Updates are mediated via the [shiny::reactiveVal()] object passed as
#' `update`, where link updates are communicated as list entry `stacks` with
#' components `add`, `rm` or `mod`, where
#' * `add` is either `NULL` or a `links` object (link IDs may not already
#'   exists),
#' * `rm` is either `NULL` or a character vector of (existing) link IDs,
#' * `mod` is either `NULL` or a `links` object (where link IDs must already
#'   exist).
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `manage_links` is returned by
#' `manage_links()`, while the UI component (e.g. `manage_links_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `manage_links_server()`) is expected to return `NULL`.
#'
#' @export
manage_links <- function(server = manage_links_server, ui = manage_links_ui) {
  new_plugin(server, ui, class = "manage_links")
}

#' @param id Namespace ID
#' @param board Reactive values object
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname manage_links
#' @export
manage_links_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(
        input$links_mod,
        showModal(links_modal(session$ns))
      )

      upd <- reactiveValues(
        add = links(),
        rm = character(),
        curr = isolate(board_links(board$board)),
        obs = list(),
        edit = NULL
      )

      observeEvent(
        board_links(board$board),
        {
          upd$curr <- board_links(board$board)
        }
      )

      output$links_dt <- DT::renderDataTable(
        link_dt(isolate(upd$curr), session$ns, isolate(board$board)),
        server = TRUE
      )

      links_proxy <- DT::dataTableProxy("links_dt", session)

      create_link_obs_observer(input, board, upd, session, links_proxy)

      edit_link_observer(upd, board)

      add_link_observer(input, board, upd, session)

      rm_link_observer(input, board, upd, session)

      cancel_link_observer(input, board, upd, session)

      modify_link_observer(input, board, upd, session, links_proxy, update)

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname manage_links
#' @export
manage_links_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "links_mod"),
      "Edit links",
      icon = icon("link")
    )
  )
}

link_dt <- function(dat, ns, board) {
  res <- DT::datatable(
    dt_board_link(dat, ns, board),
    options = list(
      pageLength = 5,
      preDrawCallback = DT::JS(
        "function() { Shiny.unbindAll(this.api().table().node()); }"
      ),
      drawCallback = DT::JS(
        "function() { Shiny.bindAll(this.api().table().node()); }"
      ),
      dom = "tp",
      ordering = FALSE,
      columnDefs = list(
        list(targets = 0, width = "125px")
      )
    ),
    rownames = FALSE,
    escape = FALSE
  )

  DT::formatStyle(res, 1L, `vertical-align` = "middle")
}

dt_board_link <- function(lnk, ns, board) {

  blks <- board_blocks(board)
  arity <- int_ply(blks, block_arity, use_names = TRUE)

  from_ids <- rep(list(names(blks)), length(lnk))

  to_avail <- arity
  cnt_to <- c(table(filter_empty(lnk$to)))

  to_avail[names(cnt_to)] <- int_mply(`-`, to_avail[names(cnt_to)], cnt_to)
  to_avail[is.na(to_avail)] <- 1L

  to_avail <- lapply(
    lapply(lnk$to, filter_empty),
    union,
    names(to_avail)[to_avail > 0L]
  )

  cnt_to <- table(lnk$to)[names(blks[is.na(arity)])]
  cnt_to[is.na(cnt_to)] <- 0L

  cur_inp <- split(lnk$input, lnk$to)

  in_avail <- c(
    lapply(blks[!is.na(arity)], block_inputs),
    Map(
      union,
      lapply(lapply(cnt_to, seq_len), as.character),
      cur_inp[names(cnt_to)]
    )
  )

  rm_inp <- lapply(
    seq_along(lnk),
    function(i) split(lnk$input[-i], lnk$to[-i])[[lnk$to[i]]]
  )

  data.frame(
    ID = lnk$id,
    From = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_from"), ns),
      lnk$from,
      Map(setdiff, from_ids, lnk$to)
    ),
    To = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_to"), ns),
      lnk$to,
      Map(setdiff, to_avail, lnk$from)
    ),
    Input = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_input"), ns),
      lnk$input,
      Map(setdiff, in_avail[lnk$to], rm_inp),
      is.na(arity[lnk$to])
    )
  )
}

dt_selectize <- function(id, val, choices, create = FALSE) {

  if (isTRUE(create)) {
    opts <- list(create = TRUE)
  } else {
    opts <- NULL
  }

  res <- selectizeInput(id, label = NULL, choices = c("", choices),
                        selected = val, options = opts)

  res <- htmltools::tagQuery(
    res
  )$addAttrs(
    style = "width: 175px; margin-bottom: 0;"
  )$allTags()

  as.character(res)
}

create_dt_link_obs <- function(ids, upd, ...) {

  create_obs <- function(col, row, upd, input, blks, sess) {

    inp <- paste0(row, "_", col)

    log_debug("creating link DT observer ", inp)

    observeEvent(
      input[[inp]],
      {
        new <- input[[inp]]
        cur <- upd$curr[[row]][[col]]

        if (new == cur || new == "") {
          return()
        }

        if (col == "from") {

          to_avail <- int_ply(blks, block_arity, use_names = TRUE)

          cnt <- c(table(filter_empty(upd$curr$to)))

          to_avail[names(cnt)] <- int_mply(`-`, to_avail[names(cnt)], cnt)
          to_avail[is.na(to_avail)] <- 1L

          to_avail <- c(
            upd$curr[[row]][["to"]],
            names(to_avail)[to_avail > 0L]
          )

          updateSelectizeInput(
            sess,
            inputId = paste0(row, "_to"),
            choices = c("", setdiff(to_avail, new)),
            selected = upd$curr[[row]][["to"]]
          )

        } else if (col == "to") {

          ids <- names(blks)

          updateSelectizeInput(
            sess,
            inputId = paste0(row, "_from"),
            choices = c("", setdiff(ids, new)),
            selected = upd$curr[[row]][["from"]]
          )

          if (identical(new, "")) {

            updateSelectizeInput(
              sess,
              inputId = paste0(row, "_input"),
              choices = list()
            )

          } else {

            blk <- blks[[which(ids == new)]]
            ary <- block_arity(blk)
            hit <- upd$curr$to == new

            if (is.na(ary)) {
              inp <- as.character(sum(hit) + 1L)
              opt <- list(create = TRUE)
            } else {
              inp <- setdiff(block_inputs(blk), upd$curr$input[hit])
              opt <-  NULL
            }

            updateSelectizeInput(
              sess,
              inputId = paste0(row, "_input"),
              choices = c("", inp),
              selected = upd$curr[[row]][["input"]],
              options = opt
            )
          }

        } else if (col != "input") {

          blockr_abort(
            "Unexpected input: column {col}.",
            class = "unexpected_link_col_input"
          )
        }

        upd$edit <- list(row = row, col = col, val = new)
      },
      ignoreInit = TRUE
    )
  }

  create_obs_for_id <- function(id, ...) {
    lapply(
      set_names(nm = c("from", "to", "input")),
      create_obs,
      id,
      ...
    )
  }

  upd$obs[ids] <- lapply(ids, create_obs_for_id, upd, ...)

  upd
}

destroy_dt_link_obs <- function(ids, update) {

  for (row in ids) {
    for (col in c("from", "to", "input")) {
      log_debug("destroying link DT observer ", row, " ", col)
      update$obs[[row]][[col]]$destroy()
    }
    update$obs[[row]] <- NULL
  }

  update
}

links_modal <- function(ns) {
  modalDialog(
    title = "Board links",
    DT::dataTableOutput(ns("links_dt")),
    footer = tagList(
      htmltools::tagQuery(
        textInput(ns("new_link_id"), NULL, placeholder = "Next ID")
      )$addAttrs(
        style = "width: 180px; margin: 0 8px;"
      )$allTags(),
      tags$style(
        type = "text/css",
        paste0("#", ns("new_link_id"), " {padding: 0.75em 2em; margin: 4px;}")
      ),
      tags$style(
        type = "text/css",
        paste0("#", ns("new_link_id"), "-label {text-align: left;}")
      ),
      actionButton(ns("add_link"), "Add", icon = icon("plus")),
      actionButton(ns("rm_link"), "Remove", icon = icon("minus")),
      actionButton(ns("cancel_links"), "Cancel", class = "btn-danger"),
      actionButton(ns("modify_links"), "OK", class = "btn-success")
    ),
    size = "l"
  )
}

create_link_obs_observer <- function(input, rv, upd, session, proxy) {

  observeEvent(
    names(upd$curr),
    {
      ids <- names(upd$curr)

      DT::replaceData(
        proxy,
        dt_board_link(upd$curr, session$ns, rv$board),
        rownames = FALSE
      )

      create_dt_link_obs(setdiff(ids, names(upd$obs)), upd, input,
                         board_blocks(rv$board), session)
      destroy_dt_link_obs(setdiff(names(upd$obs), ids), upd)
    }
  )
}

edit_link_observer <- function(upd, rv) {

  observeEvent(
    upd$edit,
    {
      row <- upd$edit$row
      col <- upd$edit$col

      if (!row %in% upd$rm && row %in% board_link_ids(rv$board)) {
        upd$rm <- c(upd$rm, row)
      }

      new <- do.call(
        `$<-`,
        list(upd$curr[row], col, coal(upd$edit$val, ""))
      )

      # vctrs 0.7.0: vec_assign() no longer modifies in place, so we must
      # explicitly capture and reassign the result
      temp_curr <- upd$curr
      temp_curr[row] <- new
      upd$curr <- temp_curr

      if (row %in% names(upd$add)) {
        temp_add <- upd$add
        temp_add[row] <- new
        upd$add <- temp_add
      } else {
        upd$add <- c(upd$add, new)
      }
    }
  )
}

add_link_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$add_link,
    {
      total <- sum(block_arity(rv$board))

      if (is.na(total) || length(upd$curr) < total) {

        new <- new_link(from = "", to = "", input = "")

        if (length(input$new_link_id) && nchar(input$new_link_id)) {

          updateTextInput(
            session = sess,
            inputId = "new_link_id",
            label = NULL,
            value = "",
            placeholder = "Next ID"
          )

          if (input$new_link_id %in% names(upd$curr)) {
            notify(
              "Please choose a unique link ID.",
              type = "warning",
              session = sess
            )
            return()
          }

          new <- set_names(list(new), input$new_link_id)
        }

        upd$curr <- c(upd$curr, new)
        upd$add <- c(upd$add, upd$curr[length(upd$curr)])

      } else {

        notify(
          "No new links can be added. Remove a row first.",
          type = "warning",
          session = sess
        )
      }
    }
  )
}

rm_link_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$rm_link,
    {
      sel <- input$links_dt_rows_selected

      if (length(sel)) {

        ids <- names(upd$curr[sel])

        upd$rm <- c(upd$rm, ids[ids %in% board_link_ids(rv$board)])

        upd$add <- upd$add[setdiff(names(upd$add), ids)]
        upd$curr <- upd$curr[setdiff(names(upd$curr), ids)]

      } else {

        notify("No row selected", type = "warning", session = sess)
      }
    }
  )
}

cancel_link_observer <- function(input, rv, upd, session) {

  observeEvent(
    input$cancel_links,
    {
      removeModal(session)

      upd$add <- links()
      upd$rm <- character()
      upd$curr <- board_links(rv$board)
    }
  )
}

modify_link_observer <- function(input, rv, upd, session, proxy, res) {

  observeEvent(
    input$modify_links,
    {
      if (!length(upd$add) && !length(upd$rm)) {

        notify(
          "No changes specified.",
          type = "warning",
          session = session
        )

        return()
      }

      new <- try_catch_continue(
        modify_board_links(rv$board, upd$add, upd$rm),
        message = function(e) {
          notify(conditionMessage(e), duration = NULL, type = "message",
                 session = session)
        },
        warning = function(e) {
          notify(conditionMessage(e), duration = NULL, type = "warning",
                 session = session)
        },
        error = function(e) {
          notify(conditionMessage(e), duration = NULL, type = "error",
                 session = session)
          NULL
        }
      )

      if (is.null(new)) {
        return()
      }

      res(
        list(
          links = list(
            add = if (length(upd$add)) upd$add,
            rm = if (length(upd$rm)) upd$rm
          )
        )
      )

      upd$add <- links()
      upd$rm <- character()
      upd$curr <- board_links(new)

      DT::replaceData(
        proxy,
        dt_board_link(upd$curr, session$ns, rv$board),
        rownames = FALSE
      )

      removeModal(session)
    }
  )
}

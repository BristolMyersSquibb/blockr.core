#' Plugin module for editing board blocks
#'
#' Logic and user experience for editing block attributes such as block titles
#' can be customized or enhanced by providing an alternate version of this
#' plugin. The default implementation only handles block titles, but if further
#' (editable) block attributes are to be introduced, corresponding UI and logic
#' can be included here. In addition to blocks titles, this default
#' implementation provides UI for removing, as well as inserting blocks before
#' or after the current one.
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `edit_block` is returned by
#' `edit_block()`, while the UI component (e.g. `edit_block_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `edit_block_server()`) is expected to return `NULL`.
#'
#' @export
edit_block <- function(server = edit_block_server, ui = edit_block_ui) {
  new_plugin(server, ui, class = "edit_block")
}

#' @param id Namespace ID
#' @param block_id Block ID
#' @param board Reactive values object containing board information
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname edit_block
#' @export
edit_block_server <- function(id, block_id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      initial_block <- isolate(
        board_blocks(board$board)[[block_id]]
      )

      cur_name <- reactive(
        block_name(board_blocks(board$board)[[block_id]])
      )

      output$block_name_out <- renderUI(
        bslib::tooltip(cur_name(), paste("Block ID: ", block_id))
      )

      observeEvent(
        cur_name(),
        updateTextInput(
          session,
          "block_name_in",
          "Block name",
          cur_name()
        )
      )

      observeEvent(
        input$block_name_in,
        {
          req(input$block_name_in)
          if (!identical(cur_name(), input$block_name_in)) {
            new_val <- board_blocks(board$board)[[block_id]]
            block_name(new_val) <- input$block_name_in
            new_val <- as_blocks(set_names(list(new_val), block_id))
            update(list(blocks = list(mod = new_val)))
          }
        }
      )

      output$block_summary <- renderText(
        block_summary(
          initial_block,
          reval_if(board$blocks[[block_id]]$server$result)
        )
      )

      observeEvent(
        input$rm_block,
        update(list(blocks = list(rm = block_id)))
      )

      position <- NULL

      if (block_has_inputs(initial_block)) {
        observeEvent(
          input$add_block_before,
          {
            inps <- setdiff(
              available_block_inputs(initial_block),
              occupied_block_inputs(block_id, board$board)
            )
            if (length(inps)) {
              position <<- "before"
              insert_block_modal(
                session,
                position,
                inputs = inps,
                stack_checkbox = is_block_in_stacks(block_id, board$board)
              )
            } else {
              showNotification(
                "All block inputs are used. Remove incoming link first.",
                type = "warning",
                session = session
              )
            }
          }
        )
      }

      observeEvent(
        input$add_block_after,
        {
          position <<- "after"
          insert_block_modal(
            session,
            position,
            stack_checkbox = is_block_in_stacks(block_id, board$board)
          )
        }
      )

      observeEvent(
        input$registry_select,
        {
          sel <- input$registry_select

          if (!is_string(sel) || !sel %in% list_blocks()) {

            showNotification(
              "Please choose a valid block type.",
              type = "warning"
            )

            return()
          }

          if (identical(position, "after")) {
            updateSelectInput(
              session,
              "input_select",
              "Select input",
              available_block_inputs(sel)
            )
          }
        }
      )

      observeEvent(
        input$confirm_insert,
        {
          sel <- input$registry_select
          bid <- input$block_id

          if (!validate_block_addition(sel, bid, board$board, session)) {
            return()
          }

          blk <- create_block(sel)

          res <- list(
            blocks = list(
              add = as_blocks(set_names(list(blk), bid))
            )
          )

          if (identical(position, "before")) {

            if (!validate_link_addition(input$link_id, input$input_select,
                                        initial_block, board$board, session)) {
              return()
            }

            lnk <- set_names(
              list(new_link(bid, block_id, input$input_select)),
              input$link_id
            )

          } else if (identical(position, "after")) {

            if (!validate_link_addition(input$link_id, input$input_select,
                                        blk, board$board, session)) {
              return()
            }

            lnk <- set_names(
              list(new_link(block_id, bid, input$input_select)),
              input$link_id
            )

          } else {

            blockr_abort(
              "Unexpected postion {position}.",
              class = "unexpected_position_value"
            )
          }

          res <- c(res, list(links = list(add = as_links(lnk))))

          if (isTRUE(input$add_to_stack)) {

            stacks <- board_stacks(board$board)

            stk <- is_block_in_stack(block_id, stacks)
            stk <- names(stk)[stk]

            res <- c(
              res,
              list(
                stacks = list(
                  add = as_stacks(lapply(stacks[stk], union, bid)),
                  rm = stk
                )
              )
            )
          }

          update(res)

          removeModal()

          position <<- NULL
        }
      )

      observeEvent(
        input$cancel_insert,
        removeModal()
      )

      NULL
    }
  )
}

#' @param x Block
#' @rdname edit_block
#' @export
edit_block_ui <- function(x, id, ...) {
  tagList(
    bslib::card_header(
      class = "d-flex justify-content-between",
      uiOutput(NS(id, "block_name_out"), inline = TRUE),
      bslib::popover(
        bsicons::bs_icon("gear"),
        textInput(
          NS(id, "block_name_in"),
          "Block name"
        ),
        actionButton(
          NS(id, "rm_block"),
          "Remove block",
          icon = icon("circle-minus"),
          class = c("btn-danger", "mb-2")
        ),
        if (block_has_inputs(x)) {
          actionButton(
            NS(id, "add_block_before"),
            "Add block before",
            icon = icon("circle-plus"),
            class = c("btn-success", "mb-2")
          )
        },
        actionButton(
          NS(id, "add_block_after"),
          "Add block after",
          icon = icon("circle-plus"),
          class = "btn-success"
        ),
        title = "Block options",
        id = NS(id, "opts"),
        options = list(customClass = id)
      )
    ),
    ...,
    bslib::card_footer(
      class = "text-center",
      uiOutput(NS(id, "block_summary"), inline = TRUE)
    )
  )
}

#' @param data Result data
#' @rdname edit_block
#' @export
block_summary <- function(x, data) {
  UseMethod("block_summary")
}

#' @rdname edit_block
#' @export
block_summary.block <- function(x, data) {

  type <- try(type_desc(data), silent = TRUE)

  if (inherits(type, "try-error")) {
    type <- "??"
  }

  dims <- try(dim_desc(data), silent = TRUE)

  if (inherits(dims, "try-error")) {
    dims <- ""
  } else {
    dims <- paste0("[", dims, "]")
  }

  paste0("&lt;", type, dims, "&gt;")
}

check_edit_block_val <- function(val) {

  observeEvent(
    TRUE,
    {
      if (!is.list(val)) {
        blockr_abort(
          "Expecting `edit_block` to return a list.",
          class = "edit_block_return_invalid"
        )
      }
    },
    once = TRUE
  )

  observeEvent(
    TRUE,
    {
      known <- block_base_attrs()
      if (!all(names(val) %in% known)) {
        blockr_abort(
          "Not expecting `edit_block` to return components ",
          "{setdiff(names(val), known)}.",
          class = "edit_block_return_invalid"
        )
      }
    },
    once = TRUE
  )

  val
}

type_desc <- function(x) {
  if (is.object(x)) {
    class(x)[[1]]
  } else if (rlang::is_vector(x)) {
    typeof(x)
  }
}

dim_desc <- function(x) {
  dim <- coal(dim(x), vec_size(x))
  format_dim <- big_mark(dim)
  format_dim[is.na(dim)] <- "??"
  paste0(format_dim, collapse = paste0(" &#10005; "))
}

big_mark <- function(x) {

  if (identical(getOption("OutDec"), ",")) {
    mark <- "."
  } else {
    mark <- ","
  }

  ret <- formatC(x, big.mark = mark, format = "d",
                 preserve.width = "individual")

  ret[is.na(x)] <- "??"

  ret
}

insert_block_modal <- function(session, pos, inputs = "",
                               stack_checkbox = FALSE) {

  ns <- session$ns

  ui <- modalDialog(
    title = switch(
      match.arg(pos, c("before", "after")),
      before = "Add block before",
      after = "Add block after"
    ),
    div(
      selectInput(
        ns("registry_select"),
        "Select block from registry",
        choices = list_suitable_blocks(pos)
      ),
      textInput(
        inputId = ns("block_id"),
        label = "Block ID",
        value = rand_names(),
        placeholder = "Enter a block ID."
      ),
      selectInput(
        ns("input_select"),
        "Select input",
        choices = inputs
      ),
      textInput(
        inputId = ns("link_id"),
        label = "Link ID",
        value = rand_names(),
        placeholder = "Enter a link ID."
      ),
      if (stack_checkbox) {
        bslib::input_switch(
          id = ns("add_to_stack"),
          label = "Add to stack",
          value = TRUE
        )
      }
    ),
    footer = tagList(
      actionButton(ns("cancel_insert"), "Cancel", class = "btn-danger"),
      actionButton(ns("confirm_insert"), "OK", class = "btn-success")
    )
  )

  showModal(ui, session)

  invisible()
}

list_suitable_blocks <- function(pos) {

  opts <- list_blocks()

  if (identical(pos, "before")) {
    return(opts)
  }

  stopifnot(identical(pos, "after"))

  arity <- int_ply(lapply(opts, create_block), block_arity)
  opts[is.na(arity) | arity >= 1L]
}

available_block_inputs <- function(block) {

  if (is.character(block)) {
    block <- create_block(block)
  }

  stopifnot(is_block(block))

  inp <- block_inputs(block)

  if (is.na(block_arity(block))) {
    inp <- c(inp, as.character(seq_len(length(inp) + 1L)))
  }

  inp
}

occupied_block_inputs <- function(block, links) {

  if (is_board(links)) {
    links <- board_links(links)
  }

  stopifnot(is_string(block), is_links(links))

  links[links$to == block]$input
}

validate_link_addition <- function(id, input, block, board, session) {

  if (nchar(id) == 0L || !is_string(id)) {

    showNotification(
      "Please choose a valid link ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  if (id %in% board_link_ids(board)) {

    showNotification(
      "Please choose a unique link ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  inps <- block_inputs(block)

  if (is.na(block_arity(block))) {
    inps <- c(inps, as.character(seq_len(length(inps) + 1L)))
  }

  if (!is_string(input) || !input %in% inps) {

    showNotification(
      "Please choose a valid block data input.",
      type = "warning"
    )

    return(FALSE)
  }

  TRUE
}

is_block_in_stack <- function(block, stacks) {

  is_el <- function(stk, el) is.element(el, stk)

  if (is_board(stacks)) {
    stacks <- board_stacks(stacks)
  }

  lgl_ply(stacks, is_el, block, use_names = TRUE)
}

is_block_in_stacks <- function(block, stacks) {
  any(is_block_in_stack(block, stacks))
}

block_has_inputs <- function(x) {
  is.na(block_arity(x)) || block_arity(x) >= 1L
}

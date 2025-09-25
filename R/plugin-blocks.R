#' Plugin module for managing board blocks
#'
#' Logic and user experience for adding/removing blocks to the board can be
#' customized or enhanced by providing an alternate version of this plugin. The
#' default implementation provides a modal-based UI with simple shiny inputs
#' such as drop-downs and text fields.
#'
#' Updates are mediated via the [shiny::reactiveVal()] object passed as
#' `update`, where block updates are communicated as list entry `blocks` with
#' components `add` and `rm`, where
#' * `add` may be `NULL` or a `block` object (block IDs may not already exist),
#' * `rm` may be `NULL` or a string (of existing block IDs).
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `manage_blocks` is returned by
#' `manage_blocks()`, while the UI component (e.g. `manage_blocks_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `manage_blocks_server()`) is expected to return `NULL`.
#'
#' @export
manage_blocks <- function(server = manage_blocks_server,
                          ui = manage_blocks_ui) {

  new_plugin(server, ui, class = "manage_blocks")
}

#' @param id Namespace ID
#' @param board Reactive values object
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname manage_blocks
#' @export
manage_blocks_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(
        input$add_block,
        showModal(add_block_modal(session$ns))
      )

      observeEvent(
        input$confirm_add,
        {
          sel <- input$registry_select
          bid <- input$block_id

          if (!validate_block_addition(sel, bid, board$board, session)) {
            return()
          }

          blk <- create_block(sel, name = id_to_sentence_case(bid))
          add <- as_blocks(set_names(list(blk), bid))

          update(
            list(blocks = list(add = add))
          )

          removeModal()
        }
      )

      observeEvent(
        input$cancel_add,
        removeModal()
      )

      observeEvent(
        input$rm_block,
        rm_block_modal(session$ns, board$board)
      )

      observeEvent(
        input$confirm_rm,
        {
          sel <- input$block_select

          if (!length(sel)) {

            notify(
              "Please choose at least one block.",
              type = "warning",
              session = session
            )

            return()
          }

          if (!all(sel %in% board_block_ids(board$board))) {

            notify(
              "Please choose valid block IDs.",
              type = "warning",
              session = session
            )

            return()
          }

          update(
            list(blocks = list(rm = sel))
          )

          removeModal()
        }
      )

      observeEvent(
        input$cancel_rm,
        removeModal()
      )

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname manage_blocks
#' @export
manage_blocks_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "add_block"),
      "Add block",
      icon = icon("circle-plus"),
      class = "btn-success"
    ),
    actionButton(
      NS(id, "rm_block"),
      "Remove block",
      icon = icon("circle-minus"),
      class = "btn-danger"
    )
  )
}

add_block_modal <- function(ns) {
  modalDialog(
    title = "Add block",
    div(
      selectInput(
        ns("registry_select"),
        "Select block from registry",
        choices = list_blocks()
      ),
      textInput(
        inputId = ns("block_id"),
        label = "Block ID",
        value = rand_names(),
        placeholder = "Enter a block ID."
      )
    ),
    footer = tagList(
      actionButton(ns("cancel_add"), "Cancel", class = "btn-danger"),
      actionButton(ns("confirm_add"), "OK", class = "btn-success")
    )
  )
}

rm_block_modal <- function(ns, board) {
  showModal(
    modalDialog(
      title = "Remove blocks",
      selectInput(
        ns("block_select"),
        "Select block(s) from board",
        choices = board_block_ids(board),
        multiple = TRUE
      ),
      footer = tagList(
        actionButton(ns("cancel_rm"), "Cancel", class = "btn-danger"),
        actionButton(ns("confirm_rm"), "OK", class = "btn-success")
      )
    )
  )
}

validate_block_addition <- function(block, id, board, session) {

  if (nchar(id) == 0L || !is_string(id)) {

    notify(
      "Please choose a valid block ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  if (id %in% board_block_ids(board)) {

    notify(
      "Please choose a unique block ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  if (!is_string(block) || !block %in% list_blocks()) {

    notify(
      "Please choose a valid block type.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  TRUE
}

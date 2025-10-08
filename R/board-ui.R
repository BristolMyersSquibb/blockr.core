#' Board UI
#'
#' As counterpart to [board_server()], `board_ui()` is responsible for rendering
#' UI for a board module. This top-level entry point for customizing board
#' appearance and functionality can be overridden by sub-classing the boar
#' object and providing an implementation for this sub-class. Such an
#' implementation is expected to handle UI for plugins and all board
#' components, including blocks, links and stacks, but may rely on
#' functionality that generates UI for these components, such as [block_ui()]
#' or [stack_ui()], as well as already available UI provided by plugins
#' themselves. Additionally, `toolbar_ui()` is responsible for creating a
#' toolbar UI component from several plugin UI components.
#'
#' Dynamic UI updates are handled by functions `insert_block_ui()` and
#' `remove_block_ui()` for adding and removing block-level UI elements to and
#' from `board` UI, whenever blocks are added or removed. These update functions
#' are provided as S3 generics with implementations for `board` and can be
#' extended if so desired.
#'
#' @param id Namespace ID
#' @param x Board
#' @param ... Generic consistency
#'
#' @return A `board_ui()` implementation is expected to return [shiny::tag] or
#' [shiny::tagList()] objects, as does `toolbar_ui()`, while updater functions
#' (`insert_block_ui()` and `remove_block_ui()`) are called for their side
#' effects (which includes UI updates such as [shiny::insertUI()],
#' [shiny::removeUI()]) and return the board object passed as `x` invisibly.
#'
#' @export
board_ui <- function(id, x, ...) {
  UseMethod("board_ui", x)
}

#' @param plugins UI for board plugins
#' @rdname board_ui
#' @export
board_ui.board <- function(id, x, plugins = board_plugins(x), ...) {

  plugin_if_exists <- function(nme) {
    if (nme %in% names(plugins)) plugins[[nme]]
  }

  tagList(
    toolbar_ui(id, x, plugins),
    board_ui(id, plugins[["notify_user"]], x),
    div(
      id = paste0(id, "_board"),
      stack_ui(id, x, edit_ui = plugin_if_exists("edit_stack")),
      block_ui(id, x, edit_ui = plugin_if_exists("edit_block"))
    )
  )
}

#' @rdname board_ui
#' @export
board_ui.NULL <- function(id, x, ...) NULL

#' @section Board-level block UI:
#' While the contents of block-level UI are created by dispatching `block_ui()`
#' on blocks another dispatch on `board` (see [new_board()]) occurs as well.
#' This can be used to control how blocks are integrated into the board UI. For
#' the default board, this uses [bslib::card()] to represent blocks. For boards
#' that extend the default `board` class, control is available for how blocks
#' are displayed by providing a board-specific `block_ui()` method.
#'
#' @param blocks (Additional) blocks (or IDs) for which to generate the UI
#' @param edit_ui Block edit plugin
#'
#' @rdname block_ui
#' @export
block_ui.board <- function(id, x, blocks = NULL, edit_ui = NULL, ...) {

  block_card <- function(x, block_id, board_ns, card_elems) {

    blk_id <- board_ns(paste0("block_", block_id))

    bslib::card(
      id = paste0(block_id, "_block"),
      card_elems(
        x,
        NS(blk_id, "edit_block"),
        bslib::card_body(
          expr_ui(blk_id, x),
          block_ui(blk_id, x)
        )
      )
    )
  }

  stopifnot(is_string(id))

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  } else if (is.character(blocks)) {
    blocks <- board_blocks(x)[blocks]
  }

  stopifnot(is_blocks(blocks))

  if (is.null(edit_ui)) {
    edit_ui <- function(x, id, ...) {
      tagList(
        bslib::card_header(block_name(x)),
        ...
      )
    }
  } else {
    edit_ui <- get_plugin_ui(edit_ui)
  }

  args <- list(
    board_ns = NS(id),
    card_elems = edit_ui
  )

  do.call(
    div,
    c(
      id = paste0(id, "_blocks"),
      map(block_card, blocks, names(blocks), MoreArgs = args)
    )
  )
}

#' @param blocks (Additional) blocks (or IDs) for which to generate the UI
#' @param session Shiny session
#' @rdname board_ui
#' @export
insert_block_ui <- function(id, x, blocks = NULL, ...,
                            session = get_session()) {

  UseMethod("insert_block_ui", x)
}

#' @rdname board_ui
#' @export
insert_block_ui.board <- function(id, x, blocks = NULL, ...,
                                  session = get_session()) {

  stopifnot(is_string(id))

  insertUI(
    paste0("#", id, "_blocks"),
    "beforeEnd",
    block_ui(id, x, blocks, ...),
    immediate = TRUE
  )

  invisible(x)
}

#' @rdname board_ui
#' @export
remove_block_ui <- function(id, x, blocks = NULL, ...,
                            session = get_session()) {

  UseMethod("remove_block_ui", x)
}

#' @rdname board_ui
#' @export
remove_block_ui.board <- function(id, x, blocks = NULL, ...,
                                  session = get_session()) {

  if (is.null(blocks)) {

    stopifnot(is_string(id))

    removeUI(
      paste0("#", id, "_blocks > div"),
      multiple = TRUE,
      immediate = TRUE
    )

  } else {

    stopifnot(is.character(blocks))

    for (block in blocks) {
      removeUI(
        paste0("#", block, "_block"),
        immediate = TRUE
      )
    }
  }

  invisible(x)
}

#' @rdname board_ui
#' @export
toolbar_ui <- function(id, x, plugins = list(), ...) {
  UseMethod("toolbar_ui", x)
}

#' @rdname board_ui
#' @export
toolbar_ui.board <- function(id, x, plugins = list(), ...) {

  plugins <- as_plugins(plugins)

  toolbar_plugins <- c("preserve_board", "manage_blocks", "manage_links",
                       "manage_stacks", "generate_code")

  toolbar_plugins <- plugins[intersect(toolbar_plugins, names(plugins))]

  toolbar_ui <- do.call(
    tagList,
    list(
      board_ui(id, toolbar_plugins, x),
      bslib::popover(
        bsicons::bs_icon("gear", size = "1.5em"),
        board_ui(id, as_board_options(x)),
        title = "Board options"
      )
    )
  )

  do.call(
    div,
    c(
      class = paste(
        "d-flex justify-content-evenly align-items-center",
        "bg-light-subtle sticky-top border rounded-4",
        "m-2 gap-5 p-2"
      ),
      toolbar_ui
    )
  )
}

#' Stack UI
#'
#' Several generics are exported in order to integrate stack UI into board UI.
#' We have `stack_ui()` which is dispatched on the `board` (and in the default
#' implementation) on individual `stack` objects. This renders stacks as
#' bootstrap accordion items (using [bslib::accordion()]). If a different way
#' of displaying stacks and integrating them with a board is desired, this can
#' be implemented by introducing a board subclass and providing a `stack_ui()`
#' method for that subclass. Inserting stacks into (and removing stacks from)
#' a board is available as `insert_stack_ui()`/`remove_stack_ui()` and blocks
#' into/from stacks via `add_block_to_stack()`/`remove_block_from_stack()`.
#' All are S3 generics with implementations for `board` and alternative
#' implementation may be provided for board sub-classes.
#'
#' @param id Parent namespace
#' @param x Object
#' @param ... Generic consistency
#'
#' @return UI set up via `stack_ui()` is expected to return [shiny::tag()] or
#' [shiny::tagList()] objects while stack/block insertion/removal functions
#' (into/from board/stack objects) are called for their side-effects. Both
#' `insert_stack_ui()`/`remove_stack_ui` and
#' `add_block_to_stack()`/`remove_block_from_stack()` return `NULL` invisibly
#' and where the former call [shiny::insertUI()]/[shiny::removeUI()] and the
#' latter modify the DOM via [shiny::session] custom messages.
#'
#' @export
stack_ui <- function(id, x, ...) {
  UseMethod("stack_ui", x)
}

#' @param stacks (Additional) stacks (or IDs) for which to generate the UI
#' @param edit_ui Stack edit plugin
#' @rdname stack_ui
#' @export
stack_ui.board <- function(id, x, stacks = NULL, edit_ui = NULL, ...) {

  stopifnot(is_string(id))

  if (is.null(stacks)) {
    stacks <- board_stacks(x)
  } else if (is.character(stacks)) {
    stacks <- board_stacks(x)[stacks]
  }

  stopifnot(is_stacks(stacks))

  cont_id <- paste0(id, "_stacks")

  log_debug("setting up stack ui container: ", cont_id)

  ns <- NS(id)

  tagList(
    do.call(
      bslib::accordion,
      c(
        list(id = cont_id),
        map(
          stack_ui,
          chr_ply(paste0("stack_", names(stacks)), ns),
          as.list(stacks),
          MoreArgs = list(edit_ui = edit_ui)
        )
      )
    ),
    htmltools::htmlDependency(
      "move-block-ui",
      pkg_version(),
      src = pkg_file("assets", "js"),
      script = "moveBlockUi.js"
    )
  )
}

#' @rdname stack_ui
#' @export
stack_ui.stack <- function(id, x, edit_ui = NULL, ...) {

  accordion_id <- paste0("stack-accordion-panel-", id)

  log_debug("setting up stack item: ", accordion_id)

  if (is.null(edit_ui)) {
    title <- stack_name(x)
  } else {
    title <- get_plugin_ui(edit_ui)(id, x)
  }

  btn <- tags$button(
    class = "accordion-button collapsed",
    type = "button",
    `data-bs-toggle` = "collapse",
    `data-bs-target` = paste0("#", accordion_id),
    `aria-expanded` = "false",
    `aria-controls` = accordion_id,
    do.call(div, c(list(class = "accordion-title"), title))
  )

  div(
    id = paste0("stack-accordion-item-", id),
    class = "accordion-item",
    `data-value` = id,
    div(class = "accordion-header", btn),
    div(
      id = accordion_id,
      class = "accordion-collapse collapse",
      div(class = "accordion-body")
    )
  )
}

#' @param board Board object
#' @param session Shiny session
#' @rdname stack_ui
#' @export
insert_stack_ui <- function(id, x, board, edit_ui = NULL,
                            session = getDefaultReactiveDomain(), ...) {

  UseMethod("insert_stack_ui", board)
}

#' @rdname stack_ui
#' @export
insert_stack_ui.board <- function(id, x, board, edit_ui = NULL,
                                  session = getDefaultReactiveDomain(), ...) {

  x <- as_stacks(x)

  insertUI(
    paste0("#", id, "_stacks"),
    "beforeEnd",
    map(
      stack_ui,
      chr_ply(paste0("stack_", names(x)), session$ns),
      x,
      MoreArgs = list(edit_ui = edit_ui)
    ),
    immediate = TRUE,
    session = session
  )

  invisible()
}

#' @rdname stack_ui
#' @export
remove_stack_ui <- function(id, board, session = getDefaultReactiveDomain(),
                            ...) {

  UseMethod("remove_stack_ui", board)
}

#' @rdname stack_ui
#' @export
remove_stack_ui.board <- function(id, board,
                                  session = getDefaultReactiveDomain(), ...) {

  for (x in id) {

    log_debug("removing stack item: ", x)

    removeUI(
      paste0(
        "#stack-accordion-item-stack_",
        session$ns(paste0("stack_", x))
      ),
      immediate = TRUE,
      session = session
    )
  }

  invisible()
}

#' @param block_id,stack_id,board_id Block/stack/board IDs
#' @rdname stack_ui
#' @export
add_block_to_stack <- function(board, block_id, stack_id,
                               session = getDefaultReactiveDomain(), ...) {

  UseMethod("add_block_to_stack", board)
}

#' @rdname stack_ui
#' @export
add_block_to_stack.board <- function(board, block_id, stack_id,
                                     session = getDefaultReactiveDomain(),
                                     ...) {

  log_debug("adding block ", block_id, " to stack ", stack_id)

  session$sendCustomMessage(
    "move-block-ui",
    list(
      sel = paste0("#", block_id, "_block"),
      dest = paste0(
        "#stack-accordion-panel-",
        session$ns(paste0("stack_", stack_id)),
        " > div.accordion-body"
      )
    )
  )

  invisible()
}

#' @rdname stack_ui
#' @export
remove_block_from_stack <- function(board, block_id, board_id,
                                    session = getDefaultReactiveDomain(),
                                    ...) {

  UseMethod("remove_block_from_stack", board)
}

#' @rdname stack_ui
#' @export
remove_block_from_stack.board <- function(board, block_id, board_id,
                                          session = getDefaultReactiveDomain(),
                                          ...) {

  log_debug("removing block ", block_id, " from stacks")

  session$sendCustomMessage(
    "move-block-ui",
    list(
      sel = paste0("#", block_id, "_block"),
      dest = paste0("#", board_id, "_blocks")
    )
  )

  invisible()
}

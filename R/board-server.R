#' Board server
#'
#' A call to `board_server()`, dispatched on objects inheriting from `board`,
#' returns a [shiny::moduleServer()], containing all necessary logic to
#' manipulate board components via UI. Extensibility over currently available
#' functionality is provided in the form of S3, where a `board_server()`
#' implementation of `board` sub-classes may be provided, as well as via a
#' plugin architecture and callback functions which can be used to register
#' additional observers.
#'
#' @param x Board
#' @param id Parent namespace
#' @param ... Generic consistency
#'
#' @return A `board_server()` implementation (such as the default for the
#' `board` base class) is expected to return a [shiny::moduleServer()].
#'
#' @export
board_server <- function(id, x, ...) {
  UseMethod("board_server", x)
}

#' @param plugins Board plugins as modules
#' @param callbacks Single (or list of) callback function(s), called only
#' for their side-effects)
#' @rdname board_server
#' @export
board_server.board <- function(id, x, plugins = list(), callbacks = list(),
                               ...) {

  plugins <- as_plugins(plugins)

  if (is.function(callbacks)) {
    callbacks <- list(callbacks)
  }

  validate_callbacks(callbacks)

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      board_options_to_userdata(x, session = session)

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        board = x,
        board_id = id,
        links = list(),
        stacks = list()
      )

      rv_ro <- list(board = make_read_only(rv))

      board_update <- reactiveVal()

      plugin_args <- c(rv_ro, list(update = board_update), dot_args)
      cb_args <- c(rv_ro, list(update = board_update, session = session),
                   dot_args)

      edit_block <- get_plugin("edit_block", plugins)
      edit_stack <- get_plugin("edit_stack", plugins)

      observeEvent(
        TRUE,
        {
          rv <- setup_board(rv, edit_block, edit_stack, plugin_args, session)
        },
        once = TRUE
      )

      call_plugin_server(
        "manage_blocks",
        server_args = plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "manage_links",
        server_args = plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "manage_stacks",
        server_args = plugin_args,
        plugins = plugins
      )

      observeEvent(
        board_update(),
        {
          upd <- validate_board_update(board_update, rv)

          if (length(upd$blocks$add)) {

            insert_block_ui(ns(NULL), rv$board, upd$blocks$add,
                            edit_ui = edit_block)

            board_blocks(rv$board) <- c(board_blocks(rv$board), upd$blocks$add)

            for (blk in names(upd$blocks$add)) {
              rv <- setup_block(
                upd$blocks$add[[blk]], blk, rv, edit_block, plugin_args
              )
            }
          }

          if (length(upd$blocks$mod)) {
            blks <- board_blocks(rv$board)
            blks[names(upd$blocks$mod)] <- upd$blocks$mod
            board_blocks(rv$board) <- blks
          }

          if (length(upd$links$mod)) {
            upd$links$add <- c(upd$links$add, upd$links$mod)
            upd$links$rm <- c(upd$links$rm, names(upd$links$mod))
          }

          if (length(upd$links$add) || length(upd$links$rm)) {

            rm <- board_links(rv$board)[upd$links$rm]
            rv <- update_block_links(rv, upd$links$add, rm)

            rv$board <- modify_board_links(rv$board, upd$links$add,
                                           upd$links$rm)
          }

          rv <- update_stack_blocks(rv, upd$stacks, edit_stack, plugin_args,
                                    session)

          rv$board <- modify_board_stacks(rv$board, upd$stacks$add,
                                          upd$stacks$rm, upd$stacks$mod)

          if (length(upd$blocks$rm)) {
            rv <- destroy_rm_blocks(upd$blocks$rm, rv, session)
            remove_block_ui(ns(NULL), rv$board, upd$blocks$rm)
          }

          board_update(NULL)
        }
      )

      board_refresh <- call_plugin_server(
        "preserve_board",
        server_args = plugin_args,
        plugins = plugins
      )

      if (not_null(board_refresh)) {

        observeEvent(
          board_refresh(),
          {
            log_trace("removing existing ui components")
            remove_block_ui(ns(NULL), rv$board)

            log_trace("refreshing rv$board")
            rv$board <- board_refresh()

            update_board_options(rv$board, session)

            log_trace("updating board ui")
            update_ui(rv$board, session)

            log_trace("inserting new ui components")
            insert_block_ui(ns(NULL), rv$board, edit_ui = edit_block)

            log_trace("setting up block observers")
            rv <- setup_board(rv, edit_block, edit_stack, plugin_args, session)

            log_trace("completed board refresh")
          }
        )
      }

      call_plugin_server(
        "notify_user",
        server_args = plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "generate_code",
        server_args = plugin_args,
        plugins = plugins
      )

      cb_res <- set_names(
        vector("list", length(callbacks)),
        names(callbacks)
      )

      for (i in seq_along(callbacks)) {
        cb_res[[i]] <- do.call(callbacks[[i]], cb_args)
      }

      observeEvent(
        get_board_option_values("thematic", "dark_mode"),
        {
          if (isTRUE(get_board_option_value("thematic"))) {
            do.call(thematic::thematic_shiny, bs_theme_colors(session))
          } else if (isFALSE(get_board_option_value("thematic"))) {
            thematic::thematic_off()
          }
        }
      )

      c(rv_ro, dot_args, cb_res)
    }
  )
}

bs_theme_colors <- function(session) {

  theme <- bslib::bs_current_theme(session)

  if (!bslib::is_bs_theme(theme)) {
    return(
      list(bg = "auto", fg = "auto", accent = "auto")
    )
  }

  if ("3" %in% bslib::theme_version(theme)) {

    vars <- c("body-bg", "text-color", "link-color")

  } else {

    vars <- c("body-bg", "body-color", "link-color")

    if (identical(get_board_option_value("dark_mode"), "dark")) {
      vars <- paste0(vars, "-dark")
    }
  }

  set_names(
    as.list(bslib::bs_get_variables(theme, vars)),
    c("bg", "fg", "accent")
  )
}

setup_board <- function(rv, blk_mod, stk_mod, args, sess) {

  stopifnot(
    is.reactivevalues(rv),
    all(c("blocks", "inputs", "board", "links", "stacks") %in% names(rv)),
    is_board(rv$board)
  )

  for (link in rv$links) {
    link$destroy()
  }

  rv$blocks <- list()
  rv$inputs <- list()
  rv$links <- list()
  rv$stacks <- list()

  blks <- board_blocks(rv$board)

  for (i in names(blks)) {
    rv <- setup_block(blks[[i]], i, rv, blk_mod, args)
  }

  rv <- setup_stacks(rv, stk_mod, args)
  rv <- add_blocks_to_stacks(rv, board_stacks(rv$board), sess)

  rv
}

setup_block <- function(blk, id, rv, mod, args) {

  arity <- block_arity(blk)
  inpts <- block_inputs(blk)

  inpts <- set_names(
    replicate(length(inpts), reactiveVal()),
    inpts
  )

  if (is.na(arity)) {
    inpts <- c(inpts, list(`...args` = reactiveValues()))
  }

  rv$inputs[[id]] <- inpts

  links <- board_links(rv$board)

  rv <- update_block_links(rv, links[links$to == id])

  rv$blocks[[id]] <- list(
    block = blk,
    server = do.call(
      block_server,
      c(list(paste0("block_", id), blk, rv$inputs[[id]], id, mod), args)
    )
  )

  rv
}

destroy_rm_blocks <- function(ids, rv, sess) {

  links <- board_links(rv$board)

  rv <- update_block_links(
    rv,
    rm = links[links$from %in% ids | links$to %in% ids]
  )

  for (id in ids) {

    blk_id <- paste0("block_", id)
    blk_ns <- sess$ns(blk_id)

    destroy_inputs(blk_id, sess)
    destroy_outputs(blk_ns, sess)
    destroy_observers(blk_ns, sess)

    remove_block_from_stack(rv$board, id, rv$board_id, sess)
  }

  rv$inputs <- rv$inputs[!names(rv$inputs) %in% ids]
  rv$blocks <- rv$blocks[!names(rv$blocks) %in% ids]

  rv$board <- rm_blocks(rv$board, ids)

  rv
}

setup_link <- function(rv, id, from, to, input) {

  if (input %in% block_inputs(board_blocks(rv$board)[[to]])) {

    rv$links[[id]] <- observeEvent(
      rv$blocks[[from]]$server$result(),
      {
        rv$inputs[[to]][[input]](
          rv$blocks[[from]]$server$result()
        )
      },
      ignoreNULL = FALSE
    )

  } else {

    rv$links[[id]] <- observeEvent(
      rv$blocks[[from]]$server$result(),
      {
        rv$inputs[[to]][["...args"]][[input]] <-
          rv$blocks[[from]]$server$result()
      },
      ignoreNULL = FALSE
    )
  }

  rv
}

destroy_link <- function(rv, id, from, to, input) {

  rv$links[[id]]$destroy()
  rv$links[[id]] <- NULL

  if (input %in% block_inputs(board_blocks(rv$board)[[to]])) {
    rv$inputs[[to]][[input]](NULL)
  } else {
    rv$inputs[[to]][["...args"]][[input]] <- NULL
  }

  rv
}

update_block_links <- function(rv, add = NULL, rm = NULL) {

  todo <- as.list(rm)

  for (i in names(todo)) {
    rv <- do.call(destroy_link, c(list(rv, i), todo[[i]]))
  }

  todo <- as.list(add)

  for (i in names(todo)) {
    rv <- do.call(setup_link, c(list(rv, i), todo[[i]]))
  }

  rv
}

setup_stacks <- function(rv, mod, args, stacks = board_stacks(rv$board)) {

  serv <- get_plugin_server(mod)

  for (i in names(stacks)) {

    if (not_null(serv)) {
      serv(c(list(id = paste0("stack_", i), stack_id = i), args))
    }

    rv$stacks[[i]] <- character()
  }

  rv
}

destroy_stacks <- function(ids, rv, sess) {

  stopifnot(all(lengths(rv$stacks[ids]) == 0L))

  for (id in ids) {

    stk_id <- paste0("stack_", id)
    stk_ns <- sess$ns(stk_id)

    destroy_inputs(stk_id, sess)
    destroy_outputs(stk_ns, sess)
    destroy_observers(stk_ns, sess)
  }

  rv$stacks[ids] <- NULL

  rv
}

update_stack_blocks <- function(rv, upd, mod, args, session) {

  if (length(upd$rm)) {
    rv <- rm_blocks_from_stacks(rv, upd$rm, session)
    rv <- destroy_stacks(upd$rm, rv, session)
    remove_stack_ui(upd$rm, rv$board)
  }

  if (length(upd$add)) {
    rv <- setup_stacks(rv, mod, args, upd$add)
    insert_stack_ui(rv$board_id, upd$add, rv$board, mod)
    rv <- add_blocks_to_stacks(rv, upd$add, session)
  }

  if (length(upd$mod)) {
    rv <- update_blocks_in_stacks(rv, upd$mod, session)
  }

  rv
}

update_blocks_in_stacks <- function(rv, mod, sess) {

  for (i in names(mod)) {
    rv <- update_blocks_in_stack(i, rv, mod[[i]], sess)
  }

  rv
}

update_blocks_in_stack <- function(id, rv, val, sess) {

  stopifnot(is_string(id), id %in% names(rv$stacks))

  targ <- stack_blocks(val)

  for (i in setdiff(rv$stacks[[id]], targ)) {
    remove_block_from_stack(rv$board, i, rv$board_id, sess)
  }

  for (i in setdiff(targ, rv$stacks[[id]])) {
    add_block_to_stack(rv$board, i, id, sess)
  }

  rv$stacks[[id]] <- targ

  rv
}

rm_blocks_from_stacks <- function(rv, rm, session) {

  stopifnot(is.character(rm), all(rm %in% names(rv$stacks)))

  for (i in rm) {

    for (j in rv$stacks[[i]]) {
      remove_block_from_stack(rv$board, j, rv$board_id, session)
    }

    rv$stacks[[i]] <- character()
  }

  rv
}

add_blocks_to_stacks <- function(rv, add, session) {

  stopifnot(
    is_stacks(add),
    all(names(add) %in% names(rv$stacks)),
    all(lengths(rv$stacks[names(add)]) == 0L)
  )

  for (i in names(add)) {

    blks <- stack_blocks(add[[i]])

    for (j in stack_blocks(add[[i]])) {
      add_block_to_stack(rv$board, j, i, session)
    }

    rv$stacks[[i]] <- blks
  }

  rv
}

validate_board_update <- function(x, rv) {

  if (!is.reactive(x)) {
    abort(
      "Expecting a board update to be passed as a reactive object.",
      class = "board_update_object_invalid"
    )
  }

  res <- x()

  expected <- c("blocks", "links", "stacks")

  if (!is.list(res)) {
    abort(
      "Expecting a board update to be specified as a list.",
      class = "board_update_type_invalid"
    )
  }

  if (!all(names(res) %in% expected)) {
    abort(
      "Expecting a board update to consist of components {expected}. Please ",
      "remove {setdiff(names(res), expected)}.",
      class = "board_update_components_invalid"
    )
  }

  expected <- c("add", "rm", "mod")

  for (cmp in res) {

    if (!is.list(cmp)) {
      abort(
        "Expecting a board update component to be specified as a list.",
        class = "board_update_component_type_invalid"
      )
    }

    if (!length(names(cmp)) == length(cmp) || !all(names(cmp) %in% expected)) {

      abort(
        "Expecting a board update component to consist of components ",
        "{expected}. Please remove {setdiff(names(cmp), expected)}.",
        class = "board_update_component_components_invalid"
      )
    }
  }

  if ("blocks" %in% names(res)) {
    validate_board_update_blocks(res$blocks, rv)
  }

  if ("links" %in% names(res)) {
    validate_board_update_links(res$links, rv)
  }

  if ("stacks" %in% names(res)) {
    validate_board_update_stacks(res$stacks, rv)
  }

  res
}

validate_board_update_blocks <- function(x, rv) {

  all_ids <- board_block_ids(rv$board)

  if ("rm" %in% names(x) && is.character(x$rm)) {
    cur_ids <- setdiff(all_ids, x$rm)
  } else {
    cur_ids <- all_ids
  }

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_blocks(x$add)) {
      abort(
        "Expecting the \"add\" block component of a board update to be ",
        "`NULL` or a `blocks` object.",
        class = "board_update_blocks_add_invalid"
      )
    }

    if (any(names(x$add) %in% cur_ids)) {
      abort(
        "Expecting the newly added block to have a unique ID.",
        class = "board_update_blocks_add_invalid"
      )
    }

    validate_blocks(x$add)

    if ("mod" %in% names(x) && !is.null(x$mod)) {
      if (length(intersect(names(x$add), names(x$mod)))) {
        abort(
          "Cannot add and modify the same IDs simulatneously.",
          class = "board_update_blocks_add_mod_clash"
        )
      }
    }
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      abort(
        "Expecting the \"rm\" block component of a board update value ",
        "to be `NULL` or a character vector.",
        class = "board_update_blocks_rm_invalid"
      )
    }

    if (!all(x$rm %in% all_ids)) {
      abort(
        "Expecting the removed block to be specified by a known ID.",
        class = "board_update_blocks_rm_invalid"
      )
    }
  }

  if ("mod" %in% names(x)) {

    if (!is_blocks(x$mod)) {
      abort(
        "Expecting the \"mod\" block component of a board update",
        "value to be `NULL` or a `blocks` object.",
        class = "board_update_blocks_mod_invalid"
      )
    }

    if (!all(names(x$mod) %in% cur_ids)) {
      abort(
        "Expecting the modified blocks to be specified by known IDs.",
        class = "board_update_blocks_mod_invalid"
      )
    }

    validate_blocks(x$mod)
  }

  invisible()
}

validate_board_update_links <- function(x, rv) {

  all_ids <- board_link_ids(rv$board)

  if ("rm" %in% names(x) && is.character(x$rm)) {
    cur_ids <- setdiff(all_ids, x$rm)
  } else {
    cur_ids <- all_ids
  }

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_links(x$add)) {
      abort(
        "Expecting the \"add\" link component of a board update",
        "value to be `NULL` or a `links` object.",
        class = "board_update_links_add_invalid"
      )
    }

    if (any(names(x$add) %in% cur_ids)) {
      abort(
        "Expecting the newly added links to have a unique ID.",
        class = "board_update_links_add_invalid"
      )
    }

    validate_links(x$add)

    if ("mod" %in% names(x) && !is.null(x$mod)) {
      if (length(intersect(names(x$add), names(x$mod)))) {
        abort(
          "Cannot add and modify the same IDs simulatneously.",
          class = "board_update_links_add_mod_clash"
        )
      }
    }
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      abort(
        "Expecting the \"rm\" link component of a board update",
        "to be `NULL` or a character vector.",
        class = "board_update_links_rm_invalid"
      )
    }

    if (!all(x$rm %in% all_ids)) {
      abort(
        "Expecting all link IDs to be removed to be known.",
        class = "board_update_links_rm_invalid"
      )
    }
  }

  if ("mod" %in% names(x) && !is.null(x$mod)) {

    if (!is_links(x$mod)) {
      abort(
        "Expecting the \"mod\" link component of a board update",
        "value to be `NULL` or a `links` object.",
        class = "board_update_links_mod_invalid"
      )
    }

    if (!all(names(x$mod) %in% cur_ids)) {
      abort(
        "Expecting the modified links to be specified by known IDs.",
        class = "board_update_links_mod_invalid"
      )
    }

    validate_links(x$mod)
  }

  invisible()
}

validate_board_update_stacks <- function(x, rv) {

  all_ids <- board_stack_ids(rv$board)

  if ("rm" %in% names(x) && is.character(x$rm)) {
    cur_ids <- setdiff(all_ids, x$rm)
  } else {
    cur_ids <- all_ids
  }

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_stacks(x$add)) {
      abort(
        "Expecting the \"add\" stack component of a board update",
        "to be `NULL` or a `stacks` object.",
        class = "board_update_stacks_add_invalid"
      )
    }

    if (any(names(x$add) %in% cur_ids)) {
      abort(
        "Expecting the newly added stacks to have a unique ID.",
        class = "board_update_stacks_add_invalid"
      )
    }

    validate_stacks(x$add)

    if ("mod" %in% names(x) && !is.null(x$mod)) {
      if (length(intersect(names(x$add), names(x$mod)))) {
        abort(
          "Cannot add and modify the same IDs simulatneously.",
          class = "board_update_stacks_add_mod_clash"
        )
      }
    }
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      abort(
        "Expecting the \"rm\" stack component of a board update",
        "to be `NULL` or a character vector.",
        class = "board_update_stacks_rm_invalid"
      )
    }

    if (!all(x$rm %in% all_ids)) {
      abort(
        "Expecting all stack IDs to be removed to be known.",
        class = "board_update_stacks_rm_invalid"
      )
    }
  }

  if ("mod" %in% names(x) && !is.null(x$mod)) {

    if (!is_stacks(x$mod)) {
      abort(
        "Expecting the \"mod\" stack component of a board update",
        "value to be `NULL` or a `stacks` object.",
        class = "board_update_stacks_mod_invalid"
      )
    }

    if (!all(names(x$mod) %in% cur_ids)) {
      abort(
        "Expecting the modified stacks to be specified by known IDs.",
        class = "board_update_stacks_mod_invalid"
      )
    }

    validate_stacks(x$mod)
  }

  invisible()
}

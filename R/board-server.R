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
#' @param options Board options (`NULL` defaults to the union of board, block
#' and registry sourced options)
#' @param callbacks Single (or list of) callback function(s), called only
#' for their side-effects)
#' @param callback_location Location of callback invocation (before or after
#' plugins)
#' @rdname board_server
#' @export
board_server.board <- function(id, x, plugins = board_plugins(x),
                               options = board_options(x),
                               callbacks = list(),
                               callback_location = c("end", "start"),
                               ...) {

  plugins <- as_plugins(plugins)

  if (is.function(callbacks)) {
    callbacks <- list(callbacks)
  }

  validate_callbacks(callbacks)

  dot_args <- list(...) # nolint: object_usage_linter.

  callback_location <- match.arg(callback_location)

  stopifnot(is_board_options(options))

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        board = x,
        board_id = id,
        links = list(),
        stacks = list()
      )

      rv_ro <- list(board = make_read_only(rv))

      do.call(
        board_options_to_userdata,
        c(
          list(options),
          rv_ro,
          dot_args,
          list(session = session)
        )
      )

      board_update <- reactiveVal()

      cb_res <- set_names(
        vector("list", length(callbacks)),
        names(callbacks)
      )

      cb_args <- c(
        rv_ro,
        list(update = board_update),
        dot_args,
        list(session = session)
      )

      if (identical(callback_location, "start")) {

        for (i in seq_along(callbacks)) {
          cb_res[[i]] <- do.call(callbacks[[i]], cb_args)
        }

        if (length(cb_res) == 1L) {
          cb_res <- cb_res[[1L]]
        }

        dot_args <- c(dot_args, cb_res)
      }

      edit_block <- get_plugin("edit_block", plugins)
      edit_stack <- get_plugin("edit_stack", plugins)

      edit_plugin_args <- c(
        rv_ro,
        list(update = board_update),
        dot_args
      )

      observeEvent(
        TRUE,
        setup_board(rv, edit_block, edit_stack, edit_plugin_args, session),
        once = TRUE
      )

      call_plugin_server(
        "manage_blocks",
        server_args = edit_plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "manage_links",
        server_args = edit_plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "manage_stacks",
        server_args = edit_plugin_args,
        plugins = plugins
      )

      observeEvent(
        board_update(),
        {
          log_debug("starting board update")
          validate_board_update(board_update, rv$board)
          log_debug("board update validated")

          log_debug("preprocessing board update")
          if (!preprocess_board_update(board_update, rv$board)) {
            log_debug("validating board links against board")
            validate_update_links_board(board_update, rv$board)
          }
        },
        priority = Inf
      )

      observeEvent(
        board_update(),
        {
          upd <- board_update()

          if (length(upd$blocks$add)) {

            log_debug("adding block{?s} {names(upd$blocks$add)}")

            do.call(
              insert_block_ui,
              c(
                list(ns(NULL), rv$board, upd$blocks$add),
                dot_args,
                list(edit_ui = edit_block, session = session)
              )
            )

            board_blocks(rv$board) <- c(board_blocks(rv$board), upd$blocks$add)

            for (blk in names(upd$blocks$add)) {
              setup_block(
                upd$blocks$add[[blk]], blk, rv, edit_block, edit_plugin_args
              )
            }
          }

          if (length(upd$blocks$mod)) {

            log_debug("modifying block{?s} {names(upd$blocks$mod)}")

            blks <- board_blocks(rv$board)
            blks[names(upd$blocks$mod)] <- upd$blocks$mod

            board_blocks(rv$board) <- blks
          }

          if (length(upd$links$mod)) {
            upd$links$add <- c(upd$links$add, upd$links$mod)
            upd$links$rm <- c(upd$links$rm, names(upd$links$mod))
          }

          if (length(upd$links$add) || length(upd$links$rm)) {

            if (length(upd$links$add)) {
              log_debug("adding link{?s} {names(upd$links$add)}")
            }

            if (length(upd$links$rm)) {
              log_debug("removing link{?s} {names(upd$links$rm)}")
            }

            rm <- board_links(rv$board)[upd$links$rm]
            update_block_links(rv, upd$links$add, rm)

            rv$board <- modify_board_links(rv$board, upd$links$add,
                                           upd$links$rm)
          }

          if (length(upd$stacks$rm)) {
            log_debug("removing stack{?s} {names(upd$stacks$rm)}")
          }

          if (length(upd$stacks$add)) {
            log_debug("adding stack{?s} {names(upd$stacks$add)}")
          }

          if (length(upd$stacks$mod)) {
            log_debug("modifying stack{?s} {names(upd$stacks$mod)}")
          }

          update_stack_blocks(
            rv, upd$stacks, edit_stack, edit_plugin_args, session
          )

          rv$board <- modify_board_stacks(rv$board, upd$stacks$add,
                                          upd$stacks$rm, upd$stacks$mod)

          if (length(upd$blocks$rm)) {

            log_debug("removing block{?s} {names(upd$blocks$rm)}")

            do.call(
              remove_block_ui,
              c(
                list(ns(NULL), rv$board, upd$blocks$rm),
                dot_args,
                list(edit_ui = edit_block, session = session)
              )
            )

            destroy_rm_blocks(upd$blocks$rm, rv, session)
          }

          board_update(NULL)

          log_debug("board update completed")
        },
        priority = -Inf
      )

      read_plugin_args <- c(rv_ro, dot_args)

      board_refresh <- call_plugin_server(
        "preserve_board",
        server_args = read_plugin_args,
        plugins = plugins
      )

      if (not_null(board_refresh)) {
        observeEvent(
          board_refresh(),
          {
            log_debug("refreshing board")
            update_serve_obj(board_refresh())
            log_debug("reloading session")
            session$reload()
          }
        )
      }

      call_plugin_server(
        "notify_user",
        server_args = read_plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "generate_code",
        server_args = read_plugin_args,
        plugins = plugins
      )

      if (identical(callback_location, "end")) {

        for (i in seq_along(callbacks)) {
          cb_res[[i]] <- do.call(callbacks[[i]], cb_args)
        }

        dot_args <- c(dot_args, cb_res)
      }

      if (all(c("thematic", "dark_mode") %in% names(options))) {
        observeEvent(
          get_board_option_values(
            c("thematic", "dark_mode"),
            opts = options,
            if_not_found = "null"
          ),
          {
            if (isTRUE(get_board_option_or_null("thematic"))) {
              do.call(thematic::thematic_shiny, bs_theme_colors(session))
            } else if (isFALSE(get_board_option_or_null("thematic"))) {
              thematic::thematic_off()
            }
          }
        )
      }

      c(rv_ro, dot_args)
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

    if (identical(get_board_option_or_null("dark_mode"), "dark")) {
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
    setup_block(blks[[i]], i, rv, blk_mod, args)
  }

  setup_stacks(rv, stk_mod, args)
  add_blocks_to_stacks(rv, board_stacks(rv$board), sess)

  invisible()
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

  update_block_links(rv, links[links$to == id])

  rv$blocks[[id]] <- list(
    block = blk,
    server = do.call(
      block_server,
      c(list(paste0("block_", id), blk, rv$inputs[[id]], id, mod), args)
    )
  )

  invisible()
}

destroy_rm_blocks <- function(ids, rv, sess) {

  links <- board_links(rv$board)

  update_block_links(
    rv,
    rm = links[links$from %in% ids | links$to %in% ids]
  )

  for (id in ids) {
    destroy_module(paste0("block_", id), session = sess)
    remove_block_from_stack(rv$board, id, rv$board_id, sess)
  }

  rv$inputs <- rv$inputs[!names(rv$inputs) %in% ids]
  rv$blocks <- rv$blocks[!names(rv$blocks) %in% ids]

  rv$board <- rm_blocks(rv$board, ids)

  invisible()
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

  invisible()
}

destroy_link <- function(rv, id, from, to, input) {

  rv$links[[id]]$destroy()
  rv$links[[id]] <- NULL

  if (input %in% block_inputs(board_blocks(rv$board)[[to]])) {
    rv$inputs[[to]][[input]](NULL)
  } else {
    rv$inputs[[to]][["...args"]][[input]] <- NULL
  }

  invisible()
}

update_block_links <- function(rv, add = NULL, rm = NULL) {

  todo <- as.list(rm)

  for (i in names(todo)) {
    do.call(destroy_link, c(list(rv, i), todo[[i]]))
  }

  todo <- as.list(add)

  for (i in names(todo)) {
    do.call(setup_link, c(list(rv, i), todo[[i]]))
  }

  invisible()
}

setup_stacks <- function(rv, mod, args, stacks = board_stacks(rv$board)) {

  serv <- get_plugin_server(mod)

  for (i in names(stacks)) {

    if (not_null(serv)) {
      serv(c(list(id = paste0("stack_", i), stack_id = i), args))
    }

    rv$stacks[[i]] <- character()
  }

  invisible()
}

destroy_stacks <- function(ids, rv, sess) {

  stopifnot(all(lengths(rv$stacks[ids]) == 0L))

  for (id in ids) {
    destroy_module(paste0("stack_", id), session = sess)
  }

  rv$stacks[ids] <- NULL

  invisible()
}

update_stack_blocks <- function(rv, upd, mod, args, session) {

  if (length(upd$rm)) {
    rm_blocks_from_stacks(rv, upd$rm, session)
    destroy_stacks(upd$rm, rv, session)
    remove_stack_ui(upd$rm, rv$board)
  }

  if (length(upd$add)) {
    setup_stacks(rv, mod, args, upd$add)
    insert_stack_ui(rv$board_id, upd$add, rv$board, mod)
    add_blocks_to_stacks(rv, upd$add, session)
  }

  if (length(upd$mod)) {
    update_blocks_in_stacks(rv, upd$mod, session)
  }

  invisible()
}

update_blocks_in_stacks <- function(rv, mod, sess) {

  for (i in names(mod)) {
    update_blocks_in_stack(i, rv, mod[[i]], sess)
  }

  invisible()
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

  invisible()
}

rm_blocks_from_stacks <- function(rv, rm, session) {

  stopifnot(is.character(rm), all(rm %in% names(rv$stacks)))

  for (i in rm) {

    for (j in rv$stacks[[i]]) {
      remove_block_from_stack(rv$board, j, rv$board_id, session)
    }

    rv$stacks[[i]] <- character()
  }

  invisible()
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

  invisible()
}

validate_board_update <- function(x, board) {

  if (!is.reactive(x)) {
    blockr_abort(
      "Expecting a board update to be passed as a reactive object.",
      class = "board_update_object_invalid"
    )
  }

  res <- x()

  expected <- c("blocks", "links", "stacks")

  if (!is.list(res)) {
    blockr_abort(
      "Expecting a board update to be specified as a list.",
      class = "board_update_type_invalid"
    )
  }

  if (!all(names(res) %in% expected)) {
    blockr_abort(
      "Expecting a board update to consist of components {expected}. Please ",
      "remove {setdiff(names(res), expected)}.",
      class = "board_update_components_invalid"
    )
  }

  expected <- c("add", "rm", "mod")

  for (cmp in res) {

    if (!is.list(cmp)) {
      blockr_abort(
        "Expecting a board update component to be specified as a list.",
        class = "board_update_component_type_invalid"
      )
    }

    if (!length(names(cmp)) == length(cmp) || !all(names(cmp) %in% expected)) {

      blockr_abort(
        "Expecting a board update component to consist of components ",
        "{expected}. Please remove {setdiff(names(cmp), expected)}.",
        class = "board_update_component_components_invalid"
      )
    }
  }

  if ("blocks" %in% names(res)) {
    validate_board_update_blocks(res$blocks, board)
  }

  if ("links" %in% names(res)) {
    validate_board_update_links(res$links, board)
  }

  if ("stacks" %in% names(res)) {
    validate_board_update_stacks(res$stacks, board)
  }

  invisible()
}

validate_board_update_blocks <- function(x, board) {

  all_ids <- board_block_ids(board)

  if ("rm" %in% names(x) && is.character(x$rm)) {
    cur_ids <- setdiff(all_ids, x$rm)
  } else {
    cur_ids <- all_ids
  }

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_blocks(x$add)) {
      blockr_abort(
        "Expecting the \"add\" block component of a board update to be ",
        "`NULL` or a `blocks` object.",
        class = "board_update_blocks_add_invalid"
      )
    }

    if (any(names(x$add) %in% cur_ids)) {
      blockr_abort(
        "Expecting the newly added block to have a unique ID.",
        class = "board_update_blocks_add_invalid"
      )
    }

    validate_blocks(x$add)

    if ("mod" %in% names(x) && !is.null(x$mod)) {
      if (length(intersect(names(x$add), names(x$mod)))) {
        blockr_abort(
          "Cannot add and modify the same IDs simulatneously.",
          class = "board_update_blocks_add_mod_clash"
        )
      }
    }
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      blockr_abort(
        "Expecting the \"rm\" block component of a board update value ",
        "to be `NULL` or a character vector.",
        class = "board_update_blocks_rm_invalid"
      )
    }

    if (!all(x$rm %in% all_ids)) {
      blockr_abort(
        "Expecting the removed block to be specified by a known ID.",
        class = "board_update_blocks_rm_invalid"
      )
    }
  }

  if ("mod" %in% names(x)) {

    if (!is_blocks(x$mod)) {
      blockr_abort(
        "Expecting the \"mod\" block component of a board update",
        "value to be `NULL` or a `blocks` object.",
        class = "board_update_blocks_mod_invalid"
      )
    }

    if (!all(names(x$mod) %in% cur_ids)) {
      blockr_abort(
        "Expecting the modified blocks to be specified by known IDs.",
        class = "board_update_blocks_mod_invalid"
      )
    }

    validate_blocks(x$mod)
  }

  invisible()
}

validate_board_update_links <- function(x, board) {

  all_ids <- board_link_ids(board)

  if ("rm" %in% names(x) && is.character(x$rm)) {
    cur_ids <- setdiff(all_ids, x$rm)
  } else {
    cur_ids <- all_ids
  }

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_links(x$add)) {
      blockr_abort(
        "Expecting the \"add\" link component of a board update",
        "value to be `NULL` or a `links` object.",
        class = "board_update_links_add_invalid"
      )
    }

    if (any(names(x$add) %in% cur_ids)) {
      blockr_abort(
        "Expecting the newly added links to have a unique ID.",
        class = "board_update_links_add_invalid"
      )
    }

    validate_links(x$add)

    if ("mod" %in% names(x) && !is.null(x$mod)) {
      if (length(intersect(names(x$add), names(x$mod)))) {
        blockr_abort(
          "Cannot add and modify the same IDs simulatneously.",
          class = "board_update_links_add_mod_clash"
        )
      }
    }
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      blockr_abort(
        "Expecting the \"rm\" link component of a board update",
        "to be `NULL` or a character vector.",
        class = "board_update_links_rm_invalid"
      )
    }

    if (!all(x$rm %in% all_ids)) {
      blockr_abort(
        "Expecting all link IDs to be removed to be known.",
        class = "board_update_links_rm_invalid"
      )
    }
  }

  if ("mod" %in% names(x) && !is.null(x$mod)) {

    if (!is_links(x$mod)) {
      blockr_abort(
        "Expecting the \"mod\" link component of a board update",
        "value to be `NULL` or a `links` object.",
        class = "board_update_links_mod_invalid"
      )
    }

    if (!all(names(x$mod) %in% cur_ids)) {
      blockr_abort(
        "Expecting the modified links to be specified by known IDs.",
        class = "board_update_links_mod_invalid"
      )
    }

    validate_links(x$mod)
  }

  invisible()
}

validate_update_links_board <- function(x, board) {

  upd <- x()

  if ("links" %in% names(upd)) {
    lnk <- upd$links
  } else {
    return(invisible())
  }

  all_ids <- board_link_ids(board)

  if ("rm" %in% names(lnk) && is.character(lnk$rm)) {
    cur_ids <- setdiff(all_ids, lnk$rm)
  } else {
    cur_ids <- all_ids
  }

  if ("add" %in% names(lnk) && !is.null(lnk$add)) {
    validate_board_blocks_links(
      board_blocks(board),
      c(board_links(board)[cur_ids], lnk$add)
    )
  }

  if ("mod" %in% names(lnk) && !is.null(lnk$mod)) {
    validate_board_blocks_links(
      board_blocks(board),
      c(board_links(board)[setdiff(cur_ids, names(lnk$mod))], lnk$mod)
    )
  }

  invisible()
}

validate_board_update_stacks <- function(x, board) {

  all_ids <- board_stack_ids(board)

  if ("rm" %in% names(x) && is.character(x$rm)) {
    cur_ids <- setdiff(all_ids, x$rm)
  } else {
    cur_ids <- all_ids
  }

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_stacks(x$add)) {
      blockr_abort(
        "Expecting the \"add\" stack component of a board update",
        "to be `NULL` or a `stacks` object.",
        class = "board_update_stacks_add_invalid"
      )
    }

    if (any(names(x$add) %in% cur_ids)) {
      blockr_abort(
        "Expecting the newly added stacks to have a unique ID.",
        class = "board_update_stacks_add_invalid"
      )
    }

    validate_stacks(x$add)

    validate_board_blocks_stacks(
      board_blocks(board),
      c(board_stacks(board)[cur_ids], x$add)
    )

    if ("mod" %in% names(x) && !is.null(x$mod)) {
      if (length(intersect(names(x$add), names(x$mod)))) {
        blockr_abort(
          "Cannot add and modify the same IDs simulatneously.",
          class = "board_update_stacks_add_mod_clash"
        )
      }
    }
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      blockr_abort(
        "Expecting the \"rm\" stack component of a board update",
        "to be `NULL` or a character vector.",
        class = "board_update_stacks_rm_invalid"
      )
    }

    if (!all(x$rm %in% all_ids)) {
      blockr_abort(
        "Expecting all stack IDs to be removed to be known.",
        class = "board_update_stacks_rm_invalid"
      )
    }
  }

  if ("mod" %in% names(x) && !is.null(x$mod)) {

    if (!is_stacks(x$mod)) {
      blockr_abort(
        "Expecting the \"mod\" stack component of a board update",
        "value to be `NULL` or a `stacks` object.",
        class = "board_update_stacks_mod_invalid"
      )
    }

    if (!all(names(x$mod) %in% cur_ids)) {
      blockr_abort(
        "Expecting the modified stacks to be specified by known IDs.",
        class = "board_update_stacks_mod_invalid"
      )
    }

    validate_stacks(x$mod)

    validate_board_blocks_stacks(
      board_blocks(board),
      c(board_stacks(board)[setdiff(cur_ids, names(x$mod))], x$mod)
    )
  }

  invisible()
}

preprocess_board_update <- function(update, board) {

  upd <- update()

  if ("blocks" %in% names(upd) && "rm" %in% names(upd$blocks)) {

    rm <- upd$blocks$rm

    links <- board_links(board)

    mis_lnk <- setdiff(
      names(links[links$from %in% rm | links$to %in% rm]),
      upd$links$rm
    )

    stacks <- board_stacks(board)

    stacks <- c(
      upd$stacks$mod,
      stacks[setdiff(names(stacks), c(names(upd$stacks$mod), upd$stacks$rm))]
    )

    upd_stk <- as_stacks(
      lapply(
        stacks[lengths(lapply(stacks, intersect, rm)) > 0L],
        setdiff,
        rm
      )
    )

  } else {

    mis_lnk <- NULL
    upd_stk <- NULL
  }

  add_lnk <- NULL

  if ("links" %in% names(upd) && "add" %in% names(upd$links)) {

    tmp <- complete_unary_inputs(upd$links$add, board_blocks(board))
    tmp <- complete_variadic_inputs(tmp, board_blocks(board))

    if (!identical(tmp$input, upd$links$add$input)) {
      add_lnk <- tmp
    }
  }

  upd_lnk <- NULL

  if ("links" %in% names(upd) && "mod" %in% names(upd$links)) {

    tmp <- complete_unary_inputs(upd$links$mod, board_blocks(board))
    tmp <- complete_variadic_inputs(tmp, board_blocks(board))

    if (!identical(tmp$input, upd$links$mod$input)) {
      upd_lnk <- tmp
    }
  }

  if (length(mis_lnk)) {
    log_debug("adding link removal{?s} for {mis_lnk}")
    upd$links$rm <- c(mis_lnk, upd$links$rm)
  }

  if (length(upd_stk)) {
    log_debug("adding stack update{?s} for {names(upd_stk)}")
    upd$stacks$mod <- c(upd_stk, upd$stacks$mod)
  }

  if (length(add_lnk)) {
    log_debug("adding link input update{?s} for {names(add_lnk)}")
    upd$links$add <- c(
      add_lnk,
      upd$links$add[setdiff(names(upd$links$add), names(add_lnk))]
    )
  }

  if (length(upd_lnk)) {
    log_debug("adding link input update{?s} for {names(upd_lnk)}")
    upd$links$mod <- c(
      upd_lnk,
      upd$links$mod[setdiff(names(upd$links$mod), names(upd_lnk))]
    )
  }

  if (length(mis_lnk) + length(upd_stk) + length(add_lnk) + length(upd_lnk)) {
    update(upd)
    return(TRUE)
  }

  FALSE
}

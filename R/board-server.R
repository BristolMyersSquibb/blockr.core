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
#' @section Active conditions:
#' Conditions raised while blocks evaluate (errors, warnings and messages) are
#' exposed as a reactive data frame `board$conditions` on the read-only board
#' handed to plugins and callbacks, with one row per active condition and
#' columns `block`, `phase`, `severity`, `message` and `id`. It combines the
#' per-block `server$conditions` reactives (see [block_server()]), so a
#' consumer reads a single reactive — the whole board, or one block's frame
#' for fine-grained updates — rather than walking nested condition state. The
#' default [notify_user()] plugin renders its toasts from this source.
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
#' @param callbacks Single (or list of) callback function(s) registering
#' additional observers. Each receives a `visibility` list with two channels,
#' `required` and `visible`, each an environment of per-block `reactiveVal`s
#' (core keeps one per board block as blocks are added and removed). Declare a
#' block needed with `visibility$required[[id]](TRUE)` (or `FALSE` for built
#' but dormant) and report the view it is rendered into with
#' `visibility$visible[[id]](view)` (or `NA_character_` for off screen); the
#' board reads both to gate construction, evaluation and rendering.
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

  # carry the resolved option set on rv$board so it serializes in full
  board_options(x) <- options

  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        sources = list(),
        board = x,
        board_id = id,
        stacks = list(),
        last_update = NULL,
        conditions = NULL
      )

      rv$eval <- reactiveValues()

      vis <- list(
        required = new.env(parent = emptyenv()),
        visible = new.env(parent = emptyenv())
      )

      add_vis_slots(vis, isolate(board_block_ids(rv$board)))

      rv_ro <- list(board = make_read_only(rv))

      rv$conditions <- reactive(
        combine_block_conditions(
          lapply(rv$blocks, function(blk) blk$server$conditions())
        )
      )

      rv$needed <- reactiveVal(TRUE)

      observe(
        {
          cur <- if (!gating_active(vis$required)) {
            TRUE
          } else {
            upstream_blocks(required_now(vis$required), rv$board)
          }

          old <- isolate(rv$needed())

          same <- if (isTRUE(cur) || isTRUE(old)) {
            identical(cur, old)
          } else {
            setequal(cur, old)
          }

          if (!same) {
            rv$needed(cur)
          }
        }
      )

      observe(
        validate_vis(vis),
        priority = Inf
      )

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
        list(update = board_update, visibility = vis),
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
      ctrl_block <- get_plugin("ctrl_block", plugins)

      edit_stack <- get_plugin("edit_stack", plugins)

      edit_plugin_args <- c(
        rv_ro,
        list(update = board_update),
        dot_args
      )

      observeEvent(
        TRUE,
        setup_board(rv, edit_block, ctrl_block, edit_stack, edit_plugin_args,
                    session, vis),
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

      update_seq <- 0L

      record_update_outcome <- function(ok, phase, message = NA_character_) {

        update_seq <<- update_seq + 1L

        rv$last_update <- list(
          seq = update_seq,
          ok = ok,
          phase = phase,
          message = message
        )
      }

      observeEvent(
        board_update(),
        {
          upd <- board_update()

          tryCatch(
            {
              if (is_board_locked(rv$board)) {

                log_debug("rejecting board update on locked board")
                board_update(NULL)

              } else {

                log_debug("starting board update")
                validate_board_update_structure(upd, rv$board)
                log_debug("board update validated")

                log_debug("preprocessing board update")
                if (!preprocess_board_update(board_update, rv$board)) {
                  log_debug("validating board links against board")
                  validate_board_update_xrefs(upd, rv$board)
                }
              }
            },
            error = function(e) {
              log_warn("board update rejected: {conditionMessage(e)}")
              notify(conditionMessage(e), type = "error", glue = FALSE,
                     session = session)
              record_update_outcome(FALSE, "validate", conditionMessage(e))
              board_update(NULL)
            }
          )
        },
        priority = Inf
      )

      observeEvent(
        board_update(),
        {
          upd <- board_update()

          tryCatch(
            {
              apply_core_board_update(
                rv, upd,
                session = session,
                edit_block = edit_block,
                ctrl_block = ctrl_block,
                edit_stack = edit_stack,
                edit_plugin_args = edit_plugin_args,
                dot_args = dot_args,
                vis = vis
              )

              new_board <- do.call(
                apply_board_update,
                c(
                  list(rv$board, upd),
                  dot_args,
                  list(session = session)
                )
              )

              stopifnot(is_board(new_board))
              rv$board <- new_board

              record_update_outcome(TRUE, "apply")
            },
            error = function(e) {
              log_warn("apply_board_update failed: {conditionMessage(e)}")
              notify(conditionMessage(e), type = "error", glue = FALSE,
                     session = session)
              record_update_outcome(FALSE, "apply", conditionMessage(e))
            }
          )

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

      call_plugin_server(
        "notify_user",
        server_args = read_plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "generate_code",
        server_args = c(read_plugin_args, list(visibility = vis)),
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

      c(rv_ro, dot_args, list(board_refresh = board_refresh))
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

deduped_board_reactive <- function(board, accessor) {

  val <- reactiveVal(isolate(accessor(board$board)))

  observe(
    {
      new <- accessor(board$board)

      if (!identical(new, isolate(val()))) {
        val(new)
      }
    }
  )

  val
}

setup_board <- function(rv, blk_ed, blk_ct, stk_mod, args, sess, vis) {

  stopifnot(
    is.reactivevalues(rv),
    all(c("blocks", "inputs", "sources", "board", "stacks") %in% names(rv)),
    is_board(rv$board)
  )

  rv$blocks <- list()
  rv$inputs <- list()
  rv$sources <- list()
  rv$stacks <- list()

  construct_remaining_blocks(rv, blk_ed, blk_ct, args, vis)

  setup_stacks(rv, stk_mod, args)
  add_blocks_to_stacks(rv, board_stacks(rv$board), sess)

  invisible()
}

combine_block_conditions <- function(frames) {

  res <- do.call(
    rbind,
    c(list(empty_conditions_frame()), unname(frames))
  )

  row.names(res) <- NULL

  res
}

construct_block <- function(id, rv, mod_ed, mod_ct, args, vis) {

  if (id %in% names(rv$blocks)) {
    return(invisible())
  }

  blk <- board_blocks(rv$board)[[id]]

  rv$sources[[id]] <- reactiveValues()
  src_rv <- rv$sources[[id]]

  inpts <- set_names(
    lapply(block_inputs(blk), upstream_result, src_rv, rv, id),
    block_inputs(blk)
  )

  if (is.na(block_arity(blk))) {
    inpts <- c(inpts, list(`...args` = reactives()))
  }

  rv$inputs[[id]] <- inpts

  links <- board_links(rv$board)

  update_block_links(rv, links[links$to == id])

  inputs_ready <- reactive(block_inputs_ready(src_rv, blk, rv))

  srv <- do.call(
    block_server,
    c(
      list(paste0("block_", id), blk, rv$inputs[[id]], id, mod_ed, mod_ct),
      args,
      list(inputs_ready = inputs_ready, visibility = vis)
    )
  )

  rv$blocks[[id]] <- list(block = blk, server = srv)

  rv$eval[[id]] <- reactive(block_eval_status(rv, id, inputs_ready, srv))

  invisible()
}

construct_blocks <- function(ids, rv, mod_ed, mod_ct, args, vis) {

  ordered <- isolate(
    intersect(topo_sort(as.matrix(rv$board)), setdiff(ids, names(rv$blocks)))
  )

  if (!length(ordered)) {
    return(invisible())
  }

  log_debug("constructing block{?s} {ordered}")

  for (id in ordered) {
    isolate(construct_block(id, rv, mod_ed, mod_ct, args, vis))
  }

  invisible()
}

construct_remaining_blocks <- function(rv, mod_ed, mod_ct, args, vis) {

  delay <- background_construction_delay()

  if (delay <= 0) {

    construct_blocks(board_block_ids(rv$board), rv, mod_ed, mod_ct, args,
                     vis)

    return(invisible())
  }

  if (is.infinite(delay)) {

    construct_needed_blocks(rv, mod_ed, mod_ct, args, vis)

    return(invisible())
  }

  construct_blocks_in_background(rv, mod_ed, mod_ct, args, vis)

  invisible()
}

construct_needed_blocks <- function(rv, mod_ed, mod_ct, args, vis) {

  observe(
    {
      need <- needed_block_ids(rv, vis$required)
      construct_blocks(need, rv, mod_ed, mod_ct, args, vis)
    }
  )

  invisible()
}

construct_blocks_in_background <- function(rv, mod_ed, mod_ct, args,
                                           vis) {

  started <- FALSE

  obs <- observe(
    {
      if (!started) {

        started <<- TRUE

        if (!isolate(gating_active(vis$required))) {

          construct_blocks(board_block_ids(rv$board), rv, mod_ed, mod_ct,
                           args, vis)
          obs$destroy()

          return(invisible())
        }

        invalidateLater(background_construction_delay())

        return(invisible())
      }

      remaining <- isolate(
        setdiff(topo_sort(as.matrix(rv$board)), names(rv$blocks))
      )

      if (!length(remaining)) {

        obs$destroy()

        return(invisible())
      }

      needed <- isolate(
        intersect(remaining, needed_block_ids(rv, vis$required))
      )

      if (length(needed)) {

        isolate(construct_block(needed[[1L]], rv, mod_ed, mod_ct, args, vis))

        invalidateLater(background_construction_delay())

        return(invisible())
      }

      if (gating_active(vis$required) && !required_fulfilled(vis)) {
        return(invisible())
      }

      isolate(construct_block(remaining[[1L]], rv, mod_ed, mod_ct, args, vis))

      invalidateLater(background_construction_delay())
    }
  )

  invisible()
}

background_construction_delay <- function() {
  as.numeric(blockr_option("background_construction_delay", 50L))
}

add_vis_slots <- function(vis, ids) {

  for (id in ids) {
    vis$required[[id]] <- reactiveVal(NA)
    vis$visible[[id]] <- reactiveVal(NA_character_)
  }

  invisible()
}

rm_vis_slots <- function(vis, ids) {

  gone <- intersect(ids, ls(vis$required))

  if (length(gone)) {
    rm(list = gone, envir = vis$required)
    rm(list = gone, envir = vis$visible)
  }

  invisible()
}

gating_active <- function(required) {
  isTRUE(blockr_option("gate_visibility", TRUE)) && has_required(required)
}

has_required <- function(required) {
  length(ever_required(required)) > 0L
}

ever_required <- function(required) {
  ids <- ls(required)
  ids[lgl_ply(ids, slot_declared, required)]
}

slot_declared <- function(id, required) {
  !is.na(required[[id]]())
}

required_now <- function(required) {
  ids <- ls(required)
  ids[lgl_ply(ids, slot_needed, required)]
}

slot_needed <- function(id, required) {
  isTRUE(required[[id]]())
}

is_visible <- function(x) {
  !is.na(x)
}

block_visible <- function(id, vis) {
  is_visible(vis$visible[[id]]())
}

required_fulfilled <- function(vis) {
  all(lgl_ply(required_now(vis$required), block_visible, vis))
}

validate_vis <- function(vis) {

  for (id in ls(vis$required)) {
    if (!valid_required(vis$required[[id]]())) {
      blockr_abort(
        "required[[{id}]] must be TRUE, FALSE or NA",
        class = "invalid_required"
      )
    }
  }

  for (id in ls(vis$visible)) {
    if (!valid_visible(vis$visible[[id]]())) {
      blockr_abort(
        "visible[[{id}]] must be a non-empty string or NA_character_",
        class = "invalid_visible"
      )
    }
  }

  invisible()
}

valid_required <- function(x) {
  is.logical(x) && length(x) == 1L
}

valid_visible <- function(x) {
  is.character(x) && length(x) == 1L && (is.na(x) || nzchar(x))
}

needed_block_ids <- function(rv, required) {

  need <- rv$needed()

  if (isTRUE(need)) {
    return(board_block_ids(rv$board))
  }

  union(need, ever_required(required))
}

block_inputs_ready <- function(src_rv, blk, rv) {

  src <- reactiveValuesToList(src_rv)
  fixed <- block_inputs(blk)
  required <- setdiff(fixed, block_optional_inputs(blk))

  if (!all(lgl_ply(src[required], input_ready, rv))) {
    return(FALSE)
  }

  if (is.na(block_arity(blk))) {

    variadic <- src[setdiff(names(src), fixed)]

    if (sum(lgl_ply(variadic, input_ready, rv)) < block_min_args(blk)) {
      return(FALSE)
    }
  }

  TRUE
}

input_ready <- function(from, rv) {
  not_null(from) && identical(reval_if(rv$eval[[from]]), "ready")
}

block_eval_status <- function(rv, id, inputs_ready, srv) {

  if (!block_needed(rv, id)) {
    return("dormant")
  }

  if (!inputs_ready()) {
    return("waiting")
  }

  if (!isTRUE(srv$state_ready())) {
    return("unset")
  }

  if (isTRUE(srv$failed())) {
    return("failed")
  }

  "ready"
}

block_needed <- function(rv, id) {
  needed <- rv$needed()
  isTRUE(needed) || id %in% needed
}

apply_block_mod_delta <- function(blk_id, delta, rv) {

  state_rvs <- rv$blocks[[blk_id]]$server$state

  for (nm in names(delta)) {

    if (identical(nm, "block_name")) {

      blks <- board_blocks(rv$board)
      block_name(blks[[blk_id]]) <- delta[[nm]]
      board_blocks(rv$board) <- blks

    } else {

      cur <- reval_if(state_rvs[[nm]])

      if (!identical(cur, delta[[nm]])) {
        state_rvs[[nm]](delta[[nm]])
      }
    }
  }

  invisible()
}

destroy_rm_blocks <- function(ids, rv, sess, args) {

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
  rv$sources <- rv$sources[!names(rv$sources) %in% ids]
  rv$blocks <- rv$blocks[!names(rv$blocks) %in% ids]

  for (id in ids) {
    rv$eval[[id]] <- NULL
  }

  rv$board <- do.call(
    rm_blocks,
    c(list(rv$board, ids), args, list(session = sess))
  )

  invisible()
}

upstream_result <- function(key, src_rv, rv, to) {

  force(key)

  reactive(
    {
      needed <- rv$needed()
      req(isTRUE(needed) || to %in% needed)

      from <- src_rv[[key]]

      srv <- if (is.null(from)) NULL else isolate(rv$blocks[[from]])[["server"]]

      if (is.null(srv)) NULL else srv$result()
    }
  )
}

link_slot_key <- function(rv, to, id, input) {

  if (input %in% block_inputs(board_blocks(rv$board)[[to]])) input else id
}

setup_link <- function(rv, id, from, to, input) {

  rv$sources[[to]][[link_slot_key(rv, to, id, input)]] <- from

  invisible()
}

destroy_link <- function(rv, id, from, to, input) {

  src_rv <- rv$sources[[to]]

  if (input %in% block_inputs(board_blocks(rv$board)[[to]])) {
    src_rv[[input]] <- NULL
  } else {
    trim_rv(src_rv, id)
  }

  invisible()
}

variadic_links <- function(rv, to, add, rm) {

  lnks <- board_links(rv$board)

  present <- intersect(names(add), names(lnks))

  if (length(present)) {
    lnks[present] <- add[present]
  }

  fresh <- add[setdiff(names(add), present)]
  drop <- setdiff(names(rm), present)

  lnks <- c(lnks[!names(lnks) %in% drop], fresh)

  fixed <- block_inputs(board_blocks(rv$board)[[to]])

  lnks[lnks$to == to & !field(lnks, "input") %in% fixed]
}

sync_dot_args <- function(rv, to, lnks) {

  args <- rv$inputs[[to]][["...args"]]
  src_rv <- rv$sources[[to]]

  for (key in isolate(raw_keys(args))) {
    drop_reactive(args, key)
  }

  ids <- names(lnks)
  inputs <- field(lnks, "input")

  for (i in seq_along(ids)) {

    slot <- upstream_result(ids[[i]], src_rv, rv, to)

    if (nzchar(inputs[[i]])) {
      set_reactive(args, inputs[[i]], slot)
    } else {
      append_reactive(args, slot)
    }
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

  blks <- board_blocks(rv$board)

  touched <- unique(c(chr_xtr(as.list(rm), "to"), chr_xtr(as.list(add), "to")))
  touched <- touched[is.na(int_ply(blks[touched], block_arity))]

  for (to in touched) {
    sync_dot_args(rv, to, variadic_links(rv, to, add, rm))
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

#' Board update
#'
#' Inside [board_server()] every state change flows through one
#' `board_update` reactive. Core registers two observers framing the
#' change: an initial one that validates the payload and runs
#' [augment_board_update()] for auto-fixups, and a final one that runs
#' [apply_board_update()] and resets the reactive. Plugins or
#' callbacks may register their own observers in between, provided they
#' use a *finite* priority — the highest and lowest reactive priorities
#' are reserved for core.
#'
#' All three functions dispatch on the `board` class. Subclasses
#' override to validate, augment, or react to their own payload slots,
#' typically composing with `NextMethod()`. [validate_board_update()]
#' is also a caller-facing entry point: it mirrors the initial
#' observer's checks against a caller-supplied payload, useful for
#' staging layers (e.g. accumulating LLM-proposed updates) that need
#' to fail loudly before publishing.
#'
#' @section Validation:
#' The default `.board` method runs a structural check on the payload
#' (block / link / stack per-slot rules) and a cross-reference check
#' that link endpoints and stack members resolve in the post-update
#' merged view. Unknown top-level keys are passed through, so subclass
#' payload slots reach subclass augment / apply methods.
#'
#' @section Augment:
#' The default `.board` method inserts implied link removals and stack
#' updates that follow from block removals, plus link-input
#' completion. Subclass methods may extend the payload with their own
#' fixups; an error thrown here aborts the update before apply runs.
#'
#' @section Apply:
#' The default `.board` method returns the supplied board unchanged —
#' the core apply path (block / link / stack mutation, block UI
#' insertion / removal) is not routed through this generic. Subclass
#' methods receive a plain `board` snapshot (no reactive surface) and
#' return a `board`, which the final observer assigns back to
#' `rv$board`. For piecemeal customization of the core apply path
#' itself, override the relevant sub-generic
#' ([modify_board_links()], [insert_block_ui()], etc.) instead.
#'
#' Errors thrown from either augment or apply are caught by the
#' observer, reported via [notify()], and the reactive is reset so the
#' app keeps running.
#'
#' @section Outcome:
#' Alongside the human-facing [notify()] toast, every update cycle
#' records a machine-readable result into `board$last_update` (the
#' read-only board handed to plugins and callbacks). It is a list with
#' a monotonically increasing `seq`, a logical `ok`, the `phase` it
#' ended in (`"validate"` or `"apply"`), and a `message`
#' (`conditionMessage()` on failure, `NA` on success); it is `NULL`
#' before the first update. The `seq` advances on every write so that
#' two consecutive identical outcomes still invalidate a downstream
#' observer. A programmatic caller can watch this field to learn
#' whether a dispatched update was rejected, failed to apply, or landed.
#'
#' @param payload,upd A board update payload — see Validation above
#' for the accepted shape.
#' @param board A `board` object.
#' @param ... Forwarded between methods. For [apply_board_update()],
#' the final observer also splices `board_server()`'s `...` in here.
#' @param session A shiny session, default [get_session()].
#'
#' @return [validate_board_update()] returns `invisible(payload)` (or
#' throws a `blockr_abort()` error). [augment_board_update()] returns
#' the (possibly extended) payload. [apply_board_update()] returns a
#' `board`.
#'
#' @seealso On a locked board (see [is_board_locked()]) the update is
#' dropped rather than applied.
#'
#' @examples
#' brd <- new_board(
#'   blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
#'   links = links(ab = new_link(from = "a", to = "b"))
#' )
#'
#' validate_board_update(
#'   list(links = list(rm = "ab")),
#'   brd
#' )
#'
#' try(
#'   validate_board_update(
#'     list(links = list(add = links(xy = new_link(from = "x", to = "y")))),
#'     brd
#'   )
#' )
#'
#' @name board_update
NULL

#' @rdname board_update
#' @export
validate_board_update <- function(payload, board, ...,
                                  session = get_session()) {
  UseMethod("validate_board_update", board)
}

#' @export
validate_board_update.board <- function(payload, board, ...,
                                        session = get_session()) {

  validate_board_update_structure(payload, board)
  validate_board_update_xrefs(payload, board)

  invisible(payload)
}

# nolint next: object_length_linter.
validate_board_update_structure <- function(payload, board) {

  exp_typ <- c("blocks", "links", "stacks")

  if (!is.list(payload)) {
    blockr_abort(
      "Expecting a board update to be specified as a list.",
      class = "board_update_type_invalid"
    )
  }

  exp_cmp <- c("rm", "add", "mod")

  for (typ in exp_typ) {

    if (!typ %in% names(payload)) {
      next
    }

    x <- payload[[typ]]

    if (!is.list(x)) {
      blockr_abort(
        "Expecting a board update component to be specified as a list.",
        class = "board_update_component_type_invalid"
      )
    }

    if (!length(names(x)) == length(x) || !all(names(x) %in% exp_cmp)) {
      blockr_abort(
        "Expecting a board update component to consist of components ",
        "{exp_cmp}. Please remove {setdiff(names(x), exp_cmp)}.",
        class = "board_update_component_components_invalid"
      )
    }

    if ("rm" %in% names(x) && !(is.null(x$rm) || is.character(x$rm))) {
      blockr_abort(
        "Expecting a board update `rm` component be specified as a character ",
        "vector (or NULL).",
        class = "board_update_rm_component_invalid"
      )
    }

    if ("add" %in% names(x) && !(is.null(x$add) || inherits(x$add, typ))) {
      blockr_abort(
        "Expecting a board update `add` component be specified as a {typ} ",
        "object (or NULL).",
        class = "board_update_add_component_invalid"
      )
    }

    if ("mod" %in% names(x) && !(is.null(x$mod) || is.list(x$mod))) {
      blockr_abort(
        "Expecting a board update `mod` component be specified as a named ",
        "list of per-entry argument deltas (or NULL).",
        class = "board_update_mod_component_invalid"
      )
    }
  }

  if ("blocks" %in% names(payload)) {
    validate_board_update_blocks(payload$blocks, board)
  }

  if ("links" %in% names(payload)) {
    validate_board_update_links(payload$links, board)
  }

  if ("stacks" %in% names(payload)) {
    validate_board_update_stacks(payload, board)
  }

  invisible()
}

has_comp <- function(comp, x) {
  comp %in% names(x) && length(x[[comp]])
}

has_comps <- function(comps, x, fun = `||`) {
  Reduce(fun, lgl_ply(comps, has_comp, x))
}

validate_board_update_blocks <- function(x, board) {

  all_ids <- board_block_ids(board)

  if (has_comp("rm", x)) {
    cur_ids <- setdiff(all_ids, x$rm)
  } else {
    cur_ids <- all_ids
  }

  if (has_comp("rm", x)) {

    if (!all(x$rm %in% all_ids)) {
      blockr_abort(
        "Expecting the removed block to be specified by a known ID.",
        class = "board_update_blocks_rm_invalid"
      )
    }
  }

  if (has_comp("add", x)) {

    if (any(names(x$add) %in% cur_ids)) {
      blockr_abort(
        "Expecting the newly added block to have a unique ID.",
        class = "board_update_blocks_add_invalid"
      )
    }

    validate_blocks(x$add)

    if (has_comp("mod", x) && length(intersect(names(x$add), names(x$mod)))) {
      blockr_abort(
        "Cannot add and modify the same IDs simulatneously.",
        class = "board_update_blocks_add_mod_clash"
      )
    }
  }

  if (has_comp("mod", x)) {

    validate_mod_deltas(x$mod, cur_ids, "blocks")

    blks <- board_blocks(board)

    for (blk_id in names(x$mod)) {

      delta <- x$mod[[blk_id]]
      allowed <- external_ctrl_vars(blks[[blk_id]])
      extra <- setdiff(names(delta), allowed)

      if (length(extra)) {
        blockr_abort(
          "Block `{blk_id}` mod delta contains argument{?s} {extra} which ",
          "{?is/are} not externally controllable. Use a `rm` + `add` payload ",
          "to replace the block.",
          class = "board_update_blocks_mod_not_ctrl"
        )
      }
    }
  }

  invisible()
}

validate_board_update_links <- function(x, board) {

  all_ids <- board_link_ids(board)

  if (has_comp("rm", x)) {
    cur_ids <- setdiff(all_ids, x$rm)
  } else {
    cur_ids <- all_ids
  }

  if (has_comp("rm", x)) {

    if (!all(x$rm %in% all_ids)) {
      blockr_abort(
        "Expecting all link IDs to be removed to be known.",
        class = "board_update_links_rm_invalid"
      )
    }
  }

  if (has_comp("add", x)) {

    if (any(names(x$add) %in% cur_ids)) {
      blockr_abort(
        "Expecting the newly added links to have a unique ID.",
        class = "board_update_links_add_invalid"
      )
    }

    validate_links(x$add)

    if (has_comp("mod", x) && length(intersect(names(x$add), names(x$mod)))) {
      blockr_abort(
        "Cannot add and modify the same IDs simulatneously.",
        class = "board_update_links_add_mod_clash"
      )
    }
  }

  if (has_comp("mod", x)) {

    validate_mod_deltas(x$mod, cur_ids, "links")
  }

  invisible()
}

validate_mod_deltas <- function(mod, cur_ids, typ) {

  err_class <- function(suffix) paste0("board_update_", typ, "_mod_", suffix)

  if (length(names(mod)) != length(mod) || any(nchar(names(mod)) == 0L)) {
    blockr_abort(
      "Expecting the {typ} `mod` component to be a named list keyed by ID.",
      class = err_class("unnamed")
    )
  }

  unknown <- setdiff(names(mod), cur_ids)

  if (length(unknown)) {
    blockr_abort(
      "Modified {typ} entries reference unknown ID{?s} {unknown}.",
      class = err_class("unknown_id")
    )
  }

  is_obj <- switch(typ, blocks = is_block, links = is_link, stacks = is_stack)

  for (id in names(mod)) {

    delta <- mod[[id]]

    if (is_obj(delta) || !is.list(delta) ||
          length(names(delta)) != length(delta) ||
          any(nchar(names(delta)) == 0L)) {
      blockr_abort(
        "Expecting each {typ} `mod` entry to be a named list of ",
        "constructor argument values.",
        class = err_class("entry_invalid")
      )
    }

    if (!length(delta)) {
      blockr_abort(
        "{typ} `mod` entry `{id}` is empty; omit it or supply at least one ",
        "argument.",
        class = err_class("entry_empty")
      )
    }
  }

  invisible()
}

combine_update_blocks <- function(upd, board) {

  all_blks <- board_blocks(board)

  if (!has_comp("blocks", upd)) {
    return(all_blks)
  }

  blk <- upd$blocks

  if (has_comp("rm", blk)) {
    all_blks <- all_blks[setdiff(names(all_blks), blk$rm)]
  }

  if (has_comp("add", blk)) {
    all_blks <- c(all_blks, blk$add)
  }

  all_blks
}

validate_board_update_xrefs <- function(payload, board) {

  payload <- augment_board_update(payload, board)

  if (has_comp("links", payload)) {
    lnk <- payload$links
  } else {
    return(invisible())
  }

  if (has_comps(c("add", "mod"), lnk)) {

    all_lnks <- board_links(board)

    if (has_comp("rm", lnk)) {
      all_lnks <- all_lnks[setdiff(names(all_lnks), lnk$rm)]
    }

    if (has_comp("add", lnk)) {
      all_lnks <- c(all_lnks, lnk$add)
    }

    if (has_comp("mod", lnk)) {
      merged <- Map(
        update_link,
        board_links(board)[names(lnk$mod)],
        lnk$mod
      )
      all_lnks <- c(
        all_lnks[setdiff(names(all_lnks), names(lnk$mod))],
        as_links(merged)
      )
    }

    validate_board_blocks_links(
      combine_update_blocks(payload, board),
      all_lnks
    )
  }

  invisible()
}

validate_board_update_stacks <- function(upd, board) {

  x <- upd$stacks

  all_stks <- board_stacks(board)

  if (has_comp("rm", x)) {

    if (!all(x$rm %in% names(all_stks))) {
      blockr_abort(
        "Expecting all stack IDs to be removed to be known.",
        class = "board_update_stacks_rm_invalid"
      )
    }

    all_stks <- all_stks[setdiff(names(all_stks), x$rm)]
  }

  if (has_comp("add", x)) {

    if (any(names(x$add) %in% names(all_stks))) {
      blockr_abort(
        "Expecting the newly added stacks to have a unique ID.",
        class = "board_update_stacks_add_invalid"
      )
    }

    if (has_comp("mod", x) && length(intersect(names(x$add), names(x$mod)))) {
      blockr_abort(
        "Cannot add and modify the same IDs simulatneously.",
        class = "board_update_stacks_add_mod_clash"
      )
    }

    validate_stacks(x$add)

    all_stks <- c(all_stks, x$add)
  }

  if (has_comp("mod", x)) {

    validate_mod_deltas(x$mod, names(all_stks), "stacks")

    merged <- Map(
      update_stack,
      board_stacks(board)[names(x$mod)],
      x$mod
    )

    all_stks <- c(
      all_stks[setdiff(names(all_stks), names(x$mod))],
      as_stacks(merged)
    )
  }

  if (has_comps(c("add", "mod"), x)) {
    validate_board_blocks_stacks(combine_update_blocks(upd, board), all_stks)
  }

  invisible()
}

#' @rdname board_update
#' @export
augment_board_update <- function(upd, board, ...,
                                 session = get_session()) {
  UseMethod("augment_board_update", board)
}

#' @export
augment_board_update.board <- function(upd, board, ...,
                                       session = get_session()) {

  if ("blocks" %in% names(upd) && "rm" %in% names(upd$blocks)) {

    rm <- upd$blocks$rm

    links <- board_links(board)

    mis_lnk <- setdiff(
      names(links[links$from %in% rm | links$to %in% rm]),
      upd$links$rm
    )

    merged_stks <- board_stacks(board)

    if (length(upd$stacks$mod)) {
      cur_for_mod <- board_stacks(board)[names(upd$stacks$mod)]
      mod_stks <- Map(update_stack, cur_for_mod, upd$stacks$mod)
      merged_stks[names(upd$stacks$mod)] <- as_stacks(mod_stks)
    }

    if (length(upd$stacks$rm)) {
      merged_stks <- merged_stks[setdiff(names(merged_stks), upd$stacks$rm)]
    }

    affected <- merged_stks[
      lengths(lapply(merged_stks, intersect, rm)) > 0L
    ]

    upd_stk <- lapply(
      affected,
      function(s) list(blocks = setdiff(stack_blocks(s), rm))
    )

  } else {

    mis_lnk <- NULL
    upd_stk <- NULL
  }

  add_lnk <- NULL

  if ("links" %in% names(upd) && "add" %in% names(upd$links)) {

    tmp <- complete_unary_inputs(upd$links$add, board_blocks(board))

    if (!identical(tmp$input, upd$links$add$input)) {
      add_lnk <- tmp
    }
  }

  if (length(mis_lnk)) {
    log_debug("adding link removal{?s} for {mis_lnk}")
    upd$links$rm <- c(mis_lnk, upd$links$rm)
  }

  if (length(upd_stk)) {
    log_debug("adding stack update{?s} for {names(upd_stk)}")
    upd$stacks$mod <- c(
      upd_stk,
      upd$stacks$mod[setdiff(names(upd$stacks$mod), names(upd_stk))]
    )
  }

  if (length(add_lnk)) {
    log_debug("adding link input update{?s} for {names(add_lnk)}")
    upd$links$add <- c(
      add_lnk,
      upd$links$add[setdiff(names(upd$links$add), names(add_lnk))]
    )
  }

  upd
}

#' @rdname board_update
#' @export
apply_board_update <- function(board, upd, ...,
                               session = get_session()) {
  UseMethod("apply_board_update", board)
}

#' @export
apply_board_update.board <- function(board, upd, ...,
                                     session = get_session()) {
  board
}

apply_core_board_update <- function(rv, upd, session,
                                    edit_block, ctrl_block, edit_stack,
                                    edit_plugin_args, vis,
                                    dot_args = list()) {

  ns <- session$ns

  if (length(upd$blocks$add)) {

    log_debug("adding block{?s} {names(upd$blocks$add)}")

    add_vis_slots(vis, names(upd$blocks$add))

    do.call(
      insert_block_ui,
      c(
        list(ns(NULL), rv$board, upd$blocks$add),
        dot_args,
        list(
          edit_ui = edit_block,
          ctrl_ui = ctrl_block,
          session = session
        )
      )
    )

    board_blocks(rv$board) <- c(board_blocks(rv$board), upd$blocks$add)

    construct_blocks(names(upd$blocks$add), rv, edit_block, ctrl_block,
                     edit_plugin_args, vis)
  }

  if (length(upd$blocks$mod)) {

    log_debug("modifying block{?s} {names(upd$blocks$mod)}")

    for (blk_id in names(upd$blocks$mod)) {

      delta <- upd$blocks$mod[[blk_id]]

      if (length(delta)) {
        construct_block(blk_id, rv, edit_block, ctrl_block, edit_plugin_args,
                        vis)
        apply_block_mod_delta(blk_id, delta, rv)
      }
    }
  }

  if (length(upd$links$mod)) {

    cur_lnks <- board_links(rv$board)[names(upd$links$mod)]
    merged <- as_links(Map(update_link, cur_lnks, upd$links$mod))

    upd$links$add <- c(upd$links$add, merged)
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

    rv$board <- do.call(
      modify_board_links,
      c(
        list(rv$board, upd$links$add, upd$links$rm),
        dot_args,
        list(session = session)
      )
    )
  }

  if (length(upd$stacks$rm)) {
    log_debug("removing stack{?s} {names(upd$stacks$rm)}")
  }

  if (length(upd$stacks$add)) {
    log_debug("adding stack{?s} {names(upd$stacks$add)}")
  }

  if (length(upd$stacks$mod)) {

    log_debug("modifying stack{?s} {names(upd$stacks$mod)}")

    cur_stks <- board_stacks(rv$board)[names(upd$stacks$mod)]
    upd$stacks$mod <- as_stacks(
      Map(update_stack, cur_stks, upd$stacks$mod)
    )
  }

  update_stack_blocks(
    rv, upd$stacks, edit_stack, edit_plugin_args, session
  )

  rv$board <- do.call(
    modify_board_stacks,
    c(
      list(rv$board, upd$stacks$add, upd$stacks$rm, upd$stacks$mod),
      dot_args,
      list(session = session)
    )
  )

  if (length(upd$blocks$rm)) {

    log_debug("removing block{?s} {names(upd$blocks$rm)}")

    do.call(
      remove_block_ui,
      c(
        list(ns(NULL), rv$board, upd$blocks$rm),
        dot_args,
        list(
          edit_ui = edit_block,
          ctrl_ui = ctrl_block,
          session = session
        )
      )
    )

    destroy_rm_blocks(upd$blocks$rm, rv, session, dot_args)

    rm_vis_slots(vis, upd$blocks$rm)
  }

  invisible()
}

preprocess_board_update <- function(update, board) {

  upd <- update()
  augmented <- augment_board_update(upd, board)

  if (identical(augmented, upd)) {
    return(FALSE)
  }

  update(augmented)
  TRUE
}

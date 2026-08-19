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
#' @section Evaluation requests:
#' Deferred evaluation leaves a block that nothing currently needs holding its
#' last run — not only its result, but the conditions it reports. Anything that
#' can reach the [board_update()] channel can ask for such a block to be brought
#' up to date, without putting it on screen, through the `evaluate` and
#' `sustain` payload components. Both name blocks, and core joins them, together
#' with their upstream closure over [board_links()] (without which they cannot
#' produce a result), to the eval set. They differ only in who lets go: an
#' `evaluate` request is a one-off that core drops once the block has run, while
#' a `sustain` claim is held until its owner releases it.
#'
#' Claims are keyed by owner, the `sustain` component mapping each owner to a
#' delta over the blocks it holds, so several consumers may hold the same block
#' and none of them writes another's claim:
#'
#' ```r
#' update(
#'   list(
#'     sustain = set_names(
#'       list(list(set = board_block_ids(board$board))),
#'       session$ns("preview")
#'     )
#'   )
#' )
#' ```
#'
#' A delta is `set`, `add` and `rm`, of which `set` states that owner's entire
#' set at once and cannot be combined with the other two. Releasing everything
#' is `set = character()`; releasing part of a claim is `rm`, which — unlike
#' `set` and `add` — may name a block the board no longer has, so a release
#' cannot be rejected by a removal that raced it. Restating a set repairs a
#' release that never arrived, rather than letting it accumulate.
#'
#' Core cannot infer the owner — the write and its effect are separated by a
#' flush — so the label travels in the payload. Nothing keys off shiny's
#' namespacing, but taking the label from `session$ns()` as above is what keeps
#' owners unique without a registry, and lets one module hold two independent
#' claims under two labels. A claim outlives the module that made it: core
#' drops a claimed block once it leaves the board, but an owner that goes away
#' without releasing holds what it held for the rest of the session.
#'
#' Requests are orthogonal to the `required` visibility channel, so neither
#' competes with the front-end's gating, and nothing about what is on screen
#' changes. Because they carry no state change, they are also the one part of a
#' payload a locked board still accepts.
#'
#' Core drops a one-off request once the block has run — or has reported why it
#' cannot, such as an unconnected data input or a user input that was never set.
#' Requesting a block that is already in the eval set does nothing.
#'
#' @section Construction requests:
#' Evaluation implies construction, but not the reverse: a consumer that needs a
#' block merely *present* — the code export reads each block's expression and
#' none of their results — had to make it run as well. The `construct` payload
#' component asks for construction on its own. Like `evaluate` it is a bare
#' character vector of block IDs, and the blocks it names are built in
#' dependency order and left `dormant`:
#'
#' ```r
#' update(list(construct = board_block_ids(board$board)))
#' ```
#'
#' Nothing is retained. Once a block is built it stays built, so unlike the two
#' evaluation components there is no owner to name and nothing to hand back, and
#' asking for a block that is already built does nothing. The request joins
#' neither the eval set nor the front-end's `required` channel, so it cannot
#' turn a lazily evaluating board into an eagerly evaluating one.
#'
#' A block that the same payload adds, or that an `evaluate` or `sustain` names,
#' is already constructed — the add builds it directly, and evaluation demand
#' joins the needed set, which the background constructor builds. Pairing
#' `construct` with either is redundant rather than wrong. The component covers
#' what neither does: a block that must exist while nothing needs it evaluated.
#'
#' The named blocks are built in the flush that applies the payload, which is
#' the work `background_construction_delay` otherwise paces out. A caller that
#' wants that pacing sends several smaller payloads rather than one.
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
#' additional observers. Each receives a `visibility` list with three channels,
#' `required`, `visible` and `frozen`, each an environment of per-block
#' `reactiveVal`s (core keeps one per board block as blocks are added and
#' removed). Declare a block needed with `visibility$required[[id]](TRUE)` (or
#' `FALSE` for built but dormant) and report whether it is currently painted
#' with `visibility$visible[[id]](TRUE)` (or `FALSE` once built but off screen,
#' leaving `NA` until it is first built); the board reads both to gate
#' construction, evaluation and rendering. Set
#' `visibility$frozen[[id]](TRUE)` to freeze a block's inputs (for example when
#' its controls are hidden), so a forged input can no longer steer it. A
#' callback also receives the `update` channel (see [board_update]), through
#' which it can request block evaluation or construction (see the Evaluation
#' requests and Construction requests sections).
#'
#' Core's own front-end drives these channels through a callback like any
#' other: `gate_stacks()` reads the stack accordion (see [stack_ui()]) and is
#' the default, so a board that renders core's UI gates on its stacks and one
#' that does not simply passes its own callbacks and core tracks nothing. A
#' consumer that wants both keeps it in the list rather than replacing it --
#' `callbacks = list(gate_stacks(), my_callback)`.
#' @param callback_location Location of callback invocation (before or after
#' plugins)
#' @rdname board_server
#' @export
board_server.board <- function(id, x, plugins = board_plugins(x),
                               options = board_options(x),
                               callbacks = gate_stacks(),
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
        visible = new.env(parent = emptyenv()),
        frozen = new.env(parent = emptyenv())
      )

      add_vis_slots(vis, isolate(board_block_ids(rv$board)))

      rv_ro <- list(board = make_read_only(rv))

      rv$conditions <- reactive(
        combine_block_conditions(
          lapply(rv$blocks, function(blk) blk$server$conditions())
        )
      )

      rv$needed <- reactiveVal(TRUE)

      # Per-block `needed` slots: an env of reactiveVals, one per block, kept in
      # step with the whole-set rv$needed() below. A block's data-input and
      # eval-status reactives read ONLY their own slot (see block_needed()), not
      # the whole set, so a view switch that flips which LEAF blocks are needed
      # does not invalidate a shared upstream block whose needed status is
      # unchanged. Reading rv$needed() directly took a dependency on the entire
      # set, so any switch re-fired every block's inputs and re-evaluated the
      # whole shared pipeline (and everything downstream) even though no data
      # changed.
      rv$needed_slots <- new.env(parent = emptyenv())

      # The two request sets fed by the `evaluate` and `sustain` board update
      # components. Both join the needed set below; they differ in who lets go.
      # Core drops an `evaluating` entry once that block has had its evaluation
      # pass (see the observer below), while `claims` holds one entry per claim
      # owner until that owner releases it.
      rv$evaluating <- reactiveVal(character())
      rv$claims <- reactiveVal(list())

      observe(
        {
          cur <- if (!gating_active(vis$required)) {
            TRUE
          } else {
            upstream_blocks(
              union(required_now(vis$required), requested_blocks(rv)),
              rv$board
            )
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

          # Fan the set out to per-block slots, value-guarded so only blocks
          # whose membership actually flipped invalidate their readers.
          for (id in board_block_ids(rv$board)) {
            set_needed_slot(rv, id, isTRUE(cur) || id %in% cur)
          }
        }
      )

      observe(
        {
          pending <- rv$evaluating()

          if (!length(pending)) {
            return(invisible())
          }

          # Reading a block's status is what pulls its evaluation: the status
          # consults `failed()`, which reads the block result, and an input that
          # is not ready reads its upstream's status in turn, so the pull
          # cascades up the chain. A block that is not built yet, or not in the
          # eval set yet, stays pending -- either read invalidates this observer
          # once it changes.
          keep <- pending[lgl_ply(pending, eval_pending, rv)]

          if (length(keep) < length(pending)) {
            rv$evaluating(keep)
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
              # A lock stops the board being edited, not evaluated: a payload of
              # request components alone carries no state change and goes
              # through. One that carries any is dropped whole, rather than
              # applied in part.
              if (is_board_locked(rv$board) &&
                    !all(names(upd) %in% update_request_components())) {

                log_debug("rejecting board update on locked board")
                record_update_outcome(FALSE, "validate", "Board is locked.")
                board_update(NULL)

              } else {

                log_debug("starting board update")
                validate_board_update_structure(upd, rv$board)
                log_debug("board update validated")

                log_debug("preprocessing board update")
                if (!preprocess_board_update(board_update, rv$board)) {
                  log_debug("validating updated board")
                  validate_board_update_result(upd, rv$board)
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
        server_args = edit_plugin_args,
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
  needed <- reactive(block_needed(rv, id))

  srv <- do.call(
    block_server,
    c(
      list(paste0("block_", id), blk, rv$inputs[[id]], id, mod_ed, mod_ct),
      args,
      list(inputs_ready = inputs_ready, needed = needed, visibility = vis)
    )
  )

  rv$blocks[[id]] <- list(block = blk, server = srv)

  # Install the eval-status reactive WITHOUT the complex assignment
  # `rv$eval[[id]] <- ...`: that form desugars to a rebind of the `eval` key on
  # `rv`, which invalidates EVERY reactive that read `rv$eval` -- i.e. every
  # built block's inputs_ready (via input_ready()) -- so constructing one new
  # block re-fired inputs_ready -> data_valid -> dat_eval -> res for the whole
  # already-computed board and re-evaluated the shared upstream pipeline on
  # every first visit to a view. Mutating the reactiveValues through a local
  # binding keeps per-key granularity: only readers of THIS block's slot (its
  # direct downstreams, waking up on the upstream's construction) are notified.
  ev <- isolate(rv$eval)
  ev[[id]] <- reactive(block_eval_status(rv, id, inputs_ready, srv))

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

construct_blocks_in_background <- function(rv, mod_ed, mod_ct, args, vis) {

  session <- getDefaultReactiveDomain()
  pace <- reactiveVal(0L)

  started <- FALSE

  obs <- observe(
    {
      pace()

      if (!started) {

        started <<- TRUE

        if (!isolate(gating_active(vis$required))) {

          construct_blocks(board_block_ids(rv$board), rv, mod_ed, mod_ct,
                           args, vis)
          obs$destroy()

          return(invisible())
        }

        schedule_construction(pace, session)

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

        schedule_construction(pace, session)

        return(invisible())
      }

      if (gating_active(vis$required) && !required_fulfilled(vis)) {
        return(invisible())
      }

      isolate(construct_block(remaining[[1L]], rv, mod_ed, mod_ct, args, vis))

      schedule_construction(pace, session)
    }
  )

  invisible()
}

schedule_construction <- function(pace, session) {

  advance <- function() {
    if (!isTRUE(session$isClosed())) {
      withReactiveDomain(session, pace(isolate(pace()) + 1L))
    }
  }

  # Begin the pacing delay only once the block just built has flushed: an
  # in-flush invalidateLater() clock is consumed by that flush (which runs the
  # new block's reactive graph), leaving the event loop no idle window to
  # service user input between ticks. Re-arming via `onFlushed()` starts the
  # delay after the flush drains, so the window is genuine idle.
  onFlushed(
    function() later::later(advance, background_construction_delay() / 1000),
    once = TRUE,
    session = session
  )
}

background_construction_delay <- function() {
  as.numeric(blockr_option("background_construction_delay", 50L))
}

add_vis_slots <- function(vis, ids) {

  for (id in ids) {
    vis$required[[id]] <- reactiveVal(NA)
    vis$visible[[id]] <- reactiveVal(NA)
    vis$frozen[[id]] <- reactiveVal(FALSE)
  }

  invisible()
}

rm_vis_slots <- function(vis, ids) {

  gone <- intersect(ids, ls(vis$required))

  if (length(gone)) {
    rm(list = gone, envir = vis$required)
    rm(list = gone, envir = vis$visible)
    rm(list = gone, envir = vis$frozen)
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
  isTRUE(x)
}

block_visible <- function(id, vis) {
  is_visible(vis$visible[[id]]())
}

block_frozen <- function(id, vis) {
  isTRUE(vis$frozen[[id]]())
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
        "visible[[{id}]] must be TRUE, FALSE or NA",
        class = "invalid_visible"
      )
    }
  }

  for (id in ls(vis$frozen)) {
    if (!valid_frozen(vis$frozen[[id]]())) {
      blockr_abort(
        "frozen[[{id}]] must be TRUE or FALSE",
        class = "invalid_frozen"
      )
    }
  }

  invisible()
}

valid_required <- function(x) {
  is.logical(x) && length(x) == 1L
}

valid_visible <- function(x) {
  is.logical(x) && length(x) == 1L
}

valid_frozen <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

requested_blocks <- function(rv) {
  union(rv$evaluating(), unlst(rv$claims()))
}

# A block owes an evaluation pass while anything it needs for a result -- itself
# or an upstream -- is unbuilt or still out of the eval set.
eval_pending <- function(id, rv) {
  any(lgl_ply(upstream_blocks(id, rv$board), block_deferred, rv))
}

block_deferred <- function(id, rv) {

  status <- reval_if(rv$eval[[id]])

  is.null(status) || status %in% c("dormant", "stale")
}

id_request_components <- function() {
  c("evaluate", "construct")
}

update_request_components <- function() {
  c(id_request_components(), "sustain")
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
  # Fetch the container under isolate() so the caller does not depend on the
  # `eval` key of `rv` (rebound on every block construction before the local-
  # binding install in construct_block, and still guarded against here). The
  # `[[from]]` read happens OUTSIDE the isolate, so the per-key dependency
  # remains: a downstream still wakes when its upstream's status reactive is
  # installed at construction, and still tracks that status thereafter.
  not_null(from) && identical(reval_if(isolate(rv$eval)[[from]]), "ready")
}

block_eval_status <- function(rv, id, inputs_ready, srv) {

  if (!block_needed(rv, id)) {

    # A dormant block reports `stale` on its own verdict (`input_stale`): a
    # ready upstream's result no longer matches what it consumed, or a direct
    # upstream is itself `stale` -- so a change flows through the whole
    # downstream cone, one dependency hop at a time. Depending on the upstreams
    # is what wakes this status (and the badge) without re-evaluating the block.
    if (isTRUE(srv$input_stale())) {
      return("stale")
    }

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
  isTRUE(block_needed_slot(rv, id)())
}

# The per-block `needed` reactiveVal for `id`. Created on first read from the
# current whole-set value if the maintaining observer in board_server() has not
# populated it yet (a block read in the same flush it is constructed);
# thereafter that observer keeps it value-guarded. Reading THIS slot -- not
# rv$needed() -- is what confines a needed-set change to the blocks whose
# membership flipped, so a shared upstream block does not re-evaluate on a view
# switch that only swaps leaves.
#
# Both helpers fetch the container under isolate() and mutate it through a
# local binding: `rv$needed_slots[[id]] <- ...` would rebind the `needed_slots`
# key on `rv`, invalidating every reactive that touched the container (the same
# whole-container churn this file fixes for `rv$eval` in construct_block).
# Callers depend on the returned reactiveVal alone, never on the container; the
# environment mutates by reference so all readers share the slots.
block_needed_slot <- function(rv, id) {
  slots <- isolate(rv$needed_slots)
  slot <- slots[[id]]
  if (is.null(slot)) {
    n <- isolate(rv$needed())
    slot <- reactiveVal(isTRUE(n) || id %in% n)
    slots[[id]] <- slot
  }
  slot
}

set_needed_slot <- function(rv, id, val) {
  slots <- isolate(rv$needed_slots)
  slot <- slots[[id]]
  if (is.null(slot)) {
    slots[[id]] <- reactiveVal(val)
  } else if (!identical(isolate(slot()), val)) {
    slot(val)
  }
  invisible()
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

destroy_rm_blocks <- function(ids, rv, sess) {

  for (id in ids) {
    sess$destroy(paste0("block_", id))
    remove_block_from_stack(rv$board, id, rv$board_id, sess)
  }

  rv$inputs <- rv$inputs[!names(rv$inputs) %in% ids]
  rv$sources <- rv$sources[!names(rv$sources) %in% ids]
  rv$blocks <- rv$blocks[!names(rv$blocks) %in% ids]

  # Local bindings for both containers: `rv$eval[[id]] <- NULL` would rebind
  # the `eval` key on `rv` (see construct_block) and churn every reader.
  ev <- isolate(rv$eval)
  slots <- isolate(rv$needed_slots)

  for (id in ids) {
    ev[[id]] <- NULL
    if (!is.null(slots[[id]])) {
      rm(list = id, envir = slots)
    }
  }

  rv$evaluating(setdiff(isolate(rv$evaluating()), ids))
  rv$claims(
    filter_empty(lapply(isolate(rv$claims()), setdiff, ids))
  )

  invisible()
}

upstream_result <- function(key, src_rv, rv, to) {

  force(key)

  reactive(
    {
      # Gate on THIS block's per-block needed slot, not the whole rv$needed()
      # set, so a view switch that flips other blocks does not invalidate this
      # input reactive and force a re-evaluation of unchanged upstream data.
      req(block_needed(rv, to))

      from <- src_rv[[key]]

      if (is.null(from)) {
        return(NULL)
      }

      # Depend on the upstream's rv$eval slot -- installed when it is
      # constructed -- so a downstream whose input runs before its upstream
      # is registered re-resolves the server once that upstream appears,
      # rather than latching the NULL it first saw. Mirrors input_ready();
      # rv$blocks stays isolated to avoid a whole-container dependency that
      # would re-fire every input on every block's construction.
      rv$eval[[from]]

      srv <- isolate(rv$blocks[[from]])[["server"]]

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
    sess$destroy(paste0("stack_", id))
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
#' Inside [board_server()] every state change, and every request a
#' consumer makes of the board, flows through one `board_update`
#' reactive. Core registers two observers framing the
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
#' @section Request components:
#' Three components carry a request rather than a state change:
#' `evaluate`, a character vector of block IDs to evaluate once;
#' `sustain`, a list of per-owner deltas over the blocks that are to
#' stay evaluated; and `construct`, a character vector of block IDs to
#' build without evaluating. Each `sustain` delta is `set`, `add` and
#' `rm` — `set` states that owner's whole set and is exclusive with the
#' other two — so no owner writes another's claim. The two evaluation
#' components put the named blocks (and their upstream closure) into
#' the eval set while `construct` leaves them `dormant`, and none of
#' the three touches what the front-end shows — see the Evaluation
#' requests and Construction requests sections of [board_server()].
#' All three resolve their IDs against the post-update block set, so a
#' payload may add a block and ask for it in one go. A `sustain` `rm`
#' is the exception, naming blocks to release rather than to evaluate,
#' and so may name one the board no longer has. They are applied after
#' the state delta, so a payload that edits a block and evaluates it
#' sees the edit.
#'
#' The three are independent sets rather than alternatives: a payload
#' may name one block in several of them and core takes the union.
#' Overlap is redundant rather than rejected, which it has to be —
#' claims are per-owner, so a consumer asking for a block cannot know
#' that another owner already holds it.
#'
#' A locked board (see [is_board_locked()]) still accepts a payload of
#' request components alone; one that also carries a state change is
#' dropped whole rather than applied in part.
#'
#' @section Augment:
#' The default `.board` method inserts implied link removals and stack
#' updates that follow from block removals, plus link-input
#' completion. Subclass methods may extend the payload with their own
#' fixups; an error thrown here aborts the update before apply runs.
#'
#' @section Apply:
#' The default `.board` method applies the core delta to the supplied
#' board and returns it: added blocks are appended, link and stack
#' deltas are folded in through [modify_board_links()] /
#' [modify_board_stacks()], and removed blocks are dropped last (once
#' the earlier steps have freed them of every link and stack). Subclass
#' methods compose with `NextMethod()` to layer their own payload slots
#' on top of the core-updated board (blockr.dock, for instance,
#' cascades view membership), so a single [apply_board_update()] call
#' yields the full board for any subclass. The board handed in is a
#' plain `board` snapshot with no reactive surface; the returned
#' `board` is what the final observer assigns back to `rv$board`. The
#' reactive side effects that mirror the delta — block UI insertion /
#' removal, server construction / teardown, link and stack wiring — run
#' around this single reduce.
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
  validate_board_update_result(payload, board)

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
    validate_board_update_blocks(payload[["blocks"]], board)
  }

  if ("links" %in% names(payload)) {
    validate_board_update_links(payload[["links"]], board)
  }

  if ("stacks" %in% names(payload)) {
    validate_board_update_stacks(payload[["stacks"]], board)
  }

  # Every request component names blocks, and a payload may add the very block
  # it asks for, so they resolve against the post-update block set.
  if (any(update_request_components() %in% names(payload))) {

    ids <- updated_block_ids(payload, board)

    for (cmp in intersect(id_request_components(), names(payload))) {
      validate_block_id_request(payload[[cmp]], ids, cmp)
    }

    if ("sustain" %in% names(payload)) {
      validate_board_update_sustain(payload[["sustain"]], ids)
    }
  }

  invisible()
}

updated_block_ids <- function(payload, board) {

  ids <- board_block_ids(board)

  if (!"blocks" %in% names(payload)) {
    return(ids)
  }

  union(setdiff(ids, payload[["blocks"]]$rm), names(payload[["blocks"]]$add))
}

validate_block_id_request <- function(x, ids, cmp) {

  if (!is.character(x)) {
    blockr_abort(
      "Expecting a board update `{cmp}` component to be specified as a ",
      "character vector.",
      class = "board_update_request_type_invalid"
    )
  }

  unknown <- setdiff(x, ids)

  if (length(unknown)) {
    blockr_abort(
      "Cannot {cmp} unknown {qty(unknown)}block{?s} {unknown}.",
      class = "board_update_request_unknown_id"
    )
  }

  invisible()
}

validate_board_update_sustain <- function(x, ids) {

  if (!is.list(x) || length(names(x)) != length(x) ||
        !all(nzchar(names(x))) || anyDuplicated(names(x)) != 0L) {
    blockr_abort(
      "Expecting a board update `sustain` component to be specified as a list ",
      "of per-owner claim deltas with unique nonempty names.",
      class = "board_update_sustain_owners_invalid"
    )
  }

  for (owner in names(x)) {
    validate_claim_delta(x[[owner]], owner, ids)
  }

  invisible()
}

validate_claim_delta <- function(x, owner, ids) {

  exp_cmp <- c("set", "add", "rm")

  if (!is.list(x) || length(names(x)) != length(x) ||
        !all(names(x) %in% exp_cmp)) {
    blockr_abort(
      "Expecting the claim of owner {owner} to consist of components ",
      "{exp_cmp}.",
      class = "board_update_sustain_components_invalid"
    )
  }

  for (cmp in names(x)) {

    if (!(is.null(x[[cmp]]) || is.character(x[[cmp]]))) {
      blockr_abort(
        "Expecting the {cmp} component of the claim of owner {owner} to be ",
        "specified as a character vector (or NULL).",
        class = "board_update_sustain_component_invalid"
      )
    }
  }

  if ("set" %in% names(x) && any(c("add", "rm") %in% names(x))) {
    blockr_abort(
      "Expecting the claim of owner {owner} to state a whole set via `set` or ",
      "a delta via `add` and `rm`, but not both.",
      class = "board_update_sustain_set_delta_clash"
    )
  }

  both <- intersect(x$add, x$rm)

  if (length(both)) {
    blockr_abort(
      "Expecting the claim of owner {owner} to either add or remove ",
      "{qty(both)}block{?s} {both}.",
      class = "board_update_sustain_add_rm_clash"
    )
  }

  # Only a claim has to name blocks that exist -- a release commonly follows
  # the very removal that made it necessary.
  unknown <- setdiff(c(x$set, x$add), ids)

  if (length(unknown)) {
    blockr_abort(
      "Owner {owner} requested evaluation of unknown {qty(unknown)}",
      "block{?s} {unknown}.",
      class = "board_update_sustain_unknown_id"
    )
  }

  invisible()
}

has_comp <- function(comp, x) {
  comp %in% names(x) && length(x[[comp]])
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

merge_link_mods <- function(board, mod) {

  if (!length(mod)) {
    return(NULL)
  }

  as_links(Map(update_link, board_links(board)[names(mod)], mod))
}

merge_stack_mods <- function(board, mod) {

  if (!length(mod)) {
    return(NULL)
  }

  as_stacks(Map(update_stack, board_stacks(board)[names(mod)], mod))
}

validate_board_update_result <- function(payload, board) {

  validate_board(
    apply_board_update(board, augment_board_update(payload, board))
  )

  invisible()
}

validate_board_update_stacks <- function(x, board) {

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

  if ("blocks" %in% names(upd) && "rm" %in% names(upd[["blocks"]])) {

    rm <- upd[["blocks"]]$rm

    links <- board_links(board)

    mis_lnk <- setdiff(
      names(links[links_incident(links, rm)]),
      upd[["links"]]$rm
    )

    merged_stks <- board_stacks(board)

    if (length(upd[["stacks"]]$mod)) {
      merged_stks[names(upd[["stacks"]]$mod)] <- merge_stack_mods(
        board, upd[["stacks"]]$mod
      )
    }

    if (length(upd[["stacks"]]$rm)) {
      merged_stks <- merged_stks[
        setdiff(names(merged_stks), upd[["stacks"]]$rm)
      ]
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

  if ("links" %in% names(upd) && "add" %in% names(upd[["links"]])) {

    tmp <- complete_unary_inputs(upd[["links"]]$add, board_blocks(board))

    if (!identical(tmp$input, upd[["links"]]$add$input)) {
      add_lnk <- tmp
    }
  }

  if (length(mis_lnk)) {
    log_debug("adding link removal{?s} for {mis_lnk}")
    upd[["links"]]$rm <- c(mis_lnk, upd[["links"]]$rm)
  }

  if (length(upd_stk)) {
    log_debug("adding stack update{?s} for {names(upd_stk)}")
    upd[["stacks"]]$mod <- c(
      upd_stk,
      upd[["stacks"]]$mod[setdiff(names(upd[["stacks"]]$mod), names(upd_stk))]
    )
  }

  if (length(add_lnk)) {
    log_debug("adding link input update{?s} for {names(add_lnk)}")
    upd[["links"]]$add <- c(
      add_lnk,
      upd[["links"]]$add[setdiff(names(upd[["links"]]$add), names(add_lnk))]
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

  if (length(upd[["blocks"]]$add)) {
    board_blocks(board) <- c(board_blocks(board), upd[["blocks"]]$add)
  }

  add <- upd[["links"]]$add
  rm <- upd[["links"]]$rm

  if (length(upd[["links"]]$mod)) {
    add <- vec_c(add, merge_link_mods(board, upd[["links"]]$mod))
    rm <- c(rm, names(upd[["links"]]$mod))
  }

  board <- modify_board_links(board, add, rm, ..., session = session)

  board <- modify_board_stacks(
    board, upd[["stacks"]]$add, upd[["stacks"]]$rm,
    merge_stack_mods(board, upd[["stacks"]]$mod),
    ...,
    session = session
  )

  if (length(upd[["blocks"]]$rm)) {
    board <- rm_blocks(board, upd[["blocks"]]$rm, ..., session = session)
  }

  board
}

apply_core_board_update <- function(rv, upd, session,
                                    edit_block, ctrl_block, edit_stack,
                                    edit_plugin_args, vis,
                                    dot_args = list()) {

  ns <- session$ns

  lnk_add <- upd[["links"]]$add
  lnk_rm <- upd[["links"]]$rm

  if (length(upd[["links"]]$mod)) {
    lnk_add <- vec_c(lnk_add, merge_link_mods(rv$board, upd[["links"]]$mod))
    lnk_rm <- c(lnk_rm, names(upd[["links"]]$mod))
  }

  # Links into a block that is added or removed in this update are wired by
  # construct_block() / dropped by destroy_rm_blocks(); the reactive link delta
  # below only touches links between surviving blocks. Resolve it (and the stack
  # mod deltas) against the pre-update board before the reduce rewrites it.
  lifecycle_blocks <- c(names(upd[["blocks"]]$add), upd[["blocks"]]$rm)
  between_survivors <- function(x) {
    if (length(x)) x[!field(x, "to") %in% lifecycle_blocks] else x
  }

  cur_links <- board_links(rv$board)

  lnk_add <- between_survivors(lnk_add)
  lnk_rm <- between_survivors(cur_links[intersect(lnk_rm, names(cur_links))])

  stk <- upd[["stacks"]]

  if (length(stk$mod)) {
    stk$mod <- merge_stack_mods(rv$board, stk$mod)
  }

  # Tear down removed blocks before the reduce drops them from the board: the
  # remove_block_ui() method (e.g. blockr.dock's) asserts the block is still
  # present and reads it to locate the live panel it detaches.
  if (length(upd[["blocks"]]$rm)) {

    log_debug("removing block{?s} {upd[['blocks']]$rm}")

    do.call(
      remove_block_ui,
      c(
        list(ns(NULL), rv$board, upd[["blocks"]]$rm),
        dot_args,
        list(
          edit_ui = edit_block,
          ctrl_ui = ctrl_block,
          session = session
        )
      )
    )

    destroy_rm_blocks(upd[["blocks"]]$rm, rv, session)

    rm_vis_slots(vis, upd[["blocks"]]$rm)
  }

  rv$board <- do.call(
    apply_board_update,
    c(list(rv$board, upd), dot_args, list(session = session))
  )

  stopifnot(is_board(rv$board))

  if (length(upd[["blocks"]]$add)) {

    log_debug("adding block{?s} {names(upd[['blocks']]$add)}")

    add_vis_slots(vis, names(upd[["blocks"]]$add))

    do.call(
      insert_block_ui,
      c(
        list(ns(NULL), rv$board, upd[["blocks"]]$add),
        dot_args,
        list(
          edit_ui = edit_block,
          ctrl_ui = ctrl_block,
          session = session
        )
      )
    )

    construct_blocks(names(upd[["blocks"]]$add), rv, edit_block, ctrl_block,
                     edit_plugin_args, vis)
  }

  if (length(upd[["blocks"]]$mod)) {

    log_debug("modifying block{?s} {names(upd[['blocks']]$mod)}")

    for (blk_id in names(upd[["blocks"]]$mod)) {

      delta <- upd[["blocks"]]$mod[[blk_id]]

      if (length(delta)) {
        construct_block(blk_id, rv, edit_block, ctrl_block, edit_plugin_args,
                        vis)
        apply_block_mod_delta(blk_id, delta, rv)
      }
    }
  }

  if (length(lnk_add) || length(lnk_rm)) {

    if (length(lnk_add)) {
      log_debug("adding link{?s} {names(lnk_add)}")
    }

    if (length(lnk_rm)) {
      log_debug("removing link{?s} {names(lnk_rm)}")
    }

    update_block_links(rv, lnk_add, lnk_rm)
  }

  update_stack_blocks(rv, stk, edit_stack, edit_plugin_args, session)

  # Last, so that a payload which edits a block and asks for it in one go
  # evaluates the edit rather than what it replaced.
  construct_blocks(upd[["construct"]], rv, edit_block, ctrl_block,
                   edit_plugin_args, vis)

  apply_eval_requests(rv, upd)

  invisible()
}

apply_eval_requests <- function(rv, upd) {

  deltas <- upd[["sustain"]]

  if (length(deltas)) {

    log_debug("updating block claims of owner{?s} {names(deltas)}")

    claims <- isolate(rv$claims())

    for (owner in names(deltas)) {
      claims[[owner]] <- apply_claim_delta(claims[[owner]], deltas[[owner]])
    }

    rv$claims(filter_empty(claims))
  }

  if (length(upd[["evaluate"]])) {
    log_debug("requesting evaluation of block{?s} {upd[['evaluate']]}")
    rv$evaluating(union(isolate(rv$evaluating()), upd[["evaluate"]]))
  }

  invisible()
}

apply_claim_delta <- function(cur, delta) {

  if ("set" %in% names(delta)) {
    return(delta$set)
  }

  union(setdiff(cur, delta$rm), delta$add)
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

#' Block server
#'
#' A block is represented by several (nested) shiny modules and the top level
#' module is created using the `block_server()` generic. S3 dispatch is offered
#' as a way to add flexibility, but in most cases the default method for the
#' `block` class should suffice at top level. Further entry points for
#' customization are offered by the generics `expr_server()` and `block_eval()`,
#' which are responsible for initializing the block "expression" module (i.e.
#' the block server function passed in [new_block()]) and block evaluation
#' (evaluating the interpolated expression in the context of input data),
#' respectively.
#'
#' The module returned from `block_server()`, at least in the default
#' implementation, provides much of the essential but block-type agnostic
#' functionality, including data input validation (if available), instantiation
#' of the block expression server (handling the block-specific functionality,
#' i.e. block user inputs and expression), and instantiation of the
#' `edit_block` module (if passed from the parent scope).
#'
#' Each block carries an *eval status* -- one of `unevaluated`, `stale`,
#' `waiting`, `unset`, `failed` or `ready` -- which, together with its
#' orthogonal front-end visibility, determines its behavior. The status is what
#' the block's last *check* found, a check being a run of the block or the
#' finding that it cannot run. A block that is *needed* -- held eager, or
#' feeding a block that is, as described below -- is checked afresh whenever its
#' status or result is read. One that is not, a *parked* block, keeps its inputs
#' unfulfilled ([shiny::req()] out) and evaluates nothing: it reports what its
#' last check found, and its result is the one that check left, for as long as
#' nothing the check read has changed. Needed or parked, a block that is current
#' reads the same. Four statuses are what a check can find, separating the two
#' input kinds (data inputs from links, user inputs from `state`) and a genuine
#' failure:
#' * `waiting` -- a required *data* input is missing: unconnected, below the
#'   required number of variadic `...args` inputs (one by default), or fed by an
#'   upstream block that is not itself `ready` (see `allow_empty_state`).
#' * `unset` -- data inputs are ready, but a required *user* input (`state`
#'   value) has not been provided (unless permitted by `allow_empty_state`).
#' * `failed` -- all inputs are present, but the block cannot produce a result:
#'   the data validator ([validate_data_inputs()]) or the block expression
#'   raised. The offending condition is surfaced through the block conditions.
#' * `ready` -- evaluation succeeded and a result (possibly a legitimate `NULL`)
#'   is available for downstream blocks to consume.
#'
#' A parked block reads one of the other two when it has no current check to
#' report on:
#' * `stale` -- something the last check read has changed since: the block's
#'   expression or eval trigger, which blocks feed its data inputs, or what one
#'   of those holds. An upstream that is itself `stale` or `unevaluated` counts
#'   as changed, so a change reaches the whole downstream cone. The block is not
#'   re-evaluated; the status only reports that what its last check found is
#'   out of date, so a front-end can flag it (e.g. a muted node badge) without
#'   forcing a recompute. An expression built from the input data cannot be
#'   rebuilt while those are withheld, so for such a block the `state` it is
#'   built from is compared instead.
#' * `unevaluated` -- the block has never been checked, which includes a board
#'   block that is not built yet.
#'
#' A consumer that needs a parked block current asks for it with a
#' [board_update()] `evaluate` request. Once none of the blocks it asked for
#' reads `stale` or `unevaluated`, what they report is current.
#'
#' A block reaches `ready` only once its upstreams have, so an unconnected or
#' pending block holds its whole downstream chain `waiting` without any of them
#' evaluating against missing data. Output rendering follows the status: the
#' block output is shown only while `ready` and cleared otherwise, so a block
#' leaving `ready` never displays a stale result. While not `ready` the block
#' surfaces a condition explaining why -- a `status`-phase note for `waiting`
#' and `unset`, or the raised error for `failed`. The note is recorded by the
#' check that finds the block unable to run, so a block checked off screen
#' carries it too, and the check that runs the block clears it. The default
#' [notify_user()] plugin toasts it only for a block on screen. Conditions
#' raised during validation and evaluation are caught and returned to be
#' surfaced to the app user.
#'
#' Block-level user inputs (provided by the expression module) are separated
#' from output, the behavior of which can be customized via the
#' [block_output()] generic. The [block_ui()] generic can then be used to
#' control rendering of outputs.
#'
#' A board is eager by default: every block is needed, so every block
#' evaluates. A front-end (such as blockr.dock) makes it lazy by returning
#' [eager()] from the callback it registers with [board_server()], naming its
#' owner label and the blocks it needs evaluated from the start. From then on
#' only the blocks some owner holds eager are needed, together with what feeds
#' them. A board whose callbacks return no such value stays eager, and setting
#' the `gate_visibility` [blockr_option()] (default `TRUE`) to `FALSE` keeps
#' every board eager. Core reads the declaration as it runs the callbacks and
#' seeds the opening eager set there and then, before the first flush decides
#' what to construct -- which no board update could do, since a payload only
#' applies at the end of the flush it is written in.
#'
#' Which blocks the front-end needs evaluated from then on is not a channel of
#' its own: it travels as an `eager` component under that same owner label,
#' leaving the front-end one owner among several rather than a special case
#' core can distinguish from a code export or an extension (see the Evaluation
#' requests section of [board_server()]).
#'
#' Evaluation follows the *needed* set, the blocks held eager together with
#' their upstream closure over [board_links()] (recomputed only when eager sets
#' or links change). A block's input data reactives stay unfulfilled (they
#' [shiny::req()] out) unless the block is needed, so a block that is neither
#' held eager nor feeding one pulls no input and stays fully quiescent: its
#' result reactive hands back what the last check left, and any observer its
#' expression server registers on the incoming data short-circuits and does
#' nothing. A needed but off-screen block (one feeding a block held eager)
#' evaluates but does not render.
#'
#' Rendering follows `visible`, the per-block channel through which the
#' front-end reports what it has painted -- the effect, where holding a block
#' eager is the cause. The render observer is suspended while a block carries
#' no visible slot and resumed once the front-end reports it painted, starting
#' suspended so nothing renders before the first report.
#'
#' Block-server *construction* is prioritized the same way: the needed set is
#' instantiated first so that first paint waits only for the blocks held eager
#' and their upstreams, and the remaining block servers are built progressively
#' in the background. That background pass holds until the front-end reports
#' every block it holds eager as visible, so it never competes with first paint.
#' Until a block is built it is absent from the `board$blocks` handed to plugins
#' and callbacks, which simply see it appear once constructed. The background
#' cadence is set by the `background_construction_delay` [blockr_option()]
#' (milliseconds between successive blocks, default 50); a value of 0 disables
#' the staggering and builds every block up front.
#'
#' Core's own board UI drives those channels through a callback, on the same
#' footing as a front-end rather than built into the board server. Stacks
#' render as a [bslib::accordion()] which opens one stack and collapses the
#' rest (see [stack_ui()]), so on a stacked board part of what is on screen is
#' hidden from the first render and any stack can be collapsed afterwards.
#' The `gate_stacks()` callback reads that accordion back, holding the blocks
#' of every open stack plus every unstacked block eager and parking the rest,
#' so collapsing a stack stops its blocks evaluating and expanding one starts
#' them again. It is [board_server()]'s default `callbacks` value. Which stacks
#' render open is core's own decision (see [stack_ui()]), so on a stacked board
#' the callback returns that set as its opening eager set: an eager board
#' evaluates every block, and a collapsed stack's blocks would otherwise
#' evaluate once before the accordion reports. The accordion's report then
#' refines the set rather than establishing it. A board with no stacks binds no
#' such input and has nothing to park, so this callback leaves it eager; a
#' board driven by another front-end never runs it, since it passes its own
#' callbacks. Setting the `gate_visibility` option to `FALSE` keeps this board
#' eager too.
#'
#' The same bundle carries a third channel, `frozen`, through which a
#' front-end reports the blocks whose inputs it has hidden (for example a
#' locked board that shows outputs but not controls). While frozen a block is
#' read-only: its expression, state readiness and the state it exposes for
#' serialization are held at the values last seen while editable, and the input
#' trigger is dropped, so a forged client input (which still fires the block's
#' own observer) reaches neither the expression, the block's status, a
#' re-evaluation, nor a save. Externally controllable inputs (see
#' [external_ctrl_vars()]) are held too -- a high-priority observer reverts any
#' write while frozen -- so not even the programmatic control channel can drive
#' a frozen block. Upstream data still flows through, and unfreezing resumes
#' normal input handling.
#'
#' @param id Namespace ID
#' @param x Object for which to generate a [shiny::moduleServer()]
#' @param data Input data (list of reactives)
#' @param ... Generic consistency
#'
#' @return Both `block_server()` and `expr_server()` return shiny server module
#' (i.e. a call to [shiny::moduleServer()]), while `block_eval()` evaluates
#' an interpolated (w.r.t. block "user" inputs) block expression in the context
#' of block data inputs.
#'
#' @export
block_server <- function(id, x, data = list(), ...) {
  UseMethod("block_server", x)
}

#' @param block_id Block ID
#' @param edit_block,ctrl_block Block plugins
#' @param board Reactive values object containing board information
#' @param update Reactive value object to initiate board updates
#' @param inputs_ready Reactive flag signaling whether the block's required
#' inputs are all connected to ready upstream blocks (supplied by
#' [board_server()]; defaults to always-ready when a block server is run
#' standalone)
#' @param needed Reactive flag signaling whether the block is currently in the
#' eval set (supplied by [board_server()]; defaults to always-needed when a
#' block server is run standalone)
#' @param visibility Front-end channel bundle -- a `gate` `reactiveVal` holding
#' the owner label of the front-end that made the board lazy, plus `visible`
#' and `frozen`, each an environment of per-block `reactiveVal`s, supplied by
#' [board_server()] to hold rendering until a block is painted and to freeze
#' block inputs; `NULL` (the standalone default) renders the block as soon as
#' it is ready
#' @rdname block_server
#' @export
block_server.block <- function(id, x, data = list(), block_id = id,
                               edit_block = NULL, ctrl_block = NULL,
                               board = reactiveValues(),
                               update = reactiveVal(),
                               inputs_ready = reactive(TRUE),
                               needed = reactive(TRUE),
                               visibility = NULL, ...) {

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      cond <- reactiveValues(
        data = NULL,
        state = NULL,
        eval = NULL,
        render = NULL,
        block = NULL,
        status = NULL
      )

      exp <- check_expr_val(
        expr_server(x, data),
        x
      )

      frozen <- reactive(
        not_null(visibility) && block_frozen(block_id, visibility)
      )

      lang <- freeze_reactive(
        reactive(exprs_to_lang(exp$expr())),
        frozen,
        session
      )

      state <- exp$state

      exposed_state <- if (not_null(visibility)) {
        freeze_exposed_state(state, x, frozen, session)
      } else {
        state
      }

      dat <- reactive(
        {
          res <- lapply(data[names(data) != "...args"], reval)

          if ("...args" %in% names(data)) {
            res <- c(res, list(`...args` = dot_arg_values(data[["...args"]])))
          }

          res
        }
      )

      data_valid <- validate_block_reactive(block_id, x, dat, cond, session,
                                            inputs_ready)

      state_ready <- freeze_reactive(
        state_ready_reactive(block_id, x, state, session),
        frozen,
        session
      )

      dat_eval <- reactive(
        {
          req(isTRUE(data_valid()), isTRUE(state_ready()))

          if (!frozen()) {
            lapply(state, reval_if)
          }

          try(lang(), silent = TRUE)

          res <- dat()

          if ("...args" %in% names(res)) {
            res <- c(res[names(res) != "...args"], res[["...args"]])
          }

          block_eval_trigger(x, session)

          res
        },
        domain = session
      )

      cur_name <- reactiveVal(block_name(x))

      if (is_board(isolate(board$board))) {

        reg_name <- reactive(
          {
            blk <- board_blocks(board$board)[[block_id]]
            if (is_block(blk)) block_name(blk) else NULL
          }
        )

        observeEvent(
          cur_name(),
          {
            new_name <- cur_name()
            if (!identical(reg_name(), new_name)) {
              update(
                list(
                  blocks = list(
                    mod = set_names(
                      list(list(block_name = new_name)),
                      block_id
                    )
                  )
                )
              )
            }
          },
          ignoreInit = TRUE
        )

        observeEvent(
          reg_name(),
          {
            if (!identical(cur_name(), reg_name())) {
              cur_name(reg_name())
            }
          },
          ignoreInit = TRUE
        )
      }

      ctrl_vars <- c(
        state[setdiff(external_ctrl_vars(x), "block_name")],
        list(block_name = cur_name)
      )

      if (!all(lgl_ply(ctrl_vars, inherits, "reactiveVal"))) {
        blockr_abort(
          "All externally controllable variables for {class(x)[1L]} are ",
          "expected to inherit from `reactiveVal`.",
          class = "unsupported_external_ctrl_variable"
        )
      }

      cb_res <- coal(
        call_plugin_server(
          ctrl_block,
          list(
            x = x,
            vars = ctrl_vars,
            data = dat_eval,
            eval = reactive(eval_impl(x, lang(), dat_eval()))
          )
        ),
        TRUE
      )

      gate <- cb_res

      # Last successful evaluation, for the unchanged-inputs skip below.
      last_eval <- new.env(parent = emptyenv())

      # The last check of the block while needed, which either ran it or found
      # that it cannot run. It keeps what the check read -- expression, state
      # and eval trigger, which block feeds each data input and what each of
      # those held -- and what it found: the status it reached and the result
      # it left, both of which the block holds once it leaves the eval set.
      last_check <- new.env(parent = emptyenv())

      record_check <- function(status, result, lang, trigger, reason = NULL) {

        sources <- isolate(block_sources(block_id, board))

        last_check$status <- status
        last_check$lang <- lang
        last_check$state <- isolate(lapply(state, state_value))
        last_check$trigger <- trigger
        last_check$sources <- sources
        last_check$consumed <- lapply(sources, upstream_last_result, board)
        last_check$result <- result

        isolate(explain_block_status(cond, reason))

        result
      }

      # Finding that the block cannot run is a check as much as running it. The
      # expression and trigger are read as a run reads them, so an edit
      # re-checks the block rather than leaving it to read `stale` once it
      # leaves the eval set.
      record_blocked <- function(status, reason = NULL) {
        record_check(
          status,
          NULL,
          tryCatch(lang(), error = identity),
          block_eval_trigger(x, session),
          reason
        )
      }

      eval_status <- function() {
        if (length(isolate(cond$eval$error))) "failed" else "ready"
      }

      res <- reactive(
        {
          # The needed set otherwise reaches a block only through its data
          # reads (see upstream_result()), which leaves one with no data inputs
          # ungated: any reader of its result -- the card summary, say -- would
          # evaluate it out of the eval set. It is handed what the last check
          # left instead, as the reader of any parked block is.
          if (!isTRUE(needed())) {
            return(last_check$result)
          }

          # State goes ahead of validation, which never runs on unset user
          # inputs.
          if (!inputs_ready()) {
            return(
              record_blocked(
                "waiting",
                "This block is waiting for its data input to be connected."
              )
            )
          }

          if (!isTRUE(state_ready())) {
            return(
              record_blocked(
                "unset",
                "This block is waiting for its inputs to be set."
              )
            )
          }

          if (!isTRUE(data_valid())) {
            return(record_blocked("failed"))
          }

          if (!isTRUE(reval_if(gate))) {
            return(record_blocked(eval_status()))
          }

          eval_data <- dat_eval()
          eval_lang <- isolate(lang())

          # The eval trigger's VALUE joins the skip key below: a block can
          # request re-evaluation with unchanged (expr, data) by returning a
          # changed value from block_eval_trigger() -- plot blocks return the
          # thematic / dark_mode option values so a theme flip re-renders the
          # plot. The reactive dependency is registered inside dat_eval();
          # here only the current value is read.
          eval_trigger <- isolate(block_eval_trigger(x, session))

          # Re-evaluate only when the expression, input data or eval trigger
          # changed. Board-wide transitions invalidate this reactive with the
          # inputs untouched -- e.g. a view switch lands its visibility across
          # several flushes, so every needed slot takes a spurious FALSE -> TRUE
          # round trip and the input chain re-pulls the same cached objects.
          # Expression and data are compared by object identity: an unchanged
          # reactive hands back its cached object, so this is O(1) whatever the
          # size and side-steps identical()'s environment-sensitive walk of
          # language objects. The trade is that a recomputed-but-equal, freshly
          # allocated input re-evaluates rather than skipping. The eval trigger
          # stays value-compared -- it is rebuilt on each read (see below).
          if (isTRUE(last_eval$has) &&
                same_ref(eval_lang, last_eval$lang) &&
                same_refs(eval_data, last_eval$data) &&
                identical(eval_trigger, last_eval$trigger)) {
            log_debug("skipping block ", block_id, " (inputs unchanged)")
            return(
              record_check(
                eval_status(),
                last_eval$result,
                eval_lang,
                eval_trigger
              )
            )
          }

          log_debug("evaluating block ", block_id)

          result <- isolate(
            capture_conditions(
              eval_impl(x, eval_lang, eval_data),
              cond,
              "eval",
              session = session
            )
          )

          last_eval$has <- TRUE
          # Keep the objects themselves (not just addresses) so they stay alive
          # and their addresses cannot be reused by a later allocation. The
          # same holds for everything `last_check` keeps.
          last_eval$lang <- eval_lang
          last_eval$data <- eval_data
          last_eval$trigger <- eval_trigger
          last_eval$result <- result

          record_check(eval_status(), result, eval_lang, eval_trigger)
        },
        domain = session
      )

      # An expression built from input data cannot be rebuilt while the block
      # is out of the eval set, as its inputs req() out. The state it is built
      # from is where an edit lands, so that is compared instead.
      own_changed <- function() {

        if (!identical(block_eval_trigger(x, session), last_check$trigger)) {
          return(TRUE)
        }

        cur <- tryCatch(lang(), error = identity)

        if (!inherits(cur, "shiny.silent.error")) {
          return(!same_ref(cur, last_check$lang))
        }

        cur <- lapply(state, state_value)
        withheld <- lgl_ply(cur, inherits, "shiny.silent.error")

        !same_refs(cur[!withheld], last_check$state[!withheld])
      }

      # An upstream counts as changed when it no longer holds what this block
      # consumed from it, or is itself `stale` or `unevaluated`, which carries a
      # change down the whole cone one hop at a time.
      inputs_changed <- function() {

        sources <- block_sources(block_id, board)

        if (!identical(sources, last_check$sources)) {
          return(TRUE)
        }

        consumed <- last_check$consumed

        for (i in seq_along(sources)) {
          if (upstream_changed(sources[[i]], consumed[[i]], board)) {
            return(TRUE)
          }
        }

        FALSE
      }

      # A needed block is checked afresh as its status is read, since reading
      # its result runs it or records why it cannot. A parked block reports on
      # its last check instead, until anything that check read has changed. The
      # dependency on needed() is also what makes a fresh verdict due:
      # `last_check` is a plain environment, so the check that refreshes it
      # invalidates nothing, and dropping back out of the eval set is exactly
      # when the comparison has to be redone.
      status <- reactive(
        {
          if (isTRUE(needed())) {
            res()
            return(last_check$status)
          }

          if (is.null(last_check$status)) {
            return("unevaluated")
          }

          if (own_changed() || inputs_changed()) {
            return("stale")
          }

          last_check$status
        },
        domain = session
      )

      block_ready <- reactive(
        isTRUE(reval_if(gate)) && identical(status(), "ready"),
        domain = session
      )

      gated <- is_board(isolate(board$board)) &&
        isTRUE(blockr_option("gate_visibility", TRUE)) &&
        not_null(visibility)

      render_obs <- output_render_observer(x, block_ready, res, cond, session,
                                           suspended = gated)

      if (gated) {
        render_gate_observer(block_id, visibility, render_obs, session)
      }

      eb_res <- call_plugin_server(
        edit_block,
        server_args = c(
          list(block_id = block_id, board = board, update = update),
          dot_args
        )
      )

      if ("cond" %in% names(exp)) {

        blk_cnd <- reactive(
          {
            include <- coal(
              get_board_option_or_null("show_conditions", session),
              match.arg(
                blockr_option("show_conditions", c("warning", "error")),
                c("message", "warning", "error"),
                several.ok = TRUE
              )
            )
            res <- reactiveValuesToList(exp[["cond"]])[include]
            set_names(lapply(res, coal, list()), include)
          }
        )

        block_cond_observer(blk_cnd, cond, session)
      }

      conditions <- reactive(
        blk_cnds(reactiveValuesToList(cond), block_id)
      )

      c(
        list(
          result = res,
          last_result = function() last_check$result,
          status = status,
          state_ready = state_ready,
          expr = lang,
          state = exposed_state,
          conditions = conditions
        ),
        eb_res
      )
    }
  )
}

#' @rdname block_server
#' @export
expr_server <- function(x, data, ...) {
  UseMethod("expr_server")
}

#' @export
expr_server.block <- function(x, data, ...) {
  do.call(block_expr_server(x), c(list(id = "expr"), data))
}

validate_block_reactive <- function(id, x, dat, cond, sess, inputs_ready) {

  has_validator <- block_has_data_validator(x)

  reactive(
    {
      if (!inputs_ready()) {

        isolate(clear_data_conditions(cond))

        return(FALSE)
      }

      if (!has_validator) {

        isolate(clear_data_conditions(cond))

        return(TRUE)
      }

      log_debug("performing input validation for block ", id)

      isolate(
        capture_conditions(
          {
            validate_data_inputs(x, dat())
            TRUE
          },
          cond,
          "data",
          session = sess
        )
      )
    },
    domain = sess
  )
}

clear_data_conditions <- function(cond) {

  if (any(lengths(cond$data))) {
    cond$data <- empty_block_condition()
  }
}

state_ready_reactive <- function(id, x, state, sess) {

  reactive(
    {
      log_debug("checking returned state values of block ", id)

      allow_empty <- block_allow_empty_state(x)

      if (isTRUE(allow_empty) || !length(state)) {
        return(TRUE)
      }

      check <- if (isFALSE(allow_empty)) {
        TRUE
      } else {
        setdiff(names(state), allow_empty)
      }

      all(lgl_ply(lapply(state[check], reval_if), Negate(is_empty)))
    },
    domain = sess
  )
}

same_ref <- function(x, y) {
  identical(rlang::obj_address(x), rlang::obj_address(y))
}

same_refs <- function(x, y) {
  identical(names(x), names(y)) &&
    identical(chr_ply(x, rlang::obj_address), chr_ply(y, rlang::obj_address))
}

state_value <- function(x) {
  tryCatch(reval_if(x), error = identity)
}

block_sources <- function(id, rv) {

  srcs <- isolate(rv$sources[[id]])

  if (is.null(srcs)) {
    return(NULL)
  }

  unlst(reactiveValuesToList(srcs), use_names = TRUE)
}

# Reading the status first is what brings a needed upstream up to date, so the
# result read after it is what its latest check left.
upstream_changed <- function(from, consumed, rv) {

  status <- reval_if(rv$eval[[from]])

  if (isTRUE(status %in% c("stale", "unevaluated"))) {
    return(TRUE)
  }

  !same_ref(upstream_last_result(from, rv), consumed)
}

upstream_last_result <- function(from, rv) {

  srv <- isolate(rv$blocks[[from]])[["server"]]

  if (is.null(srv)) NULL else srv$last_result()
}

eval_impl <- function(x, expr, dat) {

  if (identical(block_expr_type(x), "bquoted")) {
    expr <- do.call(
      bquote,
      list(
        expr,
        lapply(set_names(nm = names(dat)), as.name),
        splice = is.na(block_arity(x))
      )
    )
  }

  block_eval(x, expr, eval_env(dat))
}

#' @rdname block_server
#' @export
block_render_trigger <- function(x, session = get_session()) {
  UseMethod("block_render_trigger", x)
}

#' @export
block_render_trigger.block <- function(x, session = get_session()) {
  NULL
}

output_render_observer <- function(x, ready, res, cond, sess,
                                   suspended = FALSE) {

  observe(
    {
      block_render_trigger(x, sess)

      if (isTRUE(ready())) {
        sess$output$result <- capture_conditions(
          block_output(x, res(), sess),
          cond,
          "render",
          session = sess
        )
      } else {
        sess$output$result <- NULL
      }
    },
    domain = sess,
    suspended = suspended
  )
}

explain_block_status <- function(cond, reason) {

  new <- empty_block_condition()

  if (not_null(reason)) {
    new[["warning"]] <- list(new_blk_cnd(reason))
  }

  if (!identical(cond$status, new)) {
    cond$status <- new
  }
}

render_gate_observer <- function(id, visibility, render_obs, sess) {

  prev_render <- NA

  observe(
    {
      do_render <- !gating_active(visibility) ||
        block_visible(id, visibility)

      if (do_render) render_obs$resume() else render_obs$suspend()

      if (!identical(do_render, prev_render)) {
        prev_render <<- do_render
        log_debug(
          "block {id} rendering ",
          "{if (do_render) 'resumed' else 'suspended'}"
        )
      }
    },
    domain = sess
  )
}

freeze_reactive <- function(live, frozen, sess) {

  pin <- reactiveVal(tryCatch(isolate(live()), error = function(e) NULL))

  reactive(
    {
      if (frozen()) {
        return(isolate(pin()))
      }

      cur <- live()
      isolate(pin(cur))

      cur
    },
    domain = sess
  )
}

freeze_exposed_state <- function(state, x, frozen, sess) {

  fn_names <- names(state)[lgl_ply(state, is.function)]

  if (!length(fn_names)) {
    return(state)
  }

  ctrl <- intersect(fn_names, external_ctrl_vars(x))
  readonly <- setdiff(fn_names, ctrl)

  live <- state[fn_names]

  snapshot <- reactiveVal(NULL)

  observeEvent(
    frozen(),
    if (isTRUE(frozen())) {
      snapshot(lapply(live, reval_if))
    },
    domain = sess
  )

  if (length(ctrl)) {
    hold_ctrl_state(live[ctrl], frozen, snapshot, sess)
  }

  for (nm in readonly) {
    state[[nm]] <- freeze_readonly_field(live[[nm]], nm, frozen, snapshot, sess)
  }

  state
}

freeze_readonly_field <- function(live, nm, frozen, snapshot, sess) {

  # `live` and `nm` MUST be forced here. The caller passes them from a
  # `for (nm in readonly)` loop, and reactive() defers its body: without these
  # forces the promises are only resolved on the first read, in the caller's
  # frame, where the loop has long since finished -- so every field would bind
  # to the LAST element of `readonly`. That aliased all of a block's state
  # fields onto one value, which is what serialize_board() then wrote out.
  force(live)
  force(nm)

  reactive(
    if (frozen()) snapshot()[[nm]] else reval_if(live),
    domain = sess
  )
}

hold_ctrl_state <- function(rvs, frozen, snapshot, sess) {

  revert <- observe(
    {
      snap <- snapshot()

      if (not_null(snap)) {
        for (nm in names(rvs)) {
          if (!identical(rvs[[nm]](), snap[[nm]])) {
            rvs[[nm]](snap[[nm]])
          }
        }
      }
    },
    priority = Inf,
    suspended = TRUE,
    domain = sess
  )

  observe(
    if (frozen()) revert$resume() else revert$suspend(),
    domain = sess
  )

  invisible()
}

block_cond_observer <- function(blk, cond, sess) {

  observeEvent(
    blk(),
    {
      new_cnds <- blk()
      cur_cnds <- set_names(
        coal(cond$block, empty_block_condition())[names(new_cnds)],
        names(new_cnds)
      )

      if (any(lengths(new_cnds))) {

        new_cnds <- lapply(new_cnds, lapply, as_blk_cnd)

        chk <- lgl_mply(
          Negate(setequal),
          lapply(new_cnds, chr_ply, attr, "id"),
          lapply(cur_cnds, chr_ply, attr, "id")
        )

        if (any(chk)) {
          cond$block <- new_cnds
        }

      } else if (any(lengths(cur_cnds) > 0L)) {
        cond$block <- empty_block_condition()[names(new_cnds)]
      }
    }
  )
}

check_expr_val <- function(val, x) {

  if (!is.list(val)) {
    blockr_abort(
      "The block server for {class(x)[1L]} is expected to return a list.",
      class = "expr_server_return_type_invalid"
    )
  }

  required <- c("expr", "state")

  if (!all(required %in% names(val))) {
    blockr_abort(
      "The block server for {class(x)[1L]} is expected to return values ",
      "{setdiff(required, names(val))}.",
      class = "expr_server_return_required_component_missing"
    )
  }

  if (!is.reactive(val[["expr"]])) {
    blockr_abort(
      "The `expr` component of the return value for {class(x)[1L]} is ",
      "expected to be a reactive.",
      class = "expr_server_return_expr_invalid"
    )
  }

  if (!is.list(val[["state"]])) {
    blockr_abort(
      "The `state` component of the return value for {class(x)[1L]} is ",
      "expected to be a list.",
      class = "expr_server_return_state_type_invalid"
    )
  }

  expected <- block_ctor_inputs(x)
  current <- names(val[["state"]])
  missing <- setdiff(expected, current)

  if (length(missing)) {
    blockr_abort(
      "The `state` component of the return value for {class(x)[1L]} is ",
      "expected to additionally return {missing}.",
      class = "expr_server_return_state_missing_component"
    )
  }

  disallowed <- intersect(current, static_block_arguments())

  if (length(disallowed)) {
    blockr_abort(
      "The `state` component of the return value for {class(x)[1L]} is ",
      "is not allowed to return components {disallowed}.",
      class = "expr_server_return_state_invalid_component"
    )
  }

  if ("cond" %in% names(val)) {

    if (!is.reactivevalues(val[["cond"]])) {
      blockr_abort(
        "The `cond` component of the return value for {class(x)[1L]} is ",
        "expected to be a `reactiveValues` object.",
        class = "expr_server_return_cond_invalid"
      )
    }

    conds <- c("message", "warning", "error")

    if (!all(names(val[["cond"]]) %in% conds)) {
      blockr_abort(
        "The `cond` component of the return value for {class(x)[1L]} ",
        "is expected to contain any of components {conds}.",
        class = "expr_server_return_cond_invalid"
      )
    }

    lapply(
      conds,
      function(cnd, cnds) {
        observeEvent(
          req(length(cnds[[cnd]]) > 0L),
          {
            for (y in cnds[[cnd]]) {
              if (!(is.character(y) || is_list_of_blk_cnds(y))) {
                blockr_abort(
                  "The `cond` component of the return value for ",
                  "{class(x)[1L]} is expected to contain a nested list of ",
                  "character vectors or list of objects inheriting from ",
                  "`blk_cnd`.",
                  class = "expr_server_return_cond_invalid"
                )
              }
            }
          },
          once = TRUE
        )
      },
      val[["cond"]]
    )
  }

  val
}

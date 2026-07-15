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
#' Each block carries an *eval status* -- one of `dormant`, `waiting`, `unset`,
#' `failed` or `ready` -- which, together with its orthogonal front-end
#' visibility, determines its behaviour. The status separates the two input
#' kinds (data
#' inputs from links, user inputs from `state`) and a genuine failure:
#' * `dormant` -- not *needed* (neither on screen nor feeding, transitively over
#'   [board_links()], an on-screen block); inputs stay unfulfilled
#'   ([shiny::req()] out) and nothing evaluates.
#' * `waiting` -- needed, but a required *data* input is missing: unconnected,
#'   below the required number of variadic `...args` inputs (one by default),
#'   or fed by an upstream block that is not itself `ready` (see
#'   `allow_empty_state`).
#' * `unset` -- data inputs are ready, but a required *user* input (`state`
#'   value) has not been provided (unless permitted by `allow_empty_state`).
#' * `failed` -- all inputs are present, but the block cannot produce a result:
#'   the data validator ([validate_data_inputs()]) or the block expression
#'   raised. The offending condition is surfaced through the block conditions.
#' * `ready` -- evaluation succeeded and a result (possibly a legitimate `NULL`)
#'   is available for downstream blocks to consume.
#'
#' A block reaches `ready` only once its upstreams have, so an unconnected or
#' pending block holds its whole downstream chain `waiting` without any of them
#' evaluating against missing data. Output rendering follows the status: the
#' block output is shown only while `ready` and cleared otherwise, so a block
#' leaving `ready` never displays a stale result. While not `ready` the block
#' surfaces a condition explaining why -- a `status`-phase note for `waiting`
#' and `unset`, or the raised error for `failed`. Conditions raised during
#' validation and evaluation are caught and returned to be surfaced to the app
#' user.
#'
#' Block-level user inputs (provided by the expression module) are separated
#' from output, the behavior of which can be customized via the
#' [block_output()] generic. The [block_ui()] generic can then be used to
#' control rendering of outputs.
#'
#' A front-end (such as blockr.dock) drives per-block channels that
#' [board_server()] hands to the board callback as `visibility`. Two of them
#' gate what is built and shown: `required` (which blocks it needs built and
#' evaluated) and `visible` (which blocks it has arranged on screen).
#' Requirements are a cause the front-end -- and
#' core-side features such as code export -- declare; visibility is the effect
#' the front-end reports back once it has painted a block. Rendering is gated
#' on `visible`: the render observer is suspended while a block carries no
#' visible slot and resumed once the front-end writes a non-empty string for
#' it, starting suspended so nothing renders before the first report.
#' Evaluation is gated on the *needed* set, the `required` blocks together with
#' their upstream closure over [board_links()] (recomputed only when
#' requirements or links change). A block's input data reactives stay
#' unfulfilled (they [shiny::req()] out) unless the block is needed, so a block
#' that is neither required nor feeding a required block pulls no input and
#' stays fully quiescent: its result reactive, and any observer its expression
#' server registers on the incoming data, all short-circuit and do nothing. A
#' needed but off-screen block (one feeding a required block) evaluates but
#' does not render. Block-server *construction* is prioritized the same way:
#' the needed set is instantiated first so that first paint waits only for the
#' required blocks and their upstreams, and the remaining block servers are
#' built progressively in the background. That background pass holds until the
#' front-end reports every required block as visible, so it never competes with
#' first paint. A `required` slot of `FALSE` keeps a block built but dormant
#' (ever required, not needed now); an absent slot leaves it unbuilt. Until a
#' block is built it is absent from the `board$blocks` handed to plugins and
#' callbacks, which simply see it appear once constructed. The background
#' cadence is set by the `background_construction_delay` [blockr_option()]
#' (milliseconds between successive blocks, default 50); a value of 0 disables
#' the staggering and builds every block up front. With nothing writing
#' `required` every block is needed and behaviour is unchanged; the
#' `gate_visibility` [blockr_option()] (default `TRUE`) turns gating off
#' entirely.
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
#' @param visibility Front-end channel bundle -- a list with three channels,
#' `required`, `visible` and `frozen`, each an environment of per-block
#' `reactiveVal`s, supplied by [board_server()] to gate rendering and to
#' freeze block inputs; `NULL` (the standalone default) leaves the block
#' ungated
#' @rdname block_server
#' @export
block_server.block <- function(id, x, data = list(), block_id = id,
                               edit_block = NULL, ctrl_block = NULL,
                               board = reactiveValues(),
                               update = reactiveVal(),
                               inputs_ready = reactive(TRUE),
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

      res <- reactive(
        {
          if (!isTRUE(reval_if(gate)) || !isTRUE(data_valid()) ||
                !isTRUE(state_ready())) {
            return(NULL)
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

          # Re-evaluate only when the interpolated expression or the input
          # data actually changed. Spurious invalidations reach this reactive
          # through board-wide transitions with the inputs untouched -- e.g. a
          # view switch whose visibility updates land across several flushes,
          # transiently collapsing and re-expanding the needed closure so that
          # every needed slot (and with it the whole input chain) takes a real
          # FALSE -> TRUE round trip. Upstream results are cached, so
          # `eval_data` then holds the very same objects and identical()
          # short-circuits on pointer equality. Without this guard each such
          # transition re-ran the block expression, re-evaluating whole shared
          # pipelines (data adapters included) on every first visit to a view.
          if (isTRUE(last_eval$has) &&
                identical(eval_lang, last_eval$lang) &&
                identical(eval_data, last_eval$data) &&
                identical(eval_trigger, last_eval$trigger)) {
            log_debug("skipping block ", block_id, " (inputs unchanged)")
            return(last_eval$result)
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
          last_eval$lang <- eval_lang
          last_eval$data <- eval_data
          last_eval$trigger <- eval_trigger
          last_eval$result <- result

          result
        },
        domain = session
      )

      failed <- reactive(
        {
          if (!inputs_ready() || !isTRUE(state_ready())) {
            return(FALSE)
          }

          if (!isTRUE(data_valid())) {
            return(TRUE)
          }

          res()

          length(cond$eval$error) > 0L
        },
        domain = session
      )

      block_ready <- reactive(
        isTRUE(reval_if(gate)) && inputs_ready() && isTRUE(state_ready()) &&
          !failed()
      )

      gated <- is_board(isolate(board$board)) &&
        isTRUE(blockr_option("gate_visibility", TRUE)) &&
        not_null(visibility)

      render_obs <- output_render_observer(x, block_ready, inputs_ready,
                                           state_ready, res, cond, session,
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
          state_ready = state_ready,
          failed = failed,
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

output_render_observer <- function(x, ready, inputs_ready, state_ready, res,
                                   cond, sess, suspended = FALSE) {

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

        isolate(explain_block_status(cond, NULL))

      } else {

        sess$output$result <- NULL

        reason <- if (!inputs_ready()) {
          "This block is waiting for its data input to be connected."
        } else if (!isTRUE(state_ready())) {
          "This block is waiting for its inputs to be set."
        } else {
          NULL
        }

        isolate(explain_block_status(cond, reason))
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
      do_render <- !gating_active(visibility$required) ||
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
  views <- setdiff(fn_names, ctrl)

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

  for (nm in views) {
    state[[nm]] <- freeze_state_view(live[[nm]], nm, frozen, snapshot, sess)
  }

  state
}

freeze_state_view <- function(live, nm, frozen, snapshot, sess) {

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

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
#' A block is considered ready for evaluation whenever input data is available
#' that satisfies validation ([validate_data_inputs()]) and nonempty state
#' values are available (unless otherwise instructed via `allow_empty_state`
#' in [new_block()]). Conditions raised during validation and evaluation are
#' caught and returned in order to be surfaced to the app user.
#'
#' Block-level user inputs (provided by the expression module) are separated
#' from output, the behavior of which can be customized via the
#' [block_output()] generic. The [block_ui()] generic can then be used to
#' control rendering of outputs.
#'
#' When a front-end (such as blockr.dock) drives the `visible` write-channel
#' that [board_server()] hands to the board callback, naming the block IDs
#' currently on screen, rendering is gated on visibility: the render observer
#' is suspended while a block is off screen and resumed once it is on screen,
#' starting suspended so nothing renders before the front-end first reports.
#' Evaluation is demand-driven. A block's result is a reactive that computes
#' only when pulled, either by its own render (while on screen) or by a
#' downstream block pulling it through [board_links()]. An off-screen block
#' with no on-screen descendant is therefore never pulled and stays fully
#' quiescent, evaluating, validating and rendering nothing, while the reactive
#' graph computes the upstream closure of each visible block on its own. With
#' nothing driving `visible` every block renders and behaviour is unchanged;
#' the `gate_visibility` [blockr_option()] (default `TRUE`) turns gating off
#' entirely.
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
#' @rdname block_server
#' @export
block_server.block <- function(id, x, data = list(), block_id = id,
                               edit_block = NULL, ctrl_block = NULL,
                               board = reactiveValues(),
                               update = reactiveVal(), ...) {

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      cond <- reactiveValues(
        data = NULL,
        state = NULL,
        eval = NULL,
        render = NULL,
        block = NULL
      )

      reorder_dots_observer(data, session)

      exp <- check_expr_val(
        expr_server(x, data),
        x
      )

      lang <- reactive(
        exprs_to_lang(exp$expr())
      )

      state <- exp$state

      dat <- reactive(
        {
          res <- lapply(data[names(data) != "...args"], reval)

          if ("...args" %in% names(data)) {
            args <- lapply(reactiveValuesToList(data[["...args"]]), reval_if)
            res <- c(res, list(`...args` = args))
          }

          res
        }
      )

      data_valid <- validate_block_reactive(block_id, x, dat, cond, session)

      state_set <- state_check_reactive(block_id, x, state, data_valid, cond,
                                        session)

      dat_eval <- reactive(
        {
          req(state_set())
          lapply(state, reval_if)
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

      res <- reactive(
        {
          if (!isTRUE(reval_if(gate)) || !state_set()) {
            return(NULL)
          }

          eval_data <- dat_eval()

          log_debug("evaluating block ", block_id)

          isolate(
            capture_conditions(
              eval_impl(x, lang(), eval_data),
              cond,
              "eval",
              session = session
            )
          )
        },
        domain = session
      )

      gated <- is_board(isolate(board$board)) &&
        isTRUE(blockr_option("gate_visibility", TRUE))

      render_obs <- output_render_observer(x, res, cond, session,
                                           suspended = gated)

      if (gated) {
        render_gate_observer(block_id, board, render_obs, session)
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

      c(
        list(
          result = res,
          expr = lang,
          state = state,
          cond = cond
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

reorder_dots_observer <- function(data, sess) {

  if ("...args" %in% names(data)) {

    observeEvent(
      names(data[["...args"]]),
      {
        arg_names <- names(data[["...args"]])
        pos_args <- grepl("[1-9][0-9]*", arg_names)

        if (any(pos_args)) {

          ind <- which(pos_args)
          ind <- c(
            ind[order(as.integer(arg_names[ind]))],
            which(!pos_args)
          )

          reorder_rv(data[["...args"]], arg_names[ind])
        }
      },
      domain = sess
    )
  }
}

validate_block_reactive <- function(id, x, dat, cond, sess) {

  if (!block_has_data_validator(x)) {
    return(reactive(TRUE, domain = sess))
  }

  reactive(
    {
      log_debug("performing input validation for block ", id)

      inp <- dat()

      isolate(
        capture_conditions(
          {
            validate_data_inputs(x, inp)
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

state_check_reactive <- function(id, x, state, data_valid, cond, sess) {

  reactive(
    {
      log_debug("checking returned state values of block ", id)

      if (!isTruthy(data_valid())) {
        return(FALSE)
      }

      err <- NULL

      allow_empty <- block_allow_empty_state(x)

      if (!isTRUE(allow_empty) && length(state)) {

        if (isFALSE(allow_empty)) {
          check <- TRUE
        } else {
          check <- setdiff(names(state), allow_empty)
        }

        ok <- lgl_ply(
          lapply(state[check], reval_if),
          Negate(is_empty),
          use_names = TRUE
        )

        if (!all(ok)) {
          err <- new_condition(
            paste0("State values ", paste_enum(names(ok)[!ok]), " are ",
                   "not yet initialized.")
          )
        }
      }

      isolate(
        if (is.null(err)) {
          if (length(cond$state$error)) {
            cond$state$error <- list()
          }
        } else {
          cond$state$error <- err
        }
      )

      is.null(err)
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

output_render_observer <- function(x, res, cond, sess, suspended = FALSE) {

  observeEvent(
    {
      block_render_trigger(x, sess)
      res()
    },
    {
      sess$output$result <- capture_conditions(
        block_output(x, res(), sess),
        cond,
        "render",
        session = sess
      )
    },
    domain = sess,
    suspended = suspended
  )
}

render_gate_observer <- function(id, board, render_obs, sess) {

  prev_render <- NA

  observe(
    {
      do_render <- isTRUE(board$visible) || id %in% board$visible

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

        new_cnds <- lapply(new_cnds, lapply, new_condition,
                           as_list = FALSE)

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
              if (!(is.character(y) || is_list_of_block_cnds(y))) {
                blockr_abort(
                  "The `cond` component of the return value for ",
                  "{class(x)[1L]} is expected to contain a nested list of ",
                  "character vectors or list of objects inheriting from ",
                  "`block_cnd`.",
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

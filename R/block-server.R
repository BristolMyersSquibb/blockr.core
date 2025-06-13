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
#' @param edit_block Block edit plugin
#' @param board Reactive values object containing board information
#' @param update Reactive value object to initiate board updates
#' @rdname block_server
#' @export
block_server.block <- function(id, x, data = list(), block_id = id,
                               edit_block = NULL, board = reactiveValues(),
                               update = reactiveVal(), ...) {

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      onStop(
        function() set_globals(NULL, session = session)
      )

      set_globals(0L, session = session)

      rv <- reactiveValues(
        data_valid = if (block_has_data_validator(x)) NULL else TRUE,
        data_cond = empty_block_condition,
        state_set = NULL,
        state_cond = empty_block_condition,
        eval_cond = empty_block_condition
      )

      reorder_dots_observer(data, session)

      res <- reactiveVal()

      exp <- check_expr_val(
        expr_server(x, data),
        x
      )

      lang <- reactive(
        exprs_to_lang(exp$expr())
      )

      dat <- reactive(
        {
          res <- lapply(data[names(data) != "...args"], reval)

          if ("...args" %in% names(data)) {
            tmp <- list(`...args` = reactiveValuesToList(data[["...args"]]))
            res <- c(res, tmp)
          }

          res
        }
      )

      validate_block_observer(block_id, x, dat, res, rv, session)
      state_check_observer(block_id, x, dat, res, exp, rv, session)
      data_eval_observer(block_id, x, dat, res, exp, lang, rv, session)

      observeEvent(
        res(),
        {
          output$result <- block_output(x, res(), session)
        },
        domain = session
      )

      call_plugin_server(
        edit_block,
        server_args = c(
          list(block_id = block_id, board = board, update = update),
          dot_args
        )
      )

      block_cond <- reactive(
        {
          if ("cond" %in% names(exp)) {
            lapply(
              reactiveValuesToList(exp[["cond"]]),
              lapply,
              new_condition,
              session = session,
              as_list = FALSE
            )
          } else {
            empty_block_condition
          }
        }
      )

      list(
        result = res,
        expr = lang,
        state = exp$state,
        cond = reactive(
          list(
            data = rv$data_cond,
            state = rv$state_cond,
            eval = rv$eval_cond,
            block = block_cond()
          )
        )
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

#' @param expr Quoted expression to evaluate in the context of `data`
#' @rdname block_server
#' @export
block_eval <- function(x, expr, data, ...) {
  UseMethod("block_eval")
}

#' @export
block_eval.block <- function(x, expr, data, ...) {
  eval(expr, data)
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

new_condition <- function(x, ..., as_list = TRUE) {

  id <- get_globals(...) + 1L
  set_globals(id, ...)

  if (inherits(x, "condition")) {
    x <- conditionMessage(x)
  }

  res <- structure(x, id = id, class = "block_cnd")

  if (!isTRUE(as_list)) {
    return(res)
  }

  list(res)
}

empty_block_condition <- list(
  error = character(),
  warning = character(),
  message = character()
)

validate_block_observer <- function(id, x, dat, res, rv, sess) {

  if (block_has_data_validator(x)) {
    observeEvent(
      dat(),
      {
        log_debug("performing input validation for block ", id)

        res(NULL)

        rv$data_cond <- empty_block_condition
        rv$state_set <- NULL

        rv$data_valid <- tryCatch(
          withCallingHandlers(
            {
              validate_data_inputs(x, dat())
              TRUE
            },
            message = function(m) {
              rv$data_cond$message <- c(
                rv$data_cond$message,
                new_condition(m, session = sess)
              )
            },
            warning = function(w) {
              rv$data_cond$warning <- c(
                rv$data_cond$warning,
                new_condition(w, session = sess)
              )
            }
          ),
          error = function(e) {
            rv$data_cond$error <- new_condition(e, session = sess)
            NULL
          }
        )
      },
      domain = sess
    )
  }
}

state_check_observer <- function(id, x, dat, res, exp, rv, sess) {

  state_check <- reactive(
    {
      dat()

      if (!isTruthy(rv$data_valid)) {
        return(NULL)
      }

      allow_empty <- block_allow_empty_state(x)

      if (isTRUE(allow_empty) || !length(exp$state)) {
        return(TRUE)
      }

      if (isFALSE(allow_empty)) {
        check <- TRUE
      } else {
        check <- setdiff(names(exp$state), allow_empty)
      }

      lgl_ply(
        lapply(exp$state[check], reval_if),
        Negate(is_empty),
        use_names = TRUE
      )
    },
    domain = sess
  )

  observeEvent(
    state_check(),
    {
      log_debug("checking returned state values of block ", id)

      res(NULL)

      ok <- state_check()

      rv$state_cond <- empty_block_condition
      rv$state_set <- NULL

      if (!all(ok)) {
        rv$state_cond$error <- new_condition(
          paste0("State values ", paste_enum(names(ok)[!ok]), " are ",
                 "not yet initialized."),
          session = sess
        )
      } else {
        rv$state_set <- TRUE
      }
    },
    domain = sess
  )
}

block_eval_trigger <- function(x, session = getDefaultReactiveDomain()) {
  UseMethod("block_eval_trigger", x)
}

#' @noRd
#' @export
block_eval_trigger.block <- function(x, session = getDefaultReactiveDomain()) {
  NULL
}

data_eval_observer <- function(id, x, dat, res, exp, lang, rv, sess) {

  dat_eval <- reactive(
    {
      req(rv$state_set)
      lapply(exp$state, reval_if)
      try(lang(), silent = TRUE)

      res <- dat()

      if ("...args" %in% names(res)) {
        res <- c(res[names(res) != "...args"], res[["...args"]])
      }

      block_eval_trigger(x, sess)

      res
    },
    domain = sess
  )

  observeEvent(
    dat_eval(),
    {
      log_debug("evaluating block ", id)

      rv$eval_cond <- empty_block_condition

      out <- tryCatch(
        withCallingHandlers(
          {
            block_eval(x, lang(), dat_eval())
          },
          message = function(m) {
            rv$eval_cond$message <- c(
              rv$eval_cond$message,
              new_condition(m, session = sess)
            )
          },
          warning = function(w) {
            rv$eval_cond$warning <- c(
              rv$eval_cond$warning,
              new_condition(w, session = sess)
            )
          }
        ),
        error = function(e) {
          rv$eval_cond$error <- new_condition(e, session = sess)
          NULL
        }
      )

      res(out)
    },
    domain = sess
  )
}

check_expr_val <- function(val, x) {

  observeEvent(
    val,
    {
      cls <- class(x)[1L]

      if (!is.list(val)) {
        abort(
          paste(
            "The block server for", cls,
            "is expected to return a list."
          ),
          class = "expr_server_return_type_invalid"
        )
      }

      required <- c("expr", "state")

      if (!all(required %in% names(val))) {
        abort(
          paste0(
            "The block server for ", cls, " is expected to return ",
            "values ", paste_enum(setdiff(required, names(val))), "."
          ),
          class = "expr_server_return_component_missing"
        )
      }

      if (!is.reactive(val[["expr"]])) {
        abort(
          paste(
            "The `expr` component of the return value for", cls,
            "is expected to be a reactive."
          ),
          class = "expr_server_return_type_invalid"
        )
      }

      if (!is.list(val[["state"]])) {
        abort(
          paste(
            "The `state` component of the return value for", cls,
            "is expected to be a list."
          ),
          class = "expr_server_return_type_invalid"
        )
      }

      if ("cond" %in% names(val)) {

        if (!is.reactivevalues(val[["cond"]])) {
          abort(
            paste(
              "The `cond` component of the return value for", cls,
              "is expected to be a `reactiveValues` object."
            ),
            class = "expr_server_return_type_invalid"
          )
        }

        conds <- c("message", "warning", "error")

        if (!all(names(val[["cond"]]) %in% conds)) {
          abort(
            paste0(
              "The `cond` component of the return value for ", cls,
              " is expected to contain components ",
              paste_enum(conds), "."
            ),
            class = "expr_server_return_type_invalid"
          )
        }
      }

      expected <- block_ctor_inputs(x)
      current <- names(val[["state"]])
      missing <- setdiff(expected, current)

      if (length(missing)) {
        abort(
          paste0(
            "The `state` component of the return value for ", cls,
            " is expected to additionally return ",
            paste_enum(missing)
          ),
          class = "expr_server_return_state_invalid"
        )
      }
    },
    once = TRUE
  )

  val
}

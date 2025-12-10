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

      rv <- reactiveValues(
        data_valid = if (block_has_data_validator(x)) NULL else TRUE,
        state_set = NULL
      )

      cond <- reactiveValues(
        data = NULL,
        state = NULL,
        eval = NULL,
        render = NULL,
        block = NULL
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

      validate_block_observer(block_id, x, dat, res, rv, cond, session)
      state_check_observer(block_id, x, dat, res, exp, rv, cond, session)
      data_eval_observer(block_id, x, dat, res, exp, lang, rv, cond, session)
      output_render_observer(x, res, cond, session)

      eb_res <- call_plugin_server(
        edit_block,
        server_args = c(
          list(block_id = block_id, board = board, update = update),
          dot_args
        )
      )

      block_cond_observer(exp, cond, session)

      c(
        list(
          result = res,
          expr = lang,
          state = exp$state,
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

#' @param expr Quoted expression to evaluate in the context of `data`
#' @param env Environment in which to evaluate `expr`
#' @rdname block_server
#' @export
block_eval <- function(x, expr, env, ...) {
  UseMethod("block_eval")
}

#' @export
block_eval.block <- function(x, expr, env, ...) {
  eval(expr, env)
}

#' @rdname block_server
#' @export
eval_env <- function(data) {
  list2env(data, parent = baseenv())
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

validate_block_observer <- function(id, x, dat, res, rv, cond, sess) {

  if (block_has_data_validator(x)) {
    observeEvent(
      dat(),
      {
        log_debug("performing input validation for block ", id)

        res(NULL)

        rv$state_set <- NULL

        rv$data_valid <- capture_conditions(
          {
            validate_data_inputs(x, dat())
            TRUE
          },
          cond,
          "data",
          session = sess
        )
      },
      domain = sess
    )
  }
}

state_check_observer <- function(id, x, dat, res, exp, rv, cond, sess) {

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

      rv$state_set <- NULL

      if (!all(ok)) {
        cond$state$error <- new_condition(
          paste0("State values ", paste_enum(names(ok)[!ok]), " are ",
                 "not yet initialized.")
        )
      } else {
        cond$state$error <- list()
        rv$state_set <- TRUE
      }
    },
    domain = sess
  )
}

#' @param session Shiny session object
#' @rdname block_server
#' @export
block_eval_trigger <- function(x, session = get_session()) {
  UseMethod("block_eval_trigger", x)
}

#' @export
block_eval_trigger.block <- function(x, session = get_session()) {
  NULL
}

data_eval_observer <- function(id, x, dat, res, exp, lang, rv, cond, sess) {

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

      out <- capture_conditions(
        eval_impl(x, lang(), dat_eval()),
        cond,
        "eval",
        session = sess
      )

      res(out)
    },
    domain = sess
  )
}

eval_impl <- function(x, expr, dat) {

  if (identical(block_expr_type(x), "bquoted")) {
    expr <- do.call(bquote, list(expr, dat, splice = is.na(block_arity(x))))
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

output_render_observer <- function(x, res, cond, sess) {

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
    domain = sess
  )
}

block_cond_observer <- function(exp, cond, sess) {

  if ("cond" %in% names(exp)) {

    conds <- reactive(
      {
        include <- coal(
          get_board_option_or_null("show_conditions", sess),
          match.arg(
            blockr_option("show_conditions", c("warning", "error")),
            c("message", "warning", "error"),
            several.ok = TRUE
          )
        )
        set_names(
          lapply(reactiveValuesToList(exp[["cond"]])[include], coal, list()),
          include
        )
      }
    )

    observeEvent(
      conds(),
      {
        new_cnds <- conds()
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
      class = "expr_server_return_state_invalid"
    )
  }

  expected <- block_ctor_inputs(x)
  current <- names(val[["state"]])
  missing <- setdiff(expected, current)

  if (length(missing)) {
    blockr_abort(
      "The `state` component of the return value for {class(x)[1L]} is ",
      "expected to additionally return {missing}.",
      class = "expr_server_return_state_invalid"
    )
  }

  disallowed <- intersect(current, static_block_arguments())

  if (length(disallowed)) {
    blockr_abort(
      "The `state` component of the return value for {class(x)[1L]} is ",
      "is not allowed to return components {disallowed}.",
      class = "expr_server_return_state_invalid"
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
